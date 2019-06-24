use super::fails;
use crate::connection::network_handler::message_processor::{
    MessageManager, MessageProcessor, ProcessResult,
};
use concordium_common::{
    functor::{UnitFunction, UnitFunctor},
    UCursor,
};

use failure::{Error, Fallible};
use mio::{
    net::{TcpListener, TcpStream},
    Event, Poll, Token,
};
use snow::Keypair;

use std::{
    cell::RefCell,
    collections::HashSet,
    net::SocketAddr,
    rc::Rc,
    sync::{
        atomic::{AtomicUsize, Ordering},
        mpsc::Sender,
        Arc, RwLock,
    },
};

use crate::{
    common::{
        get_current_stamp, serialization::serialize_into_memory, NetworkRawRequest, P2PNodeId,
        P2PPeer, PeerType, RemotePeer,
    },
    connection::{default_noise_params, Connection, ConnectionBuilder, P2PEvent},
    dumper::DumpItem,
    network::{Buckets, NetworkId, NetworkMessage, NetworkRequest},
    p2p::{
        banned_nodes::BannedNode, peer_statistics::PeerStatistic,
        tls_server_private::TlsServerPrivate,
    },
    stats_export_service::StatsExportService,
    utils::clone_snow_keypair,
};

pub type PreHandshakeCW = UnitFunction<SocketAddr>;
pub type PreHandshake = UnitFunctor<SocketAddr>;

pub struct TlsServerBuilder {
    server:                  Option<TcpListener>,
    event_log:               Option<Sender<P2PEvent>>,
    self_peer:               Option<P2PPeer>,
    buckets:                 Option<Arc<RwLock<Buckets>>>,
    stats_export_service:    Option<Arc<RwLock<StatsExportService>>>,
    blind_trusted_broadcast: Option<bool>,
    networks:                Option<HashSet<NetworkId>>,
    max_allowed_peers:       Option<u16>,
}

impl Default for TlsServerBuilder {
    fn default() -> Self { TlsServerBuilder::new() }
}

impl TlsServerBuilder {
    pub fn new() -> TlsServerBuilder {
        TlsServerBuilder {
            server:                  None,
            event_log:               None,
            self_peer:               None,
            buckets:                 None,
            stats_export_service:    None,
            blind_trusted_broadcast: None,
            networks:                None,
            max_allowed_peers:       None,
        }
    }

    pub fn build(self) -> Fallible<TlsServer> {
        if let (
            Some(networks),
            Some(server),
            Some(self_peer),
            Some(buckets),
            Some(blind_trusted_broadcast),
            Some(max_allowed_peers),
        ) = (
            self.networks,
            self.server,
            self.self_peer,
            self.buckets,
            self.blind_trusted_broadcast,
            self.max_allowed_peers,
        ) {
            let mdptr = Arc::new(RwLock::new(TlsServerPrivate::new(
                networks,
                self.stats_export_service.clone(),
            )));
            let key_pair = snow::Builder::new(default_noise_params()).generate_keypair()?;

            let mut mself = TlsServer {
                server,
                next_id: AtomicUsize::new(2),
                key_pair,
                event_log: self.event_log,
                self_peer,
                stats_export_service: self.stats_export_service,
                buckets,
                message_processor: Arc::new(RwLock::new(MessageProcessor::new())),
                dptr: mdptr,
                blind_trusted_broadcast,
                prehandshake_validations: PreHandshake::new(),
                log_dumper: None,
                max_allowed_peers,
            };

            mself.add_default_prehandshake_validations();
            mself.setup_default_message_handler();
            Ok(mself)
        } else {
            Err(Error::from(fails::MissingFieldsOnTlsServerBuilder))
        }
    }

    pub fn set_server(mut self, s: TcpListener) -> TlsServerBuilder {
        self.server = Some(s);
        self
    }

    pub fn set_self_peer(mut self, sp: P2PPeer) -> TlsServerBuilder {
        self.self_peer = Some(sp);
        self
    }

    pub fn set_event_log(mut self, el: Option<Sender<P2PEvent>>) -> TlsServerBuilder {
        self.event_log = el;
        self
    }

    pub fn set_buckets(mut self, b: Arc<RwLock<Buckets>>) -> TlsServerBuilder {
        self.buckets = Some(b);
        self
    }

    pub fn set_stats_export_service(
        mut self,
        ses: Option<Arc<RwLock<StatsExportService>>>,
    ) -> TlsServerBuilder {
        self.stats_export_service = ses;
        self
    }

    pub fn set_blind_trusted_broadcast(mut self, btb: bool) -> TlsServerBuilder {
        self.blind_trusted_broadcast = Some(btb);
        self
    }

    pub fn set_networks(mut self, n: HashSet<NetworkId>) -> TlsServerBuilder {
        self.networks = Some(n);
        self
    }

    pub fn set_max_allowed_peers(mut self, max_allowed_peers: u16) -> TlsServerBuilder {
        self.max_allowed_peers = Some(max_allowed_peers);
        self
    }
}

pub struct TlsServer {
    server:                   TcpListener,
    next_id:                  AtomicUsize,
    key_pair:                 Keypair,
    event_log:                Option<Sender<P2PEvent>>,
    self_peer:                P2PPeer,
    buckets:                  Arc<RwLock<Buckets>>,
    stats_export_service:     Option<Arc<RwLock<StatsExportService>>>,
    message_processor:        Arc<RwLock<MessageProcessor>>,
    dptr:                     Arc<RwLock<TlsServerPrivate>>,
    blind_trusted_broadcast:  bool,
    prehandshake_validations: PreHandshake,
    log_dumper:               Option<Sender<DumpItem>>,
    max_allowed_peers:        u16,
}

impl TlsServer {
    pub fn log_event(&self, event: P2PEvent) {
        if let Some(ref log) = self.event_log {
            if let Err(e) = log.send(event) {
                error!("Couldn't send error {:?}", e)
            }
        }
    }

    #[inline]
    pub fn set_network_request_sender(&mut self, sender: Sender<NetworkRawRequest>) {
        write_or_die!(self.dptr).set_network_request_sender(sender);
    }

    #[inline]
    pub fn find_connection_by_token(&self, token: Token) -> Option<Rc<RefCell<Connection>>> {
        read_or_die!(self.dptr)
            .find_connection_by_token(token)
            .cloned()
    }

    pub fn get_self_peer(&self) -> P2PPeer { self.self_peer.clone() }

    #[inline]
    pub fn networks(&self) -> Arc<RwLock<HashSet<NetworkId>>> {
        Arc::clone(&read_or_die!(self.dptr).networks)
    }

    pub fn remove_network(&mut self, network_id: NetworkId) {
        write_or_die!(self.dptr).remove_network(network_id)
    }

    pub fn add_network(&mut self, network_id: NetworkId) {
        write_or_die!(self.dptr).add_network(network_id)
    }

    /// Returns true if `addr` is in the `unreachable_nodes` list.
    pub fn is_unreachable(&self, addr: SocketAddr) -> bool {
        read_or_die!(self.dptr).unreachable_nodes.contains(addr)
    }

    /// Adds the `addr` to the `unreachable_nodes` list.
    pub fn add_unreachable(&mut self, addr: SocketAddr) -> bool {
        write_or_die!(self.dptr).unreachable_nodes.insert(addr)
    }

    pub fn get_peer_stats(&self, nids: &[NetworkId]) -> Vec<PeerStatistic> {
        write_or_die!(self.dptr).get_peer_stats(nids)
    }

    pub fn ban_node(&mut self, peer: BannedNode) -> bool { write_or_die!(self.dptr).ban_node(peer) }

    pub fn unban_node(&mut self, peer: BannedNode) -> bool {
        write_or_die!(self.dptr).unban_node(peer)
    }

    pub fn get_banlist(&self) -> Vec<BannedNode> { read_or_die!(self.dptr).get_banlist() }

    pub fn accept(&mut self, poll: &mut Poll, self_peer: P2PPeer) -> Fallible<()> {
        let (socket, addr) = self.server.accept()?;

        debug!(
            "Accepting new connection from {:?} to {:?}:{}",
            addr,
            self_peer.ip(),
            self_peer.port()
        );

        if let Err(e) = self.prehandshake_validations.run_callbacks(&addr) {
            return Err(Error::from(e));
        }
        self.log_event(P2PEvent::ConnectEvent(addr));

        let token = Token(self.next_id.fetch_add(1, Ordering::SeqCst));
        let networks = self.networks();
        let key_pair = clone_snow_keypair(&self.key_pair);

        let mut conn = ConnectionBuilder::default()
            .set_socket(socket)
            .set_token(token)
            .set_key_pair(key_pair)
            .set_local_peer(self_peer)
            .set_remote_peer(RemotePeer::PreHandshake(PeerType::Node, addr))
            .set_stats_export_service(self.stats_export_service.clone())
            .set_event_log(self.event_log.clone())
            .set_local_end_networks(networks)
            .set_buckets(Arc::clone(&self.buckets))
            .set_blind_trusted_broadcast(self.blind_trusted_broadcast)
            .set_log_dumper(self.log_dumper.clone())
            .set_network_request_sender(Some(
                read_or_die!(self.dptr).network_request_sender.clone(),
            ))
            .build()?;

        self.register_message_handlers(&mut conn);

        conn.setup_pre_handshake();
        conn.setup_post_handshake();

        let register_status = conn.register(poll);
        safe_write!(self.dptr)?.add_connection(conn);

        register_status
    }

    pub fn connect(
        &mut self,
        peer_type: PeerType,
        poll: &mut Poll,
        addr: SocketAddr,
        peer_id_opt: Option<P2PNodeId>,
        self_peer: &P2PPeer,
    ) -> Fallible<()> {
        if peer_type == PeerType::Node {
            let current_peer_count = read_or_die!(self.dptr)
                .connections_posthandshake_count(Some(PeerType::Bootstrapper));
            if current_peer_count > self.max_allowed_peers {
                return Err(Error::from(fails::MaxmimumAmountOfPeers {
                    max_allowed_peers: self.max_allowed_peers,
                    number_of_peers:   current_peer_count,
                }));
            }
        }

        if peer_type == PeerType::Node && self.is_unreachable(addr) {
            error!("Node marked as unreachable, so not allowing the connection");
            return Err(Error::from(fails::UnreachablePeerError));
        }

        // Avoid duplicate ip+port peers
        if self_peer.addr == addr {
            return Err(Error::from(fails::DuplicatePeerError { peer_id_opt, addr }));
        }

        // Avoid duplicate Id entries
        if let Some(peer_id) = peer_id_opt {
            if safe_read!(self.dptr)?
                .find_connection_by_id(peer_id)
                .is_some()
            {
                return Err(Error::from(fails::DuplicatePeerError { peer_id_opt, addr }));
            }
        }

        // Avoid duplicate ip+port connections
        if safe_read!(self.dptr)?
            .find_connection_by_ip_addr(addr)
            .is_some()
        {
            return Err(Error::from(fails::DuplicatePeerError { peer_id_opt, addr }));
        }

        match TcpStream::connect(&addr) {
            Ok(socket) => {
                if let Some(ref service) = &self.stats_export_service {
                    safe_write!(service)?.conn_received_inc();
                };
                let token = Token(self.next_id.fetch_add(1, Ordering::SeqCst));
                let networks = self.networks();
                let keypair = clone_snow_keypair(&self.key_pair);
                let mut conn = ConnectionBuilder::default()
                    .set_socket(socket)
                    .set_token(token)
                    .set_key_pair(keypair)
                    .set_as_initiator(true)
                    .set_local_peer(self_peer.clone())
                    .set_remote_peer(RemotePeer::PreHandshake(peer_type, addr))
                    .set_stats_export_service(self.stats_export_service.clone())
                    .set_event_log(self.event_log.clone())
                    .set_local_end_networks(Arc::clone(&networks))
                    .set_buckets(Arc::clone(&self.buckets))
                    .set_blind_trusted_broadcast(self.blind_trusted_broadcast)
                    .set_log_dumper(self.log_dumper.clone())
                    .set_network_request_sender(Some(
                        read_or_die!(self.dptr).network_request_sender.clone(),
                    ))
                    .build()?;

                self.register_message_handlers(&mut conn);
                conn.setup_pre_handshake();
                conn.setup_post_handshake();
                conn.register(poll)?;

                safe_write!(self.dptr)?.add_connection(conn);
                self.log_event(P2PEvent::ConnectEvent(addr));
                debug!("Requesting handshake from new peer {}", addr,);
                let self_peer = self.get_self_peer();

                if let Some(ref rc_conn) = safe_read!(self.dptr)?.find_connection_by_token(token) {
                    let handshake_request = NetworkMessage::NetworkRequest(
                        NetworkRequest::Handshake(self_peer, safe_read!(networks)?.clone(), vec![]),
                        Some(get_current_stamp()),
                        None,
                    );
                    let handshake_request_data = serialize_into_memory(&handshake_request, 256)?;

                    let mut conn = rc_conn.borrow_mut();
                    conn.async_send(UCursor::from(handshake_request_data))?;
                    conn.set_measured_handshake_sent();
                }
                Ok(())
            }
            Err(e) => {
                if peer_type == PeerType::Node && !self.add_unreachable(addr) {
                    error!("Can't insert unreachable peer!");
                }
                into_err!(Err(e))
            }
        }
    }

    pub fn get_all_current_peers(&self, peer_type: Option<PeerType>) -> Box<[P2PNodeId]> {
        read_or_die!(self.dptr).get_all_current_peers(peer_type)
    }

    #[inline]
    pub fn conn_event(&mut self, event: &Event) -> Fallible<ProcessResult> {
        write_or_die!(self.dptr).conn_event(event)
    }

    pub fn cleanup_connections(&self, max_peers_number: u16, poll: &mut Poll) -> Fallible<()> {
        write_or_die!(self.dptr).cleanup_connections(self.peer_type(), max_peers_number, poll)
    }

    pub fn liveness_check(&self) -> Fallible<()> { write_or_die!(self.dptr).liveness_check() }

    /// It sends `data` message over all filtered connections.
    ///
    /// # Arguments
    /// * `data` - Raw message.
    /// * `filter_conn` - It will send using all connection, where this function
    ///   returns `true`.
    /// * `send_status` - It will called after each sent, to notify the result
    ///   of the operation.
    ///  # Returns
    /// * connections the packet was written to
    pub fn send_over_all_connections(
        &self,
        data: UCursor,
        filter_conn: &dyn Fn(&Connection) -> bool,
        send_status: &dyn Fn(&Connection, Fallible<()>),
    ) -> usize {
        write_or_die!(self.dptr).send_over_all_connections(data, filter_conn, send_status)
    }

    #[inline]
    pub fn peer_type(&self) -> PeerType { self.self_peer.peer_type() }

    #[inline]
    pub fn blind_trusted_broadcast(&self) -> bool { self.blind_trusted_broadcast }

    /// It setups default message handler at TLSServer level.
    fn setup_default_message_handler(&mut self) {
        let cloned_dptr = Arc::clone(&self.dptr);
        let banned_nodes = Rc::clone(&read_or_die!(cloned_dptr).banned_peers);
        let to_disconnect = Rc::clone(&read_or_die!(cloned_dptr).to_disconnect);
        write_or_die!(self.message_processor).add_request_action(make_atomic_callback!(
            move |req: &NetworkRequest| {
                if let NetworkRequest::Handshake(ref peer, ..) = req {
                    if banned_nodes.borrow().is_id_banned(peer.id()) {
                        to_disconnect.borrow_mut().push_back(peer.id());
                    }
                }
                Ok(())
            }
        ));
    }

    /// It adds all message handler callback to this connection.
    fn register_message_handlers(&self, conn: &mut Connection) {
        let mh = &read_or_die!(self.message_processor);
        Rc::clone(&conn.common_message_processor)
            .borrow_mut()
            .add(mh);
    }

    fn add_default_prehandshake_validations(&mut self) {
        self.prehandshake_validations
            .add_callback(self.make_check_banned());
    }

    fn make_check_banned(&self) -> PreHandshakeCW {
        let cloned_dptr = Arc::clone(&self.dptr);
        make_atomic_callback!(move |sockaddr: &SocketAddr| {
            if safe_read!(cloned_dptr)?.addr_is_banned(*sockaddr) {
                Err(Error::from(fails::BannedNodeRequestedConnectionError))
            } else {
                Ok(())
            }
        })
    }

    pub fn dump_start(&mut self, log_dumper: Sender<DumpItem>) {
        self.log_dumper = Some(log_dumper);
        write_or_die!(self.dptr).dump_all_connections(self.log_dumper.clone());
    }

    pub fn dump_stop(&mut self) {
        self.log_dumper = None;
        write_or_die!(self.dptr).dump_all_connections(None);
    }

    pub fn add_notification(&self, func: UnitFunction<NetworkMessage>) {
        write_or_die!(self.message_processor).add_notification(Arc::clone(&func));
        write_or_die!(self.dptr).add_notification(func);
    }
}

#[cfg(test)]
impl TlsServer {
    pub fn get_private_tls(&self) -> Arc<RwLock<TlsServerPrivate>> { Arc::clone(&self.dptr) }
}

impl MessageManager for TlsServer {
    fn message_processor(&self) -> Arc<RwLock<MessageProcessor>> {
        Arc::clone(&self.message_processor)
    }
}
