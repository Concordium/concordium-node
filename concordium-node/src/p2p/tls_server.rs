use super::fails;
use failure::{bail, Fallible};
use mio::{
    net::{TcpListener, TcpStream},
    Event, Poll, Token,
};
use rustls::{ClientConfig, ClientSession, ServerConfig, ServerSession};
use std::{
    collections::HashSet,
    net::{IpAddr, SocketAddr},
    rc::Rc,
    sync::{
        atomic::{AtomicUsize, Ordering},
        mpsc::Sender,
        Arc, RwLock,
    },
};
use webpki::DNSNameRef;

use crate::{
    common::functor::afunctor::{AFunctor, AFunctorCW},
    prometheus_exporter::PrometheusServer,
};

use crate::{
    common::{P2PNodeId, P2PPeer, PeerType},
    connection::{Connection, MessageHandler, MessageManager, P2PEvent},
    network::{Buckets, NetworkId, NetworkMessage, NetworkRequest},
};

use crate::p2p::{peer_statistics::PeerStatistic, tls_server_private::TlsServerPrivate};

pub type PreHandshakeCW = AFunctorCW<SocketAddr>;
pub type PreHandshake = AFunctor<SocketAddr>;

pub struct TlsServer {
    server:                   TcpListener,
    next_id:                  AtomicUsize,
    server_tls_config:        Arc<ServerConfig>,
    client_tls_config:        Arc<ClientConfig>,
    event_log:                Option<Sender<P2PEvent>>,
    self_peer:                P2PPeer,
    buckets:                  Arc<RwLock<Buckets>>,
    prometheus_exporter:      Option<Arc<RwLock<PrometheusServer>>>,
    message_handler:          Arc<RwLock<MessageHandler>>,
    dptr:                     Arc<RwLock<TlsServerPrivate>>,
    blind_trusted_broadcast:  bool,
    prehandshake_validations: PreHandshake,
}

impl TlsServer {
    pub fn new(
        server: TcpListener,
        server_cfg: Arc<ServerConfig>,
        client_cfg: Arc<ClientConfig>,
        event_log: Option<Sender<P2PEvent>>,
        self_peer: P2PPeer,
        prometheus_exporter: Option<Arc<RwLock<PrometheusServer>>>,
        networks: HashSet<NetworkId>,
        buckets: Arc<RwLock<Buckets>>,
        blind_trusted_broadcast: bool,
    ) -> Self {
        let mdptr = Arc::new(RwLock::new(TlsServerPrivate::new(
            networks,
            prometheus_exporter.clone(),
        )));

        let mut mself = TlsServer {
            server,
            next_id: AtomicUsize::new(2),
            server_tls_config: server_cfg,
            client_tls_config: client_cfg,
            event_log,
            self_peer,
            prometheus_exporter,
            buckets,
            message_handler: Arc::new(RwLock::new(MessageHandler::new())),
            dptr: mdptr,
            blind_trusted_broadcast,
            prehandshake_validations: PreHandshake::new("TlsServer::Accept"),
        };
        mself.add_default_prehandshake_validations();
        mself.setup_default_message_handler();
        mself
    }

    pub fn log_event(&self, event: P2PEvent) {
        if let Some(ref log) = self.event_log {
            if let Err(e) = log.send(event) {
                error!("Couldn't send error {:?}", e)
            }
        }
    }

    pub fn get_self_peer(&self) -> P2PPeer { self.self_peer.clone() }

    #[inline]
    pub fn networks(&self) -> Arc<RwLock<HashSet<NetworkId>>> {
        Arc::clone(&self.dptr.read().unwrap().networks)
    }

    pub fn remove_network(&mut self, network_id: NetworkId) -> Fallible<()> {
        safe_write!(self.dptr)?.remove_network(network_id)
    }

    pub fn add_network(&mut self, network_id: NetworkId) -> Fallible<()> {
        safe_write!(self.dptr)?.add_network(network_id)
    }

    /// It returns true if `ip` at port `port` is in `unreachable_nodes` list.
    pub fn is_unreachable(&self, ip: IpAddr, port: u16) -> bool {
        self.dptr
            .read()
            .unwrap()
            .unreachable_nodes
            .contains(ip, port)
    }

    /// It adds the pair `ip`,`port` to its `unreachable_nodes` list.
    pub fn add_unreachable(&mut self, ip: IpAddr, port: u16) -> bool {
        self.dptr
            .write()
            .unwrap()
            .unreachable_nodes
            .insert(ip, port)
    }

    pub fn get_peer_stats(&self, nids: &[NetworkId]) -> Vec<PeerStatistic> {
        self.dptr.write().unwrap().get_peer_stats(nids)
    }

    pub fn ban_node(&mut self, peer: P2PPeer) -> bool { self.dptr.write().unwrap().ban_node(peer) }

    pub fn unban_node(&mut self, peer: &P2PPeer) -> bool {
        self.dptr.write().unwrap().unban_node(peer)
    }

    pub fn accept(&mut self, poll: &mut Poll, self_peer: P2PPeer) -> Fallible<()> {
        let (socket, addr) = self.server.accept()?;
        debug!(
            "Accepting new connection from {:?} to {:?}:{}",
            addr,
            self_peer.ip(),
            self_peer.port()
        );

        if let Err(e) = self.prehandshake_validations.run_callbacks(&addr) {
            bail!(e);
        }

        self.log_event(P2PEvent::ConnectEvent(addr.ip().to_string(), addr.port()));

        let tls_session = ServerSession::new(&self.server_tls_config);
        let token = Token(self.next_id.fetch_add(1, Ordering::SeqCst));

        let networks = self.networks();
        let mut conn = Connection::new(
            socket,
            token,
            Some(tls_session),
            None,
            self_peer,
            self.prometheus_exporter.clone(),
            self.event_log.clone(),
            networks,
            Arc::clone(&self.buckets),
            self.blind_trusted_broadcast,
        );
        self.register_message_handlers(&mut conn);

        let register_status = conn.register(poll);
        self.dptr.write().unwrap().add_connection(conn);

        register_status
    }

    pub fn connect(
        &mut self,
        peer_type: PeerType,
        poll: &mut Poll,
        ip: IpAddr,
        port: u16,
        peer_id_opt: Option<P2PNodeId>,
        self_peer: &P2PPeer,
    ) -> Fallible<()> {
        if peer_type == PeerType::Node && self.is_unreachable(ip, port) {
            error!("Node marked as unreachable, so not allowing the connection");
            bail!(fails::UnreachablePeerError);
        }

        // Avoid duplicate ip+port peers
        if self_peer.ip() == ip && self_peer.port() == port {
            bail!(fails::DuplicatePeerError);
        }

        // Avoid duplicate Id entries
        if let Some(peer_id) = peer_id_opt {
            if safe_read!(self.dptr)?
                .find_connection_by_id(peer_id)
                .is_some()
            {
                bail!(fails::DuplicatePeerError);
            }
        }

        // Avoid duplicate ip+port connections
        if safe_read!(self.dptr)?
            .find_connection_by_ip_addr(ip, port)
            .is_some()
        {
            bail!(fails::DuplicatePeerError);
        }

        match TcpStream::connect(&SocketAddr::new(ip, port)) {
            Ok(socket) => {
                if let Some(ref prom) = &self.prometheus_exporter {
                    safe_write!(prom)?
                        .conn_received_inc()
                        .map_err(|e| error!("{}", e))
                        .ok();
                };
                let tls_session = ClientSession::new(
                    &self.client_tls_config,
                    DNSNameRef::try_from_ascii_str(&"node.concordium.com")
                        .unwrap_or_else(|e| panic!("The error is: {:?}", e)),
                );

                let token = Token(self.next_id.fetch_add(1, Ordering::SeqCst));

                let networks = self.networks();
                let mut conn = Connection::new(
                    socket,
                    token,
                    None,
                    Some(tls_session),
                    self_peer.clone(),
                    self.prometheus_exporter.clone(),
                    self.event_log.clone(),
                    Arc::clone(&networks),
                    Arc::clone(&self.buckets),
                    self.blind_trusted_broadcast,
                );

                self.register_message_handlers(&mut conn);
                conn.register(poll)?;

                self.dptr.write().unwrap().add_connection(conn);
                self.log_event(P2PEvent::ConnectEvent(ip.to_string(), port));
                debug!(
                    "Requesting handshake from new peer {}:{}",
                    ip.to_string(),
                    port
                );
                let self_peer = self.get_self_peer();

                if let Some(ref rc_conn) = self.dptr.read().unwrap().find_connection_by_token(token)
                {
                    let mut conn = rc_conn.borrow_mut();
                    conn.serialize_bytes(
                        &NetworkRequest::Handshake(
                            self_peer,
                            safe_read!(networks)?.clone(),
                            vec![],
                        )
                        .serialize(),
                    )?;
                    conn.set_measured_handshake_sent();
                }
                Ok(())
            }
            Err(e) => {
                if peer_type == PeerType::Node && !self.add_unreachable(ip, port) {
                    error!("Can't insert unreachable peer!");
                }
                into_err!(Err(e))
            }
        }
    }

    pub fn conn_event(
        &mut self,
        poll: &mut Poll,
        event: &Event,
        packet_queue: &Sender<Arc<NetworkMessage>>,
    ) -> Fallible<()> {
        self.dptr
            .write()
            .unwrap()
            .conn_event(poll, event, packet_queue)
    }

    pub fn cleanup_connections(&self, poll: &mut Poll) -> Fallible<()> {
        self.dptr
            .write()
            .unwrap()
            .cleanup_connections(self.peer_type(), poll)
    }

    pub fn liveness_check(&self) -> Fallible<()> { self.dptr.write().unwrap().liveness_check() }

    /// It sends `data` message over all filtered connections.
    ///
    /// # Arguments
    /// * `data` - Raw message.
    /// * `filter_conn` - It will send using all connection, where this function
    ///   returns `true`.
    /// * `send_status` - It will called after each sent, to notify the result
    ///   of the operation.
    pub fn send_over_all_connections(
        &self,
        data: &Vec<u8>,
        filter_conn: &dyn Fn(&Connection) -> bool,
        send_status: &dyn Fn(&Connection, Fallible<usize>),
    ) {
        self.dptr
            .write()
            .unwrap()
            .send_over_all_connections(data, filter_conn, send_status)
    }

    #[inline]
    pub fn peer_type(&self) -> PeerType { self.self_peer.peer_type() }

    #[inline]
    pub fn blind_trusted_broadcast(&self) -> bool { self.blind_trusted_broadcast }

    /// It setups default message handler at TLSServer level.
    fn setup_default_message_handler(&mut self) {}

    /// It adds all message handler callback to this connection.
    fn register_message_handlers(&self, conn: &mut Connection) {
        let mh = &self
            .message_handler
            .read()
            .expect("Couldn't read when registering message handlers");
        Rc::clone(&conn.common_message_handler)
            .borrow_mut()
            .merge(mh);
    }

    fn add_default_prehandshake_validations(&mut self) {
        self.prehandshake_validations
            .add_callback(self.make_check_banned());
    }

    fn make_check_banned(&self) -> PreHandshakeCW {
        let cloned_dptr = Arc::clone(&self.dptr);
        make_atomic_callback!(move |sockaddr: &SocketAddr| {
            if cloned_dptr.read().unwrap().addr_is_banned(sockaddr)? {
                bail!(fails::BannedNodeRequestedConnectionError);
            }
            Ok(())
        })
    }
}

impl MessageManager for TlsServer {
    fn message_handler(&self) -> Arc<RwLock<MessageHandler>> { Arc::clone(&self.message_handler) }
}
