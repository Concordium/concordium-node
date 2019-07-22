use super::fails;
use crate::connection::{
    network_handler::message_processor::{MessageManager, MessageProcessor, ProcessResult},
    MessageSendingPriority,
};
use concordium_common::{
    functor::{UnitFunction, UnitFunctor},
    stats_export_service::StatsExportService,
    UCursor,
};

use failure::{Error, Fallible};
use mio::{
    net::{TcpListener, TcpStream},
    Event, Poll, Token,
};
use rand::seq::IteratorRandom;
use snow::Keypair;

use std::{
    collections::{HashSet, VecDeque},
    net::{IpAddr, SocketAddr},
    sync::{
        atomic::{AtomicUsize, Ordering},
        mpsc::{self, SyncSender},
        Arc, RwLock,
    },
};

use crate::{
    common::{
        get_current_stamp, serialization::serialize_into_memory, NetworkRawRequest, P2PNodeId,
        P2PPeer, PeerType, RemotePeer,
    },
    connection::{Connection, ConnectionBuilder, P2PEvent},
    crypto::generate_snow_config,
    dumper::DumpItem,
    network::{Buckets, NetworkId, NetworkMessage, NetworkRequest},
    p2p::{
        banned_nodes::{BannedNode, BannedNodes},
        peer_statistics::PeerStatistic,
        unreachable_nodes::UnreachableNodes,
    },
    utils::clone_snow_keypair,
};

const MAX_FAILED_PACKETS_ALLOWED: u32 = 50;
const MAX_UNREACHABLE_MARK_TIME: u64 = 86_400_000;
const MAX_BOOTSTRAPPER_KEEP_ALIVE: u64 = 300_000;
const MAX_NORMAL_KEEP_ALIVE: u64 = 1_200_000;
const MAX_PREHANDSHAKE_KEEP_ALIVE: u64 = 120_000;

pub type PreHandshakeCW = UnitFunction<SocketAddr>;
pub type PreHandshake = UnitFunctor<SocketAddr>;

pub struct TlsServerBuilder {
    server:               Option<TcpListener>,
    event_log:            Option<SyncSender<P2PEvent>>,
    self_peer:            Option<P2PPeer>,
    buckets:              Option<Arc<RwLock<Buckets>>>,
    stats_export_service: Option<StatsExportService>,
    networks:             Option<HashSet<NetworkId>>,
    max_allowed_peers:    Option<u16>,
    noise_params:         Option<snow::params::NoiseParams>,
}

impl Default for TlsServerBuilder {
    fn default() -> Self { TlsServerBuilder::new() }
}

impl TlsServerBuilder {
    pub fn new() -> TlsServerBuilder {
        TlsServerBuilder {
            server:               None,
            event_log:            None,
            self_peer:            None,
            buckets:              None,
            stats_export_service: None,
            networks:             None,
            max_allowed_peers:    None,
            noise_params:         None,
        }
    }

    pub fn build(self) -> Fallible<TlsServer> {
        if let (
            Some(networks),
            Some(server),
            Some(self_peer),
            Some(buckets),
            Some(max_allowed_peers),
            Some(noise_params),
        ) = (
            self.networks,
            self.server,
            self.self_peer,
            self.buckets,
            self.max_allowed_peers,
            self.noise_params,
        ) {
            let key_pair = snow::Builder::new(noise_params.clone()).generate_keypair()?;
            let (network_request_sender, _) = mpsc::sync_channel(10000);

            let mut mself = TlsServer {
                server: Arc::new(server),
                next_id: Arc::new(AtomicUsize::new(2)),
                key_pair: Arc::new(key_pair),
                event_log: self.event_log,
                self_peer,
                stats_export_service: self.stats_export_service,
                buckets,
                message_processor: MessageProcessor::new(),
                prehandshake_validations: PreHandshake::new(),
                log_dumper: None,
                max_allowed_peers,
                noise_params,
                network_request_sender,
                connections: Arc::new(RwLock::new(Vec::new())),
                to_disconnect: Arc::new(RwLock::new(VecDeque::<P2PNodeId>::new())),
                unreachable_nodes: UnreachableNodes::new(),
                banned_peers: Arc::new(RwLock::new(BannedNodes::new())),
                networks: Arc::new(RwLock::new(networks)),
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

    pub fn set_event_log(mut self, el: Option<SyncSender<P2PEvent>>) -> TlsServerBuilder {
        self.event_log = el;
        self
    }

    pub fn set_buckets(mut self, b: Arc<RwLock<Buckets>>) -> TlsServerBuilder {
        self.buckets = Some(b);
        self
    }

    pub fn set_stats_export_service(mut self, ses: Option<StatsExportService>) -> TlsServerBuilder {
        self.stats_export_service = ses;
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

    pub fn set_noise_params(
        mut self,
        config: &crate::configuration::CryptoConfig,
    ) -> TlsServerBuilder {
        self.noise_params = Some(generate_snow_config(config));
        self
    }
}

#[derive(Clone)]
pub struct TlsServer {
    server:                     Arc<TcpListener>,
    next_id:                    Arc<AtomicUsize>,
    key_pair:                   Arc<Keypair>,
    event_log:                  Option<SyncSender<P2PEvent>>,
    self_peer:                  P2PPeer,
    buckets:                    Arc<RwLock<Buckets>>,
    stats_export_service:       Option<StatsExportService>,
    message_processor:          MessageProcessor,
    prehandshake_validations:   PreHandshake,
    log_dumper:                 Option<SyncSender<DumpItem>>,
    max_allowed_peers:          u16,
    noise_params:               snow::params::NoiseParams,
    pub network_request_sender: SyncSender<NetworkRawRequest>,
    connections:                Arc<RwLock<Vec<Connection>>>,
    pub to_disconnect:          Arc<RwLock<VecDeque<P2PNodeId>>>,
    pub unreachable_nodes:      UnreachableNodes,
    pub banned_peers:           Arc<RwLock<BannedNodes>>,
    pub networks:               Arc<RwLock<HashSet<NetworkId>>>,
}

impl TlsServer {
    pub fn log_event(&self, event: P2PEvent) {
        if let Some(ref log) = self.event_log {
            if let Err(e) = log.send(event) {
                error!("Couldn't send error {:?}", e)
            }
        }
    }

    pub fn get_self_peer(&self) -> P2PPeer { self.self_peer }

    #[inline]
    pub fn networks(&self) -> Arc<RwLock<HashSet<NetworkId>>> { Arc::clone(&self.networks) }

    /// Returns true if `addr` is in the `unreachable_nodes` list.
    pub fn is_unreachable(&self, addr: SocketAddr) -> bool { self.unreachable_nodes.contains(addr) }

    /// Adds the `addr` to the `unreachable_nodes` list.
    pub fn add_unreachable(&mut self, addr: SocketAddr) -> bool {
        self.unreachable_nodes.insert(addr)
    }

    pub fn accept(&mut self, poll: &RwLock<Poll>, self_peer: P2PPeer) -> Fallible<()> {
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
            .set_log_dumper(self.log_dumper.clone())
            .set_network_request_sender(Some(self.network_request_sender.clone()))
            .set_noise_params(self.noise_params.clone())
            .build()?;

        self.register_message_handlers(&mut conn);

        conn.setup_pre_handshake();
        conn.setup_post_handshake();

        let register_status = conn.register(poll);
        self.add_connection(conn);

        register_status
    }

    pub fn connect(
        &mut self,
        peer_type: PeerType,
        poll: &RwLock<Poll>,
        addr: SocketAddr,
        peer_id_opt: Option<P2PNodeId>,
        self_peer: &P2PPeer,
    ) -> Fallible<()> {
        if peer_type == PeerType::Node {
            let current_peer_count =
                self.connections_posthandshake_count(Some(PeerType::Bootstrapper));
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
            if self.find_connection_by_id(peer_id).is_some() {
                return Err(Error::from(fails::DuplicatePeerError { peer_id_opt, addr }));
            }
        }

        // Avoid duplicate ip+port connections
        if self.find_connection_by_ip_addr(addr).is_some() {
            return Err(Error::from(fails::DuplicatePeerError { peer_id_opt, addr }));
        }

        match TcpStream::connect(&addr) {
            Ok(socket) => {
                if let Some(ref service) = &self.stats_export_service {
                    service.conn_received_inc();
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
                    .set_log_dumper(self.log_dumper.clone())
                    .set_network_request_sender(Some(self.network_request_sender.clone()))
                    .set_noise_params(self.noise_params.clone())
                    .build()?;

                self.register_message_handlers(&mut conn);
                conn.setup_pre_handshake();
                conn.setup_post_handshake();
                conn.register(poll)?;

                self.add_connection(conn);
                self.log_event(P2PEvent::ConnectEvent(addr));
                debug!("Requesting handshake from new peer {}", addr,);
                let self_peer = self.get_self_peer();

                if let Some(ref mut conn) = self.find_connection_by_token(token) {
                    let handshake_request = NetworkMessage::NetworkRequest(
                        NetworkRequest::Handshake(self_peer, safe_read!(networks)?.clone(), vec![]),
                        Some(get_current_stamp()),
                        None,
                    );
                    let handshake_request_data = serialize_into_memory(&handshake_request, 256)?;

                    conn.async_send(
                        UCursor::from(handshake_request_data),
                        MessageSendingPriority::High,
                    )?;
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

    #[inline]
    pub fn peer_type(&self) -> PeerType { self.self_peer.peer_type() }

    /// It setups default message handler at TLSServer level.
    fn setup_default_message_handler(&mut self) {
        let banned_nodes = Arc::clone(&self.banned_peers);
        let to_disconnect = Arc::clone(&self.to_disconnect);
        self.message_processor
            .add_request_action(make_atomic_callback!(move |req: &NetworkRequest| {
                if let NetworkRequest::Handshake(ref peer, ..) = req {
                    if read_or_die!(banned_nodes).is_id_banned(peer.id()) {
                        write_or_die!(to_disconnect).push_back(peer.id());
                    }
                }
                Ok(())
            }));
    }

    /// It adds all message handler callback to this connection.
    fn register_message_handlers(&self, conn: &mut Connection) {
        let mh = self.message_processor.clone();
        conn.common_message_processor.add(mh);
    }

    fn add_default_prehandshake_validations(&mut self) {
        self.prehandshake_validations
            .add_callback(self.make_check_banned());
    }

    fn make_check_banned(&self) -> PreHandshakeCW {
        let banned_peers = self.banned_peers.clone();
        make_atomic_callback!(move |sockaddr: &SocketAddr| {
            if read_or_die!(banned_peers).is_addr_banned((*sockaddr).ip()) {
                Err(Error::from(fails::BannedNodeRequestedConnectionError))
            } else {
                Ok(())
            }
        })
    }

    pub fn dump_start(&mut self, log_dumper: SyncSender<DumpItem>) {
        self.log_dumper = Some(log_dumper);
        self.dump_all_connections(self.log_dumper.clone());
    }

    pub fn dump_stop(&mut self) {
        self.log_dumper = None;
        self.dump_all_connections(None);
    }

    pub fn add_notification(&mut self, func: UnitFunction<NetworkMessage>) {
        self.message_processor.add_notification(func.clone());
        write_or_die!(self.connections)
            .iter_mut()
            .for_each(|conn| conn.add_notification(func.clone()))
    }

    pub fn set_network_request_sender(&mut self, sender: SyncSender<NetworkRawRequest>) {
        for conn in write_or_die!(self.connections).iter_mut() {
            conn.set_network_request_sender(sender.clone());
        }
        self.network_request_sender = sender;
    }

    pub fn connections_posthandshake_count(&self, exclude_type: Option<PeerType>) -> u16 {
        // We will never have more than 2^16 connections per node, so this conversion is
        // safe.
        read_or_die!(self.connections)
            .iter()
            .filter(|&conn| {
                if !conn.is_post_handshake() {
                    return false;
                }
                if let Some(exclude_type) = exclude_type {
                    return conn.remote_peer().peer_type() != exclude_type;
                }
                true
            })
            .count() as u16
    }

    /// Adds a new node to the banned list and marks its connection for closure
    pub fn ban_node(&mut self, peer: BannedNode) -> bool {
        if write_or_die!(self.banned_peers).insert(peer) {
            match peer {
                BannedNode::ById(id) => {
                    if let Some(ref mut c) = self.find_connection_by_id(id) {
                        c.close();
                    }
                }
                BannedNode::ByAddr(addr) => {
                    for mut conn in self.find_connections_by_ip(addr) {
                        conn.close();
                    }
                }
            };
            true
        } else {
            false
        }
    }

    /// It removes a node from the banned peer list.
    pub fn unban_node(&mut self, peer: BannedNode) -> bool {
        write_or_die!(self.banned_peers).remove(&peer)
    }

    pub fn id_is_banned(&self, id: P2PNodeId) -> bool {
        read_or_die!(self.banned_peers).is_id_banned(id)
    }

    pub fn addr_is_banned(&self, sockaddr: SocketAddr) -> bool {
        read_or_die!(self.banned_peers).is_addr_banned(sockaddr.ip())
    }

    pub fn get_banlist(&self) -> Vec<BannedNode> {
        read_or_die!(self.banned_peers)
            .by_id
            .iter()
            .map(|id| BannedNode::ById(*id))
            .chain(
                read_or_die!(self.banned_peers)
                    .by_addr
                    .iter()
                    .map(|addr| BannedNode::ByAddr(*addr)),
            )
            .collect()
    }

    /// It removes this server from `network_id` network.
    /// *Note:* Network list is shared, and this will updated all other
    /// instances.
    pub fn remove_network(&self, network_id: NetworkId) {
        write_or_die!(self.networks).retain(|x| *x != network_id);
    }

    /// It adds this server to `network_id` network.
    pub fn add_network(&self, network_id: NetworkId) {
        write_or_die!(self.networks).insert(network_id);
    }

    /// It generates a peer statistic list for each connected peer which belongs
    /// to any of networks in `nids`.
    pub fn get_peer_stats(&self, nids: &[NetworkId]) -> Vec<PeerStatistic> {
        let mut ret = vec![];
        for conn in read_or_die!(self.connections).iter() {
            if let RemotePeer::PostHandshake(remote_peer) = conn.remote_peer() {
                if nids.is_empty()
                    || conn
                        .remote_end_networks()
                        .iter()
                        .any(|nid| nids.contains(nid))
                {
                    ret.push(PeerStatistic::new(
                        remote_peer.id().to_string(),
                        remote_peer.addr,
                        remote_peer.peer_type(),
                        conn.get_messages_sent(),
                        conn.get_messages_received(),
                        conn.get_last_latency_measured(),
                    ));
                }
            }
        }

        ret
    }

    pub fn find_connection_by_id(&self, id: P2PNodeId) -> Option<Connection> {
        write_or_die!(self.connections)
            .iter_mut()
            .find(|conn| conn.remote_id() == Some(id))
            .cloned()
    }

    pub fn find_connections_by_id(&self, id: P2PNodeId) -> Vec<Connection> {
        read_or_die!(self.connections)
            .iter()
            .filter(|&conn| conn.remote_id() == Some(id))
            .cloned()
            .collect()
    }

    pub fn find_connection_by_token(&self, token: Token) -> Option<Connection> {
        write_or_die!(self.connections)
            .iter_mut()
            .find(|conn| conn.token() == token)
            .cloned()
    }

    pub fn find_connection_by_ip_addr(&self, addr: SocketAddr) -> Option<Connection> {
        read_or_die!(self.connections)
            .iter()
            .find(|conn| conn.remote_addr() == addr)
            .cloned()
    }

    pub fn find_connections_by_ip(&self, ip: IpAddr) -> Vec<Connection> {
        read_or_die!(self.connections)
            .iter()
            .filter(|conn| conn.remote_peer().addr().ip() == ip)
            .cloned()
            .collect()
    }

    fn remove_connection(&mut self, to_remove: Token) {
        write_or_die!(self.connections).retain(|conn| conn.token() != to_remove);
    }

    pub fn add_connection(&mut self, conn: Connection) {
        write_or_die!(self.connections).push(conn);
    }

    pub fn conn_event(&mut self, event: &Event) -> Fallible<ProcessResult> {
        let token = event.token();

        if let Some(ref mut conn) = self.find_connection_by_token(token) {
            conn.ready(event).map_err(|x| {
                let x: Vec<failure::Error> = x.into_iter().map(Result::unwrap_err).collect();
                failure::Error::from(concordium_common::fails::FunctorError::from(x))
            })
        } else {
            Err(Error::from(fails::PeerNotFoundError))
        }
    }

    pub fn cleanup_connections(
        &mut self,
        max_peers_number: u16,
        poll: &RwLock<Poll>,
    ) -> Fallible<()> {
        let curr_stamp = get_current_stamp();
        let peer_type = self.peer_type();

        write_or_die!(self.to_disconnect).drain(..).for_each(|x| {
            if let Some(ref mut conn) = self.find_connection_by_id(x) {
                trace!(
                    "Disconnecting connection {} already marked as going down",
                    usize::from(conn.token())
                );
                conn.close();
            }
        });

        // Clean duplicates only if it's a regular node we're running
        if peer_type != PeerType::Bootstrapper {
            let mut connection_map: Vec<_> = read_or_die!(self.connections)
                .iter()
                .filter_map(|conn| {
                    conn.remote_id()
                        .and_then(|remote_id| Some((remote_id, conn.token(), conn.last_seen())))
                })
                .collect();
            connection_map.sort_by_key(|p| std::cmp::Reverse((p.0, p.2)));
            connection_map.dedup_by_key(|p| p.0);
            write_or_die!(self.connections).iter_mut().for_each(|conn| {
                if conn.remote_id().is_some()
                    && !connection_map
                        .iter()
                        .any(|(_, token, _)| token == &conn.token())
                {
                    conn.close();
                }
            });
        }

        let wrap_connection_already_gone_as_non_fatal =
            |token, res: Fallible<()>| -> Fallible<Token> {
                use crate::connection::fails::PeerTerminatedConnection;
                match res {
                    Err(err) => {
                        if err.downcast_ref::<PeerTerminatedConnection>().is_some() {
                            Ok(token)
                        } else {
                            Err(err)
                        }
                    }
                    _ => Ok(token),
                }
            };

        let filter_predicate_bootstrapper_no_activity_allowed_period = |conn: &Connection| -> bool {
            peer_type == PeerType::Bootstrapper
                && conn.is_post_handshake()
                && conn.last_seen() + MAX_BOOTSTRAPPER_KEEP_ALIVE < curr_stamp
        };

        let filter_predicate_node_no_activity_allowed_period = |conn: &Connection| -> bool {
            peer_type == PeerType::Node
                && conn.is_post_handshake()
                && conn.last_seen() + MAX_NORMAL_KEEP_ALIVE < curr_stamp
        };

        let filter_predicate_stable_conn_and_no_handshake = |conn: &Connection| -> bool {
            conn.failed_pkts() >= MAX_FAILED_PACKETS_ALLOWED
                || (!conn.is_post_handshake()
                    && conn.last_seen() + MAX_PREHANDSHAKE_KEEP_ALIVE < curr_stamp)
        };

        // Kill nodes which are no longer seen and also closing connections
        let (closing_conns, err_conns): (Vec<Fallible<Token>>, Vec<Fallible<Token>>) =
            write_or_die!(self.connections)
            .iter_mut()
            // Get only connections that have been inactive for more time than allowed or closing connections
            .filter(|conn| {
                conn.is_closed() ||
                    filter_predicate_stable_conn_and_no_handshake(&conn) ||
                    filter_predicate_bootstrapper_no_activity_allowed_period(&conn) ||
                    filter_predicate_node_no_activity_allowed_period(&conn)
            })
            .map(|conn| {
                // Deregister connection from the poll and shut down the socket
                let conn_token = conn.token();
                {
                    trace!("Kill connection {} {}:{}", usize::from(conn_token), conn.remote_addr().ip(), conn.remote_addr().port());
                    wrap_connection_already_gone_as_non_fatal(conn_token, conn.deregister(poll))?;
                    wrap_connection_already_gone_as_non_fatal(conn_token, conn.shutdown())?;
                }
                // Report number of peers to stats export engine
                if let Some(ref service) = &self.stats_export_service {
                    if conn.is_post_handshake() {
                        service.peers_dec();
                    }
                }
                Ok(conn_token)
            }).partition(Result::is_ok);

        // Remove the connection from the list of connections
        for conn in closing_conns.into_iter().map(Result::unwrap) {
            // safe unwrapping since we are iterating over the list that only contains `Ok`s
            trace!("Remove connection {} from TLS Server", usize::from(conn));
            self.remove_connection(conn);
        }

        if peer_type != PeerType::Bootstrapper {
            self.unreachable_nodes
                .cleanup(curr_stamp - MAX_UNREACHABLE_MARK_TIME);
        }

        if !err_conns.is_empty() {
            bail!(format!(
                "Some connections couldn't be cleaned: {:?}",
                err_conns
            ));
        }

        // Toss out a random selection, for now, of connections that's post-handshake,
        // to lower the node count to an appropriate level.
        if peer_type == PeerType::Node {
            let peer_count = self.connections_posthandshake_count(Some(PeerType::Bootstrapper));
            if peer_count > max_peers_number {
                let mut rng = rand::thread_rng();
                write_or_die!(self.connections)
                    .iter_mut()
                    .choose_multiple(&mut rng, (peer_count - max_peers_number) as usize)
                    .iter_mut()
                    .for_each(|conn| conn.close());
            }
        }

        Ok(())
    }

    pub fn liveness_check(&self) -> Fallible<()> {
        let curr_stamp = get_current_stamp();

        write_or_die!(self.connections)
            .iter_mut()
            .filter(|conn| {
                (conn.last_seen() + 120_000 < curr_stamp
                    || conn.get_last_ping_sent() + 300_000 < curr_stamp)
                    && conn.is_post_handshake()
                    && !conn.is_closed()
            })
            .for_each(|conn| {
                let request_ping = {
                    let local_peer = conn.local_peer();

                    NetworkMessage::NetworkRequest(
                        NetworkRequest::Ping(local_peer),
                        Some(get_current_stamp()),
                        None,
                    )
                };
                if let Ok(request_ping_data) = serialize_into_memory(&request_ping, 128) {
                    if let Err(e) = conn.async_send(
                        UCursor::from(request_ping_data),
                        MessageSendingPriority::High,
                    ) {
                        error!("{}", e);
                    }
                    conn.set_measured_ping_sent();
                    conn.set_last_ping_sent();
                }
            });

        Ok(())
    }

    /// It sends `data` message over all filtered connections.
    ///
    /// # Arguments
    /// * `data` - Raw message.
    /// * `filter_conn` - It will send using all connection, where this function
    ///   returns `true`.
    /// * `send_status` - It will called after each sent, to notify the result
    ///   of the operation.
    /// # Returns
    /// * amount of packets written to connections
    pub fn send_over_all_connections(
        &self,
        data: UCursor,
        filter_conn: &dyn Fn(&Connection) -> bool,
        send_status: &dyn Fn(&Connection, Fallible<()>),
    ) -> usize {
        write_or_die!(self.connections)
            .iter_mut()
            .filter(|conn| !conn.is_closed() && filter_conn(conn))
            .map(|conn| {
                let status = conn.async_send(data.clone(), MessageSendingPriority::Normal);
                send_status(&conn, status)
            })
            .count()
    }

    pub fn dump_all_connections(&mut self, log_dumper: Option<SyncSender<DumpItem>>) {
        write_or_die!(self.connections).iter_mut().for_each(|conn| {
            conn.set_log_dumper(log_dumper.clone());
        });
    }

    pub fn get_all_current_peers(&self, peer_type: Option<PeerType>) -> Box<[P2PNodeId]> {
        read_or_die!(self.connections)
        .iter()
        .filter(|conn| {
            conn.is_post_handshake() && (
                peer_type.is_none() || peer_type == Some(conn.remote_peer_type()) )
        })
        // we can safely unwrap here, because we've filetered away any
        // non-post-handshake peers already
        .map(|conn| conn.remote_peer().peer().unwrap().id() )
        .collect::<Vec<_>>()
        .into_boxed_slice()
    }
}

impl MessageManager for TlsServer {
    fn message_processor(&self) -> MessageProcessor { self.message_processor.clone() }
}
