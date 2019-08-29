use super::fails;

use concordium_common::{
    functor::{UnitFunction, UnitFunctor},
    hybrid_buf::HybridBuf,
};

use failure::{Error, Fallible};
use mio::{
    net::{TcpListener, TcpStream},
    Event, Token,
};
use rand::seq::IteratorRandom;
use snow::Keypair;

use std::{
    collections::{HashSet, VecDeque},
    convert::TryFrom,
    net::{IpAddr, SocketAddr},
    pin::Pin,
    sync::{
        atomic::{AtomicU64, AtomicUsize, Ordering},
        mpsc::SyncSender,
        Arc, RwLock,
    },
};

use crate::{
    common::{
        get_current_stamp, serialization::serialize_into_memory, NetworkRawRequest, P2PNodeId,
        PeerType, RemotePeer,
    },
    connection::{
        network_handler::message_processor::{MessageManager, MessageProcessor, ProcessResult},
        Connection, ConnectionBuilder, MessageSendingPriority, P2PEvent,
    },
    crypto::generate_snow_config,
    dumper::DumpItem,
    network::{Buckets, NetworkId, NetworkMessage, NetworkRequest},
    p2p::{
        banned_nodes::{BannedNode, BannedNodes},
        unreachable_nodes::UnreachableNodes,
        P2PNode,
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

#[derive(Default)]
pub struct NoiseProtocolHandlerBuilder {
    server:       Option<TcpListener>,
    event_log:    Option<SyncSender<P2PEvent>>,
    buckets:      Option<Arc<RwLock<Buckets>>>,
    networks:     Option<HashSet<NetworkId>>,
    noise_params: Option<snow::params::NoiseParams>,
}

impl NoiseProtocolHandlerBuilder {
    pub fn build(self) -> Fallible<NoiseProtocolHandler> {
        if let (Some(networks), Some(server), Some(buckets), Some(noise_params)) =
            (self.networks, self.server, self.buckets, self.noise_params)
        {
            let key_pair = snow::Builder::new(noise_params.clone()).generate_keypair()?;

            let mself = NoiseProtocolHandler {
                node_ref: None,
                server: Arc::new(server),
                next_id: Arc::new(AtomicUsize::new(2)),
                key_pair: Arc::new(key_pair),
                event_log: self.event_log,
                buckets,
                message_processor: MessageProcessor::new(),
                prehandshake_validations: PreHandshake::new(),
                log_dumper: None,
                noise_params,
                network_request_sender: None,
                connections: Default::default(),
                to_disconnect: Default::default(),
                unreachable_nodes: UnreachableNodes::new(),
                banned_peers: Arc::new(RwLock::new(BannedNodes::new())),
                networks: Arc::new(RwLock::new(networks)),
                last_bootstrap: Default::default(),
            };

            mself.add_default_prehandshake_validations();
            mself.setup_default_message_handler();
            Ok(mself)
        } else {
            Err(Error::from(
                fails::MissingFieldsOnNoiseProtocolHandlerBuilder,
            ))
        }
    }

    pub fn set_server(mut self, s: TcpListener) -> NoiseProtocolHandlerBuilder {
        self.server = Some(s);
        self
    }

    pub fn set_event_log(
        mut self,
        el: Option<SyncSender<P2PEvent>>,
    ) -> NoiseProtocolHandlerBuilder {
        self.event_log = el;
        self
    }

    pub fn set_buckets(mut self, b: Arc<RwLock<Buckets>>) -> NoiseProtocolHandlerBuilder {
        self.buckets = Some(b);
        self
    }

    pub fn set_networks(mut self, n: HashSet<NetworkId>) -> NoiseProtocolHandlerBuilder {
        self.networks = Some(n);
        self
    }

    pub fn set_noise_params(
        mut self,
        config: &crate::configuration::CryptoConfig,
    ) -> NoiseProtocolHandlerBuilder {
        self.noise_params = Some(generate_snow_config(config));
        self
    }
}

#[derive(Clone)]
pub struct NoiseProtocolHandler {
    pub node_ref:               Option<Pin<Arc<P2PNode>>>,
    server:                     Arc<TcpListener>,
    next_id:                    Arc<AtomicUsize>,
    key_pair:                   Arc<Keypair>,
    pub event_log:              Option<SyncSender<P2PEvent>>,
    pub buckets:                Arc<RwLock<Buckets>>,
    message_processor:          MessageProcessor,
    prehandshake_validations:   PreHandshake,
    pub log_dumper:             Option<SyncSender<DumpItem>>,
    noise_params:               snow::params::NoiseParams,
    pub network_request_sender: Option<SyncSender<NetworkRawRequest>>,
    connections:                Arc<RwLock<Vec<Connection>>>,
    pub to_disconnect:          Arc<RwLock<VecDeque<P2PNodeId>>>,
    pub unreachable_nodes:      UnreachableNodes,
    pub banned_peers:           Arc<RwLock<BannedNodes>>,
    pub networks:               Arc<RwLock<HashSet<NetworkId>>>,
    pub last_bootstrap:         Arc<AtomicU64>,
}

impl NoiseProtocolHandler {
    pub fn node(&self) -> &Pin<Arc<P2PNode>> {
        self.node_ref.as_ref().unwrap() // safe; always available
    }

    pub fn log_event(&self, event: P2PEvent) {
        if let Some(ref log) = self.event_log {
            if let Err(e) = log.send(event) {
                error!("Couldn't send error {:?}", e)
            }
        }
    }

    #[inline]
    pub fn networks(&self) -> Arc<RwLock<HashSet<NetworkId>>> { Arc::clone(&self.networks) }

    /// Returns true if `addr` is in the `unreachable_nodes` list.
    pub fn is_unreachable(&self, addr: SocketAddr) -> bool { self.unreachable_nodes.contains(addr) }

    /// Adds the `addr` to the `unreachable_nodes` list.
    pub fn add_unreachable(&self, addr: SocketAddr) -> bool { self.unreachable_nodes.insert(addr) }

    pub fn accept(&self) -> Fallible<()> {
        let self_peer = self.node().self_peer;
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

        let conn = ConnectionBuilder::default()
            .set_handler_ref(Arc::pin(self.clone()))
            .set_socket(socket)
            .set_token(token)
            .set_key_pair(key_pair)
            .set_local_peer(self_peer)
            .set_remote_peer(RemotePeer::PreHandshake(PeerType::Node, addr))
            .set_local_end_networks(networks)
            .set_noise_params(self.noise_params.clone())
            .build()?;

        self.register_message_handlers(&conn);

        conn.setup_pre_handshake();
        conn.setup_post_handshake();

        let register_status = conn.register(&self.node().poll);
        self.add_connection(conn);

        register_status
    }

    pub fn connect(
        &self,
        peer_type: PeerType,
        addr: SocketAddr,
        peer_id_opt: Option<P2PNodeId>,
    ) -> Fallible<()> {
        let self_peer = self.node().self_peer;
        if peer_type == PeerType::Node {
            let current_peer_count =
                self.connections_posthandshake_count(Some(PeerType::Bootstrapper));
            if current_peer_count > self.node().config.max_allowed_nodes {
                return Err(Error::from(fails::MaxmimumAmountOfPeers {
                    max_allowed_peers: self.node().config.max_allowed_nodes,
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
                if let Some(ref service) = self.node().stats_export_service() {
                    service.conn_received_inc();
                };
                let token = Token(self.next_id.fetch_add(1, Ordering::SeqCst));
                let networks = self.networks();
                let keypair = clone_snow_keypair(&self.key_pair);
                let conn = ConnectionBuilder::default()
                    .set_handler_ref(Arc::pin(self.clone()))
                    .set_socket(socket)
                    .set_token(token)
                    .set_key_pair(keypair)
                    .set_as_initiator(true)
                    .set_local_peer(self_peer)
                    .set_remote_peer(RemotePeer::PreHandshake(peer_type, addr))
                    .set_local_end_networks(Arc::clone(&networks))
                    .set_noise_params(self.noise_params.clone())
                    .build()?;

                self.register_message_handlers(&conn);
                conn.setup_pre_handshake();
                conn.setup_post_handshake();
                conn.register(&self.node().poll)?;

                self.add_connection(conn);
                self.log_event(P2PEvent::ConnectEvent(addr));
                debug!("Requesting handshake from new peer {}", addr,);

                if let Some(ref mut conn) = self.find_connection_by_token(token) {
                    let handshake_request = NetworkMessage::NetworkRequest(
                        NetworkRequest::Handshake(self_peer, safe_read!(networks)?.clone(), vec![]),
                        Some(get_current_stamp()),
                        None,
                    );
                    let handshake_request_data = serialize_into_memory(&handshake_request, 256)?;

                    conn.async_send(
                        HybridBuf::try_from(handshake_request_data)?,
                        MessageSendingPriority::High,
                    )?;
                    conn.set_measured_handshake_sent();
                }

                if peer_type == PeerType::Bootstrapper {
                    self.update_last_bootstrap();
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
    pub fn peer_type(&self) -> PeerType { self.node().self_peer.peer_type() }

    /// It setups default message handler at noise protocol handler level.
    fn setup_default_message_handler(&self) {
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
    fn register_message_handlers(&self, conn: &Connection) {
        let mh = self.message_processor.clone();
        conn.common_message_processor.add(mh);
    }

    fn add_default_prehandshake_validations(&self) {
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
    }

    pub fn dump_stop(&mut self) { self.log_dumper = None; }

    pub fn add_notification(&self, func: UnitFunction<NetworkMessage>) {
        self.message_processor.add_notification(func.clone());
        read_or_die!(self.connections)
            .iter()
            .for_each(|conn| conn.add_notification(func.clone()))
    }

    pub fn set_network_request_sender(&mut self, sender: SyncSender<NetworkRawRequest>) {
        self.network_request_sender = Some(sender);
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
    pub fn ban_node(&self, peer: BannedNode) -> bool {
        if write_or_die!(self.banned_peers).insert(peer) {
            match peer {
                BannedNode::ById(id) => {
                    if let Some(ref mut c) = self.find_connection_by_id(id) {
                        c.close();
                    }
                }
                BannedNode::ByAddr(addr) => {
                    for conn in self.find_connections_by_ip(addr) {
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
    pub fn unban_node(&self, peer: BannedNode) -> bool {
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

    pub fn find_connection_by_id(&self, id: P2PNodeId) -> Option<Connection> {
        read_or_die!(self.connections)
            .iter()
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
        read_or_die!(self.connections)
            .iter()
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

    fn remove_connections(&self, to_remove: &[Token]) {
        write_or_die!(self.connections).retain(|conn| !to_remove.contains(&conn.token()));
    }

    pub fn add_connection(&self, conn: Connection) { write_or_die!(self.connections).push(conn); }

    pub fn conn_event(&self, event: &Event) -> Fallible<ProcessResult> {
        let token = event.token();

        if let Some(conn) = self.find_connection_by_token(token) {
            conn.ready(event).map_err(|x| {
                let x: Vec<failure::Error> = x.into_iter().map(Result::unwrap_err).collect();
                failure::Error::from(concordium_common::fails::FunctorError::from(x))
            })
        } else {
            Err(Error::from(fails::PeerNotFoundError))
        }
    }

    pub fn connection_housekeeping(&self) -> Fallible<()> {
        let curr_stamp = get_current_stamp();
        let peer_type = self.peer_type();

        write_or_die!(self.to_disconnect).drain(..).for_each(|x| {
            if let Some(ref conn) = self.find_connection_by_id(x) {
                trace!(
                    "Disconnecting connection {} already marked as going down",
                    usize::from(conn.token())
                );
                conn.close();
            }
        });

        // clone the initial collection of connections to reduce locking
        let uncleaned_connections: Vec<_> = read_or_die!(self.connections).clone();

        // Clean duplicates only if it's a regular node we're running
        if peer_type != PeerType::Bootstrapper {
            let mut connection_map: Vec<_> = uncleaned_connections
                .iter()
                .filter_map(|conn| {
                    conn.remote_id()
                        .and_then(|remote_id| Some((remote_id, conn.token(), conn.last_seen())))
                })
                .collect();
            connection_map.sort_by_key(|p| std::cmp::Reverse((p.0, p.2)));
            connection_map.dedup_by_key(|p| p.0);
            uncleaned_connections
                .iter()
                .filter(|conn| conn.remote_id().is_some())
                .for_each(|conn| {
                    if !connection_map
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
            uncleaned_connections
            .iter()
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
                    wrap_connection_already_gone_as_non_fatal(conn_token, conn.deregister(&self.node().poll))?;
                    wrap_connection_already_gone_as_non_fatal(conn_token, conn.shutdown())?;
                }
                // Report number of peers to stats export engine
                if let Some(ref service) = &self.node().stats_export_service() {
                    if conn.is_post_handshake() {
                        service.peers_dec();
                    }
                }
                Ok(conn_token)
            }).partition(Result::is_ok);

        // safe unwrapping since we are iterating over the list that only contains `Ok`s
        let closing_conns: Vec<_> = closing_conns.into_iter().map(Result::unwrap).collect();

        self.remove_connections(&closing_conns);

        {
            let mut locked_active_peers = write_or_die!(self.node().active_peer_stats);

            for closed_connection in uncleaned_connections
                .into_iter()
                .filter(|conn| closing_conns.contains(&conn.token()))
            {
                trace!(
                    "Removed connection {} from the Noise Protocol Handler",
                    usize::from(closed_connection.token())
                );

                if let Some(obsolete_peer_id) = closed_connection
                    .remote_peer()
                    .peer()
                    .map(|peer| peer.id().as_raw())
                {
                    locked_active_peers.remove(&obsolete_peer_id);
                }
            }
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

        // If the number of peers exceeds the desired value, close a random selection of
        // post-handshake connections to lower it
        if peer_type == PeerType::Node {
            let max_allowed_nodes = self.node().config.max_allowed_nodes;
            let peer_count = self.connections_posthandshake_count(Some(PeerType::Bootstrapper));
            if peer_count > max_allowed_nodes {
                let mut rng = rand::thread_rng();
                read_or_die!(self.connections)
                    .iter()
                    .choose_multiple(&mut rng, (peer_count - max_allowed_nodes) as usize)
                    .iter()
                    .for_each(|conn| conn.close());
            }
        }

        // reconnect to bootstrappers after a specified amount of time
        if peer_type == PeerType::Node
            && curr_stamp
                >= self.get_last_bootstrap() + self.node().config.bootstrapping_interval * 1000
        {
            self.node().attempt_bootstrap();
        }

        Ok(())
    }

    pub fn liveness_check(&self) {
        let curr_stamp = get_current_stamp();

        read_or_die!(self.connections)
            .iter()
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
                    if let Err(e) = conn.async_send(request_ping_data, MessageSendingPriority::High)
                    {
                        error!("{}", e);
                    }
                    conn.set_measured_ping_sent();
                    conn.set_last_ping_sent();
                }
            });
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
        data: HybridBuf,
        filter_conn: &dyn Fn(&Connection) -> bool,
        send_status: &dyn Fn(&Connection, Fallible<()>),
    ) -> usize {
        read_or_die!(self.connections)
            .iter()
            .filter(|conn| !conn.is_closed() && filter_conn(conn))
            .map(|conn| {
                let status = conn.async_send(data.clone(), MessageSendingPriority::Normal);
                send_status(&conn, status)
            })
            .count()
    }

    pub fn get_all_current_peers(&self, peer_type: Option<PeerType>) -> Vec<P2PNodeId> {
        read_or_die!(self.connections)
        .iter()
        .filter(|conn| {
            conn.is_post_handshake() && (
                peer_type.is_none() || peer_type == Some(conn.remote_peer_type()) )
        })
        // we can safely unwrap here, because we've filetered away any
        // non-post-handshake peers already
        .map(|conn| conn.remote_peer().peer().unwrap().id() )
        .collect()
    }

    pub fn get_last_bootstrap(&self) -> u64 { self.last_bootstrap.load(Ordering::Relaxed) }

    pub fn update_last_bootstrap(&self) {
        self.last_bootstrap
            .store(get_current_stamp(), Ordering::SeqCst);
    }
}

impl MessageManager for NoiseProtocolHandler {
    fn message_processor(&self) -> MessageProcessor { self.message_processor.clone() }
}
