#[cfg(feature = "network_dump")]
use crate::dumper::create_dump_thread;
use crate::{
    common::{
        get_current_stamp, NetworkRawRequest, P2PNodeId, P2PPeer, PeerStats, PeerType, RemotePeer,
    },
    configuration::{self as config, Config},
    connection::{Connection, DeduplicationQueues, MessageSendingPriority, P2PEvent},
    crypto::generate_snow_config,
    dumper::DumpItem,
    network::{
        request::RequestedElementType, Buckets, NetworkId, NetworkMessage, NetworkPacket,
        NetworkPacketType, NetworkRequest,
    },
    p2p::{banned_nodes::BannedNode, fails, unreachable_nodes::UnreachableNodes},
    stats_engine::StatsEngine,
    utils::{self, GlobalStateReceivers, GlobalStateSenders},
};
use chrono::prelude::*;
use concordium_common::{
    cache::Cache, hybrid_buf::HybridBuf, serial::serialize_into_buffer,
    stats_export_service::StatsExportService, SerializeToBytes,
};
use failure::{err_msg, Error, Fallible};
#[cfg(not(target_os = "windows"))]
use get_if_addrs;
#[cfg(target_os = "windows")]
use ipconfig;
use mio::{
    net::{TcpListener, TcpStream},
    Event, Events, Poll, PollOpt, Ready, Token,
};
use nohash_hasher::BuildNoHashHasher;
use rand::seq::IteratorRandom;
use rkv::{Manager, Rkv, StoreOptions, Value};
use snow::Keypair;

use std::{
    collections::{HashMap, HashSet},
    net::{
        IpAddr::{self, V4, V6},
        SocketAddr,
    },
    path::PathBuf,
    pin::Pin,
    str::FromStr,
    sync::{
        atomic::{AtomicBool, AtomicU16, AtomicU64, AtomicUsize, Ordering},
        mpsc::{sync_channel, Receiver, SyncSender},
        Arc, RwLock,
    },
    thread::{JoinHandle, ThreadId},
    time::{Duration, SystemTime},
};

const SERVER: Token = Token(0);
const BAN_STORE_NAME: &str = "bans";

const MAX_FAILED_PACKETS_ALLOWED: u32 = 50;
const MAX_UNREACHABLE_MARK_TIME: u64 = 86_400_000;
const MAX_BOOTSTRAPPER_KEEP_ALIVE: u64 = 300_000;
const MAX_NORMAL_KEEP_ALIVE: u64 = 1_200_000;
const MAX_PREHANDSHAKE_KEEP_ALIVE: u64 = 120_000;

#[derive(Clone)]
pub struct P2PNodeConfig {
    pub no_net: bool,
    pub desired_nodes_count: u16,
    no_bootstrap_dns: bool,
    bootstrap_server: String,
    pub dns_resolvers: Vec<String>,
    dnssec_disabled: bool,
    bootstrap_nodes: Vec<String>,
    minimum_per_bucket: usize,
    pub max_allowed_nodes: u16,
    max_resend_attempts: u8,
    relay_broadcast_percentage: f64,
    pub global_state_catch_up_requests: bool,
    pub poll_interval: u64,
    pub housekeeping_interval: u64,
    pub bootstrapping_interval: u64,
    pub print_peers: bool,
    pub bootstrapper_wait_minimum_peers: u16,
    pub no_trust_bans: bool,
    pub data_dir_path: PathBuf,
    max_latency: Option<u64>,
    hard_connection_limit: Option<u16>,
    #[cfg(feature = "benchmark")]
    pub enable_tps_test: bool,
    #[cfg(feature = "benchmark")]
    pub tps_message_count: u64,
}

#[derive(Default)]
pub struct P2PNodeThread {
    pub join_handle: Option<JoinHandle<()>>,
    pub id:          Option<ThreadId>,
}

pub struct ResendQueueEntry {
    pub message:      NetworkMessage,
    pub last_attempt: u64,
    pub attempts:     u8,
}

impl ResendQueueEntry {
    pub fn new(message: NetworkMessage, last_attempt: u64, attempts: u8) -> Self {
        Self {
            message,
            last_attempt,
            attempts,
        }
    }
}

pub type Networks = HashSet<NetworkId, BuildNoHashHasher<u16>>;
pub type Connections = HashMap<Token, Arc<Connection>, BuildNoHashHasher<usize>>;

pub struct ConnectionHandler {
    server:                     TcpListener,
    next_id:                    AtomicUsize,
    key_pair:                   Keypair,
    pub event_log:              Option<SyncSender<P2PEvent>>,
    pub buckets:                RwLock<Buckets>,
    pub log_dumper:             Option<SyncSender<DumpItem>>,
    noise_params:               snow::params::NoiseParams,
    pub network_request_sender: SyncSender<NetworkRawRequest>,
    pub connections:            RwLock<Connections>,
    pub unreachable_nodes:      UnreachableNodes,
    pub networks:               RwLock<Networks>,
    pub last_bootstrap:         AtomicU64,
}

impl ConnectionHandler {
    fn new(
        conf: &Config,
        server: TcpListener,
        network_request_sender: SyncSender<NetworkRawRequest>,
        event_log: Option<SyncSender<P2PEvent>>,
    ) -> Self {
        let networks = conf
            .common
            .network_ids
            .iter()
            .cloned()
            .map(NetworkId::from)
            .collect();
        let noise_params = generate_snow_config(&conf.crypto);
        let key_pair = snow::Builder::new(noise_params.clone())
            .generate_keypair()
            .expect("Can't create a connection handler!");

        ConnectionHandler {
            server,
            next_id: AtomicUsize::new(2),
            key_pair,
            event_log,
            buckets: RwLock::new(Buckets::new()),
            log_dumper: None,
            noise_params,
            network_request_sender,
            connections: Default::default(),
            unreachable_nodes: UnreachableNodes::new(),
            networks: RwLock::new(networks),
            last_bootstrap: Default::default(),
        }
    }
}

pub struct Receivers {
    pub network_requests:       Receiver<NetworkRawRequest>,
    pub global_state_receivers: Option<GlobalStateReceivers>,
}

#[allow(dead_code)] // caused by the dump_network feature; will fix in a follow-up
pub struct P2PNode {
    pub self_ref:             Option<Pin<Arc<Self>>>,
    pub self_peer:            P2PPeer,
    thread:                   RwLock<P2PNodeThread>,
    pub poll:                 Poll,
    pub connection_handler:   ConnectionHandler,
    pub rpc_queue:            SyncSender<NetworkMessage>,
    dump_switch:              SyncSender<(std::path::PathBuf, bool)>,
    dump_tx:                  SyncSender<crate::dumper::DumpItem>,
    pub active_peer_stats:    RwLock<HashMap<u64, PeerStats, BuildNoHashHasher<u64>>>,
    pub stats_export_service: Option<StatsExportService>,
    pub config:               P2PNodeConfig,
    start_time:               DateTime<Utc>,
    pub is_rpc_online:        AtomicBool,
    pub is_terminated:        AtomicBool,
    pub kvs:                  Arc<RwLock<Rkv>>,
    pub transactions_cache:   RwLock<Cache<Vec<u8>>>,
    pub stats_engine:         RwLock<StatsEngine>,
    pub global_state_senders: GlobalStateSenders,
}

// a convenience macro to send an object to all connections
macro_rules! send_to_all {
    ($foo_name:ident, $object_type:ty, $req_type:ident) => {
        pub fn $foo_name(&self, object: $object_type) {
            let req = NetworkRequest::$req_type(object);
            let msg = NetworkMessage::NetworkRequest(req, None, None);
            let filter = |_: &Connection| true;

            if let Err(e) = serialize_into_buffer(&msg, 256).and_then(|data| self.send_over_all_connections(data, &filter)) {
                error!("A network message couldn't be forwarded: {}", e);
            }
        }
    }
}

impl P2PNode {
    send_to_all!(send_ban, BannedNode, BanNode);

    send_to_all!(send_unban, BannedNode, UnbanNode);

    send_to_all!(send_joinnetwork, NetworkId, JoinNetwork);

    send_to_all!(send_leavenetwork, NetworkId, LeaveNetwork);

    #[allow(clippy::too_many_arguments)]
    pub fn new(
        supplied_id: Option<String>,
        conf: &Config,
        event_log: Option<SyncSender<P2PEvent>>,
        peer_type: PeerType,
        stats_export_service: Option<StatsExportService>,
        subscription_queue_in: SyncSender<NetworkMessage>,
        data_dir_path: Option<PathBuf>,
    ) -> (Arc<Self>, Receivers) {
        let addr = if let Some(ref addy) = conf.common.listen_address {
            format!("{}:{}", addy, conf.common.listen_port)
                .parse()
                .unwrap_or_else(|_| {
                    warn!("Supplied listen address coulnd't be parsed");
                    format!("0.0.0.0:{}", conf.common.listen_port)
                        .parse()
                        .expect("Port not properly formatted. Crashing.")
                })
        } else {
            format!("0.0.0.0:{}", conf.common.listen_port)
                .parse()
                .expect("Port not properly formatted. Crashing.")
        };

        trace!("Creating new P2PNode");

        // Retrieve IP address octets, format to IP and SHA256 hash it
        let ip = if let Some(ref addy) = conf.common.listen_address {
            IpAddr::from_str(addy)
                .unwrap_or_else(|_| P2PNode::get_ip().expect("Couldn't retrieve my own ip"))
        } else {
            P2PNode::get_ip().expect("Couldn't retrieve my own ip")
        };

        debug!(
            "Listening on {}:{}",
            ip.to_string(),
            conf.common.listen_port
        );

        let id = if let Some(s) = supplied_id {
            if s.chars().count() != 16 {
                panic!(
                    "Incorrect ID specified; expected a zero-padded, hex-encoded u64 that's 16 \
                     characters long."
                );
            } else {
                P2PNodeId::from_str(&s).unwrap_or_else(|e| panic!("invalid ID provided: {}", e))
            }
        } else {
            P2PNodeId::default()
        };

        info!("My Node ID is {}", id);

        let poll = Poll::new().unwrap_or_else(|err| panic!("Couldn't create poll {:?}", err));

        let server =
            TcpListener::bind(&addr).unwrap_or_else(|_| panic!("Couldn't listen on port!"));

        if poll
            .register(&server, SERVER, Ready::readable(), PollOpt::level())
            .is_err()
        {
            panic!("Couldn't register server with poll!")
        };

        let own_peer_port = if let Some(own_port) = conf.common.external_port {
            own_port
        } else {
            conf.common.listen_port
        };

        let self_peer = P2PPeer::from(peer_type, id, SocketAddr::new(ip, own_peer_port));

        let (dump_tx, _dump_rx) = std::sync::mpsc::sync_channel(config::DUMP_QUEUE_DEPTH);
        let (act_tx, _act_rx) = std::sync::mpsc::sync_channel(config::DUMP_SWITCH_QUEUE_DEPTH);

        #[cfg(feature = "network_dump")]
        create_dump_thread(ip, id, _dump_rx, _act_rx, &conf.common.data_dir);

        let config = P2PNodeConfig {
            no_net: conf.cli.no_network,
            desired_nodes_count: conf.connection.desired_nodes,
            no_bootstrap_dns: conf.connection.no_bootstrap_dns,
            bootstrap_server: conf.connection.bootstrap_server.clone(),
            dns_resolvers: utils::get_resolvers(
                &conf.connection.resolv_conf,
                &conf.connection.dns_resolver,
            ),
            dnssec_disabled: conf.connection.dnssec_disabled,
            bootstrap_nodes: conf.connection.bootstrap_nodes.clone(),
            minimum_per_bucket: conf.common.min_peers_bucket,
            max_allowed_nodes: if let Some(max) = conf.connection.max_allowed_nodes {
                max
            } else {
                f64::floor(
                    f64::from(conf.connection.desired_nodes)
                        * (f64::from(conf.connection.max_allowed_nodes_percentage) / 100f64),
                ) as u16
            },
            max_resend_attempts: conf.connection.max_resend_attempts,
            relay_broadcast_percentage: conf.connection.relay_broadcast_percentage,
            global_state_catch_up_requests: conf.connection.global_state_catch_up_requests,
            poll_interval: conf.cli.poll_interval,
            housekeeping_interval: conf.connection.housekeeping_interval,
            bootstrapping_interval: conf.connection.bootstrapping_interval,
            print_peers: true,
            bootstrapper_wait_minimum_peers: match peer_type {
                PeerType::Bootstrapper => conf.bootstrapper.wait_until_minimum_nodes,
                PeerType::Node => 0,
            },
            no_trust_bans: conf.common.no_trust_bans,
            data_dir_path: data_dir_path.unwrap_or_else(|| ".".into()),
            max_latency: conf.connection.max_latency,
            hard_connection_limit: conf.connection.hard_connection_limit,
            #[cfg(feature = "benchmark")]
            enable_tps_test: conf.cli.tps.enable_tps_test,
            #[cfg(feature = "benchmark")]
            tps_message_count: conf.cli.tps.tps_message_count,
        };

        let (network_request_sender, network_request_receiver) =
            sync_channel(config::RAW_NETWORK_MSG_QUEUE_DEPTH);
        let (global_state_senders, global_state_receivers) = utils::create_global_state_queues();

        let receivers = Receivers {
            network_requests:       network_request_receiver,
            global_state_receivers: Some(global_state_receivers),
        };

        let connection_handler =
            ConnectionHandler::new(conf, server, network_request_sender, event_log);

        // Create the node key-value store environment
        let kvs = Manager::singleton()
            .write()
            .unwrap()
            .get_or_create(config.data_dir_path.as_path(), Rkv::new)
            .unwrap();

        let transactions_cache = Default::default();
        let stats_engine = RwLock::new(StatsEngine::new(&conf.cli));

        let node = Arc::new(P2PNode {
            self_ref: None,
            poll,
            rpc_queue: subscription_queue_in,
            start_time: Utc::now(),
            thread: RwLock::new(P2PNodeThread::default()),
            config,
            dump_switch: act_tx,
            dump_tx,
            is_rpc_online: AtomicBool::new(false),
            connection_handler,
            self_peer,
            active_peer_stats: Default::default(),
            stats_export_service,
            is_terminated: Default::default(),
            kvs,
            transactions_cache,
            stats_engine,
            global_state_senders,
        });

        // note: in order to avoid a lock over the self_ref, write to it as soon as it's
        // available using the unsafe ptr::write
        // this is safe, as at this point the node is not shared with any other object
        // or thread
        let self_ref =
            &node.self_ref as *const Option<Pin<Arc<P2PNode>>> as *mut Option<Pin<Arc<P2PNode>>>;
        unsafe { std::ptr::write(self_ref, Some(Pin::new(Arc::clone(&node)))) };

        node.clear_bans()
            .unwrap_or_else(|e| error!("Couldn't reset the ban list: {}", e));

        (node, receivers)
    }

    /// It sends `data` message over all filtered connections.
    ///
    /// # Arguments
    /// * `data` - Raw message.
    /// * `conn_filter` - A closure filtering the connections
    /// # Returns number of messages sent to connections
    fn send_over_all_connections(
        &self,
        data: HybridBuf,
        conn_filter: &dyn Fn(&Connection) -> bool,
    ) -> Fallible<usize> {
        let mut sent_messages = 0usize;

        for conn in read_or_die!(self.connections())
            .values()
            .filter(|conn| conn.is_post_handshake() && conn_filter(conn))
        {
            if let Err(e) = conn.async_send(data.clone(), MessageSendingPriority::Normal) {
                error!("Couldn't send a message to {}: {}", conn, e);
            } else {
                sent_messages += 1;
            }
        }

        Ok(sent_messages)
    }

    pub fn get_last_bootstrap(&self) -> u64 {
        self.connection_handler
            .last_bootstrap
            .load(Ordering::Relaxed)
    }

    pub fn update_last_bootstrap(&self) {
        self.connection_handler
            .last_bootstrap
            .store(get_current_stamp(), Ordering::SeqCst);
    }

    pub fn forward_network_packet(&self, msg: NetworkMessage) -> Fallible<()> {
        if let Err(e) = self.rpc_queue.send(msg) {
            error!("Can't relay a message to the RPC outbound queue: {}", e);
        }

        Ok(())
    }

    /// This function is called periodically to print information about current
    /// nodes.
    fn print_stats(&self, peer_stat_list: &[PeerStats]) {
        trace!("Printing out stats");
        debug!(
            "I currently have {}/{} peers",
            peer_stat_list.len(),
            self.config.max_allowed_nodes,
        );

        // Print nodes
        if self.config.print_peers {
            for (i, peer) in peer_stat_list.iter().enumerate() {
                trace!(
                    "Peer {}: {}/{}/{}",
                    i,
                    P2PNodeId(peer.id),
                    peer.addr,
                    peer.peer_type
                );
            }
        }
    }

    pub fn attempt_bootstrap(&self) {
        if !self.config.no_net {
            info!("Attempting to bootstrap");

            let bootstrap_nodes = utils::get_bootstrap_nodes(
                &self.config.bootstrap_server,
                &self.config.dns_resolvers,
                self.config.dnssec_disabled,
                &self.config.bootstrap_nodes,
            );

            match bootstrap_nodes {
                Ok(nodes) => {
                    for addr in nodes {
                        info!("Found a bootstrap node: {}", addr);
                        let _ = self
                            .connect(PeerType::Bootstrapper, addr, None)
                            .map_err(|e| error!("{}", e));
                    }
                }
                Err(e) => error!("Can't bootstrap: {:?}", e),
            }
        }
    }

    fn check_peers(&self, peer_stat_list: &[PeerStats]) {
        trace!("Checking for needed peers");
        if self.peer_type() != PeerType::Bootstrapper
            && !self.config.no_net
            && self.config.desired_nodes_count
                > peer_stat_list
                    .iter()
                    .filter(|peer| peer.peer_type != PeerType::Bootstrapper)
                    .count() as u16
        {
            if peer_stat_list.is_empty() {
                info!("Sending out GetPeers to any bootstrappers we may still be connected to");
                {
                    self.send_get_peers();
                }
                if !self.config.no_bootstrap_dns {
                    info!("No peers at all - retrying bootstrapping");
                    self.attempt_bootstrap();
                } else {
                    info!(
                        "No nodes at all - Not retrying bootstrapping using DNS since \
                         --no-bootstrap is specified"
                    );
                }
            } else {
                info!("Not enough peers, sending GetPeers requests");
                self.send_get_peers();
            }
        }
    }

    pub fn spawn(&self, receivers: Receivers) {
        // Prepare poll-loop channels.
        let self_clone = self.self_ref.clone().unwrap(); // safe, always available

        let join_handle = spawn_or_die!("P2PNode spawned thread", move || {
            let mut events = Events::with_capacity(10);
            let mut log_time = SystemTime::now();

            let mut deduplication_queues = DeduplicationQueues::default();

            loop {
                let _ = self_clone
                    .process(&mut events, &mut deduplication_queues)
                    .map_err(|e| error!("{}", e));

                self_clone.process_network_requests(&receivers);

                // Check the termination switch
                if self_clone.is_terminated.load(Ordering::Relaxed) {
                    break;
                }

                // Run periodic tasks
                let now = SystemTime::now();
                if let Ok(difference) = now.duration_since(log_time) {
                    if difference > Duration::from_secs(self_clone.config.housekeeping_interval) {
                        if let Err(e) = self_clone.connection_housekeeping() {
                            error!("Issue with connection cleanups: {:?}", e);
                        }
                        if self_clone.peer_type() != PeerType::Bootstrapper {
                            self_clone.liveness_check();
                        }

                        let peer_stat_list = self_clone.get_peer_stats();
                        self_clone.check_peers(&peer_stat_list);
                        self_clone.print_stats(&peer_stat_list);

                        log_time = now;
                    }
                }
            }
        });

        // Register info about thread into P2PNode.
        {
            let mut locked_thread = write_or_die!(self.thread);
            locked_thread.id = Some(join_handle.thread().id());
            locked_thread.join_handle = Some(join_handle);
        }
    }

    fn liveness_check(&self) {
        debug!("Running connection liveness checks");

        let curr_stamp = get_current_stamp();
        let connections = read_or_die!(self.connections()).clone();

        connections
            .values()
            .filter(|conn| {
                conn.is_post_handshake()
                    && (conn.last_seen() + 120_000 < curr_stamp
                        || conn.get_last_ping_sent() + 300_000 < curr_stamp)
            })
            .for_each(|conn| {
                if let Err(e) = conn.send_ping() {
                    error!("Can't send a ping to {}: {}", conn.remote_addr(), e);
                }
            });
    }

    fn connection_housekeeping(&self) -> Fallible<()> {
        debug!("Running connection housekeeping");

        let curr_stamp = get_current_stamp();
        let peer_type = self.peer_type();

        let is_conn_faulty = |conn: &Connection| -> bool {
            conn.failed_pkts() >= MAX_FAILED_PACKETS_ALLOWED
                || if let Some(max_latency) = self.config.max_latency {
                    conn.get_last_latency() >= max_latency
                } else {
                    false
                }
        };

        let is_conn_inactive = |conn: &Connection| -> bool {
            conn.is_post_handshake()
                && ((peer_type == PeerType::Node
                    && conn.last_seen() + MAX_NORMAL_KEEP_ALIVE < curr_stamp)
                    || (peer_type == PeerType::Bootstrapper
                        && conn.last_seen() + MAX_BOOTSTRAPPER_KEEP_ALIVE < curr_stamp))
        };

        let is_conn_without_handshake = |conn: &Connection| -> bool {
            !conn.is_post_handshake() && conn.last_seen() + MAX_PREHANDSHAKE_KEEP_ALIVE < curr_stamp
        };

        // Kill connections to nodes which are no longer seen
        write_or_die!(self.connections()).retain(|_, conn| {
            !(is_conn_faulty(&conn) || is_conn_inactive(&conn) || is_conn_without_handshake(&conn))
        });

        if peer_type != PeerType::Bootstrapper {
            self.connection_handler
                .unreachable_nodes
                .cleanup(curr_stamp - MAX_UNREACHABLE_MARK_TIME);
        }

        // If the number of peers exceeds the desired value, close a random selection of
        // post-handshake connections to lower it
        if peer_type == PeerType::Node {
            let max_allowed_nodes = self.config.max_allowed_nodes;
            let peer_count = self.connections_posthandshake_count(Some(PeerType::Bootstrapper));
            if peer_count > max_allowed_nodes {
                let mut rng = rand::thread_rng();
                let to_drop = read_or_die!(self.connections())
                    .keys()
                    .copied()
                    .choose_multiple(&mut rng, (peer_count - max_allowed_nodes) as usize);

                for token in to_drop {
                    self.remove_connection(token);
                }
            }
        }

        // recreate the active peer list if it's not aligned with the connection list
        // while unlikely to happen in practice, we definitely don't want it to happen
        if read_or_die!(self.connections()).len() != self.get_all_current_peers(None).len() {
            warn!("The peer stats are not aligned with the connections; fixing");
            let mut active_peers = write_or_die!(self.active_peer_stats);
            active_peers.clear();
            for conn in read_or_die!(self.connections()).values() {
                if let Some(id) = conn.remote_id() {
                    active_peers.insert(id.as_raw(), conn.remote_peer_stats()?);
                }
            }
        }

        // reconnect to bootstrappers after a specified amount of time
        if peer_type == PeerType::Node
            && curr_stamp >= self.get_last_bootstrap() + self.config.bootstrapping_interval * 1000
        {
            self.attempt_bootstrap();
        }

        Ok(())
    }

    #[inline]
    pub fn connections(&self) -> &RwLock<Connections> { &self.connection_handler.connections }

    #[inline]
    pub fn networks(&self) -> &RwLock<Networks> { &self.connection_handler.networks }

    /// Returns true if `addr` is in the `unreachable_nodes` list.
    pub fn is_unreachable(&self, addr: SocketAddr) -> bool {
        self.connection_handler.unreachable_nodes.contains(addr)
    }

    /// Adds the `addr` to the `unreachable_nodes` list.
    pub fn add_unreachable(&self, addr: SocketAddr) -> bool {
        self.connection_handler.unreachable_nodes.insert(addr)
    }

    fn accept(&self) -> Fallible<()> {
        let self_peer = self.self_peer;
        let (socket, addr) = self.connection_handler.server.accept()?;

        {
            let conn_read_lock = read_or_die!(self.connections());

            if self.self_peer.peer_type() == PeerType::Node
                && self.config.hard_connection_limit.is_some()
                && conn_read_lock.values().len()
                    >= self.config.hard_connection_limit.unwrap() as usize
            {
                bail!("Too many connections, rejecting attempt from {:?}", addr);
            }

            if conn_read_lock
                .values()
                .any(|conn| conn.remote_addr() == addr)
            {
                bail!("Duplicate connection attempt from {:?}; rejecting", addr);
            }
        }

        debug!(
            "Accepting new connection from {:?} to {:?}:{}",
            addr,
            self_peer.ip(),
            self_peer.port()
        );

        self.log_event(P2PEvent::ConnectEvent(addr));

        let token = Token(
            self.connection_handler
                .next_id
                .fetch_add(1, Ordering::SeqCst),
        );
        let key_pair = utils::clone_snow_keypair(&self.connection_handler.key_pair);

        let remote_peer = RemotePeer {
            id: Default::default(),
            addr,
            peer_external_port: Arc::new(AtomicU16::new(addr.port())),
            peer_type: PeerType::Node,
        };

        let conn = Connection::new(
            self,
            socket,
            token,
            remote_peer,
            self_peer.peer_type(),
            key_pair,
            false,
            self.connection_handler.noise_params.clone(),
        );

        let register_status = conn.register(&self.poll);
        self.add_connection(conn);

        register_status
    }

    pub fn connect(
        &self,
        peer_type: PeerType,
        addr: SocketAddr,
        peer_id_opt: Option<P2PNodeId>,
    ) -> Fallible<()> {
        debug!("Attempting to connect to {}", addr);

        self.log_event(P2PEvent::InitiatingConnection(addr));
        let self_peer = self.self_peer;
        if peer_type == PeerType::Node {
            let current_peer_count =
                self.connections_posthandshake_count(Some(PeerType::Bootstrapper));
            if current_peer_count > self.config.max_allowed_nodes {
                return Err(Error::from(fails::MaxmimumAmountOfPeers {
                    max_allowed_peers: self.config.max_allowed_nodes,
                    number_of_peers:   current_peer_count,
                }));
            }
        }

        // Don't connect to ourselves
        if self.self_peer.addr == addr || peer_id_opt == Some(self.id()) {
            return Err(Error::from(fails::DuplicatePeerError { peer_id_opt, addr }));
        }

        // Don't connect to peers with a known P2PNodeId or IP+port
        for conn in read_or_die!(self.connections()).values() {
            if conn.remote_addr() == addr
                || (peer_id_opt.is_some() && conn.remote_id() == peer_id_opt)
            {
                return Err(Error::from(fails::DuplicatePeerError { peer_id_opt, addr }));
            }
        }

        if peer_type == PeerType::Node && self.is_unreachable(addr) {
            error!("Node marked as unreachable, so not allowing the connection");
            return Err(Error::from(fails::UnreachablePeerError));
        }

        match TcpStream::connect(&addr) {
            Ok(socket) => {
                if let Some(ref service) = self.stats_export_service {
                    service.conn_received_inc();
                };
                let token = Token(
                    self.connection_handler
                        .next_id
                        .fetch_add(1, Ordering::SeqCst),
                );

                let keypair = utils::clone_snow_keypair(&self.connection_handler.key_pair);
                let remote_peer = RemotePeer {
                    id: Default::default(),
                    addr,
                    peer_external_port: Arc::new(AtomicU16::new(addr.port())),
                    peer_type,
                };

                let conn = Connection::new(
                    self,
                    socket,
                    token,
                    remote_peer,
                    self_peer.peer_type(),
                    keypair,
                    true,
                    self.connection_handler.noise_params.clone(),
                );

                conn.register(&self.poll)?;

                self.add_connection(conn);
                self.log_event(P2PEvent::ConnectEvent(addr));

                if let Some(ref conn) = self.find_connection_by_token(token) {
                    conn.send_handshake_request()?;
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

    pub fn dump_start(&mut self, log_dumper: SyncSender<DumpItem>) {
        self.connection_handler.log_dumper = Some(log_dumper);
    }

    pub fn dump_stop(&mut self) { self.connection_handler.log_dumper = None; }

    fn connections_posthandshake_count(&self, exclude_type: Option<PeerType>) -> u16 {
        // We will never have more than 2^16 connections per node, so this conversion is
        // safe.
        read_or_die!(self.active_peer_stats)
            .values()
            .filter(|&peer| {
                if let Some(exclude_type) = exclude_type {
                    peer.peer_type != exclude_type
                } else {
                    true
                }
            })
            .count() as u16
    }

    /// Adds a new node to the banned list and marks its connection for closure
    pub fn ban_node(&self, peer: BannedNode) -> Fallible<()> {
        info!("Banning node {:?}", peer);

        let store_key = peer.serialize();
        {
            let ban_kvs_env = safe_read!(self.kvs)?;
            let ban_store = ban_kvs_env.open_single(BAN_STORE_NAME, StoreOptions::create())?;
            let mut writer = ban_kvs_env.write()?;
            // TODO: insert ban expiry timestamp as the Value
            ban_store.put(&mut writer, store_key, &Value::U64(0))?;
            writer.commit().unwrap();
        }

        match peer {
            BannedNode::ById(id) => {
                if let Some(conn) = self.find_connection_by_id(id) {
                    self.remove_connection(conn.token);
                }
            }
            BannedNode::ByAddr(addr) => {
                for conn in self.find_connections_by_ip(addr) {
                    self.remove_connection(conn.token);
                }
            }
        }

        Ok(())
    }

    /// It removes a node from the banned peer list.
    pub fn unban_node(&self, peer: BannedNode) -> Fallible<()> {
        info!("Unbanning node {:?}", peer);

        let store_key = peer.serialize();
        {
            let ban_kvs_env = safe_read!(self.kvs)?;
            let ban_store = ban_kvs_env.open_single(BAN_STORE_NAME, StoreOptions::create())?;
            let mut writer = ban_kvs_env.write()?;
            // TODO: insert ban expiry timestamp as the Value
            ban_store.delete(&mut writer, store_key)?;
            writer.commit().unwrap();
        }

        Ok(())
    }

    pub fn is_banned(&self, peer: BannedNode) -> Fallible<bool> {
        let ban_kvs_env = safe_read!(self.kvs)?;
        let ban_store = ban_kvs_env.open_single(BAN_STORE_NAME, StoreOptions::create())?;

        let ban_reader = ban_kvs_env.read()?;
        let store_key = peer.serialize();

        Ok(ban_store.get(&ban_reader, store_key)?.is_some())
    }

    pub fn get_banlist(&self) -> Fallible<Vec<BannedNode>> {
        let ban_kvs_env = safe_read!(self.kvs)?;
        let ban_store = ban_kvs_env.open_single(BAN_STORE_NAME, StoreOptions::create())?;

        let ban_reader = ban_kvs_env.read()?;
        let ban_iter = ban_store.iter_start(&ban_reader)?;

        let mut banlist = Vec::new();
        for entry in ban_iter {
            let (id_bytes, _expiry) = entry?;
            let node_to_ban = BannedNode::deserialize(id_bytes)?;
            banlist.push(node_to_ban);
        }

        Ok(banlist)
    }

    fn clear_bans(&self) -> Fallible<()> {
        let kvs_env = safe_read!(self.kvs)?;
        let ban_store = kvs_env.open_single(BAN_STORE_NAME, StoreOptions::create())?;
        let mut writer = kvs_env.write()?;
        ban_store.clear(&mut writer)?;
        into_err!(writer.commit())
    }

    /// It adds this server to `network_id` network.
    pub fn add_network(&self, network_id: NetworkId) {
        write_or_die!(self.connection_handler.networks).insert(network_id);
    }

    pub fn find_connection_by_id(&self, id: P2PNodeId) -> Option<Arc<Connection>> {
        read_or_die!(self.connections())
            .values()
            .find(|conn| conn.remote_id() == Some(id))
            .map(|conn| Arc::clone(conn))
    }

    pub fn find_connection_by_token(&self, token: Token) -> Option<Arc<Connection>> {
        read_or_die!(self.connections())
            .get(&token)
            .map(|conn| Arc::clone(conn))
    }

    pub fn find_connection_by_ip_addr(&self, addr: SocketAddr) -> Option<Arc<Connection>> {
        read_or_die!(self.connections())
            .values()
            .find(|conn| conn.remote_addr() == addr)
            .map(|conn| Arc::clone(conn))
    }

    pub fn find_connections_by_ip(&self, ip: IpAddr) -> Vec<Arc<Connection>> {
        read_or_die!(self.connections())
            .values()
            .filter(|conn| conn.remote_peer().addr().ip() == ip)
            .map(|conn| Arc::clone(conn))
            .collect()
    }

    pub fn remove_connection(&self, token: Token) -> bool {
        if let Some(conn) = write_or_die!(self.connections()).remove(&token) {
            write_or_die!(conn.low_level).conn_ref = None; // necessary in order for Drop to kick in
            true
        } else {
            false
        }
    }

    pub fn add_connection(&self, conn: Arc<Connection>) {
        write_or_die!(self.connections()).insert(conn.token, conn);
    }

    pub fn conn_event(&self, event: &Event, deduplication_queues: &mut DeduplicationQueues) {
        let token = event.token();

        if let Some(conn) = self.find_connection_by_token(token) {
            if let Err(e) = conn.ready(event, deduplication_queues) {
                error!("Error while processing a connection event: {}", e);
                conn.handler().remove_connection(conn.token);
            }
        }
    }

    /// Waits for P2PNode termination. Use `P2PNode::close` to notify the
    /// termination.
    ///
    /// It is safe to call this function several times, even from internal
    /// P2PNode thread.
    pub fn join(&self) -> Fallible<()> {
        let id_opt = read_or_die!(self.thread).id;
        if let Some(id) = id_opt {
            let current_thread_id = std::thread::current().id();
            if id != current_thread_id {
                let join_handle_opt = write_or_die!(self.thread).join_handle.take();
                if let Some(join_handle) = join_handle_opt {
                    join_handle.join().map_err(|e| {
                        let join_error = format!("{:?}", e);
                        fails::JoinError {
                            cause: err_msg(join_error),
                        }
                    })?;
                    Ok(())
                } else {
                    Err(Error::from(fails::JoinError {
                        cause: err_msg("Event thread has already be joined"),
                    }))
                }
            } else {
                Err(Error::from(fails::JoinError {
                    cause: err_msg("It is called from inside event thread"),
                }))
            }
        } else {
            Err(Error::from(fails::JoinError {
                cause: err_msg("Missing event thread id"),
            }))
        }
    }

    pub fn get_version(&self) -> String { crate::VERSION.to_string() }

    pub fn id(&self) -> P2PNodeId { self.self_peer.id }

    #[inline]
    pub fn peer_type(&self) -> PeerType { self.self_peer.peer_type }

    fn log_event(&self, event: P2PEvent) {
        if let Some(ref log) = self.connection_handler.event_log {
            if let Err(e) = log.send(event) {
                error!("Couldn't send error {:?}", e)
            }
        }
    }

    pub fn get_uptime(&self) -> i64 {
        Utc::now().timestamp_millis() - self.start_time.timestamp_millis()
    }

    fn process_network_packet(
        &self,
        inner_pkt: NetworkPacket,
        source_id: P2PNodeId,
    ) -> Fallible<usize> {
        let serialized_packet = serialize_into_buffer(
            &NetworkMessage::NetworkPacket(inner_pkt.clone(), Some(get_current_stamp()), None),
            256,
        )?;

        let peers_to_skip = match inner_pkt.packet_type {
            NetworkPacketType::DirectMessage(..) => vec![],
            NetworkPacketType::BroadcastedMessage(ref dont_send_to) => {
                if self.config.relay_broadcast_percentage < 1.0 {
                    use rand::seq::SliceRandom;
                    let mut rng = rand::thread_rng();
                    let mut peers = self.get_all_current_peers(Some(PeerType::Node));
                    peers.retain(|peer| !dont_send_to.contains(peer));
                    let peers_to_take = f64::floor(
                        f64::from(peers.len() as u32) * self.config.relay_broadcast_percentage,
                    );
                    peers
                        .choose_multiple(&mut rng, peers_to_take as usize)
                        .copied()
                        .collect::<Vec<_>>()
                } else {
                    Vec::new()
                }
            }
        };

        match inner_pkt.packet_type {
            NetworkPacketType::DirectMessage(ref receiver) => {
                // safe, used only in a post-handshake context
                let filter =
                    |conn: &Connection| read_or_die!(conn.remote_peer.id).unwrap() == *receiver;

                self.send_over_all_connections(serialized_packet, &filter)
            }
            NetworkPacketType::BroadcastedMessage(ref dont_relay_to) => {
                let filter = |conn: &Connection| {
                    is_valid_connection_in_broadcast(
                        conn,
                        source_id,
                        &peers_to_skip,
                        &dont_relay_to,
                        inner_pkt.network_id,
                    )
                };

                self.send_over_all_connections(serialized_packet, &filter)
            }
        }
    }

    pub fn get_peer_stats(&self) -> Vec<PeerStats> {
        read_or_die!(self.active_peer_stats)
            .values()
            .cloned()
            .collect()
    }

    pub fn get_all_current_peers(&self, peer_type: Option<PeerType>) -> Vec<P2PNodeId> {
        read_or_die!(self.active_peer_stats)
            .values()
            .filter(|peer| peer_type.is_none() || peer_type == Some(peer.peer_type))
            .map(|peer| P2PNodeId(peer.id))
            .collect()
    }

    pub fn get_node_peer_ids(&self) -> Vec<u64> {
        read_or_die!(self.active_peer_stats)
            .iter()
            .filter(|(_, stats)| stats.peer_type == PeerType::Node)
            .map(|(id, _)| id)
            .copied()
            .collect()
    }

    #[cfg(not(windows))]
    pub fn get_ip() -> Option<IpAddr> {
        let localhost = IpAddr::from_str("127.0.0.1").unwrap();
        let mut ip: IpAddr = localhost;

        if let Ok(addresses) = get_if_addrs::get_if_addrs() {
            for adapter in addresses {
                if let Some(addr) = get_ip_if_suitable(&adapter.addr.ip()) {
                    ip = addr
                }
            }
        }
        if ip == localhost {
            None
        } else {
            Some(ip)
        }
    }

    #[cfg(windows)]
    pub fn get_ip() -> Option<IpAddr> {
        let localhost = IpAddr::from_str("127.0.0.1").unwrap();
        let mut ip: IpAddr = localhost;

        if let Ok(adapters) = ipconfig::get_adapters() {
            for adapter in adapters {
                for ip_new in adapter.ip_addresses() {
                    if let Some(addr) = get_ip_if_suitable(ip_new) {
                        ip = addr
                    }
                }
            }
        }

        if ip == localhost {
            None
        } else {
            Some(ip)
        }
    }

    pub fn internal_addr(&self) -> SocketAddr { self.self_peer.addr }

    #[inline(always)]
    fn process(
        &self,
        events: &mut Events,
        deduplication_queues: &mut DeduplicationQueues,
    ) -> Fallible<()> {
        self.poll.poll(
            events,
            Some(Duration::from_millis(self.config.poll_interval)),
        )?;

        for event in events.iter() {
            match event.token() {
                SERVER => {
                    debug!("Got a new connection!");
                    self.accept().map_err(|e| error!("{}", e)).ok();
                    if let Some(ref service) = &self.stats_export_service {
                        service.conn_received_inc();
                    };
                }
                _ => {
                    self.conn_event(&event, deduplication_queues);
                }
            }
        }

        events.clear();

        Ok(())
    }

    /// It extracts and sends each queued request.
    ///
    /// # Mio poll-loop thread
    ///
    /// The read process is executed inside the MIO
    /// poll-loop thread, and any write is queued to be processed later in that
    /// poll-loop.
    #[inline(always)]
    pub fn process_network_requests(&self, receivers: &Receivers) {
        for request in receivers.network_requests.try_iter() {
            trace!(
                "Processing network raw request ({} bytes) in connection {}",
                request.data.len().unwrap_or(0),
                usize::from(request.token)
            );

            if let Some(ref conn) = self.find_connection_by_token(request.token) {
                if let Err(err) = conn.async_send_from_poll_loop(request.data, request.priority) {
                    error!("Can't send a raw network request to {}: {}", conn, err);
                    conn.handler().remove_connection(conn.token);
                }
            } else {
                debug!(
                    "Can't send a raw network request; connection {} is missing",
                    usize::from(request.token)
                );
                return;
            }
        }
    }

    pub fn close(&self) -> bool {
        info!("P2PNode shutting down.");
        self.is_terminated.store(true, Ordering::Relaxed);
        true
    }

    pub fn close_and_join(&self) -> Fallible<()> {
        self.close();
        self.join()
    }

    pub fn rpc_subscription_start(&self) { self.is_rpc_online.store(true, Ordering::Relaxed); }

    pub fn rpc_subscription_stop(&self) -> bool {
        self.is_rpc_online.store(false, Ordering::Relaxed);
        true
    }

    #[cfg(feature = "network_dump")]
    pub fn activate_dump(&self, path: &str, raw: bool) -> Fallible<()> {
        let path = std::path::PathBuf::from(path);
        self.dump_switch.send((path, raw))?;
        self.dump_start(self.dump_tx.clone());
        Ok(())
    }

    #[cfg(feature = "network_dump")]
    pub fn stop_dump(&self) -> Fallible<()> {
        let path = std::path::PathBuf::new();
        self.dump_switch.send((path, false))?;
        self.dump_stop();
        Ok(())
    }

    fn send_get_peers(&self) {
        if let Ok(nids) = safe_read!(self.networks()) {
            let req = NetworkRequest::GetPeers(nids.iter().copied().collect());
            let msg = NetworkMessage::NetworkRequest(req, None, None);
            let filter = |_: &Connection| true;

            if let Err(e) = serialize_into_buffer(&msg, 256)
                .and_then(|data| self.send_over_all_connections(data, &filter))
            {
                error!("A network message couldn't be forwarded: {}", e);
            }
        }
    }

    pub fn send_retransmit(
        &self,
        requested_type: RequestedElementType,
        since: u64,
        nid: NetworkId,
    ) {
        let req = NetworkRequest::Retransmit(requested_type, since, nid);
        let msg = NetworkMessage::NetworkRequest(req, None, None);
        let filter = |_: &Connection| true;

        if let Err(e) = serialize_into_buffer(&msg, 256)
            .and_then(|data| self.send_over_all_connections(data, &filter))
        {
            error!("A network message couldn't be forwarded: {}", e);
        }
    }
}

impl Drop for P2PNode {
    fn drop(&mut self) { let _ = self.close_and_join(); }
}

/// Connetion is valid for a broadcast if sender is not target,
/// network_id is owned by connection, and the remote peer is not
/// a bootstrap node.
fn is_valid_connection_in_broadcast(
    conn: &Connection,
    sender: P2PNodeId,
    peers_to_skip: &[P2PNodeId],
    dont_relay_to: &[P2PNodeId],
    network_id: NetworkId,
) -> bool {
    // safe, used only in a post-handshake context
    let peer_id = read_or_die!(conn.remote_peer.id).unwrap();

    if conn.remote_peer.peer_type() != PeerType::Bootstrapper
        && peer_id != sender
        && !peers_to_skip.contains(&peer_id)
        && !dont_relay_to.contains(&peer_id)
    {
        read_or_die!(conn.remote_end_networks()).contains(&network_id)
    } else {
        false
    }
}

/// Connection is valid to send over as it has completed the handshake
pub fn is_valid_connection_post_handshake(conn: &Connection) -> bool { conn.is_post_handshake() }

fn get_ip_if_suitable(addr: &IpAddr) -> Option<IpAddr> {
    match addr {
        V4(x) => {
            if !x.is_loopback() && !x.is_link_local() && !x.is_multicast() && !x.is_broadcast() {
                Some(IpAddr::V4(*x))
            } else {
                None
            }
        }
        V6(_) => None,
    }
}

#[inline]
pub fn send_direct_message(
    node: &P2PNode,
    source_id: P2PNodeId,
    target_id: Option<P2PNodeId>,
    network_id: NetworkId,
    msg: HybridBuf,
) -> Fallible<()> {
    send_message_from_cursor(node, source_id, target_id, vec![], network_id, msg, false)
}

#[inline]
pub fn send_broadcast_message(
    node: &P2PNode,
    source_id: P2PNodeId,
    dont_relay_to: Vec<P2PNodeId>,
    network_id: NetworkId,
    msg: HybridBuf,
) -> Fallible<()> {
    send_message_from_cursor(node, source_id, None, dont_relay_to, network_id, msg, true)
}

pub fn send_message_from_cursor(
    node: &P2PNode,
    source_id: P2PNodeId,
    target_id: Option<P2PNodeId>,
    dont_relay_to: Vec<P2PNodeId>,
    network_id: NetworkId,
    message: HybridBuf,
    broadcast: bool,
) -> Fallible<()> {
    trace!("Queueing message!");

    let packet_type = if broadcast {
        NetworkPacketType::BroadcastedMessage(dont_relay_to)
    } else {
        let receiver =
            target_id.ok_or_else(|| err_msg("Direct Message requires a valid target id"))?;

        NetworkPacketType::DirectMessage(receiver)
    };

    // Create packet.
    let packet = NetworkPacket {
        packet_type,
        network_id,
        message,
    };

    if let Ok(sent_packets) = node.process_network_packet(packet, source_id) {
        trace!("Send a packet to {} peers", sent_packets);
    } else {
        error!("Couldn't send a packet");
    }

    Ok(())
}
