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
        request::RequestedElementType, Buckets, NetworkId, NetworkMessage, NetworkMessagePayload,
        NetworkPacket, NetworkPacketType, NetworkRequest,
    },
    p2p::{banned_nodes::BannedNode, fails, unreachable_nodes::UnreachableNodes},
    stats_engine::StatsEngine,
    utils,
};
use chrono::prelude::*;
use concordium_common::{
    cache::Cache, hybrid_buf::HybridBuf, serial::Serial, stats_export_service::StatsExportService,
};
use failure::{err_msg, Error, Fallible};
#[cfg(not(target_os = "windows"))]
use get_if_addrs;
#[cfg(target_os = "windows")]
use ipconfig;
use mio::{
    net::{TcpListener, TcpStream},
    Events, Poll, PollOpt, Ready, Token,
};
use nohash_hasher::BuildNoHashHasher;
use rand::seq::IteratorRandom;
use rkv::{Manager, Rkv, StoreOptions, Value};
use snow::Keypair;

use std::{
    cmp::Reverse,
    collections::{HashMap, HashSet},
    mem,
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
    thread::{self, JoinHandle},
    time::{Duration, SystemTime},
};

const SERVER: Token = Token(0);
const BAN_STORE_NAME: &str = "bans";

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
    pub catch_up_batch_limit: u64,
    pub timeout_bucket_entry_period: u64,
    pub bucket_cleanup_interval: u64,
    #[cfg(feature = "beta")]
    pub beta_username: String,
}

#[derive(Default)]
pub struct P2PNodeThreads {
    pub join_handles: Vec<JoinHandle<()>>,
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
    server:                  TcpListener,
    next_id:                 AtomicUsize,
    key_pair:                Keypair,
    pub event_log:           Option<SyncSender<P2PEvent>>,
    pub buckets:             RwLock<Buckets>,
    pub log_dumper:          Option<SyncSender<DumpItem>>,
    pub noise_params:        snow::params::NoiseParams,
    pub network_messages_hi: SyncSender<NetworkRawRequest>,
    pub network_messages_lo: SyncSender<NetworkRawRequest>,
    pub connections:         RwLock<Connections>,
    pub unreachable_nodes:   UnreachableNodes,
    pub networks:            RwLock<Networks>,
    pub last_bootstrap:      AtomicU64,
    pub last_peer_update:    AtomicU64,
}

impl ConnectionHandler {
    fn new(
        conf: &Config,
        server: TcpListener,
        network_messages_hi: SyncSender<NetworkRawRequest>,
        network_messages_lo: SyncSender<NetworkRawRequest>,
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
            next_id: AtomicUsize::new(1),
            key_pair,
            event_log,
            buckets: RwLock::new(Buckets::new()),
            log_dumper: None,
            noise_params,
            network_messages_hi,
            network_messages_lo,
            connections: Default::default(),
            unreachable_nodes: UnreachableNodes::new(),
            networks: RwLock::new(networks),
            last_bootstrap: Default::default(),
            last_peer_update: Default::default(),
        }
    }
}

pub struct Receivers {
    pub network_messages_hi: Receiver<NetworkRawRequest>,
    pub network_messages_lo: Receiver<NetworkRawRequest>,
}

#[allow(dead_code)] // caused by the dump_network feature; will fix in a follow-up
pub struct P2PNode {
    pub self_ref:             Option<Pin<Arc<Self>>>,
    pub self_peer:            P2PPeer,
    threads:                  RwLock<P2PNodeThreads>,
    pub poll:                 Poll,
    pub connection_handler:   ConnectionHandler,
    pub rpc_queue:            SyncSender<NetworkMessage>,
    dump_switch:              SyncSender<(std::path::PathBuf, bool)>,
    dump_tx:                  SyncSender<crate::dumper::DumpItem>,
    pub stats_export_service: Option<StatsExportService>,
    pub config:               P2PNodeConfig,
    start_time:               DateTime<Utc>,
    pub is_rpc_online:        AtomicBool,
    pub is_terminated:        AtomicBool,
    pub kvs:                  Arc<RwLock<Rkv>>,
    pub transactions_cache:   RwLock<Cache<Vec<u8>>>,
    pub stats_engine:         RwLock<StatsEngine>,
}
// a convenience macro to send an object to all connections
macro_rules! send_to_all {
    ($foo_name:ident, $object_type:ty, $req_type:ident) => {
        pub fn $foo_name(&self, object: $object_type) {
            let request = NetworkRequest::$req_type(object);
            let mut message = NetworkMessage {
                timestamp1: None,
                timestamp2: None,
                payload: NetworkMessagePayload::NetworkRequest(request)
            };
            let filter = |_: &Connection| true;

            if let Err(e) = HybridBuf::with_capacity(256)
                .map_err(Error::from)
                .and_then(|mut buf| message.serialize(&mut buf).map(|_| buf))
                .and_then(|buf| self.send_over_all_connections(buf, &filter))
            {
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
            catch_up_batch_limit: conf.connection.catch_up_batch_limit,
            timeout_bucket_entry_period: if peer_type == PeerType::Bootstrapper {
                conf.bootstrapper.bootstrapper_timeout_bucket_entry_period
            } else {
                conf.cli.timeout_bucket_entry_period
            },
            bucket_cleanup_interval: conf.common.bucket_cleanup_interval,
            #[cfg(feature = "beta")]
            beta_username: conf.cli.beta_username.clone(),
        };

        let (network_msgs_sender_hi, network_msgs_receiver_hi) =
            sync_channel(config::RAW_NETWORK_MSG_QUEUE_DEPTH_HI);
        let (network_msgs_sender_lo, network_msgs_receiver_lo) =
            sync_channel(config::RAW_NETWORK_MSG_QUEUE_DEPTH_LO);

        let receivers = Receivers {
            network_messages_hi: network_msgs_receiver_hi,
            network_messages_lo: network_msgs_receiver_lo,
        };

        let connection_handler = ConnectionHandler::new(
            conf,
            server,
            network_msgs_sender_hi,
            network_msgs_sender_lo,
            event_log,
        );

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
            threads: RwLock::new(P2PNodeThreads::default()),
            config,
            dump_switch: act_tx,
            dump_tx,
            is_rpc_online: AtomicBool::new(false),
            connection_handler,
            self_peer,
            stats_export_service,
            is_terminated: Default::default(),
            kvs,
            transactions_cache,
            stats_engine,
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

    fn is_bucket_cleanup_enabled(&self) -> bool { self.config.timeout_bucket_entry_period > 0 }

    pub fn spawn(&self, receivers: Receivers) {
        let self_clone = self.self_ref.clone().unwrap(); // safe, always available
        let poll_thread = spawn_or_die!("Poll thread", move || {
            let mut events = Events::with_capacity(10);
            let mut log_time = SystemTime::now();
            let mut last_buckets_cleaned = SystemTime::now();

            let mut deduplication_queues = DeduplicationQueues::default();

            loop {
                let _ = self_clone
                    .receive_network_events(&mut events, &mut deduplication_queues)
                    .map_err(|e| error!("{}", e));

                // Run periodic tasks
                let now = SystemTime::now();
                if let Ok(difference) = now.duration_since(log_time) {
                    if difference >= Duration::from_secs(self_clone.config.housekeeping_interval) {
                        // Check the termination switch
                        if self_clone.is_terminated.load(Ordering::Relaxed) {
                            break;
                        }

                        if let Err(e) = self_clone.connection_housekeeping() {
                            error!("Issue with connection cleanups: {:?}", e);
                        }
                        if self_clone.peer_type() != PeerType::Bootstrapper {
                            self_clone.measure_connection_latencies();
                        }

                        let peer_stat_list = self_clone.get_peer_stats(None);
                        self_clone.check_peers(&peer_stat_list);
                        self_clone.print_stats(&peer_stat_list);

                        log_time = now;
                    }
                }

                if self_clone.is_bucket_cleanup_enabled() {
                    if let Ok(difference) = now.duration_since(last_buckets_cleaned) {
                        if difference
                            >= Duration::from_millis(self_clone.config.bucket_cleanup_interval)
                        {
                            write_or_die!(self_clone.connection_handler.buckets)
                                .clean_buckets(self_clone.config.timeout_bucket_entry_period);
                            last_buckets_cleaned = now;
                        }
                    }
                }
            }
        });

        let self_clone = self.self_ref.clone().unwrap(); // safe, always available
        let send_thread = spawn_or_die!("Send thread", move || {
            loop {
                self_clone.send_queued_messages(&receivers);
                thread::sleep(Duration::from_millis(self_clone.config.poll_interval));
            }
        });

        // Register info about thread into P2PNode.
        let mut locked_threads = write_or_die!(self.threads);
        locked_threads.join_handles.push(poll_thread);
        locked_threads.join_handles.push(send_thread);
    }

    fn measure_connection_latencies(&self) {
        debug!("Measuring connection latencies");

        let connections = read_or_die!(self.connections()).clone();
        for conn in connections.values().filter(|conn| conn.is_post_handshake()) {
            // don't send pings to lagging connections so
            // that the latency calculation is not invalid
            if conn.last_seen() > conn.get_last_ping_sent() {
                if let Err(e) = conn.send_ping() {
                    error!("Can't send a ping on {}: {}", conn, e);
                }
            }
        }
    }

    fn connection_housekeeping(&self) -> Fallible<()> {
        debug!("Running connection housekeeping");

        let curr_stamp = get_current_stamp();
        let peer_type = self.peer_type();

        // deduplicate by P2PNodeId
        {
            let conns = read_or_die!(self.connections()).clone();
            let mut conns = conns
                .values()
                .filter(|conn| conn.is_post_handshake())
                .collect::<Vec<_>>();
            conns.sort_by_key(|conn| (conn.remote_id(), Reverse(conn.token)));
            conns.dedup_by_key(|conn| conn.remote_id());
            write_or_die!(self.connections())
                .retain(|_, conn| conns.iter().map(|c| c.token).any(|t| conn.token == t));
        }

        let is_conn_faulty = |conn: &Connection| -> bool {
            conn.failed_pkts() >= config::MAX_FAILED_PACKETS_ALLOWED
                || if let Some(max_latency) = self.config.max_latency {
                    conn.get_last_latency() >= max_latency
                } else {
                    false
                }
        };

        let is_conn_inactive = |conn: &Connection| -> bool {
            conn.is_post_handshake()
                && ((peer_type == PeerType::Node
                    && conn.last_seen() + config::MAX_NORMAL_KEEP_ALIVE < curr_stamp)
                    || (peer_type == PeerType::Bootstrapper
                        && conn.last_seen() + config::MAX_BOOTSTRAPPER_KEEP_ALIVE < curr_stamp))
        };

        let is_conn_without_handshake = |conn: &Connection| -> bool {
            !conn.is_post_handshake()
                && conn.last_seen() + config::MAX_PREHANDSHAKE_KEEP_ALIVE < curr_stamp
        };

        // Kill faulty and inactive connections
        write_or_die!(self.connections()).retain(|_, conn| {
            !(is_conn_faulty(&conn) || is_conn_inactive(&conn) || is_conn_without_handshake(&conn))
        });

        if peer_type != PeerType::Bootstrapper {
            self.connection_handler
                .unreachable_nodes
                .cleanup(curr_stamp - config::MAX_UNREACHABLE_MARK_TIME);
        }

        // If the number of peers exceeds the desired value, close a random selection of
        // post-handshake connections to lower it
        if peer_type == PeerType::Node {
            let max_allowed_nodes = self.config.max_allowed_nodes;
            let peer_count = self.get_peer_stats(Some(PeerType::Node)).len() as u16;
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
        if peer_type == PeerType::Node {
            let current_peer_count = self.get_peer_stats(Some(PeerType::Node)).len() as u16;
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

    /// Adds a new node to the banned list and marks its connection for closure
    pub fn ban_node(&self, peer: BannedNode) -> Fallible<()> {
        info!("Banning node {:?}", peer);

        let mut store_key = Vec::new();
        peer.serial(&mut store_key)?;
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

        let mut store_key = Vec::new();
        peer.serial(&mut store_key)?;
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
        let mut store_key = Vec::new();
        peer.serial(&mut store_key)?;

        Ok(ban_store.get(&ban_reader, store_key)?.is_some())
    }

    pub fn get_banlist(&self) -> Fallible<Vec<BannedNode>> {
        let ban_kvs_env = safe_read!(self.kvs)?;
        let ban_store = ban_kvs_env.open_single(BAN_STORE_NAME, StoreOptions::create())?;

        let ban_reader = ban_kvs_env.read()?;
        let ban_iter = ban_store.iter_start(&ban_reader)?;

        let mut banlist = Vec::new();
        for entry in ban_iter {
            let (mut id_bytes, _expiry) = entry?;
            let node_to_ban = BannedNode::deserial(&mut id_bytes)?;
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
            self.bump_last_peer_update();
            true
        } else {
            false
        }
    }

    pub fn add_connection(&self, conn: Arc<Connection>) {
        write_or_die!(self.connections()).insert(conn.token, conn);
    }

    /// Waits for P2PNode termination. Use `P2PNode::close` to notify the
    /// termination.
    pub fn join(&self) -> Fallible<()> {
        for handle in mem::replace(
            &mut write_or_die!(self.threads).join_handles,
            Default::default(),
        ) {
            handle.join().map_err(|e| {
                let join_error = format!("{:?}", e);
                fails::JoinError {
                    cause: err_msg(join_error),
                }
            })?;
        }
        Ok(())
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
        let peers_to_skip = match inner_pkt.packet_type {
            NetworkPacketType::DirectMessage(..) => vec![],
            NetworkPacketType::BroadcastedMessage(ref dont_relay_to) => {
                if self.config.relay_broadcast_percentage < 1.0 {
                    use rand::seq::SliceRandom;
                    let mut rng = rand::thread_rng();
                    let mut peers = self.get_node_peer_ids();
                    peers.retain(|id| !dont_relay_to.contains(&P2PNodeId(*id)));
                    let peers_to_take = f64::floor(
                        f64::from(peers.len() as u32) * self.config.relay_broadcast_percentage,
                    );
                    peers
                        .choose_multiple(&mut rng, peers_to_take as usize)
                        .map(|id| P2PNodeId(*id))
                        .collect::<Vec<_>>()
                } else {
                    dont_relay_to.to_owned()
                }
            }
        };

        let target = if let NetworkPacketType::DirectMessage(receiver) = inner_pkt.packet_type {
            Some(receiver)
        } else {
            None
        };
        let network_id = inner_pkt.network_id;

        let mut message = NetworkMessage {
            timestamp1: Some(get_current_stamp()),
            timestamp2: None,
            payload:    NetworkMessagePayload::NetworkPacket(inner_pkt),
        };

        if let Some(target_id) = target {
            // direct messages
            let filter =
                |conn: &Connection| read_or_die!(conn.remote_peer.id).unwrap() == target_id;

            let mut serialized = HybridBuf::with_capacity(256)?;
            message.serialize(&mut serialized)?;

            self.send_over_all_connections(serialized, &filter)
        } else {
            // broadcast messages
            let filter = |conn: &Connection| {
                is_valid_connection_in_broadcast(conn, source_id, &peers_to_skip, network_id)
            };

            let mut serialized = HybridBuf::with_capacity(256)?;
            message.serialize(&mut serialized)?;

            self.send_over_all_connections(serialized, &filter)
        }
    }

    pub fn get_peer_stats(&self, peer_type: Option<PeerType>) -> Vec<PeerStats> {
        read_or_die!(self.connections())
            .values()
            .filter(|conn| conn.is_post_handshake())
            .filter(|conn| peer_type.is_none() || peer_type == Some(conn.remote_peer_type()))
            .filter_map(|conn| conn.remote_peer_stats().ok())
            .collect()
    }

    pub fn get_node_peer_ids(&self) -> Vec<u64> {
        self.get_peer_stats(Some(PeerType::Node))
            .into_iter()
            .map(|stats| stats.id)
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
    fn receive_network_events(
        &self,
        events: &mut Events,
        deduplication_queues: &mut DeduplicationQueues,
    ) -> Fallible<()> {
        events.clear();
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
                    let readiness = event.readiness();
                    if readiness.is_readable() || readiness.is_writable() {
                        if let Some(conn) = self.find_connection_by_token(event.token()) {
                            let mut read_result = Ok(());
                            {
                                let mut conn_lock = write_or_die!(conn.low_level);
                                if readiness.is_readable() {
                                    read_result = conn_lock.read_stream(deduplication_queues);
                                }
                                if readiness.is_writable() {
                                    // TODO: we might want to close the connection
                                    // when specific write errors are detected
                                    conn_lock.flush_socket()?;
                                }
                            }
                            if let Err(e) = read_result {
                                error!("Couldn't process a connection read event: {}", e);
                                conn.handler().remove_connection(conn.token);
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }

    #[inline(always)]
    pub fn send_queued_messages(&self, receivers: &Receivers) {
        for request in receivers.network_messages_hi.try_iter() {
            self.process_network_request(request);
        }

        for _ in 0..512 {
            if let Ok(request) = receivers.network_messages_lo.try_recv() {
                self.process_network_request(request);
            } else {
                break;
            }
        }
    }

    #[inline(always)]
    pub fn process_network_request(&self, request: NetworkRawRequest) {
        if let Some(ref conn) = self.find_connection_by_token(request.token) {
            trace!(
                "Attempting to send {}B to {}",
                request.data.len().unwrap_or(0),
                conn,
            );

            if let Err(err) = conn.async_send_from_poll_loop(request.data) {
                error!("Can't send a raw network request to {}: {}", conn, err);
                conn.handler().remove_connection(conn.token);
            }
        } else {
            debug!(
                "Can't send a raw network request; connection {} is missing",
                usize::from(request.token)
            );
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
            let request = NetworkRequest::GetPeers(nids.iter().copied().collect());
            let mut message = NetworkMessage {
                timestamp1: None,
                timestamp2: None,
                payload:    NetworkMessagePayload::NetworkRequest(request),
            };
            let filter = |_: &Connection| true;

            if let Err(e) = HybridBuf::with_capacity(256)
                .map_err(Error::from)
                .and_then(|mut buf| message.serialize(&mut buf).map(|_| buf))
                .and_then(|buf| self.send_over_all_connections(buf, &filter))
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
        let request = NetworkRequest::Retransmit(requested_type, since, nid);
        let mut message = NetworkMessage {
            timestamp1: None,
            timestamp2: None,
            payload:    NetworkMessagePayload::NetworkRequest(request),
        };
        let filter = |_: &Connection| true;

        if let Err(e) = HybridBuf::with_capacity(256)
            .map_err(Error::from)
            .and_then(|mut buf| message.serialize(&mut buf).map(|_| buf))
            .and_then(|buf| self.send_over_all_connections(buf, &filter))
        {
            error!("A network message couldn't be forwarded: {}", e);
        }
    }

    pub fn bump_last_peer_update(&self) {
        self.connection_handler
            .last_peer_update
            .store(get_current_stamp(), Ordering::SeqCst)
    }

    pub fn last_peer_update(&self) -> u64 {
        self.connection_handler
            .last_peer_update
            .load(Ordering::SeqCst)
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
    network_id: NetworkId,
) -> bool {
    // safe, used only in a post-handshake context
    let peer_id = read_or_die!(conn.remote_peer.id).unwrap();

    conn.remote_peer.peer_type() != PeerType::Bootstrapper
        && peer_id != sender
        && !peers_to_skip.contains(&peer_id)
        && read_or_die!(conn.remote_end_networks()).contains(&network_id)
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
        if sent_packets > 0 {
            trace!("Sent a packet to {} peers", sent_packets);
        }
    } else {
        error!("Couldn't send a packet");
    }

    Ok(())
}
