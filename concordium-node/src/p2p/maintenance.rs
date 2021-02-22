//! Node maintenance methods.

use chrono::prelude::*;
use crossbeam_channel::{self, Receiver, Sender};
use failure::Fallible;
use mio::{net::TcpListener, Events, Interest, Poll, Registry, Token};
use nohash_hasher::BuildNoHashHasher;
use rkv::{
    backend::{Lmdb, LmdbEnvironment},
    Manager, Rkv,
};

#[cfg(feature = "network_dump")]
use crate::dumper::{create_dump_thread, DumpItem};
#[cfg(feature = "staging_net")]
use crate::plugins::staging_net::get_username_from_jwt;
use crate::{
    common::{get_current_stamp, P2PNodeId, P2PPeer, PeerType},
    configuration::{self as config, Config},
    connection::{ConnChange, Connection, DeduplicationHashAlgorithm, DeduplicationQueues},
    consensus_ffi::{
        catch_up::PeerList,
        consensus::{ConsensusContainer, CALLBACK_QUEUE},
    },
    lock_or_die,
    network::{Buckets, NetworkId, Networks},
    p2p::{
        bans::BanId,
        connectivity::{accept, connect, connection_housekeeping, SELF_TOKEN},
        peers::check_peers,
    },
    plugins::consensus::{check_peer_states, update_peer_list},
    read_or_die, spawn_or_die,
    stats_export_service::StatsExportService,
    utils, write_or_die,
};

use std::{
    collections::HashMap,
    mem,
    net::{
        IpAddr::{self, V4, V6},
        Ipv4Addr, SocketAddr,
    },
    path::PathBuf,
    str::FromStr,
    sync::{
        atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering},
        Arc, Mutex, RwLock,
    },
    thread::JoinHandle,
    time::{Duration, Instant},
};

/// Configuration bits applicable to a node.
pub struct NodeConfig {
    pub no_net: bool,
    pub desired_nodes_count: u16,
    pub no_bootstrap_dns: bool,
    pub bootstrap_server: String,
    pub dns_resolvers: Vec<String>,
    pub dnssec_disabled: bool,
    pub bootstrap_nodes: Vec<String>,
    pub max_allowed_nodes: u16,
    pub relay_broadcast_percentage: f64,
    pub poll_interval: u64,
    pub housekeeping_interval: u64,
    pub bootstrapping_interval: u64,
    pub print_peers: bool,
    pub bootstrapper_wait_minimum_peers: u16,
    pub data_dir_path: PathBuf,
    pub max_latency: Option<u64>,
    pub hard_connection_limit: u16,
    pub catch_up_batch_limit: i64,
    pub timeout_bucket_entry_period: u64,
    pub bucket_cleanup_interval: u64,
    #[cfg(feature = "staging_net")]
    pub staging_net_username: String,
    pub thread_pool_size: usize,
    pub dedup_size_long: usize,
    pub dedup_size_short: usize,
    pub socket_read_size: usize,
    pub socket_write_size: usize,
    pub no_rebroadcast_consensus_validation: bool,
    pub drop_rebroadcast_probability: Option<f64>,
    #[cfg(feature = "malicious_testing")]
    pub partition_network_for_time: Option<usize>,
    #[cfg(feature = "malicious_testing")]
    pub breakage: Option<(String, u8, usize)>,
    pub bootstrapper_peer_list_size: usize,
    pub default_network: NetworkId,
    pub socket_so_linger: Option<u16>,
    pub events_queue_size: usize,
    pub deduplication_hashing_algorithm: DeduplicationHashAlgorithm,
}

/// The collection of connections to peer nodes.
pub type Connections = HashMap<Token, Connection, BuildNoHashHasher<usize>>;

/// Intercepts changes to connections and provides change notifiers.
pub struct ConnChanges {
    pub changes: Receiver<ConnChange>,
    notifier:    Sender<ConnChange>,
}

/// The set of objects related to node's connections.
pub struct ConnectionHandler {
    pub socket_server: TcpListener,
    pub next_token: AtomicUsize,
    pub buckets: RwLock<Buckets>,
    #[cfg(feature = "network_dump")]
    pub log_dumper: RwLock<Option<Sender<DumpItem>>>,
    pub conn_candidates: Mutex<Connections>,
    pub connections: RwLock<Connections>,
    pub conn_changes: ConnChanges,
    pub soft_bans: RwLock<HashMap<BanId, Instant>>, // (id, expiry)
    pub networks: RwLock<Networks>,
    pub deduplication_queues: DeduplicationQueues,
    pub last_bootstrap: AtomicU64,
    pub last_peer_update: AtomicU64,
    pub total_received: AtomicU64,
    pub total_sent: AtomicU64,
}

impl ConnectionHandler {
    fn new(conf: &Config, socket_server: TcpListener) -> Self {
        let networks = conf.common.network_ids.iter().cloned().map(NetworkId::from).collect();
        let (sndr, rcvr) = crossbeam_channel::bounded(conf.connection.desired_nodes as usize);
        let conn_changes = ConnChanges {
            changes:  rcvr,
            notifier: sndr,
        };

        let deduplication_queues = DeduplicationQueues::new(
            conf.connection.deduplication_hashing_algorithm,
            conf.connection.dedup_size_long,
            conf.connection.dedup_size_short,
        );

        ConnectionHandler {
            socket_server,
            next_token: AtomicUsize::new(1),
            buckets: Default::default(),
            #[cfg(feature = "network_dump")]
            log_dumper: Default::default(),
            conn_candidates: Default::default(),
            connections: Default::default(),
            conn_changes,
            soft_bans: Default::default(),
            networks: RwLock::new(networks),
            deduplication_queues,
            last_bootstrap: Default::default(),
            last_peer_update: Default::default(),
            total_received: Default::default(),
            total_sent: Default::default(),
        }
    }
}

/// Facilitates the `network_dump` feature.
#[cfg(feature = "network_dump")]
pub struct NetworkDumper {
    switch: Sender<(std::path::PathBuf, bool)>,
    sender: Sender<crate::dumper::DumpItem>,
}

#[cfg(feature = "network_dump")]
impl NetworkDumper {
    fn new(ip: IpAddr, id: P2PNodeId, config: &Config) -> Self {
        let (dump_tx, dump_rx) = crossbeam_channel::bounded(config::DUMP_QUEUE_DEPTH);
        let (act_tx, act_rx) = crossbeam_channel::bounded(config::DUMP_SWITCH_QUEUE_DEPTH);
        create_dump_thread(ip, id, dump_rx, act_rx, &config.common.data_dir);

        Self {
            switch: act_tx,
            sender: dump_tx,
        }
    }
}

/// The central object belonging to a node in the network; it handles
/// connectivity and contains the metadata, statistics etc.
pub struct P2PNode {
    pub self_peer: P2PPeer,
    /// Holds the handles to threads spawned by the node.
    pub threads: RwLock<Vec<JoinHandle<()>>>,
    /// The handle to the poll registry.
    pub poll_registry: Registry,
    pub connection_handler: ConnectionHandler,
    #[cfg(feature = "network_dump")]
    pub network_dumper: NetworkDumper,
    pub stats: Arc<StatsExportService>,
    pub config: NodeConfig,
    /// The time the node was launched.
    pub start_time: DateTime<Utc>,
    /// The flag indicating whether a node should shut down.
    pub is_terminated: AtomicBool,
    /// The key-value store holding the node's persistent data.
    pub kvs: Arc<RwLock<Rkv<LmdbEnvironment>>>,
    /// The catch-up list of peers.
    pub peers: RwLock<PeerList>,
}

impl P2PNode {
    /// Creates a new node and its Poll.
    pub fn new(
        supplied_id: Option<String>,
        conf: &Config,
        peer_type: PeerType,
        stats: Arc<StatsExportService>,
        data_dir_path: Option<PathBuf>,
    ) -> (Arc<Self>, Poll) {
        let addr = if let Some(ref addy) = conf.common.listen_address {
            format!("{}:{}", addy, conf.common.listen_port).parse().unwrap_or_else(|_| {
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

        trace!("Creating a new P2PNode");

        let ip = if let Some(ref addy) = conf.common.listen_address {
            IpAddr::from_str(addy)
                .unwrap_or_else(|_| P2PNode::get_ip().expect("Couldn't retrieve my own ip"))
        } else {
            P2PNode::get_ip().expect("Couldn't retrieve my own ip")
        };

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
        debug!("Listening on {}:{}", ip, conf.common.listen_port);

        let poll = Poll::new().expect("Couldn't create poll");
        let mut server = TcpListener::bind(addr).expect("Couldn't listen on port");
        let poll_registry = poll.registry().try_clone().expect("Can't clone the poll registry");
        poll_registry
            .register(&mut server, SELF_TOKEN, Interest::READABLE)
            .expect("Couldn't register server with poll!");

        let own_peer_port = if let Some(own_port) = conf.common.external_port {
            own_port
        } else {
            conf.common.listen_port
        };

        let self_peer = P2PPeer::from((peer_type, id, SocketAddr::new(ip, own_peer_port)));

        // TODO: Remove surrounding block expr once cargo fmt has been updated in
        // pipeline.
        #[cfg(feature = "malicious_testing")]
        let breakage = {
            if let (Some(ty), Some(tgt), Some(lvl)) =
                (&conf.cli.breakage_type, conf.cli.breakage_target, conf.cli.breakage_level)
            {
                Some((ty.to_owned(), tgt, lvl))
            } else {
                None
            }
        };

        let config = NodeConfig {
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
            max_allowed_nodes: if let Some(max) = conf.connection.max_allowed_nodes {
                max
            } else {
                f64::floor(
                    f64::from(conf.connection.desired_nodes)
                        * (f64::from(conf.connection.max_allowed_nodes_percentage) / 100f64),
                ) as u16
            },
            relay_broadcast_percentage: conf.connection.relay_broadcast_percentage,
            poll_interval: conf.cli.poll_interval,
            housekeeping_interval: conf.connection.housekeeping_interval,
            bootstrapping_interval: conf.connection.bootstrapping_interval,
            print_peers: true,
            bootstrapper_wait_minimum_peers: match peer_type {
                PeerType::Bootstrapper => conf.bootstrapper.wait_until_minimum_nodes,
                PeerType::Node => 0,
            },
            data_dir_path: data_dir_path.unwrap_or_else(|| ".".into()),
            max_latency: conf.connection.max_latency,
            hard_connection_limit: conf.connection.hard_connection_limit,
            catch_up_batch_limit: conf.connection.catch_up_batch_limit,
            timeout_bucket_entry_period: if peer_type == PeerType::Bootstrapper {
                conf.bootstrapper.bootstrapper_timeout_bucket_entry_period
            } else {
                conf.cli.timeout_bucket_entry_period
            },
            bucket_cleanup_interval: conf.common.bucket_cleanup_interval,
            #[cfg(feature = "staging_net")]
            staging_net_username: get_username_from_jwt(&conf.cli.staging_net_token),
            thread_pool_size: conf.connection.thread_pool_size,
            dedup_size_long: conf.connection.dedup_size_long,
            dedup_size_short: conf.connection.dedup_size_short,
            socket_read_size: conf.connection.socket_read_size,
            socket_write_size: conf.connection.socket_write_size,
            no_rebroadcast_consensus_validation: conf.cli.no_rebroadcast_consensus_validation,
            drop_rebroadcast_probability: match peer_type {
                PeerType::Node => conf.cli.drop_rebroadcast_probability,
                _ => None,
            },
            #[cfg(feature = "malicious_testing")]
            partition_network_for_time: match peer_type {
                PeerType::Bootstrapper => conf.bootstrapper.partition_network_for_time,
                _ => None,
            },
            #[cfg(feature = "malicious_testing")]
            breakage,
            bootstrapper_peer_list_size: conf.bootstrapper.peer_list_size,
            default_network: NetworkId::from(conf.common.network_ids[0]), // always present
            socket_so_linger: conf.connection.socket_so_linger,
            events_queue_size: conf.connection.events_queue_size,
            deduplication_hashing_algorithm: conf.connection.deduplication_hashing_algorithm,
        };

        let connection_handler = ConnectionHandler::new(conf, server);

        // Create the node key-value store environment
        let kvs = Manager::<LmdbEnvironment>::singleton()
            .write()
            .unwrap()
            .get_or_create(config.data_dir_path.as_path(), Rkv::new::<Lmdb>)
            .unwrap();

        let node = Arc::new(P2PNode {
            poll_registry,
            start_time: Utc::now(),
            threads: Default::default(),
            config,
            #[cfg(feature = "network_dump")]
            network_dumper: NetworkDumper::new(ip, id, conf),
            connection_handler,
            self_peer,
            stats,
            is_terminated: Default::default(),
            kvs,
            peers: Default::default(),
        });

        node.clear_bans().unwrap_or_else(|e| error!("Couldn't reset the ban list: {}", e));

        (node, poll)
    }

    /// Get the timestamp of the node's last bootstrap attempt.
    pub fn get_last_bootstrap(&self) -> u64 {
        self.connection_handler.last_bootstrap.load(Ordering::Relaxed)
    }

    /// Update the timestamp of the last bootstrap attempt.
    pub fn update_last_bootstrap(&self) {
        self.connection_handler.last_bootstrap.store(get_current_stamp(), Ordering::Relaxed);
    }

    fn is_bucket_cleanup_enabled(&self) -> bool { self.config.timeout_bucket_entry_period > 0 }

    /// A convenience method for accessing the collection of node's connections.
    #[inline]
    pub fn connections(&self) -> &RwLock<Connections> { &self.connection_handler.connections }

    /// A convenience method for accessing the collection of node's connection
    /// candidates.
    #[inline]
    pub fn conn_candidates(&self) -> &Mutex<Connections> {
        &self.connection_handler.conn_candidates
    }

    /// A convenience method for accessing the collection of  node's networks.
    #[inline]
    pub fn networks(&self) -> &RwLock<Networks> { &self.connection_handler.networks }

    /// A convenience method for accessing the collection of  node's buckets.
    #[inline]
    pub fn buckets(&self) -> &RwLock<Buckets> { &self.connection_handler.buckets }

    /// Notify the node handler that a connection needs to undergo a major
    /// change.
    #[inline]
    pub fn register_conn_change(&self, change: ConnChange) {
        if self.connection_handler.conn_changes.notifier.try_send(change).is_err() {
            warn!("Connection change queue full; change request will be delayed");
        }
    }

    /// Activate the network dump feature.
    #[cfg(feature = "network_dump")]
    pub fn activate_dump(&self, path: &str, raw: bool) -> Fallible<()> {
        let path = std::path::PathBuf::from(path);
        self.network_dumper.switch.send((path, raw))?;
        self.dump_start(self.network_dumper.sender.clone());
        Ok(())
    }

    /// Deactivate the network dump feature.
    #[cfg(feature = "network_dump")]
    pub fn stop_dump(&self) -> Fallible<()> {
        let path = std::path::PathBuf::new();
        self.network_dumper.switch.send((path, false))?;
        self.dump_stop();
        Ok(())
    }

    /// Start dumping network data to the disk.
    #[cfg(feature = "network_dump")]
    pub fn dump_start(&self, log_dumper: Sender<DumpItem>) {
        *write_or_die!(self.connection_handler.log_dumper) = Some(log_dumper);
    }

    /// Stop dumping network data to the disk.
    #[cfg(feature = "network_dump")]
    pub fn dump_stop(&self) { *write_or_die!(self.connection_handler.log_dumper) = None; }

    /// Get the node's client version.
    pub fn get_version(&self) -> String { crate::VERSION.to_string() }

    /// Get the node's identifier.
    pub fn id(&self) -> P2PNodeId { self.self_peer.id }

    /// Get the node's `PeerType`.
    #[inline]
    pub fn peer_type(&self) -> PeerType { self.self_peer.peer_type }

    /// Get the node's uptime in milliseconds.
    pub fn get_uptime(&self) -> i64 {
        Utc::now().timestamp_millis() - self.start_time.timestamp_millis()
    }

    /// Procure an IP address for the node.
    #[cfg(not(windows))]
    fn get_ip() -> Option<IpAddr> {
        let localhost = Ipv4Addr::LOCALHOST;
        let mut ip: IpAddr = IpAddr::V4(localhost);

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

    /// Procure an IP address for the node.
    #[cfg(windows)]
    pub fn get_ip() -> Option<IpAddr> {
        let localhost = Ipv4Addr::LOCALHOST;
        let mut ip: IpAddr = IpAddr::V4(localhost);

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

    /// Get the IP of the node.
    pub fn internal_addr(&self) -> SocketAddr { self.self_peer.addr }

    /// Shut the node down gracefully without terminating its threads.
    pub fn close(&self) -> bool {
        // First notify the maintenance thread to stop processing new connections or
        // network packets.
        self.is_terminated.store(true, Ordering::Relaxed);
        // Then process all messages we still have in the Consensus queues.
        let queues_stopped = CALLBACK_QUEUE.stop().is_ok();
        // Finally close all connections
        // Make sure to drop connections so that the peers or peer candidates can
        // quickly free up their endpoints.
        lock_or_die!(self.conn_candidates()).clear();
        write_or_die!(self.connections()).clear();
        queues_stopped
    }

    /// Joins the threads spawned by the node.
    pub fn join(&self) -> Fallible<()> {
        for handle in mem::take(&mut *write_or_die!(self.threads)) {
            if let Err(e) = handle.join() {
                error!("Can't join a node thread: {:?}", e);
            }
        }
        Ok(())
    }

    /// Shut the node down gracefully and terminate its threads.
    pub fn close_and_join(&self) -> Fallible<()> {
        self.close();
        self.join()
    }
}

/// Spawn the node's poll thread.
pub fn spawn(node_ref: &Arc<P2PNode>, mut poll: Poll, consensus: Option<ConsensusContainer>) {
    let node = Arc::clone(node_ref);
    let poll_thread = spawn_or_die!("poll loop", move || {
        let mut events = Events::with_capacity(node.config.events_queue_size);
        let mut log_time = Instant::now();
        let mut last_buckets_cleaned = Instant::now();
        let mut last_peer_list_update = 0;

        let num_socket_threads = match node.self_peer.peer_type {
            PeerType::Bootstrapper => 1,
            PeerType::Node => node.config.thread_pool_size,
        };
        let pool = rayon::ThreadPoolBuilder::new().num_threads(num_socket_threads).build().unwrap();
        let poll_interval = Duration::from_millis(node.config.poll_interval);

        // Process network events until signalled to terminate.
        // For each loop iteration do the following in sequence
        // - check whether ther are any incoming connection requests
        // - then process any connection changes, e.g., drop connections, promote to
        //   initial connections to peers, ...
        // - then read from all existing connections in parallel, using the above
        //   allocated thread pool
        // - occassionally (dictated by the housekeeping_interval) do connection
        //   housekeeping, checking whether peers and connections are active.
        while !node.is_terminated.load(Ordering::Relaxed) {
            // check for new events or wait
            if let Err(e) = poll.poll(&mut events, Some(poll_interval)) {
                error!("{}", e);
                continue;
            }

            // perform socket reads and writes in parallel across connections
            // check for new connections
            for i in 0..events.iter().filter(|event| event.token() == SELF_TOKEN).count() {
                accept(&node).map_err(|e| error!("{}", e)).ok();
                if i == 9 {
                    warn!("too many connection attempts received at once; dropping the rest");
                    break;
                }
            }

            for conn_change in node.connection_handler.conn_changes.changes.try_iter() {
                process_conn_change(&node, conn_change)
            }

            if let Some(ref consensus) = consensus {
                let new_last_peer_update = node.last_peer_update();
                if new_last_peer_update > last_peer_list_update {
                    update_peer_list(&node);
                    last_peer_list_update = new_last_peer_update;
                }
                check_peer_states(&node, consensus);
            }

            pool.install(|| node.process_network_events(&events));

            // Run periodic tasks
            let now = Instant::now();
            if now.duration_since(log_time)
                >= Duration::from_secs(node.config.housekeeping_interval)
            {
                if cfg!(test) && read_or_die!(node.connections()).is_empty() {
                    panic!("the test timed out: no valid connections available");
                }

                connection_housekeeping(&node);
                if node.peer_type() != PeerType::Bootstrapper {
                    node.measure_connection_latencies();
                }

                let peer_stat_list = node.get_peer_stats(None);
                check_peers(&node, &peer_stat_list);
                node.measure_throughput(&peer_stat_list);

                log_time = now;
            }

            if node.is_bucket_cleanup_enabled()
                && now.duration_since(last_buckets_cleaned)
                    >= Duration::from_millis(node.config.bucket_cleanup_interval)
            {
                write_or_die!(node.buckets())
                    .clean_buckets(node.config.timeout_bucket_entry_period);
                last_buckets_cleaned = now;
            }
        }
        info!("Shutting down");
    });

    // Register info about thread into P2PNode.
    write_or_die!(node_ref.threads).push(poll_thread);
}

/// Process a change to the set of connections.
fn process_conn_change(node: &Arc<P2PNode>, conn_change: ConnChange) {
    match conn_change {
        ConnChange::NewConn(addr, peer_type) => {
            if let Err(e) = connect(node, peer_type, addr, None) {
                error!("Can't connect to the desired address: {}", e);
            }
        }
        ConnChange::Promotion(token) => {
            if let Some(conn) = lock_or_die!(node.conn_candidates()).remove(&token) {
                let mut conns = write_or_die!(node.connections());
                // Remove previous connection from same `PeerId`, as this is then to be seen
                // as a reconnect.
                conns.retain(|_, c| c.remote_id() != conn.remote_id());
                conns.insert(conn.token, conn);
                node.bump_last_peer_update();
            }
        }
        ConnChange::NewPeers(peers) => {
            let mut new_peers = 0;
            let current_peers = node.get_peer_stats(Some(PeerType::Node));

            let curr_peer_count = current_peers.len();

            let applicable_candidates = peers.iter().filter(|candidate| {
                !current_peers
                    .iter()
                    .any(|peer| peer.id == candidate.id.as_raw() || peer.addr == candidate.addr)
            });

            for peer in applicable_candidates {
                trace!("Got info for peer {} ({})", peer.id, peer.addr);
                if connect(node, PeerType::Node, peer.addr, Some(peer.id)).is_ok() {
                    new_peers += 1;
                }

                if new_peers + curr_peer_count >= node.config.desired_nodes_count as usize {
                    break;
                }
            }
        }
        ConnChange::Expulsion(token) => {
            if let Some(remote_peer) = node.remove_connection(token) {
                let ip = remote_peer.addr.ip();
                warn!("Soft-banning {} due to a breach of protocol", ip);
                write_or_die!(node.connection_handler.soft_bans).insert(
                    BanId::Ip(ip),
                    Instant::now() + Duration::from_secs(config::SOFT_BAN_DURATION_SECS),
                );
            }
        }
        ConnChange::RemovalByToken(token) => {
            trace!("Removing connection with token {:?}", token);
            node.remove_connection(token);
        }
        ConnChange::RemovalByNodeId(remote_id) => {
            trace!("Removing connection to {} by node id.", remote_id);
            node.find_conn_token_by_id(remote_id).and_then(|token| node.remove_connection(token));
        }
        ConnChange::RemovalByIp(ip_addr) => {
            trace!("Removing all connections to IP {}", ip_addr);
            node.remove_connections(&node.find_conn_tokens_by_ip(ip_addr));
        }
    }
}

/// Try to bootstrap the node based on the addresses in the config.
pub fn attempt_bootstrap(node: &Arc<P2PNode>) {
    if !node.config.no_net {
        info!("Attempting to bootstrap");

        let bootstrap_nodes = utils::get_bootstrap_nodes(
            &node.config.bootstrap_server,
            &node.config.dns_resolvers,
            node.config.dnssec_disabled,
            &node.config.bootstrap_nodes,
        );

        match bootstrap_nodes {
            Ok(nodes) => {
                for addr in nodes {
                    info!("Using bootstrapper {}", addr);
                    node.register_conn_change(ConnChange::NewConn(addr, PeerType::Bootstrapper));
                }
            }
            Err(e) => error!("Can't bootstrap: {:?}", e),
        }
    }
}

impl Drop for P2PNode {
    fn drop(&mut self) { let _ = self.close_and_join(); }
}

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
