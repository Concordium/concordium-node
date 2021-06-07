//! Node maintenance methods.

use chrono::prelude::*;
use crossbeam_channel::{self, Receiver, Sender};
use failure::{Fallible, ResultExt};
use mio::{net::TcpListener, Events, Interest, Poll, Registry, Token};
use nohash_hasher::BuildNoHashHasher;
use rand::{prelude::SliceRandom, thread_rng, Rng};
use rkv::{
    backend::{Lmdb, LmdbEnvironment},
    Manager, Rkv,
};

#[cfg(feature = "network_dump")]
use crate::dumper::{create_dump_thread, DumpItem};
use crate::{
    common::{get_current_stamp, p2p_peer::RemotePeerId, P2PNodeId, P2PPeer, PeerType},
    configuration::{self as config, Config},
    connection::{ConnChange, Connection, DeduplicationHashAlgorithm, DeduplicationQueues},
    consensus_ffi::{
        blockchain_types::BlockHash,
        catch_up::PeerList,
        consensus::{ConsensusContainer, CALLBACK_QUEUE},
    },
    lock_or_die,
    network::{Buckets, NetworkId, Networks},
    p2p::{
        bans::BanId,
        connectivity::{accept, connect, connection_housekeeping, AcceptFailureReason, SELF_TOKEN},
        peers::check_peers,
    },
    plugins::consensus::{check_peer_states, update_peer_list},
    read_or_die, spawn_or_die,
    stats_export_service::StatsExportService,
    utils, write_or_die,
};

use std::{
    collections::{HashMap, HashSet},
    io::ErrorKind,
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
    /// Do not clear persistent bans on startup.
    pub no_clear_bans: bool,
    pub bootstrap_server: Option<String>,
    pub dns_resolvers: Vec<String>,
    pub dnssec_disabled: bool,
    pub disallow_multiple_peers_on_ip: bool,
    pub bootstrap_nodes: Vec<String>,
    /// Nodes to try and keep the connections to. A node will maintain two
    /// classes of connections, one which is explicitly given, and one which is
    /// discovered by bootstrapping or through other peers. The IP addresses
    /// are resolved on startup or when they are added and during execution
    /// we only keep them instead of the domain name.
    pub given_addresses: RwLock<HashSet<SocketAddr>>,
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
    pub conn_requests_batch_limit: u16,
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
    pub bootstrapper_peer_list_size: usize,
    pub default_network: NetworkId,
    pub socket_so_linger: Option<u16>,
    pub events_queue_size: usize,
    pub deduplication_hashing_algorithm: DeduplicationHashAlgorithm,
    pub regenesis_arc: Arc<RwLock<Vec<BlockHash>>>,
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
        let (sndr, rcvr) =
            crossbeam_channel::bounded(conf.connection.hard_connection_limit as usize);
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

    /// Check whether the given address is soft-banned.
    /// NB: This acquires and releases a read lock to the `soft_bans` structure.
    pub(crate) fn is_soft_banned(&self, addr: SocketAddr) -> bool {
        let soft_bans = read_or_die!(self.soft_bans);
        soft_bans.get(&BanId::Ip(addr.ip())).is_some()
            || soft_bans.get(&BanId::Socket(addr)).is_some()
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
        create_dump_thread(ip, id, dump_rx, act_rx, config.common.data_dir.clone());

        Self {
            switch: act_tx,
            sender: dump_tx,
        }
    }
}

/// A count of bad events indexed by peer. We use this to not spam warnings
/// constantly and instead only emit warnings on each iteration of connection
/// housekeeping.
///
/// NB: At the moment these are all simple mutexes since there is never a need
/// for shared access. Ideally this would be joined together with connection
/// stats so that it would not need the additional indexing that we have now
/// (because connections are already per-peer) but that will require a bit more
/// care so that we can update the values in the appropriate places without
/// introducing deadlocks and a bit of redesign. In contrast to connection stats
/// this structure is more transient and is cleared on each housekeeping
/// interval.
#[derive(Debug, Default)]
pub struct BadEvents {
    /// Number of high priority messages that were dropped because they could
    /// not be enqueued.
    pub dropped_high_queue: Mutex<HashMap<RemotePeerId, u64>>,
    /// Number of low priority messages that were dropped because they could not
    /// be enqueued.
    pub dropped_low_queue: Mutex<HashMap<RemotePeerId, u64>>,
    /// Number of invalid messages received from the given peer.
    pub invalid_messages: Mutex<HashMap<RemotePeerId, u64>>,
}

impl BadEvents {
    /// Register a new dropped value for the given peer and return the amount of
    /// dropped high priority messages for the peer.
    pub fn inc_dropped_high_queue(&self, peer_id: RemotePeerId) -> u64 {
        *lock_or_die!(self.dropped_high_queue).entry(peer_id).and_modify(|x| *x += 1).or_insert(1)
    }

    /// Register a new dropped value for the given peer and return the amount of
    /// dropped high priority messages for the peer.
    pub fn inc_dropped_low_queue(&self, peer_id: RemotePeerId) -> u64 {
        *lock_or_die!(self.dropped_low_queue).entry(peer_id).and_modify(|x| *x += 1).or_insert(1)
    }

    /// Register a new dropped value for the given peer and return the amount of
    /// invalid messages that were received.
    pub fn inc_invalid_messages(&self, peer_id: RemotePeerId) -> u64 {
        *lock_or_die!(self.invalid_messages).entry(peer_id).and_modify(|x| *x += 1).or_insert(1)
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
    /// Cache of bad events that we report on each connection housekeeping
    /// interval to avoid spamming the logs in case of failure.
    pub bad_events: BadEvents,
}

impl P2PNode {
    /// Creates a new node and its Poll. If the node id is provided the node
    /// will be started with that Peer ID. If it is not a fresh one will be
    /// generated.
    pub fn new(
        supplied_id: Option<P2PNodeId>,
        conf: &Config,
        peer_type: PeerType,
        stats: Arc<StatsExportService>,
        regenesis_arc: Arc<RwLock<Vec<BlockHash>>>,
    ) -> Fallible<(Arc<Self>, Poll)> {
        let addr = if let Some(ref addy) = conf.common.listen_address {
            let ip_addr = addy.parse::<IpAddr>().context(
                "Supplied listen address could not be parsed. The address must be a valid IP \
                 address.",
            )?;
            SocketAddr::new(ip_addr, conf.common.listen_port)
        } else {
            SocketAddr::new(IpAddr::V4(Ipv4Addr::UNSPECIFIED), conf.common.listen_port)
        };

        trace!("Creating a new P2PNode");

        let ip = if let Some(ref addy) = conf.common.listen_address {
            IpAddr::from_str(addy).context("Could not parse the provided listen address.")?
        } else {
            P2PNode::get_ip().ok_or_else(|| {
                format_err!("Could not compute my own ip. Use `--listen-address` to specify it.")
            })?
        };

        let id = supplied_id.unwrap_or_else(|| rand::thread_rng().gen::<P2PNodeId>());

        info!("My Node ID is {}", id);
        info!("Listening on {}:{}", ip, conf.common.listen_port);

        let poll =
            Poll::new().context("Could not create the poll to listen for incoming connections.")?;
        let mut server = TcpListener::bind(addr).context(format!(
            "Could not listen on the given listen-port ({}).",
            conf.common.listen_port
        ))?;
        let poll_registry =
            poll.registry().try_clone().context("Could not clone the poll registry.")?;
        poll_registry
            .register(&mut server, SELF_TOKEN, Interest::READABLE)
            .context("Could not register server with poll!")?;

        let own_peer_port = if let Some(own_port) = conf.common.external_port {
            own_port
        } else {
            conf.common.listen_port
        };

        let self_peer = P2PPeer {
            id,
            peer_type,
            addr: SocketAddr::new(ip, own_peer_port),
        };

        let dns_resolvers =
            utils::get_resolvers(&conf.connection.resolv_conf, &conf.connection.dns_resolver);
        let given_addresses = RwLock::new(parse_config_nodes(&conf.connection, &dns_resolvers)?);

        let config = NodeConfig {
            no_net: conf.cli.no_network,
            desired_nodes_count: conf.connection.desired_nodes,
            no_bootstrap_dns: conf.connection.no_bootstrap_dns,
            no_clear_bans: conf.connection.no_clear_bans,
            bootstrap_server: conf.connection.bootstrap_server.clone(),
            dns_resolvers,
            dnssec_disabled: conf.connection.dnssec_disabled,
            disallow_multiple_peers_on_ip: conf.connection.disallow_multiple_peers_on_ip,
            bootstrap_nodes: conf.connection.bootstrap_nodes.clone(),
            given_addresses,
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
            data_dir_path: conf.common.data_dir.clone(),
            max_latency: conf.connection.max_latency,
            conn_requests_batch_limit: conf.connection.conn_requests_batch_limit,
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
            bootstrapper_peer_list_size: conf.bootstrapper.peer_list_size,
            default_network: NetworkId::from(conf.common.network_ids[0]), // always present
            socket_so_linger: conf.connection.socket_so_linger,
            events_queue_size: conf.connection.events_queue_size,
            deduplication_hashing_algorithm: conf.connection.deduplication_hashing_algorithm,
            regenesis_arc,
        };

        let connection_handler = ConnectionHandler::new(conf, server);

        // Create the node key-value store environment
        let kvs = Manager::<LmdbEnvironment>::singleton()
            .write()
            .unwrap()
            .get_or_create(config.data_dir_path.as_path(), Rkv::new::<Lmdb>)
            .context("Could not create or obtain the ban database.")?;

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
            bad_events: BadEvents::default(),
        });

        if !node.config.no_clear_bans {
            node.clear_bans().unwrap_or_else(|e| error!("Couldn't reset the ban list: {}", e));
        }

        Ok((node, poll))
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

    /// Check whether the given connection is one of the connections to the
    /// given nodes.
    /// Given nodes are identified by the pair of IP and port.
    pub fn is_given_connection(&self, conn: &Connection) -> bool {
        // Because it can be that either we are connected to them, or they are connected
        // to us, we check both the address we are connected to them, as well as
        // the external port the peer advertises. This latter is not great if it
        // is possible that a malicious node is on the same IP as one of the
        // given ones but in the absence of a global identifier there's
        // little else we can do.
        let addrs = read_or_die!(self.config.given_addresses);
        addrs.contains(&conn.remote_addr()) || addrs.contains(&conn.remote_peer.external_addr())
    }

    /// Get the list of unconnected given peers.
    pub fn unconnected_given_addresses(&self) -> HashSet<SocketAddr> {
        let mut ret = read_or_die!(self.config.given_addresses).clone();
        for conn in read_or_die!(self.connections()).values() {
            ret.remove(&conn.remote_addr());
            ret.remove(&conn.remote_peer.external_addr());
        }
        ret
    }

    /// Check whether any of the node's peers has the addr as either the remote
    /// addrs or the advertised address (i.e., the address where they listen
    /// for incoming connections) NB: This needs to acquire a read lock on
    /// the connections object.
    pub fn is_connected(&self, addr: SocketAddr) -> bool {
        read_or_die!(self.connections())
            .values()
            .any(|conn| conn.remote_addr() == addr || conn.remote_peer.external_addr() == addr)
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

    /// Waits for all the spawned threads to terminate.
    /// This may panic or deadlock (depending on platform) if used from two
    /// different node threads.
    pub fn join(&self) -> Fallible<()> {
        // try to acquire the thread handles.
        let handles = {
            match self.threads.write() {
                Ok(mut wlock) => mem::replace::<Vec<_>>(&mut wlock, Vec::new()),
                // if unsuccessful then most likely some other thread acquired the threads lock and
                // panicked. There is not much we can easily do, so we just do nothing and
                // terminate.
                Err(_) => Vec::new(),
            }
            // write lock released
        };
        for handle in handles {
            if let Err(e) = handle.join() {
                error!("Can't join a node thread: {:?}", e);
            }
        }
        Ok(())
    }

    /// Shut the node down gracefully and terminate its threads.
    /// This method should only be called once by the thread that created the
    /// node. It may panic or deadlock (depending on platform) if used from
    /// two different node threads.
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
        // The number of polling loop iterations since the last housekeeping.
        let mut iterations_since_housekeeping = 0;

        let num_socket_threads = match node.self_peer.peer_type {
            PeerType::Bootstrapper => 1,
            PeerType::Node => node.config.thread_pool_size,
        };
        let pool = rayon::ThreadPoolBuilder::new().num_threads(num_socket_threads).build().unwrap();
        let poll_interval = Duration::from_millis(node.config.poll_interval);

        // A flag indicating whether there are unprocessed incoming connection attempts.
        // We only process a bounded number of them each iteration of the loop below,
        // and due to the way mio works we might not get new events until we've
        // processed all existing ones.
        let mut unprocessed_attempts = false;

        // Maximum number of connection requests to process per iteration.
        let max_num_requests = node.config.conn_requests_batch_limit;

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

            // check for new connections
            if unprocessed_attempts || events.iter().any(|event| event.token() == SELF_TOKEN) {
                let mut attempt_number = 0;
                unprocessed_attempts = true;
                while attempt_number < max_num_requests {
                    match node.connection_handler.socket_server.accept() {
                        Ok((socket, addr)) => {
                            if let Err(e) = accept(&node, socket, addr) {
                                error!("{}", e);
                                if let AcceptFailureReason::TooManyConnections {
                                    addr: _,
                                } = e
                                {
                                    break;
                                }
                            }
                        }
                        Err(e) if e.kind() == ErrorKind::WouldBlock => {
                            unprocessed_attempts = false;
                            break;
                        }
                        Err(e) => {
                            error!("{}", e);
                        }
                    }
                    attempt_number += 1;
                }
                if attempt_number >= max_num_requests {
                    warn!(
                        "Received too many connection requests at once. Delaying accepting the \
                         remaining ones."
                    );
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

            // perform socket reads and writes in parallel across connections
            pool.install(|| node.process_network_events(&events));

            // Run periodic tasks
            // We prevent housekeeping from occurring too often so that new connections have
            // a chance to complete the handshake in between invocations of
            // housekeeping.
            if iterations_since_housekeeping >= 10 {
                if Instant::now().duration_since(log_time)
                    >= Duration::from_secs(node.config.housekeeping_interval)
                {
                    let attempted_bootstrap = connection_housekeeping(&node);
                    if node.peer_type() != PeerType::Bootstrapper {
                        node.measure_connection_latencies();
                    }

                    let peer_stat_list = node.get_peer_stats(None);
                    check_peers(&node, &peer_stat_list, attempted_bootstrap);
                    node.measure_throughput(&peer_stat_list);

                    log_time = Instant::now();
                    iterations_since_housekeeping = 0;
                }
            } else {
                iterations_since_housekeeping += 1;
            }

            if node.is_bucket_cleanup_enabled()
                && Instant::now().duration_since(last_buckets_cleaned)
                    >= Duration::from_millis(node.config.bucket_cleanup_interval)
            {
                write_or_die!(node.buckets())
                    .clean_buckets(node.config.timeout_bucket_entry_period);
                last_buckets_cleaned = Instant::now();
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
        ConnChange::NewConn {
            addr,
            peer_type,
            given,
        } => {
            // for given addresses we do not respect the max peer bound, for discovered
            // peers that are automatically discovered we do
            if let Err(e) = connect(node, peer_type, addr, None, !given) {
                error!("Can't connect to the desired address: {}", e);
            } else if given && !write_or_die!(node.config.given_addresses).insert(addr) {
                info!("New given address recorded {}", given);
            }
        }
        ConnChange::Promotion(token) => {
            if let Some(conn) = lock_or_die!(node.conn_candidates()).remove(&token) {
                // check if we are connected to the peer already on the port they advertise.
                // This is only needed for incoming connections since they typically come from
                // unrecognizable ports.
                // NB: We do not use node.is_connected here since that acquires a read lock on
                // the same connections object.
                let mut conns = write_or_die!(node.connections());
                let addr = conn.remote_peer.external_addr();
                let is_connected = conns.values().any(|existing| {
                    existing.remote_addr() == addr || existing.remote_peer.external_addr() == addr
                });
                if !is_connected {
                    conns.insert(conn.token(), conn);
                    node.bump_last_peer_update();
                } else {
                    warn!("Already connected to a peer on the given address.")
                }
            }
        }
        ConnChange::NewPeers(mut peers) => {
            let mut new_peers = 0;
            let current_peers = node.get_peer_stats(Some(PeerType::Node));

            let curr_peer_count = current_peers.len();

            // Shuffle the peers we received try to discover more useful peers over time
            // and not get stuck continuously connecting to useless ones, and then dropping
            // connections.
            peers.shuffle(&mut thread_rng());

            // Try to connect to each peer in turn.
            // If we are already connected to a peer, this will fail.
            for peer in peers {
                if new_peers + curr_peer_count >= node.config.desired_nodes_count as usize {
                    break;
                }

                trace!("Got info for peer {} ({})", peer.id, peer.addr);
                if let Err(e) = connect(node, PeerType::Node, peer.addr, Some(peer.id), true) {
                    debug!("Could not connect to discovered peer {}", e);
                } else {
                    new_peers += 1;
                }
            }
        }
        ConnChange::ExpulsionByToken(token) => {
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
        ConnChange::RemoveAllByTokens(tokens) => {
            trace!("Removing connections with tokens {:?}", tokens);
            node.remove_connections(&tokens);
        }
    }
}

/// Try to bootstrap the node based on the addresses in the config.
pub fn attempt_bootstrap(node: &Arc<P2PNode>) {
    if !node.config.no_net {
        info!("Attempting to bootstrap");

        let bootstrap_nodes = utils::get_bootstrap_nodes(
            node.config.bootstrap_server.as_deref(),
            &node.config.dns_resolvers,
            node.config.dnssec_disabled,
            &node.config.bootstrap_nodes,
        );

        match bootstrap_nodes {
            Ok(nodes) => {
                for addr in nodes {
                    info!("Using bootstrapper {}", addr);
                    node.register_conn_change(ConnChange::NewConn {
                        addr,
                        peer_type: PeerType::Bootstrapper,
                        given: false,
                    });
                }
            }
            Err(e) => error!("Can't bootstrap: {:?}", e),
        }
    }
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

/// Parse and potentially resolve IPs (via DNS) of nodes supplied on startup.
fn parse_config_nodes(
    conf: &config::ConnectionConfig,
    dns_resolvers: &[String],
) -> Fallible<HashSet<SocketAddr>> {
    let mut out = HashSet::new();
    for connect_to in &conf.connect_to {
        let new_addresses =
            utils::parse_host_port(connect_to, dns_resolvers, conf.dnssec_disabled)?;
        out.extend(new_addresses)
    }
    Ok(out)
}
