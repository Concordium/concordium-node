use chrono::prelude::*;
use crossbeam_channel::{self, Sender};
use failure::Fallible;
#[cfg(not(target_os = "windows"))]
use get_if_addrs;
#[cfg(target_os = "windows")]
use ipconfig;
use mio::{net::TcpListener, Events, Poll, PollOpt, Ready, Token};
use nohash_hasher::BuildNoHashHasher;
use rkv::{Manager, Rkv};

#[cfg(feature = "network_dump")]
use crate::dumper::create_dump_thread;
#[cfg(feature = "beta")]
use crate::plugins::beta::get_username_from_jwt;
use crate::{
    common::{get_current_stamp, P2PNodeId, P2PPeer, PeerType},
    configuration::{self as config, Config},
    connection::{Connection, DeduplicationQueues, P2PEvent},
    dumper::DumpItem,
    network::{Buckets, NetworkId},
    p2p::{
        bans::BanId,
        connectivity::{accept, connect, connection_housekeeping, SERVER},
        peers::check_peers,
    },
    stats_export_service::StatsExportService,
    utils,
};
use concordium_common::QueueMsg::{self, Relay};
use consensus_rust::{consensus::CALLBACK_QUEUE, transferlog::TRANSACTION_LOG_QUEUE};

use std::{
    collections::{HashMap, HashSet},
    io, mem,
    net::{
        IpAddr::{self, V4, V6},
        Ipv4Addr, SocketAddr,
    },
    path::PathBuf,
    str::FromStr,
    sync::{
        atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering},
        Arc, RwLock,
    },
    thread::JoinHandle,
    time::{Duration, Instant},
};

pub struct P2PNodeConfig {
    pub no_net: bool,
    pub desired_nodes_count: u16,
    pub no_bootstrap_dns: bool,
    pub bootstrap_server: String,
    pub dns_resolvers: Vec<String>,
    pub dnssec_disabled: bool,
    pub bootstrap_nodes: Vec<String>,
    pub max_allowed_nodes: u16,
    pub relay_broadcast_percentage: f64,
    pub global_state_catch_up_requests: bool,
    pub poll_interval: u64,
    pub housekeeping_interval: u64,
    pub bootstrapping_interval: u64,
    pub print_peers: bool,
    pub bootstrapper_wait_minimum_peers: u16,
    pub no_trust_bans: bool,
    pub data_dir_path: PathBuf,
    pub max_latency: Option<u64>,
    pub hard_connection_limit: Option<u16>,
    pub catch_up_batch_limit: u64,
    pub timeout_bucket_entry_period: u64,
    pub bucket_cleanup_interval: u64,
    #[cfg(feature = "beta")]
    pub beta_username: String,
    pub thread_pool_size: usize,
    pub dedup_size_long: usize,
    pub dedup_size_short: usize,
    pub socket_read_size: usize,
    pub socket_write_size: usize,
    pub no_rebroadcast_consensus_validation: bool,
    pub drop_rebroadcast_probability: Option<f64>,
    pub partition_network_for_time: Option<usize>,
    pub breakage: Option<(String, u8, usize)>,
}

pub type Networks = HashSet<NetworkId, BuildNoHashHasher<u16>>;
pub type Connections = HashMap<Token, Arc<Connection>, BuildNoHashHasher<usize>>;

pub struct ConnectionHandler {
    pub server:           TcpListener,
    pub next_id:          AtomicUsize,
    pub event_log:        Option<Sender<QueueMsg<P2PEvent>>>,
    pub buckets:          RwLock<Buckets>,
    pub log_dumper:       RwLock<Option<Sender<DumpItem>>>,
    pub connections:      RwLock<Connections>,
    pub soft_bans:        RwLock<HashMap<BanId, Instant>>, // (id, expiry)
    pub networks:         RwLock<Networks>,
    pub last_bootstrap:   AtomicU64,
    pub last_peer_update: AtomicU64,
}

impl ConnectionHandler {
    pub fn new(
        conf: &Config,
        server: TcpListener,
        event_log: Option<Sender<QueueMsg<P2PEvent>>>,
    ) -> Self {
        let networks = conf.common.network_ids.iter().cloned().map(NetworkId::from).collect();

        ConnectionHandler {
            server,
            next_id: AtomicUsize::new(1),
            event_log,
            buckets: RwLock::new(Buckets::new()),
            log_dumper: Default::default(),
            connections: Default::default(),
            soft_bans: Default::default(),
            networks: RwLock::new(networks),
            last_bootstrap: Default::default(),
            last_peer_update: Default::default(),
        }
    }
}

#[derive(Default)]
pub struct P2PNodeThreads {
    pub join_handles: Vec<JoinHandle<()>>,
}

pub struct P2PNode {
    pub self_peer:          P2PPeer,
    pub threads:            RwLock<P2PNodeThreads>,
    pub poll:               Poll,
    pub connection_handler: ConnectionHandler,
    pub dump_switch:        Sender<(std::path::PathBuf, bool)>,
    pub dump_tx:            Sender<crate::dumper::DumpItem>,
    pub stats:              Arc<StatsExportService>,
    pub config:             P2PNodeConfig,
    pub start_time:         DateTime<Utc>,
    pub is_rpc_online:      AtomicBool,
    pub is_terminated:      AtomicBool,
    pub kvs:                Arc<RwLock<Rkv>>,
    pub total_received:     AtomicU64,
    pub total_sent:         AtomicU64,
}

impl P2PNode {
    pub fn new(
        supplied_id: Option<String>,
        conf: &Config,
        event_log: Option<Sender<QueueMsg<P2PEvent>>>,
        peer_type: PeerType,
        stats: Arc<StatsExportService>,
        data_dir_path: Option<PathBuf>,
    ) -> Arc<Self> {
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

        trace!("Creating new P2PNode");

        // Retrieve IP address octets, format to IP and SHA256 hash it
        let ip = if let Some(ref addy) = conf.common.listen_address {
            IpAddr::from_str(addy)
                .unwrap_or_else(|_| P2PNode::get_ip().expect("Couldn't retrieve my own ip"))
        } else {
            P2PNode::get_ip().expect("Couldn't retrieve my own ip")
        };

        debug!("Listening on {}:{}", ip.to_string(), conf.common.listen_port);

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

        if poll.register(&server, SERVER, Ready::readable(), PollOpt::level()).is_err() {
            panic!("Couldn't register server with poll!")
        };

        let own_peer_port = if let Some(own_port) = conf.common.external_port {
            own_port
        } else {
            conf.common.listen_port
        };

        let self_peer = P2PPeer::from(peer_type, id, SocketAddr::new(ip, own_peer_port));

        let (dump_tx, _dump_rx) = crossbeam_channel::bounded(config::DUMP_QUEUE_DEPTH);
        let (act_tx, _act_rx) = crossbeam_channel::bounded(config::DUMP_SWITCH_QUEUE_DEPTH);

        #[cfg(feature = "network_dump")]
        create_dump_thread(ip, id, _dump_rx, _act_rx, &conf.common.data_dir);

        let breakage = if let (Some(ty), Some(tgt), Some(lvl)) =
            (&conf.cli.breakage_type, conf.cli.breakage_target, conf.cli.breakage_level)
        {
            Some((ty.to_owned(), tgt, lvl))
        } else {
            None
        };

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
            max_allowed_nodes: if let Some(max) = conf.connection.max_allowed_nodes {
                max
            } else {
                f64::floor(
                    f64::from(conf.connection.desired_nodes)
                        * (f64::from(conf.connection.max_allowed_nodes_percentage) / 100f64),
                ) as u16
            },
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
            catch_up_batch_limit: conf.connection.catch_up_batch_limit,
            timeout_bucket_entry_period: if peer_type == PeerType::Bootstrapper {
                conf.bootstrapper.bootstrapper_timeout_bucket_entry_period
            } else {
                conf.cli.timeout_bucket_entry_period
            },
            bucket_cleanup_interval: conf.common.bucket_cleanup_interval,
            #[cfg(feature = "beta")]
            beta_username: get_username_from_jwt(&conf.cli.beta_token),
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
            partition_network_for_time: match peer_type {
                PeerType::Bootstrapper => conf.bootstrapper.partition_network_for_time,
                _ => None,
            },
            breakage,
        };

        let connection_handler = ConnectionHandler::new(conf, server, event_log);

        // Create the node key-value store environment
        let kvs = Manager::singleton()
            .write()
            .unwrap()
            .get_or_create(config.data_dir_path.as_path(), Rkv::new)
            .unwrap();

        let node = Arc::new(P2PNode {
            poll,
            start_time: Utc::now(),
            threads: RwLock::new(P2PNodeThreads::default()),
            config,
            dump_switch: act_tx,
            dump_tx,
            is_rpc_online: AtomicBool::new(false),
            connection_handler,
            self_peer,
            stats,
            is_terminated: Default::default(),
            kvs,
            total_received: Default::default(),
            total_sent: Default::default(),
        });

        node.clear_bans().unwrap_or_else(|e| error!("Couldn't reset the ban list: {}", e));

        node
    }

    pub fn get_last_bootstrap(&self) -> u64 {
        self.connection_handler.last_bootstrap.load(Ordering::Relaxed)
    }

    pub fn update_last_bootstrap(&self) {
        self.connection_handler.last_bootstrap.store(get_current_stamp(), Ordering::Relaxed);
    }

    fn is_bucket_cleanup_enabled(&self) -> bool { self.config.timeout_bucket_entry_period > 0 }

    #[inline]
    pub fn connections(&self) -> &RwLock<Connections> { &self.connection_handler.connections }

    #[inline]
    pub fn networks(&self) -> &RwLock<Networks> { &self.connection_handler.networks }

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

    pub fn dump_start(&self, log_dumper: Sender<DumpItem>) {
        *write_or_die!(self.connection_handler.log_dumper) = Some(log_dumper);
    }

    pub fn dump_stop(&self) { *write_or_die!(self.connection_handler.log_dumper) = None; }

    /// Waits for P2PNode termination. Use `P2PNode::close` to notify the
    /// termination.
    pub fn join(&self) -> Fallible<()> {
        for handle in
            mem::replace(&mut write_or_die!(self.threads).join_handles, Default::default())
        {
            if let Err(e) = handle.join() {
                bail!("Thread join error: {:?}", e);
            }
        }
        Ok(())
    }

    pub fn get_version(&self) -> String { crate::VERSION.to_string() }

    pub fn id(&self) -> P2PNodeId { self.self_peer.id }

    #[inline]
    pub fn peer_type(&self) -> PeerType { self.self_peer.peer_type }

    pub fn log_event(&self, event: P2PEvent) {
        if let Some(ref log) = self.connection_handler.event_log {
            if let Err(e) = log.send(Relay(event)) {
                error!("Couldn't send error {:?}", e)
            }
        }
    }

    pub fn get_uptime(&self) -> i64 {
        Utc::now().timestamp_millis() - self.start_time.timestamp_millis()
    }

    #[cfg(not(windows))]
    pub fn get_ip() -> Option<IpAddr> {
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

    pub fn internal_addr(&self) -> SocketAddr { self.self_peer.addr }

    pub fn close(&self) -> bool {
        info!("P2PNode shutting down.");
        self.is_terminated.store(true, Ordering::Relaxed);
        CALLBACK_QUEUE.stop().is_ok() && TRANSACTION_LOG_QUEUE.stop().is_ok()
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
}

pub fn spawn(node: &Arc<P2PNode>) {
    let self_clone = Arc::clone(node);
    let poll_thread = spawn_or_die!("Poll thread", {
        let mut events = Events::with_capacity(10);
        let mut log_time = Instant::now();
        let mut last_buckets_cleaned = Instant::now();

        let deduplication_queues = DeduplicationQueues::new(
            self_clone.config.dedup_size_long,
            self_clone.config.dedup_size_short,
        );

        let num_socket_threads = match self_clone.self_peer.peer_type {
            PeerType::Bootstrapper => 1,
            PeerType::Node => self_clone.config.thread_pool_size,
        };
        let pool = rayon::ThreadPoolBuilder::new().num_threads(num_socket_threads).build().unwrap();

        let mut connections = Vec::with_capacity(8);

        loop {
            // check for new events or wait
            if let Err(e) = self_clone
                .poll
                .poll(&mut events, Some(Duration::from_millis(self_clone.config.poll_interval)))
            {
                error!("{}", e);
                continue;
            }

            // perform socket reads and writes in parallel across connections
            // check for new connections
            let _new_conn = if events.iter().any(|event| event.token() == SERVER) {
                debug!("Got a new connection!");
                accept(&self_clone).map_err(|e| error!("{}", e)).ok()
            } else {
                None
            };

            let (bad_tokens, bad_ips) = pool.install(|| {
                self_clone.process_network_events(&events, &deduplication_queues, &mut connections)
            });

            let now = Instant::now();

            if !bad_tokens.is_empty() {
                let mut soft_bans = write_or_die!(self_clone.connection_handler.soft_bans);
                for (ip, e) in bad_ips.into_iter() {
                    if let Ok(_io_err) = e.downcast::<io::Error>() {
                        // potentially ban on IO errors we consider fatal
                    } else {
                        warn!("Soft-banning {:?} due to a breach of protocol", ip);
                        soft_bans.insert(
                            BanId::Ip(ip),
                            Instant::now() + Duration::from_secs(config::SOFT_BAN_DURATION_SECS),
                        );
                    }
                }
                self_clone.remove_connections(&bad_tokens);
            }

            // Run periodic tasks
            if now.duration_since(log_time)
                >= Duration::from_secs(self_clone.config.housekeeping_interval)
            {
                // Check the termination switch
                if self_clone.is_terminated.load(Ordering::Relaxed) {
                    break;
                }

                if let Err(e) = connection_housekeeping(&self_clone) {
                    error!("Issue with connection cleanups: {:?}", e);
                }
                if self_clone.peer_type() != PeerType::Bootstrapper {
                    self_clone.measure_connection_latencies();
                }

                let peer_stat_list = self_clone.get_peer_stats(None);
                check_peers(&self_clone, &peer_stat_list);
                self_clone.print_stats(&peer_stat_list);
                self_clone.measure_throughput(&peer_stat_list);

                log_time = now;
            }

            if self_clone.is_bucket_cleanup_enabled()
                && now.duration_since(last_buckets_cleaned)
                    >= Duration::from_millis(self_clone.config.bucket_cleanup_interval)
            {
                write_or_die!(self_clone.connection_handler.buckets)
                    .clean_buckets(self_clone.config.timeout_bucket_entry_period);
                last_buckets_cleaned = now;
            }
        }
    });

    // Register info about thread into P2PNode.
    write_or_die!(node.threads).join_handles.push(poll_thread);
}

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
                    info!("Found a bootstrap node: {}", addr);
                    let _ = connect(node, PeerType::Bootstrapper, addr, None)
                        .map_err(|e| error!("{}", e));
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
