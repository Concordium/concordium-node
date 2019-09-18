#[cfg(feature = "network_dump")]
use crate::dumper::create_dump_thread;
use crate::{
    common::{
        get_current_stamp, process_network_requests, serialization::serialize_into_memory,
        NetworkRawRequest, P2PNodeId, P2PPeer, PeerStats, PeerType, RemotePeer,
    },
    configuration::{self as config, Config},
    connection::{Connection, ConnectionBuilder, MessageSendingPriority, P2PEvent},
    crypto::generate_snow_config,
    dumper::DumpItem,
    network::{
        packet::MessageId, request::RequestedElementType, Buckets, NetworkId, NetworkMessage,
        NetworkPacket, NetworkPacketType, NetworkRequest,
    },
    p2p::{banned_nodes::BannedNode, fails, unreachable_nodes::UnreachableNodes},
    utils,
};
use chrono::prelude::*;
use concordium_common::{
    hybrid_buf::HybridBuf, stats_export_service::StatsExportService, RelayOrStopSenderHelper,
    RelayOrStopSyncSender, SerializeToBytes,
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
    convert::TryFrom,
    net::{
        IpAddr::{self, V4, V6},
        SocketAddr,
    },
    path::PathBuf,
    str::FromStr,
    sync::{
        atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering},
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
    pub desired_nodes_count: u8,
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

#[derive(Clone)]
pub struct ConnectionHandler {
    server:                     Arc<TcpListener>,
    next_id:                    Arc<AtomicUsize>,
    key_pair:                   Arc<Keypair>,
    pub event_log:              Option<SyncSender<P2PEvent>>,
    pub buckets:                Arc<RwLock<Buckets>>,
    pub log_dumper:             Option<SyncSender<DumpItem>>,
    noise_params:               snow::params::NoiseParams,
    pub network_request_sender: SyncSender<NetworkRawRequest>,
    pub connections:            Arc<RwLock<HashSet<Connection, BuildNoHashHasher<usize>>>>,
    pub unreachable_nodes:      UnreachableNodes,
    pub networks:               Arc<RwLock<HashSet<NetworkId>>>,
    pub last_bootstrap:         Arc<AtomicU64>,
}

impl ConnectionHandler {
    fn new(
        conf: &Config,
        server: TcpListener,
        network_request_sender: SyncSender<NetworkRawRequest>,
        event_log: Option<SyncSender<P2PEvent>>,
    ) -> Self {
        let networks: HashSet<NetworkId> = conf
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
            server: Arc::new(server),
            next_id: Arc::new(AtomicUsize::new(2)),
            key_pair: Arc::new(key_pair),
            event_log,
            buckets: Arc::new(RwLock::new(Buckets::new())),
            log_dumper: None,
            noise_params,
            network_request_sender,
            connections: Default::default(),
            unreachable_nodes: UnreachableNodes::new(),
            networks: Arc::new(RwLock::new(networks)),
            last_bootstrap: Default::default(),
        }
    }
}

pub struct Receivers {
    send_queue_out:       Receiver<NetworkMessage>,
    resend_queue_out:     Receiver<ResendQueueEntry>,
    pub network_requests: Receiver<NetworkRawRequest>,
}

#[derive(Clone)]
pub struct P2PNode {
    pub self_peer:            P2PPeer,
    external_addr:            SocketAddr,
    thread:                   Arc<RwLock<P2PNodeThread>>,
    pub poll:                 Arc<Poll>,
    pub connection_handler:   ConnectionHandler,
    pub send_queue_in:        SyncSender<NetworkMessage>,
    resend_queue_in:          SyncSender<ResendQueueEntry>,
    pub queue_to_super:       RelayOrStopSyncSender<NetworkMessage>,
    pub rpc_queue:            SyncSender<NetworkMessage>,
    dump_switch:              SyncSender<(std::path::PathBuf, bool)>,
    dump_tx:                  SyncSender<crate::dumper::DumpItem>,
    pub active_peer_stats:    Arc<RwLock<HashMap<u64, PeerStats>>>,
    pub stats_export_service: Option<StatsExportService>,
    pub config:               P2PNodeConfig,
    start_time:               DateTime<Utc>,
    pub is_rpc_online:        Arc<AtomicBool>,
    pub is_terminated:        Arc<AtomicBool>,
    pub kvs:                  Arc<RwLock<Rkv>>,
}

impl P2PNode {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        supplied_id: Option<String>,
        conf: &Config,
        pkt_queue: RelayOrStopSyncSender<NetworkMessage>,
        event_log: Option<SyncSender<P2PEvent>>,
        peer_type: PeerType,
        stats_export_service: Option<StatsExportService>,
        subscription_queue_in: SyncSender<NetworkMessage>,
        data_dir_path: Option<PathBuf>,
    ) -> (Self, Receivers) {
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

        let own_peer_ip = if let Some(ref own_ip) = conf.common.external_ip {
            match IpAddr::from_str(own_ip) {
                Ok(ip) => ip,
                _ => ip,
            }
        } else {
            ip
        };

        let own_peer_port = if let Some(own_port) = conf.common.external_port {
            own_port
        } else {
            conf.common.listen_port
        };

        let self_peer = P2PPeer::from(peer_type, id, SocketAddr::new(own_peer_ip, own_peer_port));

        let (dump_tx, _dump_rx) = std::sync::mpsc::sync_channel(config::DUMP_QUEUE_DEPTH);
        let (act_tx, _act_rx) = std::sync::mpsc::sync_channel(config::DUMP_SWITCH_QUEUE_DEPTH);

        #[cfg(feature = "network_dump")]
        create_dump_thread(own_peer_ip, id, _dump_rx, _act_rx, &conf.common.data_dir);

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
                u16::from(max)
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
        };

        let (send_queue_in, send_queue_out) = sync_channel(config::OUTBOUND_QUEUE_DEPTH);
        let (resend_queue_in, resend_queue_out) = sync_channel(config::RESEND_QUEUE_DEPTH);
        let (network_request_sender, network_request_receiver) =
            sync_channel(config::RAW_NETWORK_MSG_QUEUE_DEPTH);

        let receivers = Receivers {
            send_queue_out,
            resend_queue_out,
            network_requests: network_request_receiver,
        };

        let connection_handler =
            ConnectionHandler::new(conf, server, network_request_sender, event_log);

        // Create the node key-value store environment
        let kvs = Manager::singleton()
            .write()
            .unwrap()
            .get_or_create(config.data_dir_path.as_path(), Rkv::new)
            .unwrap();

        let node = P2PNode {
            poll: Arc::new(poll),
            resend_queue_in: resend_queue_in.clone(),
            queue_to_super: pkt_queue,
            rpc_queue: subscription_queue_in,
            start_time: Utc::now(),
            external_addr: SocketAddr::new(own_peer_ip, own_peer_port),
            thread: Arc::new(RwLock::new(P2PNodeThread::default())),
            config,
            dump_switch: act_tx,
            dump_tx,
            is_rpc_online: Arc::new(AtomicBool::new(false)),
            connection_handler,
            self_peer,
            active_peer_stats: Default::default(),
            send_queue_in,
            stats_export_service,
            is_terminated: Default::default(),
            kvs,
        };

        node.clear_bans()
            .unwrap_or_else(|e| error!("Couldn't reset the ban list: {}", e));

        (node, receivers)
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
        read_or_die!(self.connection_handler.connections)
            .iter()
            .filter(|conn| !conn.is_closed() && filter_conn(conn))
            .map(|conn| {
                let status = conn.async_send(data.clone(), MessageSendingPriority::Normal);
                send_status(&conn, status)
            })
            .count()
    }

    pub fn get_all_current_peers(&self, peer_type: Option<PeerType>) -> Vec<P2PNodeId> {
        read_or_die!(self.active_peer_stats)
            .values()
            .filter(|peer| peer_type.is_none() || peer_type == Some(peer.peer_type))
            .map(|peer| P2PNodeId(peer.id))
            .collect()
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

    pub fn forward_network_message(&self, msg: &NetworkMessage) -> Fallible<()> {
        match msg {
            NetworkMessage::NetworkResponse(msg, ..) => {
                let msg = NetworkMessage::NetworkResponse(msg.to_owned(), None, None);

                if let Err(queue_error) = self.queue_to_super.send_msg(msg) {
                    warn!("Message cannot be forwarded: {:?}", queue_error);
                };
            }
            NetworkMessage::NetworkRequest(msg, ..) => {
                let msg = NetworkMessage::NetworkRequest(msg.to_owned(), None, None);

                if let Err(queue_error) = self.queue_to_super.send_msg(msg) {
                    warn!("Message cannot be forwarded: {:?}", queue_error);
                };
            }
            NetworkMessage::NetworkPacket(pac, ..) => {
                trace!("Processing message for relaying");
                if safe_read!(self.networks())?.contains(&pac.network_id) {
                    trace!(
                        "Received message of size {} from {}",
                        pac.message.len()?,
                        pac.peer.id()
                    );
                    let outer = NetworkMessage::NetworkPacket(Arc::clone(&pac), None, None);

                    if self.is_rpc_online.load(Ordering::Relaxed) {
                        if let Err(e) = self.rpc_queue.send(outer.clone()) {
                            warn!(
                                "Can't relay a message to the RPC outbound queue: {}",
                                e.to_string()
                            );
                        }
                    }

                    if let Err(e) = self.queue_to_super.send_msg(outer.clone()) {
                        warn!(
                            "Can't relay a message on to the outer super queue: {}",
                            e.to_string()
                        );
                    }
                } else if let Some(ref service) = self.stats_export_service {
                    service.invalid_network_pkts_received_inc();
                }
            }
            _ => {}
        };

        Ok(())
    }

    /// This function is called periodically to print information about current
    /// nodes.
    fn print_stats(&self, peer_stat_list: &[PeerStats]) {
        trace!("Printing out stats");
        debug!(
            "I currently have {}/{} peers",
            peer_stat_list.len(),
            self.config.max_allowed_nodes
        );

        // Print nodes
        if self.config.print_peers {
            for (i, peer) in peer_stat_list.iter().enumerate() {
                trace!("Peer {}: {}/{}/{}", i, peer.id, peer.addr, peer.peer_type);
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
                    .count() as u8
        {
            if peer_stat_list.is_empty() {
                info!("Sending out GetPeers to any bootstrappers we may still be connected to");
                {
                    let nets = self.networks();
                    if let Ok(nids) = safe_read!(nets).map(|nets| nets.clone()) {
                        self.send_get_peers(nids);
                    }
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
                let nets = self.networks();
                if let Ok(nids) = safe_read!(nets).map(|nets| nets.clone()) {
                    self.send_get_peers(nids);
                }
            }
        }
    }

    pub fn spawn(&mut self, receivers: Receivers) {
        // Prepare poll-loop channels.
        let self_clone = self.clone();

        let join_handle = spawn_or_die!("P2PNode spawned thread", move || {
            let mut events = Events::with_capacity(10);
            let mut log_time = SystemTime::now();

            loop {
                let _ = self_clone
                    .process(&receivers, &mut events)
                    .map_err(|e| error!("{}", e));

                process_network_requests(&self_clone, &receivers);

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
        let connections = read_or_die!(self.connection_handler.connections).clone();

        connections
            .into_iter()
            .filter(|conn| {
                conn.is_post_handshake()
                    && !conn.is_closed()
                    && (conn.last_seen() + 120_000 < curr_stamp
                        || conn.get_last_ping_sent() + 300_000 < curr_stamp)
            })
            .for_each(|conn| {
                let request_ping = {
                    NetworkMessage::NetworkRequest(
                        NetworkRequest::Ping(conn.handler().self_peer),
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

    fn connection_housekeeping(&self) -> Fallible<()> {
        debug!("Running connection housekeeping");

        let curr_stamp = get_current_stamp();
        let peer_type = self.peer_type();

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

        // Kill connections to nodes which are no longer seen
        write_or_die!(self.connection_handler.connections).retain(|conn| {
            !(filter_predicate_stable_conn_and_no_handshake(&conn)
                || filter_predicate_bootstrapper_no_activity_allowed_period(&conn)
                || filter_predicate_node_no_activity_allowed_period(&conn))
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
                let to_drop = read_or_die!(self.connection_handler.connections)
                    .iter()
                    .map(|conn| conn.token)
                    .choose_multiple(&mut rng, (peer_count - max_allowed_nodes) as usize);

                write_or_die!(self.connection_handler.connections)
                    .retain(|conn| !to_drop.contains(&conn.token));
            }
        }

        *write_or_die!(self.active_peer_stats) = read_or_die!(self.connection_handler.connections)
            .iter()
            .filter(|conn| conn.is_post_handshake())
            .filter_map(|conn| conn.remote_peer_stats().ok())
            .map(|stats| (stats.id, stats))
            .collect();

        // reconnect to bootstrappers after a specified amount of time
        if peer_type == PeerType::Node
            && curr_stamp >= self.get_last_bootstrap() + self.config.bootstrapping_interval * 1000
        {
            self.attempt_bootstrap();
        }

        Ok(())
    }

    #[inline]
    pub fn networks(&self) -> Arc<RwLock<HashSet<NetworkId>>> {
        Arc::clone(&self.connection_handler.networks)
    }

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

        if read_or_die!(self.connection_handler.connections)
            .iter()
            .any(|conn| conn.remote_addr() == addr)
        {
            bail!("Duplicate connection attempt from {:?}; rejecting", addr);
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

        let conn = ConnectionBuilder::default()
            .set_handler_ref(Arc::pin(self.clone()))
            .set_socket(socket)
            .set_token(token)
            .set_key_pair(key_pair)
            .set_local_peer(self_peer)
            .set_remote_peer(RemotePeer {
                id: Default::default(),
                addr,
                peer_type: PeerType::Node,
            })
            .set_noise_params(self.connection_handler.noise_params.clone())
            .build()?;

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
                if let Some(ref service) = self.stats_export_service {
                    service.conn_received_inc();
                };
                let token = Token(
                    self.connection_handler
                        .next_id
                        .fetch_add(1, Ordering::SeqCst),
                );
                let networks = self.networks();
                let keypair = utils::clone_snow_keypair(&self.connection_handler.key_pair);
                let conn = ConnectionBuilder::default()
                    .set_handler_ref(Arc::pin(self.clone()))
                    .set_socket(socket)
                    .set_token(token)
                    .set_key_pair(keypair)
                    .set_as_initiator(true)
                    .set_local_peer(self_peer)
                    .set_remote_peer(RemotePeer {
                        id: Default::default(),
                        addr,
                        peer_type,
                    })
                    .set_noise_params(self.connection_handler.noise_params.clone())
                    .build()?;

                conn.register(&self.poll)?;

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

    pub fn dump_start(&mut self, log_dumper: SyncSender<DumpItem>) {
        self.connection_handler.log_dumper = Some(log_dumper);
    }

    pub fn dump_stop(&mut self) { self.connection_handler.log_dumper = None; }

    pub fn connections_posthandshake_count(&self, exclude_type: Option<PeerType>) -> u16 {
        // We will never have more than 2^16 connections per node, so this conversion is
        // safe.
        read_or_die!(self.active_peer_stats)
            .values()
            .filter(|&peer| {
                if let Some(exclude_type) = exclude_type {
                    return peer.peer_type != exclude_type;
                }
                true
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
                    self.remove_connections(&[conn.token]);
                }
            }
            BannedNode::ByAddr(addr) => {
                for conn in self.find_connections_by_ip(addr) {
                    self.remove_connections(&[conn.token]);
                }
            }
        }

        if !self.config.no_trust_bans {
            self.send_ban(peer);
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

        if !self.config.no_trust_bans {
            self.send_unban(peer);
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

    /// It removes this server from `network_id` network.
    /// *Note:* Network list is shared, and this will updated all other
    /// instances.
    pub fn remove_network(&self, network_id: NetworkId) {
        write_or_die!(self.connection_handler.networks).retain(|x| *x != network_id);
    }

    /// It adds this server to `network_id` network.
    pub fn add_network(&self, network_id: NetworkId) {
        write_or_die!(self.connection_handler.networks).insert(network_id);
    }

    pub fn find_connection_by_id(&self, id: P2PNodeId) -> Option<Connection> {
        read_or_die!(self.connection_handler.connections)
            .iter()
            .find(|conn| conn.remote_id() == Some(id))
            .cloned()
    }

    pub fn find_connection_by_token(&self, token: Token) -> Option<Connection> {
        read_or_die!(self.connection_handler.connections)
            .iter()
            .find(|conn| conn.token == token)
            .cloned()
    }

    pub fn find_connection_by_ip_addr(&self, addr: SocketAddr) -> Option<Connection> {
        read_or_die!(self.connection_handler.connections)
            .iter()
            .find(|conn| conn.remote_addr() == addr)
            .cloned()
    }

    pub fn find_connections_by_ip(&self, ip: IpAddr) -> Vec<Connection> {
        read_or_die!(self.connection_handler.connections)
            .iter()
            .filter(|conn| conn.remote_peer().addr().ip() == ip)
            .cloned()
            .collect()
    }

    pub fn remove_connections(&self, to_remove: &[Token]) {
        write_or_die!(self.connection_handler.connections)
            .retain(|conn| !to_remove.contains(&conn.token));
    }

    pub fn add_connection(&self, conn: Connection) -> bool {
        write_or_die!(self.connection_handler.connections).insert(conn)
    }

    pub fn conn_event(&self, event: &Event) {
        let token = event.token();

        if let Some(conn) = self.find_connection_by_token(token) {
            if !conn.is_closed() {
                if let Err(e) = conn.ready(event) {
                    error!("Error while processing a connection event: {}", e);
                }
            } else {
                self.remove_connections(&[token])
            }
        }
    }

    /// Waits for P2PNode termination. Use `P2PNode::close` to notify the
    /// termination.
    ///
    /// It is safe to call this function several times, even from internal
    /// P2PNode thread.
    pub fn join(&mut self) -> Fallible<()> {
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

    fn check_sent_status(&self, conn: &Connection, status: Fallible<()>) {
        if conn.is_post_handshake() {
            if let Err(e) = status {
                error!(
                    "Could not send to peer {} due to {}",
                    read_or_die!(conn.remote_peer.id).unwrap().to_string(),
                    e
                );
            }
        }
    }

    fn forward_network_request_over_all_connections(&self, inner_pkt: &NetworkRequest) {
        let check_sent_status_fn =
            |conn: &Connection, status: Fallible<()>| self.check_sent_status(&conn, status);

        let s11n_data = serialize_into_memory(
            &NetworkMessage::NetworkRequest(inner_pkt.clone(), Some(get_current_stamp()), None),
            256,
        );

        match s11n_data {
            Ok(data) => {
                let no_filter = |_: &Connection| true;

                self.send_over_all_connections(data, &no_filter, &check_sent_status_fn);
            }
            Err(e) => {
                error!(
                    "Network request cannot be forwarded due to a serialization issue: {}",
                    e
                );
            }
        }
    }

    fn process_unban(&self, inner_pkt: &NetworkRequest) {
        if let NetworkRequest::UnbanNode(ref peer, ref unbanned_peer) = inner_pkt {
            match unbanned_peer {
                BannedNode::ById(id) => {
                    if peer.id() != *id {
                        self.forward_network_request_over_all_connections(inner_pkt);
                    }
                }
                _ => {
                    self.forward_network_request_over_all_connections(inner_pkt);
                }
            }
        };
    }

    fn process_ban(&self, inner_pkt: &NetworkRequest) {
        let check_sent_status_fn =
            |conn: &Connection, status: Fallible<()>| self.check_sent_status(&conn, status);

        if let NetworkRequest::BanNode(_, to_ban) = inner_pkt {
            let s11n_data = serialize_into_memory(
                &NetworkMessage::NetworkRequest(inner_pkt.clone(), Some(get_current_stamp()), None),
                256,
            );

            match s11n_data {
                Ok(data) => {
                    let retain = |conn: &Connection| match to_ban {
                        BannedNode::ById(id) => {
                            conn.remote_peer().peer().map_or(true, |x| x.id() != *id)
                        }
                        BannedNode::ByAddr(addr) => {
                            conn.remote_peer().peer().map_or(true, |x| x.ip() != *addr)
                        }
                    };

                    self.send_over_all_connections(data, &retain, &check_sent_status_fn);
                }
                Err(e) => {
                    error!(
                        "BanNode message cannot be sent due to a serialization issue: {}",
                        e
                    );
                }
            }
        };
    }

    fn process_join_network(&self, inner_pkt: &NetworkRequest) {
        let check_sent_status_fn =
            |conn: &Connection, status: Fallible<()>| self.check_sent_status(&conn, status);

        let s11n_data = serialize_into_memory(
            &NetworkMessage::NetworkRequest(inner_pkt.clone(), Some(get_current_stamp()), None),
            256,
        );

        match s11n_data {
            Ok(data) => {
                self.send_over_all_connections(
                    data,
                    &is_valid_connection_post_handshake,
                    &check_sent_status_fn,
                );
                if let NetworkRequest::JoinNetwork(_, network_id) = inner_pkt {
                    self.add_network(*network_id);
                }
            }
            Err(e) => {
                error!(
                    "Join Network message cannot be sent due to a serialization issue: {}",
                    e
                );
            }
        };
    }

    fn process_leave_network(&self, inner_pkt: &NetworkRequest) {
        let check_sent_status_fn =
            |conn: &Connection, status: Fallible<()>| self.check_sent_status(&conn, status);
        let s11n_data = serialize_into_memory(
            &NetworkMessage::NetworkRequest(inner_pkt.clone(), Some(get_current_stamp()), None),
            256,
        );

        match s11n_data {
            Ok(data) => {
                self.send_over_all_connections(
                    data,
                    &is_valid_connection_post_handshake,
                    &check_sent_status_fn,
                );
                if let NetworkRequest::LeaveNetwork(_, network_id) = inner_pkt {
                    self.remove_network(*network_id);
                }
            }
            Err(e) => {
                error!(
                    "Leave Network message cannot be sent due to a serialization issue: {}",
                    e
                );
            }
        }
    }

    fn process_get_peers(&self, inner_pkt: &NetworkRequest) {
        let check_sent_status_fn =
            |conn: &Connection, status: Fallible<()>| self.check_sent_status(&conn, status);
        let s11n_data = serialize_into_memory(
            &NetworkMessage::NetworkRequest(inner_pkt.clone(), Some(get_current_stamp()), None),
            256,
        );

        match s11n_data {
            Ok(data) => {
                self.send_over_all_connections(
                    data,
                    &is_valid_connection_post_handshake,
                    &check_sent_status_fn,
                );
            }
            Err(e) => {
                error!(
                    "GetPeers message cannot be sent due to a serialization issue: {}",
                    e
                );
            }
        }
    }

    fn process_retransmit(&self, inner_pkt: &NetworkRequest) {
        let check_sent_status_fn =
            |conn: &Connection, status: Fallible<()>| self.check_sent_status(&conn, status);
        if let NetworkRequest::Retransmit(ref peer, ..) = inner_pkt {
            let filter = |conn: &Connection| is_conn_peer_id(conn, peer.id());

            let s11n_data = serialize_into_memory(
                &NetworkMessage::NetworkRequest(inner_pkt.clone(), Some(get_current_stamp()), None),
                256,
            );

            match s11n_data {
                Ok(data) => {
                    self.send_over_all_connections(data, &filter, &check_sent_status_fn);
                }
                Err(e) => {
                    error!(
                        "Retransmit message cannot be sent due to a serialization issue: {}",
                        e
                    );
                }
            }
        }
    }

    fn process_network_packet(&self, inner_pkt: Arc<NetworkPacket>) -> bool {
        let check_sent_status_fn =
            |conn: &Connection, status: Fallible<()>| self.check_sent_status(&conn, status);

        let serialized_packet = serialize_into_memory(
            &NetworkMessage::NetworkPacket(Arc::clone(&inner_pkt), Some(get_current_stamp()), None),
            256,
        );

        let (peers_to_skip, s11n_data) = match inner_pkt.packet_type {
            NetworkPacketType::DirectMessage(..) => (vec![].into_boxed_slice(), serialized_packet),
            NetworkPacketType::BroadcastedMessage(ref dont_send_to) => {
                let not_valid_receivers = if self.config.relay_broadcast_percentage < 1.0 {
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
                        .into_boxed_slice()
                } else {
                    Box::from([])
                };

                (not_valid_receivers, serialized_packet)
            }
        };

        match s11n_data {
            Ok(data) => match inner_pkt.packet_type {
                NetworkPacketType::DirectMessage(ref receiver) => {
                    let filter = |conn: &Connection| is_conn_peer_id(conn, *receiver);

                    self.send_over_all_connections(data, &filter, &check_sent_status_fn) >= 1
                }
                NetworkPacketType::BroadcastedMessage(ref dont_relay_to) => {
                    let filter = |conn: &Connection| {
                        is_valid_connection_in_broadcast(
                            conn,
                            &inner_pkt.peer,
                            &peers_to_skip,
                            &dont_relay_to,
                            inner_pkt.network_id,
                        )
                    };

                    self.send_over_all_connections(data, &filter, &check_sent_status_fn);
                    true
                }
            },
            Err(e) => {
                error!(
                    "Packet message cannot be sent due to a serialization issue: {}",
                    e
                );
                true
            }
        }
    }

    fn process_messages(&self, receivers: &Receivers) {
        receivers
            .send_queue_out
            .try_iter()
            .map(|outer_pkt| {
                trace!("Processing messages!");

                if let Some(ref service) = self.stats_export_service {
                    service.queue_size_dec();
                };
                trace!("Got message to process!");

                match outer_pkt {
                    NetworkMessage::NetworkPacket(ref inner_pkt, ..) => {
                        if !self.process_network_packet(Arc::clone(&inner_pkt)) {
                            Some(outer_pkt)
                        } else {
                            None
                        }
                    }
                    NetworkMessage::NetworkRequest(
                        ref inner_pkt @ NetworkRequest::Retransmit(..),
                        ..
                    ) => {
                        self.process_retransmit(inner_pkt);
                        None
                    }
                    NetworkMessage::NetworkRequest(
                        ref inner_pkt @ NetworkRequest::GetPeers(..),
                        ..
                    ) => {
                        self.process_get_peers(inner_pkt);
                        None
                    }
                    NetworkMessage::NetworkRequest(
                        ref inner_pkt @ NetworkRequest::UnbanNode(..),
                        ..
                    ) => {
                        self.process_unban(inner_pkt);
                        None
                    }
                    NetworkMessage::NetworkRequest(
                        ref inner_pkt @ NetworkRequest::BanNode(..),
                        ..
                    ) => {
                        self.process_ban(inner_pkt);
                        None
                    }
                    NetworkMessage::NetworkRequest(
                        ref inner_pkt @ NetworkRequest::JoinNetwork(..),
                        ..
                    ) => {
                        self.process_join_network(inner_pkt);
                        None
                    }
                    NetworkMessage::NetworkRequest(
                        ref inner_pkt @ NetworkRequest::LeaveNetwork(..),
                        ..
                    ) => {
                        self.process_leave_network(inner_pkt);
                        None
                    }
                    _ => None,
                }
            })
            .filter_map(|possible_failure| possible_failure)
            .for_each(|failed_pkt| {
                self.pks_resend_inc();
                // attempt to process failed messages again
                if self.config.max_resend_attempts > 0
                    && self
                        .resend_queue_in
                        .send(ResendQueueEntry::new(failed_pkt, get_current_stamp(), 0u8))
                        .is_ok()
                {
                    trace!("Successfully queued a failed network packet to be attempted again");
                    self.resend_queue_size_inc();
                } else {
                    self.pks_dropped_inc();
                    error!("Can't put message back in queue for later sending");
                }
            });
    }

    fn process_resend_queue(&self, receivers: &Receivers) {
        let resend_failures = receivers
            .resend_queue_out
            .try_iter()
            .map(|wrapper| {
                trace!("Processing messages!");
                self.resend_queue_size_dec();
                trace!("Got a message to reprocess!");

                match wrapper.message {
                    NetworkMessage::NetworkPacket(ref inner_pkt, ..) => {
                        if !self.process_network_packet(Arc::clone(&inner_pkt)) {
                            Some(wrapper)
                        } else {
                            None
                        }
                    }
                    _ => unreachable!("Attempted to reprocess a non-packet network message!"),
                }
            })
            .filter_map(|possible_failure| possible_failure)
            .collect::<Vec<_>>();
        resend_failures.iter().for_each(|failed_resend_pkt| {
            if failed_resend_pkt.attempts < self.config.max_resend_attempts {
                if self
                    .resend_queue_in
                    .send(ResendQueueEntry::new(
                        failed_resend_pkt.message.clone(),
                        failed_resend_pkt.last_attempt,
                        failed_resend_pkt.attempts + 1,
                    ))
                    .is_ok()
                {
                    trace!("Successfully requeued a failed network packet");
                    self.resend_queue_size_inc();
                } else {
                    error!("Can't put a packet in the resend queue!");
                    self.pks_dropped_inc();
                }
            }
        })
    }

    pub fn get_node_peer_ids(&self) -> Vec<u64> {
        read_or_die!(self.active_peer_stats)
            .iter()
            .filter(|(_, stats)| stats.peer_type == PeerType::Node)
            .map(|(id, _)| id)
            .copied()
            .collect()
    }

    pub fn get_peer_stats(&self) -> Vec<PeerStats> {
        read_or_die!(self.active_peer_stats)
            .values()
            .cloned()
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

    pub fn get_self_peer(&self) -> P2PPeer { self.self_peer }

    pub fn internal_addr(&self) -> SocketAddr { self.self_peer.addr }

    fn process(&self, receivers: &Receivers, events: &mut Events) -> Fallible<()> {
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
                    self.conn_event(&event);
                }
            }
        }

        events.clear();

        self.process_messages(receivers);

        self.process_resend_queue(receivers);
        Ok(())
    }

    pub fn close(&self) -> bool {
        info!("P2PNode shutting down.");
        self.is_terminated.store(true, Ordering::Relaxed);
        true
    }

    pub fn close_and_join(&mut self) -> Fallible<()> {
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

    pub fn send_ban(&self, id: BannedNode) {
        send_or_die!(
            self.send_queue_in,
            NetworkMessage::NetworkRequest(NetworkRequest::BanNode(self.self_peer, id), None, None,)
        );
        self.queue_size_inc();
    }

    pub fn send_unban(&self, id: BannedNode) {
        send_or_die!(
            self.send_queue_in,
            NetworkMessage::NetworkRequest(
                NetworkRequest::UnbanNode(self.self_peer, id),
                None,
                None,
            )
        );
        self.queue_size_inc();
    }

    pub fn send_joinnetwork(&self, network_id: NetworkId) {
        send_or_die!(
            self.send_queue_in,
            NetworkMessage::NetworkRequest(
                NetworkRequest::JoinNetwork(self.self_peer, network_id),
                None,
                None,
            )
        );
        self.queue_size_inc();
    }

    pub fn send_leavenetwork(&self, network_id: NetworkId) {
        send_or_die!(
            self.send_queue_in,
            NetworkMessage::NetworkRequest(
                NetworkRequest::LeaveNetwork(self.self_peer, network_id),
                None,
                None,
            )
        );
        self.queue_size_inc();
    }

    pub fn send_get_peers(&self, nids: HashSet<NetworkId>) {
        send_or_die!(
            self.send_queue_in,
            NetworkMessage::NetworkRequest(
                NetworkRequest::GetPeers(self.self_peer, nids.clone()),
                None,
                None,
            )
        );
        self.queue_size_inc();
    }

    pub fn send_retransmit(
        &self,
        requested_type: RequestedElementType,
        since: u64,
        nid: NetworkId,
    ) {
        send_or_die!(
            self.send_queue_in,
            NetworkMessage::NetworkRequest(
                NetworkRequest::Retransmit(self.self_peer, requested_type, since, nid),
                None,
                None,
            )
        );
        self.queue_size_inc();
    }

    fn queue_size_inc(&self) {
        if let Some(ref service) = self.stats_export_service {
            service.queue_size_inc();
        };
    }

    fn resend_queue_size_inc(&self) {
        if let Some(ref service) = self.stats_export_service {
            service.resend_queue_size_inc();
        };
    }

    fn resend_queue_size_dec(&self) {
        if let Some(ref service) = self.stats_export_service {
            service.resend_queue_size_dec();
        };
    }

    fn pks_dropped_inc(&self) {
        if let Some(ref service) = self.stats_export_service {
            service.pkt_dropped_inc();
        };
    }

    fn pks_resend_inc(&self) {
        if let Some(ref service) = self.stats_export_service {
            service.pkt_resend_inc();
        };
    }
}

#[cfg(test)]
impl P2PNode {
    pub fn deregister_connection(&self, conn: &Connection) -> Fallible<()> {
        conn.deregister(&self.poll)
    }
}

impl Drop for P2PNode {
    fn drop(&mut self) {
        let _ = self.queue_to_super.send_stop();
        let _ = self.close_and_join();
    }
}

fn is_conn_peer_id(conn: &Connection, id: P2PNodeId) -> bool {
    if conn.is_post_handshake() {
        read_or_die!(conn.remote_peer.id).unwrap() == id
    } else {
        false
    }
}

/// Connetion is valid for a broadcast if sender is not target,
/// network_id is owned by connection, and the remote peer is not
/// a bootstrap node.
pub fn is_valid_connection_in_broadcast(
    conn: &Connection,
    sender: &P2PPeer,
    peers_to_skip: &[P2PNodeId],
    dont_relay_to: &[P2PNodeId],
    network_id: NetworkId,
) -> bool {
    if conn.is_post_handshake() {
        let peer_id = read_or_die!(conn.remote_peer.id).unwrap();

        if conn.remote_peer.peer_type() != PeerType::Bootstrapper
            && peer_id != sender.id()
            && !peers_to_skip.contains(&peer_id)
            && !dont_relay_to.contains(&peer_id)
        {
            let remote_end_networks = conn.remote_end_networks();
            return read_or_die!(remote_end_networks).contains(&network_id);
        }
    }
    false
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
    target_id: Option<P2PNodeId>,
    network_id: NetworkId,
    msg_id: Option<MessageId>,
    msg: HybridBuf,
) -> Fallible<()> {
    send_message_from_cursor(node, target_id, vec![], network_id, msg_id, msg, false)
}

#[inline]
pub fn send_broadcast_message(
    node: &P2PNode,
    dont_relay_to: Vec<P2PNodeId>,
    network_id: NetworkId,
    msg_id: Option<MessageId>,
    msg: HybridBuf,
) -> Fallible<()> {
    send_message_from_cursor(node, None, dont_relay_to, network_id, msg_id, msg, true)
}

pub fn send_message_from_cursor(
    node: &P2PNode,
    target_id: Option<P2PNodeId>,
    dont_relay_to: Vec<P2PNodeId>,
    network_id: NetworkId,
    msg_id: Option<MessageId>,
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
        peer: node.self_peer,
        message_id: msg_id.unwrap_or_else(NetworkPacket::generate_message_id),
        network_id,
        message,
    };

    // Push packet into our `send queue`
    send_or_die!(
        node.send_queue_in,
        NetworkMessage::NetworkPacket(Arc::new(packet), None, None)
    );

    if let Some(ref service) = node.stats_export_service {
        service.queue_size_inc();
    };

    Ok(())
}
