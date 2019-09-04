#[cfg(feature = "network_dump")]
use crate::dumper::create_dump_thread;
use crate::{
    common::{
        counter::TOTAL_MESSAGES_SENT_COUNTER, get_current_stamp, process_network_requests,
        serialization::serialize_into_memory, NetworkRawRequest, P2PNodeId, P2PPeer, PeerStats,
        PeerType, RemotePeer,
    },
    configuration::Config,
    connection::{
        network_handler::{
            message_handler::NetworkMessageCW,
            message_processor::{MessageManager, MessageProcessor, ProcessResult},
        },
        Connection, ConnectionBuilder, MessageSendingPriority, NetworkRequestCW, NetworkResponseCW,
        P2PEvent, RequestHandler, ResponseHandler,
    },
    crypto::generate_snow_config,
    dumper::DumpItem,
    network::{
        packet::MessageId, request::RequestedElementType, Buckets, NetworkId, NetworkMessage,
        NetworkPacket, NetworkPacketType, NetworkRequest, NetworkResponse,
    },
    p2p::{
        banned_nodes::{BannedNode, BannedNodes},
        fails,
        p2p_node_handlers::{
            forward_network_packet_message, forward_network_request, forward_network_response,
        },
        unreachable_nodes::UnreachableNodes,
    },
    utils,
};
use chrono::prelude::*;
use concordium_common::{
    functor::{UnitFunction, UnitFunctor},
    hybrid_buf::HybridBuf,
    stats_export_service::StatsExportService,
    RelayOrStopSenderHelper, RelayOrStopSyncSender,
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
use rand::seq::IteratorRandom;
use snow::Keypair;

use std::{
    collections::{HashMap, HashSet, VecDeque},
    convert::TryFrom,
    net::{
        IpAddr::{self, V4, V6},
        SocketAddr,
    },
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

const MAX_FAILED_PACKETS_ALLOWED: u32 = 50;
const MAX_UNREACHABLE_MARK_TIME: u64 = 86_400_000;
const MAX_BOOTSTRAPPER_KEEP_ALIVE: u64 = 300_000;
const MAX_NORMAL_KEEP_ALIVE: u64 = 1_200_000;
const MAX_PREHANDSHAKE_KEEP_ALIVE: u64 = 120_000;

pub type PreHandshakeCW = UnitFunction<SocketAddr>;
pub type PreHandshake = UnitFunctor<SocketAddr>;

#[derive(Clone)]
pub struct P2PNodeConfig {
    no_net: bool,
    desired_nodes_count: u8,
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
    message_processor:          MessageProcessor,
    prehandshake_validations:   PreHandshake,
    pub log_dumper:             Option<SyncSender<DumpItem>>,
    noise_params:               snow::params::NoiseParams,
    pub network_request_sender: SyncSender<NetworkRawRequest>,
    connections:                Arc<RwLock<Vec<Connection>>>,
    pub to_disconnect:          Arc<RwLock<VecDeque<P2PNodeId>>>,
    pub unreachable_nodes:      UnreachableNodes,
    pub banned_peers:           Arc<RwLock<BannedNodes>>,
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
            message_processor: MessageProcessor::new(),
            prehandshake_validations: PreHandshake::new(),
            log_dumper: None,
            noise_params,
            network_request_sender,
            connections: Default::default(),
            to_disconnect: Default::default(),
            unreachable_nodes: UnreachableNodes::new(),
            banned_peers: Arc::new(RwLock::new(BannedNodes::new())),
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
}

impl P2PNode {
    pub fn new(
        supplied_id: Option<String>,
        conf: &Config,
        pkt_queue: RelayOrStopSyncSender<NetworkMessage>,
        event_log: Option<SyncSender<P2PEvent>>,
        peer_type: PeerType,
        stats_export_service: Option<StatsExportService>,
        subscription_queue_in: SyncSender<NetworkMessage>,
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

        let (dump_tx, _dump_rx) = std::sync::mpsc::sync_channel(10000);

        let (act_tx, _act_rx) = std::sync::mpsc::sync_channel(10000);

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
        };

        let (send_queue_in, send_queue_out) = sync_channel(10000);
        let (resend_queue_in, resend_queue_out) = sync_channel(10000);
        let (network_request_sender, network_request_receiver) = sync_channel(10000);

        let receivers = Receivers {
            send_queue_out,
            resend_queue_out,
            network_requests: network_request_receiver,
        };

        let connection_handler =
            ConnectionHandler::new(conf, server, network_request_sender, event_log);

        let mut node = P2PNode {
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
        };
        node.add_default_prehandshake_validations();
        node.setup_default_message_handler();
        node.add_default_message_handlers();

        (node, receivers)
    }

    /// It setups default message handler at noise protocol handler level.
    fn setup_default_message_handler(&self) {
        let banned_nodes = Arc::clone(&self.connection_handler.banned_peers);
        let to_disconnect = Arc::clone(&self.connection_handler.to_disconnect);
        self.connection_handler
            .message_processor
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
        let mh = self.connection_handler.message_processor.clone();
        conn.common_message_processor.add(mh);
    }

    fn add_default_prehandshake_validations(&self) {
        self.connection_handler
            .prehandshake_validations
            .add_callback(self.make_check_banned());
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
        read_or_die!(self.connection_handler.connections)
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

    /// It adds default message handler at .
    fn add_default_message_handlers(&mut self) {
        let response_handler = self.make_response_handler();
        let request_handler = self.make_request_handler();
        let packet_notifier = self.make_default_network_packet_message_notifier();

        self.message_processor()
            .add_response_action(make_atomic_callback!(move |res: &NetworkResponse| {
                response_handler.process_message(res).map_err(Error::from)
            }))
            .add_request_action(make_atomic_callback!(move |req: &NetworkRequest| {
                request_handler.process_message(req).map_err(Error::from)
            }))
            .add_notification(packet_notifier);
    }

    /// Default packet handler just forward valid messages.
    fn make_default_network_packet_message_notifier(&self) -> NetworkMessageCW {
        let own_networks = Arc::clone(&self.networks());
        let stats_export_service = self.stats_export_service().clone();
        let queue_to_super = self.queue_to_super.clone();
        let rpc_queue = self.rpc_queue.clone();
        let send_queue = self.send_queue_in().clone();
        let is_rpc_online = Arc::clone(&self.is_rpc_online);

        make_atomic_callback!(move |pac: &NetworkMessage| {
            if let NetworkMessage::NetworkPacket(pac, ..) = pac {
                let queues = crate::p2p::p2p_node_handlers::OutgoingQueues {
                    send_queue:     send_queue.clone(),
                    queue_to_super: queue_to_super.clone(),
                    rpc_queue:      rpc_queue.clone(),
                };

                forward_network_packet_message(
                    stats_export_service.clone(),
                    own_networks.clone(),
                    queues,
                    pac.clone(),
                    Arc::clone(&is_rpc_online),
                )
            } else {
                Ok(())
            }
        })
    }

    fn make_response_output_handler(&self) -> NetworkResponseCW {
        let packet_queue = self.queue_to_super.clone();
        make_atomic_callback!(move |req: &NetworkResponse| {
            forward_network_response(req, packet_queue.clone())
        })
    }

    fn make_response_handler(&self) -> ResponseHandler {
        let output_handler = self.make_response_output_handler();
        let handler = ResponseHandler::new();
        handler.add_peer_list_callback(output_handler);
        handler
    }

    fn make_requeue_handler(&self) -> NetworkRequestCW {
        let packet_queue = self.queue_to_super.clone();
        make_atomic_callback!(move |req: &NetworkRequest| {
            forward_network_request(req, &packet_queue)
        })
    }

    fn make_request_handler(&self) -> RequestHandler {
        let requeue_handler = self.make_requeue_handler();
        let handler = RequestHandler::new();

        handler
            .add_ban_node_callback(requeue_handler.clone())
            .add_unban_node_callback(requeue_handler.clone())
            .add_handshake_callback(requeue_handler.clone())
            .add_retransmit_callback(requeue_handler.clone());
        handler
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
                        let peer_stat_list = self_clone.get_peer_stats();
                        self_clone.print_stats(&peer_stat_list);
                        self_clone.check_peers(&peer_stat_list);

                        if self_clone.peer_type() != PeerType::Bootstrapper {
                            self_clone.liveness_check();
                        }
                        if let Err(e) = self_clone.connection_housekeeping() {
                            error!("Issue with connection cleanups: {:?}", e);
                        }
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
        let curr_stamp = get_current_stamp();

        read_or_die!(self.connection_handler.connections)
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

    fn connection_housekeeping(&self) -> Fallible<()> {
        let curr_stamp = get_current_stamp();
        let peer_type = self.peer_type();

        write_or_die!(self.connection_handler.to_disconnect)
            .drain(..)
            .for_each(|x| {
                if let Some(ref conn) = self.find_connection_by_id(x) {
                    trace!(
                        "Disconnecting connection {} already marked as going down",
                        usize::from(conn.token())
                    );
                    conn.close();
                }
            });

        // clone the initial collection of connections to reduce locking
        let uncleaned_connections: Vec<_> =
            read_or_die!(self.connection_handler.connections).clone();

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
                    wrap_connection_already_gone_as_non_fatal(conn_token, conn.deregister(&self.poll))?;
                    wrap_connection_already_gone_as_non_fatal(conn_token, conn.shutdown())?;
                }
                // Report number of peers to stats export engine
                if let Some(ref service) = &self.stats_export_service() {
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
            let mut locked_active_peers = write_or_die!(self.active_peer_stats);

            for closed_connection in uncleaned_connections
                .into_iter()
                .filter(|conn| closing_conns.contains(&conn.token()))
            {
                trace!(
                    "Removed connection {} from the Connection Handler",
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
            self.connection_handler
                .unreachable_nodes
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
            let max_allowed_nodes = self.config.max_allowed_nodes;
            let peer_count = self.connections_posthandshake_count(Some(PeerType::Bootstrapper));
            if peer_count > max_allowed_nodes {
                let mut rng = rand::thread_rng();
                read_or_die!(self.connection_handler.connections)
                    .iter()
                    .choose_multiple(&mut rng, (peer_count - max_allowed_nodes) as usize)
                    .iter()
                    .for_each(|conn| conn.close());
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

    pub fn accept(&self) -> Fallible<()> {
        let self_peer = self.self_peer;
        let (socket, addr) = self.connection_handler.server.accept()?;

        debug!(
            "Accepting new connection from {:?} to {:?}:{}",
            addr,
            self_peer.ip(),
            self_peer.port()
        );

        if let Err(e) = self
            .connection_handler
            .prehandshake_validations
            .run_callbacks(&addr)
        {
            return Err(Error::from(e));
        }
        self.log_event(P2PEvent::ConnectEvent(addr));

        let token = Token(
            self.connection_handler
                .next_id
                .fetch_add(1, Ordering::SeqCst),
        );
        let networks = self.networks();
        let key_pair = utils::clone_snow_keypair(&self.connection_handler.key_pair);

        let conn = ConnectionBuilder::default()
            .set_handler_ref(Arc::pin(self.clone()))
            .set_socket(socket)
            .set_token(token)
            .set_key_pair(key_pair)
            .set_local_peer(self_peer)
            .set_remote_peer(RemotePeer::PreHandshake(PeerType::Node, addr))
            .set_local_end_networks(networks)
            .set_noise_params(self.connection_handler.noise_params.clone())
            .build()?;

        self.register_message_handlers(&conn);

        conn.setup_pre_handshake();
        conn.setup_post_handshake();

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
                if let Some(ref service) = self.stats_export_service() {
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
                    .set_remote_peer(RemotePeer::PreHandshake(peer_type, addr))
                    .set_local_end_networks(Arc::clone(&networks))
                    .set_noise_params(self.connection_handler.noise_params.clone())
                    .build()?;

                self.register_message_handlers(&conn);
                conn.setup_pre_handshake();
                conn.setup_post_handshake();
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

    fn make_check_banned(&self) -> PreHandshakeCW {
        let banned_peers = self.connection_handler.banned_peers.clone();
        make_atomic_callback!(move |sockaddr: &SocketAddr| {
            if read_or_die!(banned_peers).is_addr_banned((*sockaddr).ip()) {
                Err(Error::from(fails::BannedNodeRequestedConnectionError))
            } else {
                Ok(())
            }
        })
    }

    pub fn dump_start(&mut self, log_dumper: SyncSender<DumpItem>) {
        self.connection_handler.log_dumper = Some(log_dumper);
    }

    pub fn dump_stop(&mut self) { self.connection_handler.log_dumper = None; }

    pub fn add_notification(&self, func: UnitFunction<NetworkMessage>) {
        self.connection_handler
            .message_processor
            .add_notification(func.clone());
        read_or_die!(self.connection_handler.connections)
            .iter()
            .for_each(|conn| conn.add_notification(func.clone()))
    }

    pub fn connections_posthandshake_count(&self, exclude_type: Option<PeerType>) -> u16 {
        // We will never have more than 2^16 connections per node, so this conversion is
        // safe.
        read_or_die!(self.connection_handler.connections)
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
        if write_or_die!(self.connection_handler.banned_peers).insert(peer) {
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
        write_or_die!(self.connection_handler.banned_peers).remove(&peer)
    }

    pub fn id_is_banned(&self, id: P2PNodeId) -> bool {
        read_or_die!(self.connection_handler.banned_peers).is_id_banned(id)
    }

    pub fn addr_is_banned(&self, sockaddr: SocketAddr) -> bool {
        read_or_die!(self.connection_handler.banned_peers).is_addr_banned(sockaddr.ip())
    }

    pub fn get_banlist(&self) -> Vec<BannedNode> {
        read_or_die!(self.connection_handler.banned_peers)
            .by_id
            .iter()
            .map(|id| BannedNode::ById(*id))
            .chain(
                read_or_die!(self.connection_handler.banned_peers)
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

    pub fn find_connections_by_id(&self, id: P2PNodeId) -> Vec<Connection> {
        read_or_die!(self.connection_handler.connections)
            .iter()
            .filter(|&conn| conn.remote_id() == Some(id))
            .cloned()
            .collect()
    }

    pub fn find_connection_by_token(&self, token: Token) -> Option<Connection> {
        read_or_die!(self.connection_handler.connections)
            .iter()
            .find(|conn| conn.token() == token)
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

    fn remove_connections(&self, to_remove: &[Token]) {
        write_or_die!(self.connection_handler.connections)
            .retain(|conn| !to_remove.contains(&conn.token()));
    }

    pub fn add_connection(&self, conn: Connection) {
        write_or_die!(self.connection_handler.connections).push(conn);
    }

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

    pub fn send_queue_in(&self) -> &SyncSender<NetworkMessage> { &self.send_queue_in }

    pub fn stats_export_service(&self) -> &Option<StatsExportService> { &self.stats_export_service }

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
        if let RemotePeer::PostHandshake(remote_peer) = conn.remote_peer() {
            match status {
                Ok(_) => {
                    self.pks_sent_inc(); // assuming non-failable
                    TOTAL_MESSAGES_SENT_COUNTER.fetch_add(1, Ordering::Relaxed);
                }
                Err(e) => {
                    error!(
                        "Could not send to peer {} due to {}",
                        remote_peer.id().to_string(),
                        e
                    );
                }
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

                if let Some(ref service) = self.stats_export_service() {
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
                    if let Some(ref service) = &self.stats_export_service() {
                        service.conn_received_inc();
                    };
                }
                _ => {
                    trace!("Got data!");
                    self.conn_event(&event)
                        .map_err(|e| error!("Error occurred while parsing event: {}", e))
                        .ok();
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

    fn pks_sent_inc(&self) {
        if let Some(ref service) = self.stats_export_service {
            service.pkt_sent_inc();
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

impl MessageManager for P2PNode {
    fn message_processor(&self) -> MessageProcessor {
        self.connection_handler.message_processor.clone()
    }
}

fn is_conn_peer_id(conn: &Connection, id: P2PNodeId) -> bool {
    if let RemotePeer::PostHandshake(remote_peer) = conn.remote_peer() {
        remote_peer.id() == id
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
    if let RemotePeer::PostHandshake(remote_peer) = conn.remote_peer() {
        if remote_peer.peer_type() != PeerType::Bootstrapper
            && remote_peer.id() != sender.id()
            && !peers_to_skip.contains(&remote_peer.id())
            && !dont_relay_to.contains(&remote_peer.id())
        {
            let remote_end_networks = conn.remote_end_networks();
            return remote_end_networks.contains(&network_id);
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
