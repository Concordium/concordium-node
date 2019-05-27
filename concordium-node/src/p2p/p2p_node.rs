#[cfg(feature = "network_dump")]
use crate::dumper::create_dump_thread;
use crate::{
    common::{
        counter::TOTAL_MESSAGES_SENT_COUNTER, get_current_stamp,
        serialization::serialize_into_memory, P2PNodeId, P2PPeer, PeerType, RemotePeer,
    },
    configuration,
    connection::{
        Connection, MessageHandler, MessageManager, NetworkPacketCW, NetworkRequestCW,
        NetworkResponseCW, P2PEvent, RequestHandler, ResponseHandler, SeenMessagesList,
    },
    crypto,
    network::{
        Buckets, NetworkId, NetworkMessage, NetworkPacket, NetworkPacketBuilder, NetworkPacketType,
        NetworkRequest, NetworkResponse,
    },
    p2p::{
        banned_nodes::BannedNode,
        fails,
        no_certificate_verification::NoCertificateVerification,
        p2p_node_handlers::{
            forward_network_packet_message, forward_network_request, forward_network_response,
        },
        peer_statistics::PeerStatistic,
        tls_server::{TlsServer, TlsServerBuilder},
    },
    stats_export_service::StatsExportService,
    utils,
};
use chrono::prelude::*;
use concordium_common::{
    functor::FilterFunctor, RelayOrStopSender, RelayOrStopSenderHelper, UCursor,
};
use failure::{err_msg, Error, Fallible};
#[cfg(not(target_os = "windows"))]
use get_if_addrs;
#[cfg(target_os = "windows")]
use ipconfig;
use mio::{net::TcpListener, Events, Poll, PollOpt, Ready, Token};
use rustls::{Certificate, ClientConfig, NoClientAuth, ServerConfig};
#[cfg(test)]
use std::cell::RefCell;

use std::{
    collections::HashSet,
    net::{
        IpAddr::{self, V4, V6},
        SocketAddr,
    },
    rc::Rc,
    str::FromStr,
    sync::{
        atomic::Ordering,
        mpsc::{channel, Receiver, Sender},
        Arc, Mutex, RwLock,
    },
    thread::{JoinHandle, ThreadId},
    time::{Duration, SystemTime},
};

const SERVER: Token = Token(0);

#[derive(Clone)]
pub struct P2PNodeConfig {
    no_net:                  bool,
    desired_nodes_count:     u8,
    no_bootstrap_dns:        bool,
    bootstrappers_conf:      String,
    dns_resolvers:           Vec<String>,
    dnssec_disabled:         bool,
    bootstrap_node:          Vec<String>,
    minimum_per_bucket:      usize,
    blind_trusted_broadcast: bool,
    max_allowed_nodes:       u16,
}

#[derive(Default)]
pub struct P2PNodeThread {
    pub join_handle: Option<JoinHandle<()>>,
    pub id:          Option<ThreadId>,
}

#[derive(Clone)]
pub struct P2PNode {
    tls_server:           Arc<RwLock<TlsServer>>,
    poll:                 Arc<RwLock<Poll>>,
    id:                   P2PNodeId,
    send_queue_in:        Sender<Arc<NetworkMessage>>,
    send_queue_out:       Rc<Receiver<Arc<NetworkMessage>>>,
    pub internal_addr:    SocketAddr,
    queue_to_super:       RelayOrStopSender<Arc<NetworkMessage>>,
    rpc_queue:            Arc<Mutex<Option<Sender<Arc<NetworkMessage>>>>>,
    start_time:           DateTime<Utc>,
    stats_export_service: Option<Arc<RwLock<StatsExportService>>>,
    peer_type:            PeerType,
    external_addr:        SocketAddr,
    seen_messages:        SeenMessagesList,
    thread:               Arc<RwLock<P2PNodeThread>>,
    quit_tx:              Option<Sender<bool>>,
    pub max_nodes:        Option<u16>,
    pub print_peers:      bool,
    config:               P2PNodeConfig,
    broadcasting_checks:  Arc<FilterFunctor<NetworkPacket>>,
    dump_switch:          Sender<(std::path::PathBuf, bool)>,
    dump_tx:              Sender<crate::dumper::DumpItem>,
}

unsafe impl Send for P2PNode {}

impl P2PNode {
    pub fn new(
        supplied_id: Option<String>,
        conf: &configuration::Config,
        pkt_queue: RelayOrStopSender<Arc<NetworkMessage>>,
        event_log: Option<Sender<P2PEvent>>,
        peer_type: PeerType,
        stats_export_service: Option<Arc<RwLock<StatsExportService>>>,
        broadcasting_checks: Arc<FilterFunctor<NetworkPacket>>,
    ) -> Self {
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

        let poll = Poll::new().unwrap_or_else(|_| panic!("Couldn't create poll"));

        let server =
            TcpListener::bind(&addr).unwrap_or_else(|_| panic!("Couldn't listen on port!"));

        if poll
            .register(&server, SERVER, Ready::readable(), PollOpt::edge())
            .is_err()
        {
            panic!("Couldn't register server with poll!")
        };

        // Generate key pair and cert
        let (cert, private_key) = match crypto::generate_certificate(&id.to_string()) {
            Ok(x) => {
                match x.x509.to_der() {
                    Ok(der) => {
                        // When setting the server single certificate on rustls, it requires a
                        // rustls::PrivateKey. such PrivateKey is defined as
                        // `pub struct PrivateKey(pub Vec<u8>);` and it expects the key
                        // to come in DER format.
                        //
                        // As we have an `openssl::pkey::PKey`, inside the `utils::Cert` struct, we
                        // could just use the function
                        // `private_key_to_der()` over such key, BUT the output of that function
                        // is reported as invalid key when fed into `set_single_cert` IF the
                        // original key was an EcKey (if it was an RSA key,
                        // the DER method works fine).
                        //
                        // There must be a bug somewhere in between the DER encoding of openssl or
                        // the DER decoding of rustls when dealing with
                        // EcKeys.
                        //
                        // Luckily for us, rustls offers a way to import a key from a
                        // pkcs8-PEM-encoded buffer and openssl offers a
                        // function for exporting a key into pkcs8-PEM-encoded buffer so connecting
                        // those two functions, we get a valid `rustls::PrivateKey`.
                        match rustls::internal::pemfile::pkcs8_private_keys(
                            &mut std::io::BufReader::new(
                                x.private_key
                                    .private_key_to_pem_pkcs8()
                                    .expect(
                                        "Something went wrong when exporting a key through openssl",
                                    )
                                    .as_slice(),
                            ),
                        ) {
                            Ok(der_keys) => (Certificate(der), der_keys[0].to_owned()),
                            Err(e) => {
                                panic!("Couldn't convert certificate to DER! {:?}", e);
                            }
                        }
                    }
                    Err(e) => {
                        panic!("Couldn't convert certificate to DER! {:?}", e);
                    }
                }
            }
            Err(e) => {
                panic!("Couldn't create certificate! {:?}", e);
            }
        };

        let mut server_conf = ServerConfig::new(NoClientAuth::new());
        server_conf
            .set_single_cert(vec![cert], private_key)
            .map_err(|e| error!("{}", e))
            .ok();

        let mut client_conf = ClientConfig::new();
        client_conf
            .dangerous()
            .set_certificate_verifier(Arc::new(NoCertificateVerification));

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

        let seen_messages = SeenMessagesList::new(conf.connection.gossip_seen_message_ids_size);

        let (dump_tx, _dump_rx) = std::sync::mpsc::channel();

        let (act_tx, _act_rx) = std::sync::mpsc::channel();

        #[cfg(feature = "network_dump")]
        create_dump_thread(own_peer_ip, id, _dump_rx, _act_rx, &conf.common.data_dir);

        let config = P2PNodeConfig {
            no_net:                  conf.cli.no_network,
            desired_nodes_count:     conf.connection.desired_nodes,
            no_bootstrap_dns:        conf.connection.no_bootstrap_dns,
            bootstrappers_conf:      conf.connection.bootstrap_server.clone(),
            dns_resolvers:           utils::get_resolvers(
                &conf.connection.resolv_conf,
                &conf.connection.dns_resolver,
            ),
            dnssec_disabled:         conf.connection.dnssec_disabled,
            bootstrap_node:          conf.connection.bootstrap_node.clone(),
            minimum_per_bucket:      conf.common.min_peers_bucket,
            blind_trusted_broadcast: !conf.connection.no_trust_broadcasts,
            max_allowed_nodes:       if let Some(max) = conf.connection.max_allowed_nodes {
                u16::from(max)
            } else {
                f64::floor(
                    f64::from(conf.connection.desired_nodes)
                        * (f64::from(conf.connection.max_allowed_nodes_percentage) / 100f64),
                ) as u16
            },
        };

        let networks: HashSet<NetworkId> = conf
            .common
            .network_ids
            .iter()
            .cloned()
            .map(NetworkId::from)
            .collect();
        let tlsserv = TlsServerBuilder::new()
            .set_server(server)
            .set_server_tls_config(Arc::new(server_conf))
            .set_client_tls_config(Arc::new(client_conf))
            .set_max_allowed_peers(config.max_allowed_nodes)
            .set_event_log(event_log)
            .set_stats_export_service(stats_export_service.clone())
            .set_blind_trusted_broadcast(conf.connection.no_trust_broadcasts)
            .set_self_peer(self_peer)
            .set_networks(networks)
            .set_buckets(Arc::new(RwLock::new(Buckets::new())))
            .build()
            .expect("P2P Node creation couldn't create a Tls Server");

        let (send_queue_in, send_queue_out) = channel();

        let mut mself = P2PNode {
            tls_server: Arc::new(RwLock::new(tlsserv)),
            poll: Arc::new(RwLock::new(poll)),
            id,
            send_queue_in: send_queue_in.clone(),
            send_queue_out: Rc::new(send_queue_out),
            internal_addr: SocketAddr::new(ip, conf.common.listen_port),
            queue_to_super: pkt_queue,
            rpc_queue: Arc::new(Mutex::new(None)),
            start_time: Utc::now(),
            stats_export_service,
            external_addr: SocketAddr::new(own_peer_ip, own_peer_port),
            peer_type,
            seen_messages,
            thread: Arc::new(RwLock::new(P2PNodeThread::default())),
            quit_tx: None,
            max_nodes: None,
            print_peers: true,
            config,
            broadcasting_checks,
            dump_switch: act_tx,
            dump_tx,
        };
        mself.add_default_message_handlers();
        mself
    }

    /// It adds default message handler at .
    fn add_default_message_handlers(&mut self) {
        let response_handler = self.make_response_handler();
        let request_handler = self.make_request_handler();
        let packet_handler = self.make_default_network_packet_message_handler();

        write_or_die!(self.message_handler())
            .add_packet_callback(packet_handler)
            .add_response_callback(make_atomic_callback!(move |res: &NetworkResponse| {
                response_handler.process_message(res).map_err(Error::from)
            }))
            .add_request_callback(make_atomic_callback!(move |req: &NetworkRequest| {
                request_handler.process_message(req).map_err(Error::from)
            }));
    }

    /// Default packet handler just forward valid messages.
    fn make_default_network_packet_message_handler(&self) -> NetworkPacketCW {
        let seen_messages = self.seen_messages.clone();
        let own_networks = Arc::clone(&read_or_die!(self.tls_server).networks());
        let own_id = self.id();
        let stats_export_service = self.stats_export_service.clone();
        let queue_to_super = self.queue_to_super.clone();
        let rpc_queue = Arc::clone(&self.rpc_queue);
        let send_queue = self.send_queue_in.clone();
        let trusted_broadcast = self.config.blind_trusted_broadcast;
        let broadcasting_checks = Arc::clone(&self.broadcasting_checks);

        make_atomic_callback!(move |pac: &NetworkPacket| {
            let queues = crate::p2p::p2p_node_handlers::OutgoingQueues {
                send_queue:     &send_queue,
                queue_to_super: &queue_to_super,
                rpc_queue:      &rpc_queue,
            };
            match pac.packet_type {
                NetworkPacketType::BroadcastedMessage => {
                    if broadcasting_checks.run_filters(pac) {
                        forward_network_packet_message(
                            own_id,
                            &seen_messages,
                            &stats_export_service,
                            &own_networks,
                            &queues,
                            pac,
                            trusted_broadcast,
                        )
                    } else {
                        Ok(())
                    }
                }
                NetworkPacketType::DirectMessage(..) => forward_network_packet_message(
                    own_id,
                    &seen_messages,
                    &stats_export_service,
                    &own_networks,
                    &queues,
                    pac,
                    trusted_broadcast,
                ),
            }
        })
    }

    fn make_response_output_handler(&self) -> NetworkResponseCW {
        let packet_queue = self.queue_to_super.clone();
        make_atomic_callback!(move |req: &NetworkResponse| {
            forward_network_response(&req, &packet_queue)
        })
    }

    fn make_response_handler(&self) -> ResponseHandler {
        let output_handler = self.make_response_output_handler();
        let mut handler = ResponseHandler::new();
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
        let mut handler = RequestHandler::new();

        handler
            .add_ban_node_callback(Arc::clone(&requeue_handler))
            .add_unban_node_callback(Arc::clone(&requeue_handler))
            .add_handshake_callback(Arc::clone(&requeue_handler))
            .add_retransmit_callback(Arc::clone(&requeue_handler));
        handler
    }

    /// This function is called periodically to print information about current
    /// nodes.
    fn print_stats(&self, peer_stat_list: &[PeerStatistic]) {
        trace!("Printing out stats");
        if let Some(max_nodes) = self.max_nodes {
            debug!(
                "I currently have {}/{} nodes!",
                peer_stat_list.len(),
                max_nodes
            )
        } else {
            debug!("I currently have {} nodes!", peer_stat_list.len())
        }

        // Print nodes
        if self.print_peers {
            for (i, peer) in peer_stat_list.iter().enumerate() {
                debug!("Peer {}: {}/{}/{}", i, peer.id, peer.addr, peer.peer_type);
            }
        }
    }

    fn check_peers(&mut self, peer_stat_list: &[PeerStatistic]) {
        trace!("Checking for needed peers");
        if self.peer_type != PeerType::Bootstrapper
            && !self.config.no_net
            && self.config.desired_nodes_count
                > peer_stat_list
                    .iter()
                    .filter(|peer| peer.peer_type != PeerType::Bootstrapper)
                    .count() as u8
        {
            if peer_stat_list.is_empty() {
                info!("Sending out GetPeers to any bootstrappers we may still be connected to");
                let nets = read_or_die!(self.tls_server).networks();
                if let Ok(nids) = safe_read!(nets).map(|nets| nets.clone()) {
                    self.send_get_peers(nids);
                }
                if !self.config.no_bootstrap_dns {
                    info!("No nodes at all - retrying bootstrapping");
                    match utils::get_bootstrap_nodes(
                        self.config.bootstrappers_conf.clone(),
                        &self.config.dns_resolvers,
                        self.config.dnssec_disabled,
                        &self.config.bootstrap_node,
                    ) {
                        Ok(nodes) => {
                            for addr in nodes {
                                info!("Found bootstrap node addr {}", addr);
                                self.connect(PeerType::Bootstrapper, addr, None)
                                    .map_err(|e| info!("{}", e))
                                    .ok();
                            }
                        }
                        _ => error!("Can't find any bootstrap nodes - check DNS!"),
                    }
                } else {
                    info!(
                        "No nodes at all - Not retrying bootstrapping using DNS since \
                         --no-bootstrap is specified"
                    );
                }
            } else {
                info!("Not enough nodes, sending GetPeers requests");
                let nets = read_or_die!(self.tls_server).networks();
                if let Ok(nids) = safe_read!(nets).map(|nets| nets.clone()) {
                    self.send_get_peers(nids);
                }
            }
        }
    }

    pub fn spawn(&mut self) {
        let mut self_clone = self.clone();
        let (tx, rx) = channel();
        self.quit_tx = Some(tx);

        let join_handle = spawn_or_die!("P2PNode spawned thread", move || {
            let mut events = Events::with_capacity(1024);
            let mut log_time = SystemTime::now();

            loop {
                let _ = self_clone.process(&mut events).map_err(|e| error!("{}", e));

                // Check termination channel.
                if rx.try_recv().is_ok() {
                    break;
                }

                // Run periodic tasks (every 30 seconds).
                let now = SystemTime::now();
                if let Ok(difference) = now.duration_since(log_time) {
                    if difference > Duration::from_secs(30) {
                        let peer_stat_list = self_clone.get_peer_stats(&[]);
                        self_clone.print_stats(&peer_stat_list);
                        self_clone.check_peers(&peer_stat_list);
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
                    bail!(fails::JoinError {
                        cause: err_msg("Event thread has already be joined"),
                    });
                }
            } else {
                bail!(fails::JoinError {
                    cause: err_msg("It is called from inside event thread"),
                });
            }
        } else {
            bail!(fails::JoinError {
                cause: err_msg("Missing event thread id"),
            });
        }
    }

    pub fn get_version(&self) -> String { crate::VERSION.to_string() }

    pub fn connect(
        &mut self,
        peer_type: PeerType,
        addr: SocketAddr,
        peer_id: Option<P2PNodeId>,
    ) -> Fallible<()> {
        self.log_event(P2PEvent::InitiatingConnection(addr));
        let mut locked_server = write_or_die!(self.tls_server);
        let mut locked_poll = write_or_die!(self.poll);
        locked_server.connect(
            peer_type,
            &mut locked_poll,
            addr,
            peer_id,
            &self.get_self_peer(),
        )
    }

    pub fn id(&self) -> P2PNodeId { self.id }

    pub fn peer_type(&self) -> PeerType { self.peer_type }

    fn log_event(&self, event: P2PEvent) { read_or_die!(self.tls_server).log_event(event); }

    pub fn get_uptime(&self) -> i64 {
        Utc::now().timestamp_millis() - self.start_time.timestamp_millis()
    }

    fn check_sent_status(&self, conn: &Connection, status: Fallible<usize>) {
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
            |conn: &Connection, status: Fallible<usize>| self.check_sent_status(&conn, status);

        let s11n_data = serialize_into_memory(
            &NetworkMessage::NetworkRequest(inner_pkt.clone(), Some(get_current_stamp()), None),
            256,
        );

        match s11n_data {
            Ok(data) => {
                let no_filter = |_: &Connection| true;

                write_or_die!(self.tls_server).send_over_all_connections(
                    &data,
                    &no_filter,
                    &check_sent_status_fn,
                );
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
            |conn: &Connection, status: Fallible<usize>| self.check_sent_status(&conn, status);
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

                    write_or_die!(self.tls_server).send_over_all_connections(
                        &data,
                        &retain,
                        &check_sent_status_fn,
                    );
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
            |conn: &Connection, status: Fallible<usize>| self.check_sent_status(&conn, status);

        let s11n_data = serialize_into_memory(
            &NetworkMessage::NetworkRequest(inner_pkt.clone(), Some(get_current_stamp()), None),
            256,
        );

        match s11n_data {
            Ok(data) => {
                let mut locked_tls_server = write_or_die!(self.tls_server);
                locked_tls_server.send_over_all_connections(
                    &data,
                    &is_valid_connection_post_handshake,
                    &check_sent_status_fn,
                );
                if let NetworkRequest::JoinNetwork(_, network_id) = inner_pkt {
                    locked_tls_server.add_network(*network_id);
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
            |conn: &Connection, status: Fallible<usize>| self.check_sent_status(&conn, status);
        let s11n_data = serialize_into_memory(
            &NetworkMessage::NetworkRequest(inner_pkt.clone(), Some(get_current_stamp()), None),
            256,
        );

        match s11n_data {
            Ok(data) => {
                let mut locked_tls_server = write_or_die!(self.tls_server);
                locked_tls_server.send_over_all_connections(
                    &data,
                    &is_valid_connection_post_handshake,
                    &check_sent_status_fn,
                );
                if let NetworkRequest::LeaveNetwork(_, network_id) = inner_pkt {
                    locked_tls_server.remove_network(*network_id);
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
            |conn: &Connection, status: Fallible<usize>| self.check_sent_status(&conn, status);
        let s11n_data = serialize_into_memory(
            &NetworkMessage::NetworkRequest(inner_pkt.clone(), Some(get_current_stamp()), None),
            256,
        );

        match s11n_data {
            Ok(data) => {
                write_or_die!(self.tls_server).send_over_all_connections(
                    &data,
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
            |conn: &Connection, status: Fallible<usize>| self.check_sent_status(&conn, status);
        if let NetworkRequest::Retransmit(ref peer, ..) = inner_pkt {
            let filter = |conn: &Connection| is_conn_peer_id(conn, peer.id());

            let s11n_data = serialize_into_memory(
                &NetworkMessage::NetworkRequest(inner_pkt.clone(), Some(get_current_stamp()), None),
                256,
            );

            match s11n_data {
                Ok(data) => {
                    write_or_die!(self.tls_server).send_over_all_connections(
                        &data,
                        &filter,
                        &check_sent_status_fn,
                    );
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

    fn process_network_packet(&self, inner_pkt: &NetworkPacket) -> bool {
        let check_sent_status_fn =
            |conn: &Connection, status: Fallible<usize>| self.check_sent_status(&conn, status);

        let s11n_data = serialize_into_memory(
            &NetworkMessage::NetworkPacket(inner_pkt.clone(), Some(get_current_stamp()), None),
            256,
        );

        match s11n_data {
            Ok(data) => match inner_pkt.packet_type {
                NetworkPacketType::DirectMessage(ref receiver) => {
                    let filter = |conn: &Connection| is_conn_peer_id(conn, *receiver);

                    write_or_die!(self.tls_server).send_over_all_connections(
                        &data,
                        &filter,
                        &check_sent_status_fn,
                    ) >= 1
                }
                NetworkPacketType::BroadcastedMessage => {
                    let filter = |conn: &Connection| {
                        is_valid_connection_in_broadcast(
                            conn,
                            &inner_pkt.peer,
                            inner_pkt.network_id,
                        )
                    };

                    write_or_die!(self.tls_server).send_over_all_connections(
                        &data,
                        &filter,
                        &check_sent_status_fn,
                    );
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

    pub fn process_messages(&mut self) {
        let messages = self.send_queue_out.try_iter();
        for outer_pkt in messages {
            trace!("Processing messages!");
            if let Some(ref service) = &self.stats_export_service {
                let _ = safe_write!(service).map(|mut lock| lock.queue_size_dec());
            };
            trace!("Got message to process!");

            match *outer_pkt {
                NetworkMessage::NetworkPacket(ref inner_pkt, ..) => {
                    if !self.process_network_packet(inner_pkt) {
                        if self.send_queue_in.send(outer_pkt).is_ok() {
                            trace!("Successfully requeued a network packet for sending");
                            self.queue_size_inc();
                        } else {
                            error!("Can't put message back in queue for later sending");
                        }
                    }
                }
                NetworkMessage::NetworkRequest(
                    ref inner_pkt @ NetworkRequest::Retransmit(..),
                    ..
                ) => self.process_retransmit(inner_pkt),
                NetworkMessage::NetworkRequest(
                    ref inner_pkt @ NetworkRequest::GetPeers(..),
                    ..
                ) => self.process_get_peers(inner_pkt),
                NetworkMessage::NetworkRequest(
                    ref inner_pkt @ NetworkRequest::UnbanNode(..),
                    ..
                ) => self.process_unban(inner_pkt),
                NetworkMessage::NetworkRequest(ref inner_pkt @ NetworkRequest::BanNode(..), ..) => {
                    self.process_ban(inner_pkt)
                }
                NetworkMessage::NetworkRequest(
                    ref inner_pkt @ NetworkRequest::JoinNetwork(..),
                    ..
                ) => self.process_join_network(inner_pkt),
                NetworkMessage::NetworkRequest(
                    ref inner_pkt @ NetworkRequest::LeaveNetwork(..),
                    ..
                ) => self.process_leave_network(inner_pkt),
                _ => {}
            }
        }
    }

    fn queue_size_inc(&self) {
        if let Some(ref service) = &self.stats_export_service {
            let _ = safe_write!(service).map(|ref mut lock| {
                lock.queue_size_inc();
            });
        };
    }

    fn pks_sent_inc(&self) {
        if let Some(ref service) = &self.stats_export_service {
            let _ = safe_write!(service).map(|ref mut lock| {
                lock.pkt_sent_inc();
            });
        };
    }

    #[inline]
    pub fn send_message(
        &mut self,
        id: Option<P2PNodeId>,
        network_id: NetworkId,
        msg_id: Option<String>,
        msg: Vec<u8>,
        broadcast: bool,
    ) -> Fallible<()> {
        let cursor = UCursor::from(msg);
        self.send_message_from_cursor(id, network_id, msg_id, cursor, broadcast)
    }

    pub fn send_message_from_cursor(
        &mut self,
        id: Option<P2PNodeId>,
        network_id: NetworkId,
        msg_id: Option<String>,
        msg: UCursor,
        broadcast: bool,
    ) -> Fallible<()> {
        trace!("Queueing message!");

        // Create packet.
        let packet = if broadcast {
            NetworkPacketBuilder::default()
                .peer(self.get_self_peer())
                .message_id(msg_id.unwrap_or_else(NetworkPacket::generate_message_id))
                .network_id(network_id)
                .message(msg)
                .build_broadcast()?
        } else {
            let receiver =
                id.ok_or_else(|| err_msg("Direct Message requires a valid target id"))?;

            NetworkPacketBuilder::default()
                .peer(self.get_self_peer())
                .message_id(msg_id.unwrap_or_else(NetworkPacket::generate_message_id))
                .network_id(network_id)
                .message(msg)
                .build_direct(receiver)?
        };

        // Push packet into our `send queue`
        send_or_die!(
            self.send_queue_in,
            Arc::new(NetworkMessage::NetworkPacket(packet, None, None))
        );
        self.queue_size_inc();
        Ok(())
    }

    pub fn send_ban(&mut self, id: BannedNode) {
        send_or_die!(
            self.send_queue_in,
            Arc::new(NetworkMessage::NetworkRequest(
                NetworkRequest::BanNode(self.get_self_peer(), id),
                None,
                None,
            ))
        );
        self.queue_size_inc();
    }

    pub fn send_unban(&mut self, id: BannedNode) {
        send_or_die!(
            self.send_queue_in,
            Arc::new(NetworkMessage::NetworkRequest(
                NetworkRequest::UnbanNode(self.get_self_peer(), id),
                None,
                None,
            ))
        );
        self.queue_size_inc();
    }

    pub fn send_joinnetwork(&mut self, network_id: NetworkId) {
        send_or_die!(
            self.send_queue_in,
            Arc::new(NetworkMessage::NetworkRequest(
                NetworkRequest::JoinNetwork(self.get_self_peer(), network_id),
                None,
                None,
            ))
        );
        self.queue_size_inc();
    }

    pub fn send_leavenetwork(&mut self, network_id: NetworkId) {
        send_or_die!(
            self.send_queue_in,
            Arc::new(NetworkMessage::NetworkRequest(
                NetworkRequest::LeaveNetwork(self.get_self_peer(), network_id),
                None,
                None,
            ))
        );
        self.queue_size_inc();
    }

    pub fn send_get_peers(&mut self, nids: HashSet<NetworkId>) {
        send_or_die!(
            self.send_queue_in,
            Arc::new(NetworkMessage::NetworkRequest(
                NetworkRequest::GetPeers(self.get_self_peer(), nids.clone()),
                None,
                None,
            ))
        );
        self.queue_size_inc();
    }

    pub fn send_retransmit(&mut self, receiver: P2PPeer, since: u64, nid: NetworkId) {
        send_or_die!(
            self.send_queue_in,
            Arc::new(NetworkMessage::NetworkRequest(
                NetworkRequest::Retransmit(receiver, since, nid),
                None,
                None,
            ))
        );
        self.queue_size_inc();
    }

    pub fn get_peer_stats(&self, nids: &[NetworkId]) -> Vec<PeerStatistic> {
        read_or_die!(self.tls_server).get_peer_stats(nids)
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

    fn get_self_peer(&self) -> P2PPeer {
        P2PPeer::from(self.peer_type, self.id(), self.internal_addr)
    }

    pub fn ban_node(&mut self, peer: BannedNode) { write_or_die!(self.tls_server).ban_node(peer); }

    pub fn unban_node(&mut self, peer: BannedNode) {
        write_or_die!(self.tls_server).unban_node(peer);
    }

    pub fn process(&mut self, events: &mut Events) -> Fallible<()> {
        trace!("Going to process MIO events");
        read_or_die!(self.poll).poll(events, Some(Duration::from_millis(1000)))?;

        if self.peer_type != PeerType::Bootstrapper {
            read_or_die!(self.tls_server).liveness_check()?;
        }

        for event in events.iter() {
            let mut tls_ref = write_or_die!(self.tls_server);
            let mut poll_ref = write_or_die!(self.poll);
            match event.token() {
                SERVER => {
                    debug!("Got new connection!");
                    tls_ref
                        .accept(&mut poll_ref, self.get_self_peer())
                        .map_err(|e| error!("{}", e))
                        .ok();
                    if let Some(ref service) = &self.stats_export_service {
                        let _ = safe_write!(service).map(|mut s| s.conn_received_inc());
                    };
                }
                _ => {
                    trace!("Got data!");
                    tls_ref
                        .conn_event(&event)
                        .map_err(|e| error!("Error occurred while parsing event: {}", e))
                        .ok();
                }
            }
        }

        events.clear();

        {
            let tls_ref = read_or_die!(self.tls_server);
            let mut poll_ref = write_or_die!(self.poll);
            tls_ref.cleanup_connections(&mut poll_ref)?;
        }

        trace!("Processing new outbound messages");
        self.process_messages();
        Ok(())
    }

    pub fn close(&mut self) -> Fallible<()> {
        if let Some(ref q) = self.quit_tx {
            q.send(true)?;
            info!("P2PNode shutting down.");
        }
        Ok(())
    }

    pub fn close_and_join(&mut self) -> Fallible<()> {
        self.close()?;
        self.join()
    }

    pub fn get_banlist(&self) -> Vec<BannedNode> { read_or_die!(self.tls_server).get_banlist() }

    pub fn rpc_subscription_start(&mut self, sender: Sender<Arc<NetworkMessage>>) {
        if let Ok(mut locked) = safe_lock!(self.rpc_queue) {
            locked.replace(sender);
        }
    }

    pub fn rpc_subscription_stop(&mut self) -> bool {
        if let Ok(locked) = safe_lock!(self.rpc_queue) {
            locked.is_some()
        } else {
            false
        }
    }

    #[cfg(feature = "network_dump")]
    pub fn activate_dump(&self, path: &str, raw: bool) -> Fallible<()> {
        let path = std::path::PathBuf::from(path);
        self.dump_switch.send((path, raw))?;
        write_or_die!(self.tls_server).dump_start(self.dump_tx.clone());
        Ok(())
    }

    #[cfg(feature = "network_dump")]
    pub fn stop_dump(&self) -> Fallible<()> {
        let path = std::path::PathBuf::new();
        self.dump_switch.send((path, false))?;
        write_or_die!(self.tls_server).dump_stop();
        Ok(())
    }
}

#[cfg(test)]
impl P2PNode {
    pub fn get_tls_server(&self) -> Arc<RwLock<TlsServer>> { Arc::clone(&self.tls_server) }

    pub fn deregister_connection(&self, conn: &RefCell<Connection>) -> Fallible<()> {
        let mut locked_poll = safe_write!(self.poll)?;
        conn.borrow().deregister(&mut locked_poll)
    }
}

impl Drop for P2PNode {
    fn drop(&mut self) {
        let _ = self.queue_to_super.send_stop();
        let _ = self.close_and_join();
    }
}

impl MessageManager for P2PNode {
    fn message_handler(&self) -> Arc<RwLock<MessageHandler>> {
        read_or_die!(self.tls_server).message_handler()
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
    network_id: NetworkId,
) -> bool {
    if let RemotePeer::PostHandshake(remote_peer) = conn.remote_peer() {
        if remote_peer.id() != sender.id() && remote_peer.peer_type() != PeerType::Bootstrapper {
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
