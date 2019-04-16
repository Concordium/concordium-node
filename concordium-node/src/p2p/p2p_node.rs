use crate::{prometheus_exporter::PrometheusServer, utils};
use chrono::prelude::*;
use failure::{err_msg, Error, Fallible};
#[cfg(not(target_os = "windows"))]
use get_if_addrs;
#[cfg(target_os = "windows")]
use ipconfig;
use mio::{net::TcpListener, Events, Poll, PollOpt, Ready, Token};
use rustls::{Certificate, ClientConfig, NoClientAuth, ServerConfig};
use std::{
    collections::{HashSet, VecDeque},
    net::IpAddr::{self, V4, V6},
    str::FromStr,
    sync::{
        atomic::Ordering,
        mpsc::{channel, Sender},
        Arc, RwLock,
    },
    thread::{ JoinHandle, ThreadId},
    time::{Duration, SystemTime},
};

use crate::{
    common::{counter::TOTAL_MESSAGES_SENT_COUNTER, P2PNodeId, P2PPeer, PeerType, UCursor},
    configuration,
    connection::{
        Connection, MessageHandler, MessageManager, NetworkPacketCW, NetworkRequestCW,
        NetworkResponseCW, P2PEvent, RequestHandler, ResponseHandler, SeenMessagesList,
    },
    network::{
        Buckets, NetworkId, NetworkMessage, NetworkPacket, NetworkPacketBuilder, NetworkPacketType,
        NetworkRequest, NetworkResponse,
    },
};

use crate::p2p::{
    fails,
    no_certificate_verification::NoCertificateVerification,
    p2p_node_handlers::{
        forward_network_packet_message, forward_network_request, forward_network_response,
    },
    peer_statistics::PeerStatistic,
    tls_server::TlsServer,
};

const SERVER: Token = Token(0);

#[derive(Clone)]
pub struct P2PNodeConfig {
    no_net:                  bool,
    desired_nodes_count:     u8,
    no_bootstrap_dns:        bool,
    bootstrappers_conf:      String,
    dns_resolvers:           Vec<String>,
    dnssec:                  bool,
    bootstrap_node:          Vec<String>,
    minimum_per_bucket:      usize,
    blind_trusted_broadcast: bool,
}

#[derive(Default)]
pub struct P2PNodeThread {
    pub join_handle: Option<JoinHandle<Fallible<()>>>,
    pub id: Option<ThreadId>
}


#[derive(Clone)]
pub struct P2PNode {
    tls_server:          Arc<RwLock<TlsServer>>,
    poll:                Arc<RwLock<Poll>>,
    id:                  P2PNodeId,
    send_queue:          Arc<RwLock<VecDeque<Arc<NetworkMessage>>>>,
    ip:                  IpAddr,
    port:                u16,
    incoming_pkts:       Sender<Arc<NetworkMessage>>,
    event_log:           Option<Sender<P2PEvent>>,
    start_time:          DateTime<Utc>,
    prometheus_exporter: Option<Arc<RwLock<PrometheusServer>>>,
    peer_type:           PeerType,
    external_ip:         IpAddr,
    external_port:       u16,
    seen_messages:       SeenMessagesList,
    thread:              Arc< RwLock< P2PNodeThread >>,
    quit_tx:             Option<Sender<bool>>,
    pub max_nodes:       Option<u16>,
    pub print_peers:     bool,
    config:              P2PNodeConfig,
}

unsafe impl Send for P2PNode {}

impl P2PNode {
    pub fn new(
        supplied_id: Option<String>,
        conf: &configuration::Config,
        pkt_queue: Sender<Arc<NetworkMessage>>,
        event_log: Option<Sender<P2PEvent>>,
        peer_type: PeerType,
        prometheus_exporter: Option<Arc<RwLock<PrometheusServer>>>,
    ) -> Self {
        let addr = if let Some(ref addy) = conf.common.listen_address {
            format!("{}:{}", addy, conf.common.listen_port)
                .parse()
                .unwrap_or_else(|_| {
                    warn!("Supplied listen address coulnd't be parsed");
                    format!("0.0.0.0:{}", conf.common.listen_port)
                        .parse()
                        .unwrap()
                })
        } else {
            format!("0.0.0.0:{}", conf.common.listen_port)
                .parse()
                .unwrap()
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
                P2PNodeId::from_str(s).unwrap_or_else(|e| panic!("invalid ID provided: {}", e))
            }
        } else {
            P2PNodeId::default()
        };

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
        let (cert, private_key) = match utils::generate_certificate(&id.to_string()) {
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

        let self_peer = P2PPeer::from(PeerType::Node, id, own_peer_ip, own_peer_port);

        let seen_messages = SeenMessagesList::new();

        let networks: HashSet<NetworkId> = conf
            .common
            .network_ids
            .iter()
            .cloned()
            .map(NetworkId::from)
            .collect();
        let tlsserv = TlsServer::new(
            server,
            Arc::new(server_conf),
            Arc::new(client_conf),
            event_log.clone(),
            self_peer,
            prometheus_exporter.clone(),
            networks,
            Arc::new(RwLock::new(Buckets::new())),
            conf.connection.no_trust_broadcasts,
        );

        let config = P2PNodeConfig {
            no_net:                  conf.cli.no_network,
            desired_nodes_count:     conf.connection.desired_nodes,
            no_bootstrap_dns:        conf.connection.no_bootstrap_dns,
            bootstrappers_conf:      conf.connection.bootstrap_server.clone(),
            dns_resolvers:           conf.connection.dns_resolver.clone(),
            dnssec:                  !conf.connection.no_dnssec,
            bootstrap_node:          conf.connection.bootstrap_node.clone(),
            minimum_per_bucket:      conf.common.min_peers_bucket,
            blind_trusted_broadcast: conf.connection.no_trust_broadcasts,
        };

        let mut mself = P2PNode {
            tls_server: Arc::new(RwLock::new(tlsserv)),
            poll: Arc::new(RwLock::new(poll)),
            id,
            send_queue: Arc::new(RwLock::new(VecDeque::new())),
            ip,
            port: conf.common.listen_port,
            incoming_pkts: pkt_queue,
            event_log,
            start_time: Utc::now(),
            prometheus_exporter,
            external_ip: own_peer_ip,
            external_port: own_peer_port,
            peer_type,
            seen_messages,
            thread: Arc::new( RwLock::new( P2PNodeThread::default())),
            quit_tx: None,
            max_nodes: None,
            print_peers: true,
            config,
        };
        mself.add_default_message_handlers();
        mself
    }

    /// It adds default message handler at .
    fn add_default_message_handlers(&mut self) {
        let response_handler = self.make_response_handler();
        let request_handler = self.make_request_handler();
        let packet_handler = self.make_default_network_packet_message_handler();

        let shared_mh = self.message_handler();
        let mut locked_mh = shared_mh
            .write()
            .expect("Coulnd't set the default message handlers");
        locked_mh
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
        let own_networks = Arc::clone(
            &safe_read!(self.tls_server)
                .expect("Couldn't lock the tls server")
                .networks(),
        );
        let prometheus_exporter = self.prometheus_exporter.clone();
        let packet_queue = self.incoming_pkts.clone();
        let send_queue = Arc::clone(&self.send_queue);
        let trusted_broadcast = self.config.blind_trusted_broadcast;

        make_atomic_callback!(move |pac: &NetworkPacket| {
            forward_network_packet_message(
                &seen_messages,
                &prometheus_exporter,
                &own_networks,
                &send_queue,
                &packet_queue,
                pac,
                trusted_broadcast,
            )
        })
    }

    fn make_response_output_handler(&self) -> NetworkResponseCW {
        let packet_queue = self.incoming_pkts.clone();
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
        let packet_queue = self.incoming_pkts.clone();

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
            .add_handshake_callback(Arc::clone(&requeue_handler));

        handler
    }

    /// This function is called periodically to print information about current
    /// nodes.
    fn print_stats(&self, peer_stat_list: &[PeerStatistic]) {
        if let Some(max_nodes) = self.max_nodes {
            info!(
                "I currently have {}/{} nodes!",
                peer_stat_list.len(),
                max_nodes
            )
        } else {
            info!("I currently have {} nodes!", peer_stat_list.len())
        }

        // Print nodes
        if self.print_peers {
            for (i, peer) in peer_stat_list.iter().enumerate() {
                info!("Peer {}: {}/{}:{}", i, peer.id(), peer.ip(), peer.port());
            }
        }
    }

    fn check_peers(&mut self, peer_stat_list: &[PeerStatistic]) {
        if !self.config.no_net
            && self.config.desired_nodes_count > peer_stat_list.len() as u8
            && self.peer_type != PeerType::Bootstrapper
        {
            if peer_stat_list.is_empty() {
                if !self.config.no_bootstrap_dns {
                    info!("No nodes at all - retrying bootstrapping");
                    match utils::get_bootstrap_nodes(
                        self.config.bootstrappers_conf.clone(),
                        &self.config.dns_resolvers,
                        self.config.dnssec,
                        &self.config.bootstrap_node,
                    ) {
                        Ok(nodes) => {
                            for (ip, port) in nodes {
                                info!("Found bootstrap node IP: {} and port: {}", ip, port);
                                self.connect(PeerType::Bootstrapper, ip, port, None)
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
                if let Ok(nids) = safe_read!(self.tls_server)
                    .and_then(|x| safe_read!(x.networks()).map(|nets| nets.clone()))
                {
                    self.send_get_peers(nids)
                        .unwrap_or_else(|e| error!("{}", e));
                }
            }
        }
    }

    pub fn spawn(&mut self) -> Fallible<()> {
        let mut self_clone = self.clone();
        let (tx, rx) = channel();
        self.quit_tx = Some(tx);

        let join_handle = std::thread::spawn(move || -> Fallible<()> {
            let mut events = Events::with_capacity(1024);
            let mut log_time = SystemTime::now();

            loop {
                let _ = self_clone.process(&mut events).map_err(|e| error!("{}", e));

                // Check termination channel.
                if let Ok(_) = rx.try_recv() {
                    break;
                }

                // Run periodic tasks (every 30 seconds).
                let now = SystemTime::now();
                if let Ok(difference) = now.duration_since(log_time) {
                    if difference > Duration::from_secs(30) {
                        match self_clone.get_peer_stats(&[]) {
                            Ok(peer_stat_list) => {
                                self_clone.print_stats(&peer_stat_list);
                                self_clone.check_peers(&peer_stat_list);
                            }
                            Err(e) => error!("Couldn't get node list, {:?}", e),
                        }
                        log_time = now;
                    }
                }
            }

            Ok(())
        });

        // Register info about thread into P2PNode.
        {
            let mut locked_thread = safe_write!(self.thread)?;
            locked_thread.id = Some( join_handle.thread().id());
            locked_thread.join_handle = Some(join_handle);
        }

        Ok(())
    }

    /// Waits for P2PNode termination. Use `P2PNode::close` to notify the termination.
    ///
    /// It is safe to call this function several times, even from internal P2PNode thread.
    pub fn join(&mut self) -> Fallible<()> {
        let id_opt =  safe_read!(self.thread)?.id.clone();
        if let Some(id) = id_opt {
            let current_thread_id = std::thread::current().id();
            if id != current_thread_id {
                if let Some(join_handle) = safe_write!(self.thread)?.join_handle.take() {
                    return join_handle.join().map_err( |_| fails::JoinError)?;
                }
            } else {
                bail!( fails::JoinError);
            }
        }

        Ok(())
    }

    pub fn get_version(&self) -> String { crate::VERSION.to_string() }

    pub fn connect(
        &mut self,
        peer_type: PeerType,
        ip: IpAddr,
        port: u16,
        peer_id: Option<P2PNodeId>,
    ) -> Fallible<()> {
        self.log_event(P2PEvent::InitiatingConnection(ip, port));
        let mut locked_server = safe_write!(self.tls_server)?;
        let mut locked_poll = safe_write!(self.poll)?;
        locked_server.connect(
            peer_type,
            &mut locked_poll,
            ip,
            port,
            peer_id,
            &self.get_self_peer(),
        )
    }

    pub fn id(&self) -> P2PNodeId { self.id }

    pub fn get_listening_ip(&self) -> IpAddr { self.ip }

    pub fn get_listening_port(&self) -> u16 { self.port }

    pub fn peer_type(&self) -> PeerType { self.peer_type }

    fn log_event(&mut self, event: P2PEvent) {
        if let Some(ref mut x) = self.event_log {
            if let Err(e) = x.send(event) {
                error!("Couldn't send event {:?}", e)
            }
        }
    }

    pub fn get_uptime(&self) -> i64 {
        Utc::now().timestamp_millis() - self.start_time.timestamp_millis()
    }

    fn check_sent_status(&self, conn: &Connection, status: Fallible<usize>) {
        if let Some(remote_peer) = conn.remote_peer() {
            match status {
                Ok(_) => {
                    self.pks_sent_inc().unwrap(); // assuming non-failable
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

    pub fn process_messages(&mut self) -> Fallible<()> {
        let mut send_q = safe_write!(self.send_queue)?;
        if send_q.len() == 0 {
            return Ok(());
        }
        let mut resend_queue: VecDeque<Arc<NetworkMessage>> = VecDeque::new();
        loop {
            trace!("Processing messages!");
            let outer_pkt = send_q.pop_front();
            match outer_pkt {
                Some(ref x) => {
                    if let Some(ref prom) = &self.prometheus_exporter {
                        let ref mut lock = safe_write!(prom)?;
                        lock.queue_size_dec().unwrap_or_else(|e| error!("{}", e));
                    };
                    trace!("Got message to process!");
                    let check_sent_status_fn = |conn: &Connection, status: Fallible<usize>| {
                        self.check_sent_status(conn, status)
                    };

                    match **x {
                        NetworkMessage::NetworkPacket(ref inner_pkt, ..) => {
                            let data = inner_pkt.serialize();
                            match inner_pkt.packet_type {
                                NetworkPacketType::DirectMessage(ref receiver) => {
                                    let filter =
                                        |conn: &Connection| is_conn_peer_id(conn, receiver);
                                    safe_write!(self.tls_server)?.send_over_all_connections(
                                        &data,
                                        &filter,
                                        &check_sent_status_fn,
                                    );
                                }
                                NetworkPacketType::BroadcastedMessage => {
                                    let filter = |conn: &Connection| {
                                        is_valid_connection_in_broadcast(
                                            conn,
                                            &inner_pkt.peer,
                                            inner_pkt.network_id,
                                        )
                                    };
                                    safe_write!(self.tls_server)?.send_over_all_connections(
                                        &data,
                                        &filter,
                                        &check_sent_status_fn,
                                    );
                                }
                            };
                        }
                        NetworkMessage::NetworkRequest(
                            ref inner_pkt @ NetworkRequest::UnbanNode(..),
                            ..
                        )
                        | NetworkMessage::NetworkRequest(
                            ref inner_pkt @ NetworkRequest::GetPeers(..),
                            ..
                        )
                        | NetworkMessage::NetworkRequest(
                            ref inner_pkt @ NetworkRequest::BanNode(..),
                            ..
                        ) => {
                            let data = inner_pkt.serialize();
                            let no_filter = |_: &Connection| true;

                            safe_write!(self.tls_server)?.send_over_all_connections(
                                &data,
                                &no_filter,
                                &check_sent_status_fn,
                            );
                        }
                        NetworkMessage::NetworkRequest(
                            ref inner_pkt @ NetworkRequest::JoinNetwork(..),
                            ..
                        ) => {
                            let data = inner_pkt.serialize();
                            let no_filter = |_: &Connection| true;

                            let mut locked_tls_server = safe_write!(self.tls_server)?;
                            locked_tls_server.send_over_all_connections(
                                &data,
                                &no_filter,
                                &check_sent_status_fn,
                            );

                            if let NetworkRequest::JoinNetwork(_, network_id) = inner_pkt {
                                locked_tls_server
                                    .add_network(*network_id)
                                    .map_err(|e| error!("{}", e))
                                    .ok();
                            }
                        }
                        NetworkMessage::NetworkRequest(
                            ref inner_pkt @ NetworkRequest::LeaveNetwork(..),
                            ..
                        ) => {
                            let data = inner_pkt.serialize();
                            let no_filter = |_: &Connection| true;

                            let mut locked_tls_server = safe_write!(self.tls_server)?;
                            locked_tls_server.send_over_all_connections(
                                &data,
                                &no_filter,
                                &check_sent_status_fn,
                            );

                            if let NetworkRequest::LeaveNetwork(_, network_id) = inner_pkt {
                                locked_tls_server
                                    .remove_network(*network_id)
                                    .map_err(|e| error!("{}", e))
                                    .ok();
                            }
                        }
                        _ => {}
                    }
                }
                _ => {
                    if resend_queue.len() > 0 {
                        if let Some(ref prom) = &self.prometheus_exporter {
                            match safe_write!(prom) {
                                Ok(ref mut lock) => {
                                    lock.queue_size_inc_by(resend_queue.len() as i64)
                                        .map_err(|e| error!("{}", e))
                                        .ok();
                                    lock.queue_resent_inc_by(resend_queue.len() as i64)
                                        .map_err(|e| error!("{}", e))
                                        .ok();
                                }
                                _ => error!("Couldn't lock prometheus instance"),
                            }
                        };
                        send_q.append(&mut resend_queue);
                        resend_queue.clear();
                    }
                    break;
                }
            }
        }
        Ok(())
    }

    fn queue_size_inc(&self) -> Fallible<()> {
        if let Some(ref prom) = &self.prometheus_exporter {
            match safe_write!(prom) {
                Ok(ref mut lock) => {
                    lock.queue_size_inc().unwrap_or_else(|e| error!("{}", e));
                }
                _ => error!("Couldn't lock prometheus instance"),
            }
        };
        Ok(())
    }

    fn pks_sent_inc(&self) -> Fallible<()> {
        if let Some(ref prom) = &self.prometheus_exporter {
            match safe_write!(prom) {
                Ok(ref mut lock) => {
                    lock.pkt_sent_inc().unwrap_or_else(|e| error!("{}", e));
                }
                _ => error!("Couldn't lock prometheus instance"),
            }
        };
        Ok(())
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
        debug!("Queueing message!");

        // Create packet.
        let packet = if broadcast {
            NetworkPacketBuilder::default()
                .peer(self.get_self_peer())
                .message_id(msg_id.unwrap_or(NetworkPacket::generate_message_id()))
                .network_id(network_id)
                .message(msg)
                .build_broadcast()?
        } else {
            let receiver =
                id.ok_or_else(|| err_msg("Direct Message requires a valid target id"))?;

            NetworkPacketBuilder::default()
                .peer(self.get_self_peer())
                .message_id(msg_id.unwrap_or(NetworkPacket::generate_message_id()))
                .network_id(network_id)
                .message(msg)
                .build_direct(receiver)?
        };

        // Push packet into our `send queue`
        safe_write!(self.send_queue)?
            .push_back(Arc::new(NetworkMessage::NetworkPacket(packet, None, None)));
        self.queue_size_inc()?;
        Ok(())
    }

    pub fn send_ban(&mut self, id: P2PPeer) -> Fallible<()> {
        safe_write!(self.send_queue)?.push_back(Arc::new(NetworkMessage::NetworkRequest(
            NetworkRequest::BanNode(self.get_self_peer(), id),
            None,
            None,
        )));
        self.queue_size_inc()?;
        Ok(())
    }

    pub fn send_unban(&mut self, id: P2PPeer) -> Fallible<()> {
        safe_write!(self.send_queue)?.push_back(Arc::new(NetworkMessage::NetworkRequest(
            NetworkRequest::UnbanNode(self.get_self_peer(), id),
            None,
            None,
        )));
        self.queue_size_inc()?;
        Ok(())
    }

    pub fn send_joinnetwork(&mut self, network_id: NetworkId) -> Fallible<()> {
        safe_write!(self.send_queue)?.push_back(Arc::new(NetworkMessage::NetworkRequest(
            NetworkRequest::JoinNetwork(self.get_self_peer(), network_id),
            None,
            None,
        )));
        self.queue_size_inc()?;
        Ok(())
    }

    pub fn send_leavenetwork(&mut self, network_id: NetworkId) -> Fallible<()> {
        safe_write!(self.send_queue)?.push_back(Arc::new(NetworkMessage::NetworkRequest(
            NetworkRequest::LeaveNetwork(self.get_self_peer(), network_id),
            None,
            None,
        )));
        self.queue_size_inc()?;
        Ok(())
    }

    pub fn send_get_peers(&mut self, nids: HashSet<NetworkId>) -> Fallible<()> {
        safe_write!(self.send_queue)?.push_back(Arc::new(NetworkMessage::NetworkRequest(
            NetworkRequest::GetPeers(self.get_self_peer(), nids.clone()),
            None,
            None,
        )));
        self.queue_size_inc()?;
        Ok(())
    }

    pub fn peek_queue(&self) -> Vec<String> {
        if let Ok(lock) = safe_read!(self.send_queue) {
            return lock
                .iter()
                .map(|x| format!("{:?}", x))
                .collect::<Vec<String>>();
        };
        vec![]
    }

    pub fn get_peer_stats(&self, nids: &[NetworkId]) -> Fallible<Vec<PeerStatistic>> {
        Ok(safe_read!(self.tls_server)?.get_peer_stats(nids))
    }

    #[cfg(not(windows))]
    pub fn get_ip() -> Option<IpAddr> {
        let localhost = IpAddr::from_str("127.0.0.1").unwrap();
        let mut ip: IpAddr = localhost;

        for adapter in get_if_addrs::get_if_addrs().unwrap() {
            match adapter.addr.ip() {
                V4(x) => {
                    if !x.is_loopback()
                        && !x.is_link_local()
                        && !x.is_multicast()
                        && !x.is_broadcast()
                    {
                        ip = IpAddr::V4(x);
                    }
                }
                V6(_) => {
                    // Ignore for now
                }
            };
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

        for adapter in ipconfig::get_adapters().unwrap() {
            for ip_new in adapter.ip_addresses() {
                match ip_new {
                    V4(x) => {
                        if !x.is_loopback()
                            && !x.is_link_local()
                            && !x.is_multicast()
                            && !x.is_broadcast()
                        {
                            ip = IpAddr::V4(*x);
                        }
                    }
                    V6(_) => {
                        // Ignore for now
                    }
                };
            }
        }

        if ip == localhost {
            None
        } else {
            Some(ip)
        }
    }

    fn get_self_peer(&self) -> P2PPeer {
        P2PPeer::from(
            PeerType::Node,
            self.id(),
            self.get_listening_ip(),
            self.get_listening_port(),
        )
    }

    pub fn ban_node(&mut self, peer: P2PPeer) -> Fallible<()> {
        safe_write!(self.tls_server)?.ban_node(peer);
        Ok(())
    }

    pub fn unban_node(&mut self, peer: P2PPeer) -> Fallible<()> {
        safe_write!(self.tls_server)?.unban_node(&peer);
        Ok(())
    }

    pub fn process(&mut self, events: &mut Events) -> Fallible<()> {
        safe_read!(self.poll)?.poll(events, Some(Duration::from_millis(1000)))?;

        if self.peer_type != PeerType::Bootstrapper {
            safe_read!(self.tls_server)?.liveness_check()?;
        }

        for event in events.iter() {
            let mut tls_ref = safe_write!(self.tls_server)?;
            let mut poll_ref = safe_write!(self.poll)?;
            match event.token() {
                SERVER => {
                    debug!("Got new connection!");
                    tls_ref
                        .accept(&mut poll_ref, self.get_self_peer())
                        .map_err(|e| error!("{}", e))
                        .ok();
                    if let Some(ref prom) = &self.prometheus_exporter {
                        safe_write!(prom)?
                            .conn_received_inc()
                            .map_err(|e| error!("{}", e))
                            .ok();
                    };
                }
                _ => {
                    trace!("Got data!");
                    tls_ref
                        .conn_event(&mut poll_ref, &event, &self.incoming_pkts)
                        .map_err(|e| error!("Error occurred while parsing event: {}", e))
                        .ok();
                }
            }
        }

        events.clear();

        {
            let tls_ref = safe_read!(self.tls_server)?;
            let mut poll_ref = safe_write!(self.poll)?;
            tls_ref.cleanup_connections(&mut poll_ref)?;
        }

        self.process_messages()?;
        Ok(())
    }

    pub fn close(&mut self) -> Fallible<()> {
        if let Some(ref q) = self.quit_tx {
            q.send(true)?;
        }

        Ok(())
    }

    pub fn close_and_join(&mut self) -> Fallible<()> {
        self.close()?;
        self.join()
    }
}

impl Drop for P2PNode {
    fn drop(&mut self) { let _ = self.close_and_join(); }
}

impl MessageManager for P2PNode {
    fn message_handler(&self) -> Arc<RwLock<MessageHandler>> {
        safe_read!(self.tls_server)
            .expect("Couldn't lock the tls server")
            .message_handler()
    }
}

fn is_conn_peer_id(conn: &Connection, id: &P2PNodeId) -> bool {
    if let Some(remote_peer) = conn.remote_peer() {
        remote_peer.id() == *id
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
    if let Some(remote_peer) = conn.remote_peer() {
        if remote_peer.id() != sender.id() && remote_peer.peer_type() != PeerType::Bootstrapper {
            let local_end_networks = conn.local_end_networks();
            return safe_read!(local_end_networks)
                .expect("Couldn't lock local-end networks")
                .contains(&network_id);
        }
    }
    false
}
