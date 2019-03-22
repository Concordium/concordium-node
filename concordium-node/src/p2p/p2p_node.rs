use failure::{Error, Fallible};
use std::sync::{Arc, Mutex, RwLock};
use std::sync::mpsc::Sender;
use std::collections::{ VecDeque };
use std::net::{ IpAddr };
use super::fails;
#[cfg(not(target_os = "windows"))]
use get_if_addrs;
#[cfg(target_os = "windows")]
use ipconfig;
use crate::prometheus_exporter::PrometheusServer;
use rustls::{ Certificate, ClientConfig, NoClientAuth, ServerConfig };
use atomic_counter::{ AtomicCounter };
use std::net::IpAddr::{V4, V6};
use std::str::FromStr;
use std::time::{ Duration };
use mio::net::{ TcpListener };
use mio::{ Poll, PollOpt, Token, Ready, Events };
use time::{ Timespec };
use crate::utils;
use std::thread;

use crate::common::{ P2PNodeId, P2PPeer, ConnectionType };
use crate::common::counter::{ TOTAL_MESSAGES_SENT_COUNTER };
use crate::network::{ NetworkMessage, NetworkPacket, NetworkRequest, NetworkResponse, Buckets };
use crate::connection::{ Connection, P2PEvent, P2PNodeMode, SeenMessagesList, MessageManager,
    MessageHandler, RequestHandler, ResponseHandler, NetworkPacketCW, NetworkRequestCW, NetworkResponseCW};

use crate::p2p::tls_server::{ TlsServer };
use crate::p2p::no_certificate_verification::{ NoCertificateVerification };
use crate::p2p::peer_statistics::{ PeerStatistic };
use crate::p2p::p2p_node_handlers::{ forward_network_request, forward_network_packet_message, forward_network_response };

const SERVER: Token = Token(0);

#[derive(Clone)]
pub struct P2PNode {
    tls_server: Arc<Mutex<TlsServer>>,
    poll: Arc<Mutex<Poll>>,
    id: P2PNodeId,
    buckets: Arc<RwLock<Buckets>>,
    send_queue: Arc<Mutex<VecDeque<Arc<Box<NetworkMessage>>>>>,
    ip: IpAddr,
    port: u16,
    incoming_pkts: Sender<Arc<Box<NetworkMessage>>>,
    event_log: Option<Sender<P2PEvent>>,
    start_time: Timespec,
    prometheus_exporter: Option<Arc<Mutex<PrometheusServer>>>,
    mode: P2PNodeMode,
    external_ip: IpAddr,
    external_port: u16,
    seen_messages: SeenMessagesList,
    minimum_per_bucket: usize,
    blind_trusted_broadcast: bool,
}

unsafe impl Send for P2PNode {}


impl P2PNode {
    pub fn new(supplied_id: Option<String>,
               listen_address: Option<String>,
               listen_port: u16,
               external_ip: Option<String>,
               external_port: Option<u16>,
               pkt_queue: Sender<Arc<Box<NetworkMessage>>>,
               event_log: Option<Sender<P2PEvent>>,
               mode: P2PNodeMode,
               prometheus_exporter: Option<Arc<Mutex<PrometheusServer>>>,
               networks: Vec<u16>,
               minimum_per_bucket: usize,
               blind_trusted_broadcast: bool,)
               -> Self {
        let addr = if let Some(ref addy) = listen_address {
            format!("{}:{}", addy, listen_port).parse().unwrap_or_else(|_| {
                warn!("Supplied listen address coulnd't be parsed");
                format!("0.0.0.0:{}", listen_port).parse().unwrap()
            })
        } else {
            format!("0.0.0.0:{}", listen_port).parse().unwrap()
        };

        trace!("Creating new P2PNode");

        //Retrieve IP address octets, format to IP and SHA256 hash it
        let ip = if let Some(ref addy) = listen_address {
            IpAddr::from_str(addy).unwrap_or_else(|_|
                P2PNode::get_ip().expect("Couldn't retrieve my own ip")
            )
        } else {
            P2PNode::get_ip().expect("Couldn't retrieve my own ip")
        };
        let ip_port = format!("{}:{}", ip.to_string(), listen_port);
        debug!("Listening on {:?}", ip_port);

        let id = match supplied_id {
            Some(x) => {
                if x.chars().count() != 64 {
                    panic!("Incorrect ID specified. Should be a sha256 value or 64 characters long!");
                }
                x
            }
            _ => {
                let instant = time::get_time();
                utils::to_hex_string(&utils::sha256(&format!("{}.{}", instant.sec, instant.nsec)))
            }
        };

        let _id = P2PNodeId::from_string(&id).expect("Couldn't parse the id");

        let poll = Poll::new().unwrap_or_else(|_|
            panic!("Couldn't create poll")
        );

        let server = TcpListener::bind(&addr).unwrap_or_else(|_|
            panic!("Couldn't listen on port!")
        );

        if poll.register(&server, SERVER, Ready::readable(), PollOpt::edge()).is_err() {
            panic!("Couldn't register server with poll!")
        };

        //Generate key pair and cert
        let (cert, private_key) = match utils::generate_certificate(id) {
            Ok(x) => {
                match x.x509.to_der() {
                    Ok(der) => {
                        // When setting the server single certificate on rustls, it requires a rustls::PrivateKey.
                        // such PrivateKey is defined as `pub struct PrivateKey(pub Vec<u8>);` and it expects the key
                        // to come in DER format.
                        //
                        // As we have an `openssl::pkey::PKey`, inside the `utils::Cert` struct, we could just
                        // use the function `private_key_to_der()` over such key, BUT the output of that function
                        // is reported as invalid key when fed into `set_single_cert` IF the original key was an EcKey
                        // (if it was an RSA key, the DER method works fine).
                        //
                        // There must be a bug somewhere in between the DER encoding of openssl or the
                        // DER decoding of rustls when dealing with EcKeys.
                        //
                        // Luckily for us, rustls offers a way to import a key from a pkcs8-PEM-encoded buffer and
                        // openssl offers a function for exporting a key into pkcs8-PEM-encoded buffer so connecting
                        // those two functions, we get a valid `rustls::PrivateKey`.
                        match rustls::internal::pemfile::pkcs8_private_keys(& mut std::io::BufReader::new(x.private_key
                                                                                                          .private_key_to_pem_pkcs8().expect("Something went wrong when exporting a key through openssl").as_slice())) {
                            Ok(der_keys) => {
                                (Certificate(der), der_keys[0].clone())
                            }
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
        server_conf.set_single_cert(vec![cert], private_key)
                   .map_err(|e| error!("{}", e))
                   .ok();

        let mut client_conf = ClientConfig::new();
        client_conf.dangerous()
                   .set_certificate_verifier(Arc::new(NoCertificateVerification {}));

        let own_peer_ip = if let Some(ref own_ip) = external_ip {
            match IpAddr::from_str(own_ip) {
                Ok(ip) => ip,
                _ => ip,
            }
        } else {
            ip
        };

        let own_peer_port = if let Some(own_port) = external_port {
            own_port
        } else {
            listen_port
        };

        let self_peer = P2PPeer::from(ConnectionType::Node,
                                      _id.clone(),
                                      own_peer_ip,
                                      own_peer_port);

        let seen_messages = SeenMessagesList::new();

        let buckets = Arc::new(RwLock::new(Buckets::new()));

        let tlsserv = TlsServer::new(server,
                                     Arc::new(server_conf),
                                     Arc::new(client_conf),
                                     _id.clone(),
                                     event_log.clone(),
                                     self_peer,
                                     mode,
                                     prometheus_exporter.clone(),
                                     networks,
                                     buckets.clone(),
                                     blind_trusted_broadcast,);

        let mut mself = P2PNode {
                  tls_server: Arc::new(Mutex::new(tlsserv)),
                  poll: Arc::new(Mutex::new(poll)),
                  id: _id,
                  buckets: buckets,
                  send_queue: Arc::new(Mutex::new(VecDeque::new())),
                  ip: ip,
                  port: listen_port,
                  incoming_pkts: pkt_queue,
                  event_log,
                  start_time: time::get_time(),
                  prometheus_exporter: prometheus_exporter,
                  external_ip: own_peer_ip,
                  external_port: own_peer_port,
                  mode: mode,
                  seen_messages: seen_messages,
                  minimum_per_bucket: minimum_per_bucket,
                  blind_trusted_broadcast,
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
        let mut locked_mh = shared_mh.write().expect("Coulnd't set the default message handlers");
        locked_mh.add_packet_callback( packet_handler)
                .add_response_callback( make_atomic_callback!(
                    move |res: &NetworkResponse| { (response_handler)(res).map_err(Error::from) }))
                .add_request_callback( make_atomic_callback!(
                    move |req: &NetworkRequest| { (request_handler)(req).map_err(Error::from) }));
    }

    /// Default packet handler just forward valid messages.
    fn make_default_network_packet_message_handler(&self) -> NetworkPacketCW {
        let seen_messages = self.seen_messages.clone();
        let own_networks = self.tls_server.lock().expect("Couldn't lock the tls server").networks().clone();
        let prometheus_exporter = self.prometheus_exporter.clone();
        let packet_queue = self.incoming_pkts.clone();
        let send_queue = self.send_queue.clone();
        let trusted_broadcast = self.blind_trusted_broadcast.clone();

        make_atomic_callback!( move|pac: &NetworkPacket| {
            forward_network_packet_message( &seen_messages, &prometheus_exporter,
                                                   &own_networks, &send_queue, &packet_queue, pac, trusted_broadcast)
        })
    }

    fn make_response_output_handler(&self) -> NetworkResponseCW {
        let packet_queue = self.incoming_pkts.clone();
        make_atomic_callback!( move |req: &NetworkResponse| {
            forward_network_response( &req, &packet_queue)
        })
    }

    fn make_response_handler(&self) -> ResponseHandler {
        let output_handler = self.make_response_output_handler();
        let mut handler = ResponseHandler::new();
        handler.add_peer_list_callback(output_handler.clone() );
        handler
    }

    fn make_requeue_handler(&self) -> NetworkRequestCW {
        let packet_queue = self.incoming_pkts.clone();

        make_atomic_callback!( move |req: &NetworkRequest| {
            forward_network_request( req, &packet_queue)
        })
    }

    fn make_request_handler(&self) -> RequestHandler {
        let requeue_handler = self.make_requeue_handler();
        let mut handler = RequestHandler::new();

        handler
            .add_ban_node_callback( requeue_handler.clone())
            .add_unban_node_callback( requeue_handler.clone())
            .add_handshake_callback( requeue_handler.clone());

        handler
    }

    pub fn spawn(&mut self) -> thread::JoinHandle<()> {
        let mut self_clone = self.clone();
        thread::spawn(move || {
                          let mut events = Events::with_capacity(1024);
                          loop {
                              self_clone.process(&mut events)
                                        .map_err(|e| error!("{}", e))
                                        .ok();
                          }
                      })
    }

    pub fn get_version(&self) -> String {
        crate::VERSION.to_string()
    }

    pub fn connect(&mut self,
                   connection_type: ConnectionType,
                   ip: IpAddr,
                   port: u16,
                   peer_id: Option<P2PNodeId>)
                   -> Fallible<()> {
        self.log_event(P2PEvent::InitiatingConnection(ip.clone(), port));
        let mut locked_server = safe_lock!(self.tls_server)?;
        let mut locked_poll = safe_lock!(self.poll)?;
        locked_server.connect(connection_type,
                              &mut locked_poll,
                              ip,
                              port,
                              peer_id,
                              &self.get_self_peer())
    }

    pub fn get_own_id(&self) -> P2PNodeId {
        self.id.clone()
    }

    pub fn get_listening_ip(&self) -> IpAddr {
        self.ip.clone()
    }

    pub fn get_listening_port(&self) -> u16 {
        self.port
    }

    pub fn get_node_mode(&self) -> P2PNodeMode {
        self.mode
    }

    fn log_event(&mut self, event: P2PEvent) {
        if let Some(ref mut x) = self.event_log {
            if let Err(e) = x.send(event) {
                error!("Couldn't send event {:?}", e)
            }
        }
    }

    pub fn get_uptime(&self) -> i64 {
        (time::get_time() - self.start_time).num_milliseconds()
    }

    fn check_sent_status(&self, conn: &Connection, status: Fallible<usize>) {
        if let Some(ref peer) = conn.peer().clone() {
            match status {
                Ok(_) => {
                    self.pks_sent_inc().unwrap(); // assuming non-failable
                    TOTAL_MESSAGES_SENT_COUNTER.inc();
                },
                Err(e) => {
                    error!("Could not send to peer {} due to {}",
                           peer.id().to_string(), e);
                }
            }
        }
    }

    pub fn process_messages(&mut self) -> Fallible<()>  {
            let mut send_q = safe_lock!(self.send_queue)?;
            if send_q.len() == 0 {
                return Ok(());
            }
            let mut resend_queue: VecDeque<Arc<Box<NetworkMessage>>> = VecDeque::new();
            loop {
                trace!("Processing messages!");
                let outer_pkt = send_q.pop_front();
                match outer_pkt.clone() {
                    Some(ref x) => {
                        if let Some(ref prom) = &self.prometheus_exporter {
                            let ref mut lock = safe_lock!(prom)?;
                            lock.queue_size_dec().map_err(|e| error!("{}", e)).ok();
                        };
                        trace!("Got message to process!");
                        let check_sent_status_fn =
                            |conn: &Connection, status: Fallible<usize>|
                                self.check_sent_status( conn, status);

                        match *x.clone() {
                            box NetworkMessage::NetworkPacket(ref inner_pkt
                                    @ NetworkPacket::DirectMessage(..), _, _) => {
                                if let NetworkPacket::DirectMessage(_, _msgid, receiver, _network_id,  _) = inner_pkt {
                                    let data = inner_pkt.serialize();
                                    let filter = |conn: &Connection|{ is_conn_peer_id( conn, receiver)};

                                    safe_lock!(self.tls_server)?
                                        .send_over_all_connections( &data, &filter,
                                                                    &check_sent_status_fn);
                                }
                            }
                            box NetworkMessage::NetworkPacket(ref inner_pkt @ NetworkPacket::BroadcastedMessage(_, _, _, _), _, _) => {
                                if let NetworkPacket::BroadcastedMessage(ref sender, ref _msgid, ref network_id, _ ) = inner_pkt {
                                    let data = inner_pkt.serialize();
                                    let filter = |conn: &Connection| {
                                        is_valid_connection_in_broadcast( conn, sender, network_id)
                                    };

                                    safe_lock!(self.tls_server)?
                                        .send_over_all_connections( &data, &filter,
                                                                    &check_sent_status_fn);
                                }
                            }
                            box NetworkMessage::NetworkRequest(ref inner_pkt @ NetworkRequest::UnbanNode(_, _), _, _)
                            | box NetworkMessage::NetworkRequest(ref inner_pkt @ NetworkRequest::GetPeers(_,_), _, _)
                            | box NetworkMessage::NetworkRequest(ref inner_pkt @ NetworkRequest::BanNode(_, _), _, _) => {
                                let data = inner_pkt.serialize();
                                let no_filter = |_: &Connection| true;

                                safe_lock!(self.tls_server)?
                                    .send_over_all_connections( &data, &no_filter,
                                                                &check_sent_status_fn);
                            }
                            box NetworkMessage::NetworkRequest(ref inner_pkt @ NetworkRequest::JoinNetwork(_, _), _, _) => {
                                let data = inner_pkt.serialize();
                                let no_filter = |_: &Connection| true;

                                let mut locked_tls_server = safe_lock!(self.tls_server)?;
                                locked_tls_server
                                    .send_over_all_connections( &data, &no_filter,
                                                                &check_sent_status_fn);

                                if let NetworkRequest::JoinNetwork(_, network_id) = inner_pkt {
                                    locked_tls_server.add_network(*network_id)
                                        .map_err(|e| error!("{}", e)).ok();
                                }
                            }
                            box NetworkMessage::NetworkRequest(ref inner_pkt @ NetworkRequest::LeaveNetwork(_,_), _, _) => {
                                let data = inner_pkt.serialize();
                                let no_filter = |_: &Connection| true;

                                let mut locked_tls_server = safe_lock!(self.tls_server)?;
                                locked_tls_server
                                    .send_over_all_connections( &data, &no_filter,
                                                                &check_sent_status_fn);

                                if let NetworkRequest::LeaveNetwork(_, network_id) = inner_pkt {
                                   locked_tls_server.remove_network(*network_id)
                                            .map_err(|e| error!("{}", e)).ok();
                                }
                            }
                            _ => {}
                        }
                    }
                    _ => {
                        if resend_queue.len() > 0 {
                            if let Some(ref prom) = &self.prometheus_exporter {
                                match prom.lock() {
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
            match prom.lock() {
                Ok(ref mut lock) => {
                    lock.queue_size_inc().map_err(|e| error!("{}", e)).ok();
                }
                _ => error!("Couldn't lock prometheus instance"),
            }
        };
        Ok(())
    }

    fn pks_sent_inc(&self) -> Fallible<()> {
        if let Some(ref prom) = &self.prometheus_exporter {
            match prom.lock() {
                Ok(ref mut lock) => {
                    lock.pkt_sent_inc().map_err(|e| error!("{}", e)).ok();
                }
                _ => error!("Couldn't lock prometheus instance"),
            }
        };
        Ok(())
    }

    pub fn send_message(&mut self,
                        id: Option<P2PNodeId>,
                        network_id: u16,
                        msg_id: Option<String>,
                        msg: &[u8],
                        broadcast: bool)
                        -> Fallible<()> {
        debug!("Queueing message!");
        if broadcast {
            safe_lock!(self.send_queue)?
                .push_back( Arc::new(
                    box NetworkMessage::NetworkPacket(
                        NetworkPacket::BroadcastedMessage(
                            self.get_self_peer(),
                            msg_id.unwrap_or(NetworkPacket::generate_message_id()),
                            network_id,
                            msg.to_vec()),
                        None,
                        None)));
            self.queue_size_inc()?;
            Ok(())
        } else {
            id.map_or_else(
                || Err(fails::EmptyIdInSendRequest{})?,
                |x| {
                    safe_lock!(self.send_queue)?
                        .push_back( Arc::new(
                            box NetworkMessage::NetworkPacket(
                                NetworkPacket::DirectMessage(
                                    self.get_self_peer(),
                                    msg_id.unwrap_or(NetworkPacket::generate_message_id()),
                                    x,
                                    network_id,
                                    msg.to_vec()),
                                None,
                                None)));
                    self.queue_size_inc()?;
                    Ok(())
                }
            )
        }
    }

    pub fn send_ban(&mut self, id: P2PPeer) -> Fallible<()> {
        safe_lock!(self.send_queue)?
            .push_back(Arc::new(box NetworkMessage::NetworkRequest(NetworkRequest::BanNode(self.get_self_peer(), id),
                                                               None,
                                                               None)));
        self.queue_size_inc()?;
        Ok(())
    }

    pub fn send_unban(&mut self, id: P2PPeer) -> Fallible<()> {
        safe_lock!(self.send_queue)?
            .push_back(Arc::new(box NetworkMessage::NetworkRequest(NetworkRequest::UnbanNode(self.get_self_peer(), id),
                                                               None,
                                                               None)));
        self.queue_size_inc()?;
        Ok(())
    }

    pub fn send_joinnetwork(&mut self, network_id: u16) -> Fallible<()> {
        safe_lock!(self.send_queue)?
            .push_back(Arc::new(box NetworkMessage::NetworkRequest(NetworkRequest::JoinNetwork(self.get_self_peer(), network_id),
                                                               None,
                                                               None)));
        self.queue_size_inc()?;
        Ok(())
    }

    pub fn send_leavenetwork(&mut self, network_id: u16) -> Fallible<()> {
        safe_lock!(self.send_queue)?
            .push_back(Arc::new(box NetworkMessage::NetworkRequest(NetworkRequest::LeaveNetwork(self.get_self_peer(), network_id),
                                                               None,
                                                               None)));
        self.queue_size_inc()?;
        Ok(())
    }

    pub fn send_get_peers(&mut self, nids: Vec<u16>) -> Fallible<()> {
        safe_lock!(self.send_queue)?
            .push_back(Arc::new(box NetworkMessage::NetworkRequest(NetworkRequest::GetPeers(self.get_self_peer(),nids.clone() ),
                                                               None,
                                                               None)));
        self.queue_size_inc()?;
        Ok(())
    }

    pub fn peek_queue(&self) -> Vec<String> {
        if let Ok(lock) = self.send_queue.lock() {
            return lock.iter()
                       .map(|x| format!("{:?}", x))
                       .collect::<Vec<String>>();
        };
        vec![]
    }

    pub fn get_peer_stats(&self, nids: &[u16]) -> Fallible<Vec<PeerStatistic>> {
        Ok(safe_lock!(self.tls_server)?
            .get_peer_stats(nids))
    }

    #[cfg(not(windows))]
    pub fn get_ip() -> Option<IpAddr> {
        let localhost = IpAddr::from_str("127.0.0.1").unwrap();
        let mut ip: IpAddr = localhost.clone();

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
                    //Ignore for now
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
        let mut ip: IpAddr = localhost.clone();

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
                        //Ignore for now
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
        P2PPeer::from(ConnectionType::Node,
                      self.get_own_id().clone(),
                      self.get_listening_ip().clone(),
                      self.get_listening_port())
    }

    pub fn ban_node(&mut self, peer: P2PPeer) -> Fallible<()> {
        safe_lock!(self.tls_server)?
            .ban_node(peer);
        Ok(())
    }

    pub fn unban_node(&mut self, peer: P2PPeer) -> Fallible<()> {
        safe_lock!(self.tls_server)?
            .unban_node(&peer);
        Ok(())
    }

    pub fn process(&mut self, events: &mut Events) -> Fallible<()> {
        safe_lock!(self.poll)?
            .poll(events, Some(Duration::from_millis(1000)))?;

        if self.mode != P2PNodeMode::BootstrapperMode
           && self.mode != P2PNodeMode::BootstrapperPrivateMode
        {
            safe_lock!(self.tls_server)?
                .liveness_check()?;
        }

        for event in events.iter() {
            let mut tls_ref = safe_lock!(self.tls_server)?;
            let mut poll_ref = safe_lock!(self.poll)?;
            match event.token() {
                SERVER => {
                    debug!("Got new connection!");
                    tls_ref.accept(&mut poll_ref, self.get_self_peer().clone())
                           .map_err(|e| error!("{}", e))
                           .ok();
                    if let Some(ref prom) = &self.prometheus_exporter {
                        safe_lock!(prom)?
                            .conn_received_inc()
                            .map_err(|e| error!("{}", e))
                            .ok();
                    };
                },
                _ => {
                    trace!("Got data!");
                    tls_ref.conn_event(&mut poll_ref,
                                       &event,
                                       &self.incoming_pkts)
                           .map_err(|e| { error!( "Error occurred while parsing event: {}", e)})
                           .ok();
                }
            }
        }

        events.clear();

        {
            let tls_ref = safe_lock!(self.tls_server)?;
            let mut poll_ref = safe_lock!(self.poll)?;
            tls_ref.cleanup_connections(&mut poll_ref)?;
            if self.mode == P2PNodeMode::BootstrapperMode
               || self.mode == P2PNodeMode::BootstrapperPrivateMode
            {
                let mut buckets_ref = safe_write!(self.buckets)?;
                buckets_ref.clean_peers(self.minimum_per_bucket);
            }
        }

        self.process_messages()?;
        Ok(())
    }
}

impl MessageManager for P2PNode {
    fn message_handler(&self) -> Arc< RwLock< MessageHandler>> {
        self.tls_server.lock().expect("Couldn't lock the tls server")
            .message_handler().clone()
    }
}

fn is_conn_peer_id( conn: &Connection, id: &P2PNodeId) -> bool {
    if let Some(ref peer) = conn.peer() {
        peer.id() == *id
    } else {
        false
    }
}

/// Connetion is valid for a broadcast if sender is not target and
/// and network_id is owned by connection.
pub fn is_valid_connection_in_broadcast(
    conn: &Connection,
    sender: &P2PPeer,
    network_id: &u16) -> bool {

    if let Some(ref peer) = conn.peer() {
        if peer.id() != sender.id() {
            let own_networks = conn.own_networks();
            return own_networks.lock().expect("Couldn't lock own networks").contains(network_id);
        }
    }
    false
}
