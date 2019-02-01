use std::sync::{Arc, Mutex, RwLock};
use std::sync::mpsc::Sender;
use std::collections::{ VecDeque };
use std::net::{ IpAddr };
use errors::*;
#[cfg(not(target_os = "windows"))]
use get_if_addrs;
#[cfg(target_os = "windows")]
use ipconfig;
use prometheus_exporter::PrometheusServer;
use rustls::{ Certificate, ClientConfig, NoClientAuth, PrivateKey, ServerConfig };

use atomic_counter::{ AtomicCounter };
use std::io::{ Error, ErrorKind };
use std::net::IpAddr::{V4, V6};
use std::str::FromStr;
use std::time::{ Duration };
use mio::net::{ TcpListener };
use mio::{ Poll, PollOpt, Token, Ready, Events };
use time::{ Timespec };
use utils;
use std::thread;

use common::{ P2PNodeId, P2PPeer, ConnectionType };
use common::counter::{ TOTAL_MESSAGES_SENT_COUNTER };
use network::{ NetworkMessage, NetworkPacket, NetworkRequest, NetworkResponse, Buckets };
use connection::{ P2PEvent, P2PNodeMode, Connection, SeenMessagesList, MessageManager,
    MessageHandler, RequestHandler, ResponseHandler, PacketHandler,
    NetworkPacketCW, NetworkRequestCW
    };

use p2p::tls_server::{ TlsServer };
use p2p::no_certificate_verification::{ NoCertificateVerification };
use p2p::peer_statistics::{ PeerStatistic };
use p2p::p2p_node_handlers::{ forward_network_request, forward_network_packet_message };

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

    message_handler: MessageHandler
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
               minimum_per_bucket: usize)
               -> Self {
        let addr = if let Some(ref addy) = listen_address {
            format!("{}:{}", addy, listen_port).parse().unwrap()
        } else {
            format!("0.0.0.0:{}", listen_port).parse().unwrap()
        };

        trace!("Creating new P2PNode");

        //Retrieve IP address octets, format to IP and SHA256 hash it
        let ip = if let Some(ref addy) = listen_address {
            match IpAddr::from_str(addy) {
                Ok(x) => x,
                _ => P2PNode::get_ip().unwrap(),
            }
        } else {
            P2PNode::get_ip().unwrap()
        };
        let ip_port = format!("{}:{}", ip.to_string(), listen_port);
        debug!("Listening on {:?}", ip_port);

        let id = match supplied_id {
            Some(x) => {
                if x.chars().count() != 64 {
                    panic!("Incorrect ID specified.. Should be a sha256 value or 64 characters long!");
                }
                x
            }
            _ => {
                let instant = time::get_time();
                utils::to_hex_string(&utils::sha256(&format!("{}.{}", instant.sec, instant.nsec)))
            }
        };

        let _id = P2PNodeId::from_string(&id).unwrap();

        let poll = match Poll::new() {
            Ok(x) => x,
            _ => panic!("Couldn't create poll"),
        };

        let server = match TcpListener::bind(&addr) {
            Ok(x) => x,
            _ => panic!("Couldn't listen on port!"),
        };

        match poll.register(&server, SERVER, Ready::readable(), PollOpt::edge()) {
            Ok(_x) => (),
            _ => panic!("Couldn't register server with poll!"),
        };

        //Generate key pair and cert
        let (cert, private_key) = match utils::generate_certificate(id) {
            Ok(x) => {
                match x.x509.to_der() {
                    Ok(der) => {
                        match x.private_key.private_key_to_der() {
                            Ok(private_part) => (Certificate(der), PrivateKey(private_part)),
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
                                     buckets.clone());

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
                  message_handler: MessageHandler::new()
        };
        mself.add_default_message_handlers();
        mself
    }

    /// It adds default message handler at .
    fn add_default_message_handlers(&mut self) {
        let packet_handler = self.make_packet_handler();
        let response_handler = self.make_response_handler();
        let request_handler = self.make_request_handler();

        self.message_handler
            .add_packet_callback( make_atomic_callback!(
                    move |pac: &NetworkPacket| { (packet_handler)(pac) }))
            .add_response_callback( make_atomic_callback!(
                    move |res: &NetworkResponse| { (response_handler)(res) }))
            .add_request_callback( make_atomic_callback!(
                    move |req: &NetworkRequest| { (request_handler)(req) }));

        self.register_message_handlers();
    }

    /// Default packet handler just forward valid messages.
    fn make_default_network_packet_message_handler(&self) -> NetworkPacketCW {
        let seen_messages = self.seen_messages.clone();
        let own_networks = self.tls_server.lock().unwrap().networks.clone();
        let prometheus_exporter = self.prometheus_exporter.clone();
        let packet_queue = self.incoming_pkts.clone();

        make_atomic_callback!( move|pac: &NetworkPacket| {
            forward_network_packet_message( &seen_messages, &prometheus_exporter,
                                                   &own_networks, &packet_queue, pac)
        })
    }

    fn make_packet_handler(&self) -> PacketHandler {
        let mut handler = PacketHandler::new();
        handler.add_callback( self.make_default_network_packet_message_handler());
        handler
    }

    fn make_response_handler(&self) -> ResponseHandler {
        ResponseHandler::new()
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
        ::VERSION.to_string()
    }

    pub fn connect(&mut self,
                   connection_type: ConnectionType,
                   ip: IpAddr,
                   port: u16,
                   peer_id: Option<P2PNodeId>)
                   -> ResultExtWrapper<()> {
        self.log_event(P2PEvent::InitiatingConnection(ip.clone(), port));
        match self.tls_server.lock() {
            Ok(mut x) => {
                match self.poll.lock() {
                    Ok(mut y) => {
                        x.connect(connection_type,
                                  &mut y,
                                  ip,
                                  port,
                                  peer_id,
                                  &self.get_self_peer())
                    }
                    Err(e) => Err(ErrorWrapper::from(e).into()),
                }
            }
            Err(e) => Err(ErrorWrapper::from(e).into()),
        }
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

    pub fn get_nodes(&self, nids: &Vec<u16>) -> Result<Vec<PeerStatistic>, Error> {
        match self.tls_server.lock() {
            Ok(x) => Ok(x.get_peer_stats(nids)),
            Err(_e) => Err(Error::new(ErrorKind::Other, "Couldn't get lock on buckets!")),
        }
    }

    fn log_event(&mut self, event: P2PEvent) {
        match self.event_log {
            Some(ref mut x) => {
                match x.send(event) {
                    Ok(_) => {}
                    Err(e) => error!("Couldn't send event {:?}", e),
                };
            }
            _ => {}
        }
    }

    pub fn get_uptime(&self) -> i64 {
        (time::get_time() - self.start_time).num_milliseconds()
    }

    /// Connetion is valid for a broadcast if sender is not target and
    /// and network_id is owned by connection.
    fn is_valid_connection_in_broadcast(&self, conn: &Connection, sender: &P2PPeer, network_id: &u16) -> bool {
        if let Some(ref peer) = conn.peer() {
            if peer.id() != sender.id() {
                let own_networks = conn.own_networks();
                return own_networks.lock().unwrap().contains(network_id);
            }
        }
        false
    }

    fn process_broadcasted_message(&self, sender: &P2PPeer, msgid: &String,  network_id: &u16, data: &Vec<u8> ) -> ResultExtWrapper<()> {
        self.tls_server.lock()?
            .connections.values_mut()
            .filter( |conn| {
                self.is_valid_connection_in_broadcast( conn, sender, network_id)
            })
        .fold( Ok(()), |status, ref mut conn| {
            conn.serialize_bytes( data)
                .and_then( |_| {
                    self.seen_messages.append(msgid);
                    TOTAL_MESSAGES_SENT_COUNTER.inc();
                    self.pks_sent_inc()
                })
            .and( status)
        })
    }

    pub fn process_messages(&mut self) -> ResultExtWrapper<()> {
        {
            let mut send_q = self.send_queue.lock()?;
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
                            match prom.lock() {
                                Ok(ref mut lock) => {
                                    lock.queue_size_dec().map_err(|e| error!("{}", e)).ok();
                                }
                                _ => error!("Couldn't lock prometheus instance"),
                            }
                        };
                        trace!("Got message to process!");
                        match *x.clone() {
                            box NetworkMessage::NetworkPacket(ref inner_pkt @ NetworkPacket::DirectMessage(_,
                                                                                          _,
                                                                                           _,
                                                                                           _,
                                                                                           _),
                                                              _,
                                                              _) => {
                                if let NetworkPacket::DirectMessage(_, msgid, receiver, network_id,  _) = inner_pkt {
                                    match self.tls_server.lock()?.find_connection(receiver.clone()) {
                                        Some(ref mut conn) => {
                                            let own_networks = conn.own_networks();
                                            if own_networks.lock().unwrap().contains(network_id) {
                                                if let Some(ref peer) = conn.peer().clone() {
                                                    match conn.serialize_bytes( &inner_pkt.serialize()) {
                                                        Ok(_) => {
                                                            self.seen_messages.append(&msgid);
                                                            TOTAL_MESSAGES_SENT_COUNTER.inc();
                                                            self.pks_sent_inc()?;
                                                            debug!("Sent message");
                                                        }
                                                        Err(e) => {
                                                            error!("Could not send to peer {} due to {}",
                                                                peer.id().to_string(),
                                                                e);
                                                            resend_queue.push_back(outer_pkt.unwrap().clone());
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                        _ => {
                                            resend_queue.push_back(outer_pkt.unwrap().clone());
                                            trace!("Couldn't find connection, requeuing message!");
                                        }
                                    }
                                }
                            }
                            box NetworkMessage::NetworkPacket(ref inner_pkt @ NetworkPacket::BroadcastedMessage(_, _, _, _), _, _) => {
                                if let NetworkPacket::BroadcastedMessage(ref sender, ref msgid, ref network_id, _ ) = inner_pkt {
                                    match self.process_broadcasted_message( sender, msgid, network_id, &inner_pkt.serialize()) {
                                        Ok(_) => {},
                                        Err(e) => {
                                            error!("Could not send to peer XXX due to {}", e);
                                        }
                                    };
                                }
                            }
                            box NetworkMessage::NetworkRequest(ref inner_pkt @ NetworkRequest::BanNode(_, _), _, _) => {
                                for (_, mut conn) in &mut self.tls_server.lock()?.connections {
                                    if let Some(ref peer) = conn.peer().clone() {
                                        match conn.serialize_bytes( &inner_pkt.serialize()) {
                                            Ok(_) => {
                                                self.pks_sent_inc()?;
                                                TOTAL_MESSAGES_SENT_COUNTER.inc();
                                            }
                                            Err(e) => {
                                                error!("Could not send to peer {} due to {}", peer.id().to_string(), e);
                                            }
                                        }
                                    }
                                }
                            }
                            box NetworkMessage::NetworkRequest(ref inner_pkt @ NetworkRequest::JoinNetwork(_, _), _, _) => {
                                {
                                    let mut tls_server = self.tls_server.lock()?;
                                    for (_, mut conn) in &mut tls_server.connections {
                                        if let Some(ref peer) = conn.peer().clone() {
                                            match conn.serialize_bytes( &inner_pkt.serialize()) {
                                                Ok(_) => {
                                                    self.pks_sent_inc()?;
                                                    TOTAL_MESSAGES_SENT_COUNTER.inc();
                                                }
                                                Err(e) => {
                                                    error!("Could not send to peer {} due to {}", peer.id().to_string(), e);
                                                }
                                            }
                                        }
                                    }
                                    if let NetworkRequest::JoinNetwork(_, network_id) = inner_pkt {
                                        tls_server.add_network(network_id).map_err(|e| error!("{}", e)).ok();
                                    }
                                }
                            }
                            box NetworkMessage::NetworkRequest(ref inner_pkt @ NetworkRequest::LeaveNetwork(_,_), _, _) => {
                                {
                                    let mut tls_server = self.tls_server.lock()?;
                                    for (_, mut conn) in &mut tls_server.connections {
                                        if let Some(ref peer) = conn.peer().clone() {
                                            match conn.serialize_bytes( &inner_pkt.serialize()) {
                                                Ok(_) => {
                                                    self.pks_sent_inc()?;
                                                    TOTAL_MESSAGES_SENT_COUNTER.inc();
                                                }
                                                Err(e) => {
                                                    error!("Could not send to peer {} due to {}", peer.id().to_string(), e);
                                                }
                                            }
                                        }
                                    }
                                    if let NetworkRequest::LeaveNetwork(_, network_id) = inner_pkt {
                                        tls_server.remove_network(network_id).map_err(|e| error!("{}", e)).ok();
                                    }
                                }
                            }
                            box NetworkMessage::NetworkRequest(ref inner_pkt @ NetworkRequest::UnbanNode(_, _),
                                                               _,
                                                               _) => {
                                for (_, mut conn) in &mut self.tls_server.lock()?.connections {
                                    if let Some(ref peer) = conn.peer().clone() {
                                        match conn.serialize_bytes( &inner_pkt.serialize()) {
                                            Ok(_) => {
                                                self.pks_sent_inc()?;
                                                TOTAL_MESSAGES_SENT_COUNTER.inc();
                                            }
                                            Err(e) => {
                                                error!("Could not send to peer {} due to {}", peer.id().to_string(), e);
                                            }
                                        }
                                    }
                                }
                            }
                            box NetworkMessage::NetworkRequest(ref inner_pkt @ NetworkRequest::GetPeers(_,_), _, _) => {
                                for (_, mut conn) in &mut self.tls_server.lock()?.connections {
                                    if let Some(ref peer) = conn.peer().clone() {
                                        match conn.serialize_bytes( &inner_pkt.serialize()) {
                                            Ok(_) => {
                                                self.pks_sent_inc()?;
                                                TOTAL_MESSAGES_SENT_COUNTER.inc();
                                            }
                                            Err(e) => {
                                                error!("Could not send to peer {} due to {}", peer.id().to_string(), e);
                                            }
                                        }
                                    }
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
        }
        Ok(())
    }

    fn queue_size_inc(&self) -> ResultExtWrapper<()> {
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

    fn pks_sent_inc(&self) -> ResultExtWrapper<()> {
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
                        -> ResultExtWrapper<()> {
        debug!("Queueing message!");
        match broadcast {
            true => {
                self.send_queue.lock()?.push_back(Arc::new(box NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(self.get_self_peer(), msg_id.unwrap_or(NetworkPacket::generate_message_id()), network_id,  msg.to_vec()), None, None)));
                self.queue_size_inc()?;
                return Ok(());
            }
            false => {
                match id {
                    Some(x) => {
                        self.send_queue.lock()?.push_back(Arc::new(box NetworkMessage::NetworkPacket(NetworkPacket::DirectMessage(self.get_self_peer(), msg_id.unwrap_or(NetworkPacket::generate_message_id()), x, network_id, msg.to_vec()), None, None)));
                        self.queue_size_inc()?;
                        return Ok(());
                    }
                    None => {
                        return Err(ErrorKindWrapper::ParseError("Invalid receiver ID for message".to_string()).into());
                    }
                }
            }
        }
    }

    pub fn send_ban(&mut self, id: P2PPeer) -> ResultExtWrapper<()> {
        self.send_queue
            .lock()?
            .push_back(Arc::new(box NetworkMessage::NetworkRequest(NetworkRequest::BanNode(self.get_self_peer(), id),
                                                               None,
                                                               None)));
        self.queue_size_inc()?;
        Ok(())
    }

    pub fn send_unban(&mut self, id: P2PPeer) -> ResultExtWrapper<()> {
        self.send_queue
            .lock()?
            .push_back(Arc::new(box NetworkMessage::NetworkRequest(NetworkRequest::UnbanNode(self.get_self_peer(), id),
                                                               None,
                                                               None)));
        self.queue_size_inc()?;
        Ok(())
    }

    pub fn send_joinnetwork(&mut self, network_id: u16) -> ResultExtWrapper<()> {
        self.send_queue
            .lock()?
            .push_back(Arc::new(box NetworkMessage::NetworkRequest(NetworkRequest::JoinNetwork(self.get_self_peer(), network_id),
                                                               None,
                                                               None)));
        self.queue_size_inc()?;
        Ok(())
    }

    pub fn send_leavenetwork(&mut self, network_id: u16) -> ResultExtWrapper<()> {
        self.send_queue
            .lock()?
            .push_back(Arc::new(box NetworkMessage::NetworkRequest(NetworkRequest::LeaveNetwork(self.get_self_peer(), network_id),
                                                               None,
                                                               None)));
        self.queue_size_inc()?;
        Ok(())
    }

    pub fn send_get_peers(&mut self, nids: Vec<u16>) -> ResultExtWrapper<()> {
        self.send_queue
            .lock()?
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

    pub fn get_peer_stats(&self, nids: &Vec<u16>) -> ResultExtWrapper<Vec<PeerStatistic>> {
        match self.tls_server.lock() {
            Ok(x) => Ok(x.get_peer_stats(nids)),
            Err(e) => {
                error!("Couldn't lock for tls_server: {:?}", e);
                Err(ErrorWrapper::from(e))
            }
        }
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

    pub fn ban_node(&mut self, peer: P2PPeer) -> ResultExtWrapper<()> {
        match self.tls_server.lock() {
            Ok(mut x) => {
                x.ban_node(peer);
                Ok(())
            }
            Err(e) => Err(ErrorWrapper::from(e)),
        }
    }

    pub fn unban_node(&mut self, peer: P2PPeer) -> ResultExtWrapper<()> {
        match self.tls_server.lock() {
            Ok(mut x) => {
                x.unban_node(peer);
                Ok(())
            }
            Err(e) => Err(ErrorWrapper::from(e)),
        }
    }

    pub fn process(&mut self, events: &mut Events) -> ResultExtWrapper<()> {
        self.poll
            .lock()?
            .poll(events, Some(Duration::from_millis(500)))?;

        if self.mode != P2PNodeMode::BootstrapperMode
           && self.mode != P2PNodeMode::BootstrapperPrivateMode
        {
            self.tls_server.lock()?.liveness_check()?;
        }

        for event in events.iter() {
            let mut tls_ref = self.tls_server.lock()?;
            let mut poll_ref = self.poll.lock()?;
            match event.token() {
                SERVER => {
                    debug!("Got new connection!");
                    tls_ref.accept(&mut poll_ref, self.get_self_peer().clone())
                           .map_err(|e| error!("{}", e))
                           .ok();
                    if let Some(ref prom) = &self.prometheus_exporter {
                        prom.lock()?
                            .conn_received_inc()
                            .map_err(|e| error!("{}", e))
                            .ok();
                    };
                }
                _ => {
                    trace!("Got data!");
                    tls_ref.conn_event(&mut poll_ref,
                                       &event,
                                       &self.incoming_pkts)
                           .map_err(|e| error!("Error occured while parsing event '{}'", e))
                           .ok();
                }
            }
        }

        {
            let mut tls_ref = self.tls_server.lock()?;
            let mut poll_ref = self.poll.lock()?;
            tls_ref.cleanup_connections(&mut poll_ref)?;
            if self.mode == P2PNodeMode::BootstrapperMode
               || self.mode == P2PNodeMode::BootstrapperPrivateMode
            {
                let mut buckets_ref = self.buckets.write()?;
                buckets_ref.clean_peers(self.minimum_per_bucket);
            }
        }

        self.process_messages()?;
        Ok(())
    }

    /// It adds all message handler callback to this connection.
    fn register_message_handlers(&self) {
        if let Some(mut tls_server_lock) = self.tls_server.lock().ok() {

            let ref mut mh = tls_server_lock.mut_message_handler();

            for callback in self.message_handler.packet_parser.callbacks.iter() {
                mh.add_packet_callback( callback.clone());
            }

            for callback in self.message_handler.response_parser.callbacks.iter() {
                mh.add_response_callback( callback.clone());
            }

            for callback in self.message_handler.request_parser.callbacks.iter() {
                mh.add_request_callback( callback.clone());
            }
        }
    }
}

impl MessageManager for P2PNode {
    fn message_handler(&self) -> &MessageHandler {
        & self.message_handler
    }

    fn mut_message_handler(&mut self) -> &mut MessageHandler {
        &mut self.message_handler
    }
}

