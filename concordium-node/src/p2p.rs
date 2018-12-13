use errors::*;
use get_if_addrs;
use mio::net::{ TcpListener, TcpStream };
use mio::*;
use prometheus_exporter::PrometheusServer;
use rustls::{
    Certificate, ClientConfig, ClientSession, NoClientAuth, PrivateKey, RootCertStore,
    ServerCertVerified, ServerCertVerifier, ServerConfig, ServerSession, TLSError
};

use atomic_counter::{ AtomicCounter };
use std::collections::{HashMap, HashSet, VecDeque};
use std::io::{ Error, ErrorKind };
use std::net::{ SocketAddr, IpAddr };
use std::net::IpAddr::{V4, V6};
use std::str::FromStr;
use std::sync::mpsc;
use std::sync::mpsc::Sender;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;
use time;
use time::Timespec;
use utils;
use webpki::DNSNameRef;

use common;
use common::{ ConnectionType, P2PNodeId, P2PPeer };
use network::{ NetworkMessage, NetworkPacket, NetworkRequest, Buckets };
use connection::{ P2PEvent, P2PNodeMode, Connection, SeenMessagesList, TOTAL_MESSAGES_SENT_COUNTER }; 

const SERVER: Token = Token(0);
const MAX_UNREACHABLE_MARK_TIME: u64 = 1000 * 60 * 60 * 24;
const MAX_FAILED_PACKETS_ALLOWED: u32 = 50;

#[derive(Debug)]
pub struct PeerStatistic {
    pub id: String,
    pub ip: IpAddr,
    pub port: u16,
    pub sent: u64,
    pub received: u64,
    pub measured_latency: Option<u64>,
}

impl PeerStatistic {
    pub fn new(id: String,
               ip: IpAddr,
               port: u16,
               sent: u64,
               received: u64,
               measured_latency: Option<u64>)
               -> PeerStatistic {
        PeerStatistic { id,
                        ip,
                        port,
                        sent,
                        received,
                        measured_latency, }
    }

    pub fn id(&self) -> String {
        self.id.clone()
    }

    pub fn sent(&self) -> u64 {
        self.sent
    }

    pub fn received(&self) -> u64 {
        self.received
    }

    pub fn measured_latency(&self) -> Option<u64> {
        self.measured_latency.clone()
    }

    pub fn ip(&self) -> IpAddr {
        self.ip.clone()
    }

    pub fn port(&self) -> u16 {
        self.port
    }
}

#[derive(Clone, Debug)]
struct UnreachableNodes {
    nodes: Arc<Mutex<Vec<(u64, IpAddr, u16)>>>,
}

impl UnreachableNodes {
    fn new() -> Self {
        UnreachableNodes { nodes: Arc::new(Mutex::new(vec![])), }
    }

    fn contains(&self, ip: IpAddr, port: u16) -> bool {
        if let Ok(ref mut nodes) = self.nodes.lock() {
            return nodes.iter()
                        .find(|&&x| {
                                  let (_, mip, mport) = x;
                                  ip == mip && port == mport
                              })
                        .is_some();
        }
        true
    }

    fn insert(&mut self, ip: IpAddr, port: u16) -> bool {
        if let Ok(ref mut nodes) = self.nodes.lock() {
            nodes.push((common::get_current_stamp(), ip.clone(), port));
            true
        } else {
            false
        }
    }

    fn cleanup(&mut self, since: u64) -> bool {
        if let Ok(ref mut nodes) = self.nodes.lock() {
            nodes.retain(|&x| {
                             let (time, _, _) = x;
                             time >= since
                         });
            true
        } else {
            false
        }
    }
}

struct TlsServer {
    server: TcpListener,
    connections: HashMap<Token, Connection>,
    next_id: usize,
    server_tls_config: Arc<ServerConfig>,
    client_tls_config: Arc<ClientConfig>,
    own_id: P2PNodeId,
    event_log: Option<Sender<P2PEvent>>,
    self_peer: P2PPeer,
    banned_peers: HashSet<P2PPeer>,
    mode: P2PNodeMode,
    prometheus_exporter: Option<Arc<Mutex<PrometheusServer>>>,
    networks: Arc<Mutex<Vec<u16>>>,
    unreachable_nodes: UnreachableNodes,
    seen_messages: SeenMessagesList,
}

impl TlsServer {
    fn new(server: TcpListener,
           server_cfg: Arc<ServerConfig>,
           client_cfg: Arc<ClientConfig>,
           own_id: P2PNodeId,
           event_log: Option<Sender<P2PEvent>>,
           self_peer: P2PPeer,
           mode: P2PNodeMode,
           prometheus_exporter: Option<Arc<Mutex<PrometheusServer>>>,
           networks: Vec<u16>,
           seen_messages: SeenMessagesList)
           -> TlsServer {
        TlsServer { server,
                    connections: HashMap::new(),
                    next_id: 2,
                    server_tls_config: server_cfg,
                    client_tls_config: client_cfg,
                    own_id,
                    event_log,
                    self_peer,
                    banned_peers: HashSet::new(),
                    mode: mode,
                    prometheus_exporter: prometheus_exporter,
                    networks: Arc::new(Mutex::new(networks)),
                    unreachable_nodes: UnreachableNodes::new(),
                    seen_messages: seen_messages, }
    }

    fn log_event(&mut self, event: P2PEvent) {
        match self.event_log {
            Some(ref mut x) => {
                match x.send(event) {
                    Ok(_) => {}
                    Err(e) => error!("Couldn't send error {:?}", e),
                };
            }
            _ => {}
        }
    }

    fn get_self_peer(&self) -> P2PPeer {
        self.self_peer.clone()
    }

    fn remove_network(&mut self, network_id: &u16) -> ResultExtWrapper<()> {
        self.networks.lock()?.retain(|x| x == network_id);
        Ok(())
    }

    fn add_network(&mut self, network_id: &u16) -> ResultExtWrapper<()> {
        {
            let mut networks = self.networks.lock()?;
            if !networks.contains(network_id) {
                networks.push(*network_id)
            }
        }
        Ok(())
    }

    pub fn get_peer_stats(&self, nids: &Vec<u16>) -> Vec<PeerStatistic> {
        let mut ret = vec![];
        for (_, ref conn) in &self.connections {
            match conn.peer {
                Some(ref x) => {
                    if nids.len() == 0 || conn.networks.iter().any(|nid| nids.contains(nid)) {
                        ret.push(PeerStatistic::new(x.id().to_string(),
                                                    x.ip().clone(),
                                                    x.port(),
                                                    conn.get_messages_sent(),
                                                    conn.get_messages_received(),
                                                    conn.get_last_latency_measured()));
                    }
                }
                None => {}
            }
        }

        ret
    }

    pub fn ban_node(&mut self, peer: P2PPeer) -> bool {
        self.banned_peers.insert(peer)
    }

    pub fn unban_node(&mut self, peer: P2PPeer) -> bool {
        self.banned_peers.remove(&peer)
    }

    fn accept(&mut self, poll: &mut Poll, self_id: P2PPeer) -> ResultExtWrapper<()> {
        match self.server.accept() {
            Ok((socket, addr)) => {
                debug!("Accepting new connection from {:?} to {:?}:{}", addr, self_id.ip(), self_id.port());
                self.log_event(P2PEvent::ConnectEvent(format!("{}", addr.ip()), addr.port()));

                let tls_session = ServerSession::new(&self.server_tls_config);
                let token = Token(self.next_id);
                self.next_id += 1;

                self.connections.insert(token,
                                        Connection::new(ConnectionType::Node,
                                                        socket,
                                                        token,
                                                        Some(tls_session),
                                                        None,
                                                        false,
                                                        self.own_id.clone(),
                                                        self_id.clone(),
                                                        addr.ip().clone(),
                                                        addr.port().clone(),
                                                        self.mode,
                                                        self.prometheus_exporter.clone(),
                                                        self.event_log.clone(),
                                                        self.networks.clone(),
                                                        self.seen_messages.clone()));
                self.connections[&token].register(poll)
            }
            Err(e) => Err(ErrorKindWrapper::InternalIOError(e).into()),
        }
    }

    fn connect(&mut self,
               connection_type: ConnectionType,
               poll: &mut Poll,
               ip: IpAddr,
               port: u16,
               peer_id: Option<P2PNodeId>,
               self_id: &P2PPeer)
               -> ResultExtWrapper<()> {
        if connection_type == ConnectionType::Node && self.unreachable_nodes.contains(ip, port) {
            error!("Node marked as unreachable, so not allowing the connection");
            return Err(ErrorKindWrapper::UnreachablePeerError("Peer marked as unreachable, won't try it".to_string()).into());
        }
        let self_peer = self.get_self_peer();
        if self_peer.ip() == ip && self_peer.port() == port {
            return Err(ErrorKindWrapper::DuplicatePeerError("Already connected to peer".to_string()).into());
        }
        for (_, ref conn) in &self.connections {
            if let Some(ref peer) = conn.peer {
                if peer.ip() == ip && peer.port() == port {
                    return Err(ErrorKindWrapper::DuplicatePeerError("Already connected to peer".to_string()).into());
                } else if let Some(ref new_peer_id) = peer_id {
                    if new_peer_id == &peer.id() {
                        return Err(ErrorKindWrapper::DuplicatePeerError("Already connected to peer".to_string()).into());
                    }
                }
            } else if conn.ip() == ip && conn.port() == port {
                return Err(ErrorKindWrapper::DuplicatePeerError("Already connected to peer".to_string()).into());
            }
        }
        match TcpStream::connect(&SocketAddr::new(ip, port)) {
            Ok(x) => {
                if let Some(ref prom) = &self.prometheus_exporter {
                    prom.lock()?
                        .conn_received_inc()
                        .map_err(|e| error!("{}", e))
                        .ok();
                };
                let tls_session =
                    ClientSession::new(&self.client_tls_config,
                                       match DNSNameRef::try_from_ascii_str(&"node.concordium.com")
                                       {
                                           Ok(x) => x,
                                           Err(e) => panic!("The error is: {:?}", e),
                                       });

                let token = Token(self.next_id);

                let conn = Connection::new(connection_type,
                                           x,
                                           token,
                                           None,
                                           Some(tls_session),
                                           true,
                                           self.own_id.clone(),
                                           self_id.clone(),
                                           ip,
                                           port,
                                           self.mode,
                                           self.prometheus_exporter.clone(),
                                           self.event_log.clone(),
                                           self.networks.clone(),
                                           self.seen_messages.clone());

                conn.register(poll)?;
                self.next_id += 1;
                self.connections.insert(token, conn);
                self.log_event(P2PEvent::ConnectEvent(ip.to_string(), port));
                debug!("Requesting handshake from new peer {}:{}",
                       ip.to_string(),
                       port);
                let self_peer = self.get_self_peer().clone();
                if let Some(ref mut conn) = self.connections.get_mut(&token) {
                    conn.serialize_bytes(
                                    &NetworkRequest::Handshake(self_peer,
                                                               self.networks
                                                                   .lock()
                                                                   .unwrap()
                                                                   .clone(),
                                                               vec![]).serialize())?;
                    conn.set_measured_handshake_sent();
                }
                Ok(())
            }
            Err(e) => {
                if connection_type == ConnectionType::Node
                   && !self.unreachable_nodes.insert(ip, port)
                {
                    error!("Can't insert unreachable peer!");
                }
                Err(ErrorKindWrapper::InternalIOError(e).into())
            }
        }
    }

    fn find_connection(&mut self, id: P2PNodeId) -> Option<&mut Connection> {
        let mut tok = Token(0);
        for (token, mut connection) in &self.connections {
            match connection.peer {
                Some(ref x) => {
                    if x.id() == id {
                        tok = *token;
                    } else {
                        break;
                    }
                }
                _ => {
                    break;
                }
            }
        }

        if tok == Token(0) {
            None
        } else {
            match self.connections.get_mut(&tok) {
                Some(x) => Some(x),
                None => {
                    error!("Couldn't get connections mutable");
                    None
                }
            }
        }
    }

    fn conn_event(&mut self,
                  poll: &mut Poll,
                  event: &Event,
                  mut buckets: &mut Buckets,
                  packet_queue: &mpsc::Sender<Arc<Box<NetworkMessage>>>)
                  -> ResultExtWrapper<()> {
        let token = event.token();
        if self.connections.contains_key(&token) {
            match self.connections.get_mut(&token) {
                Some(x) => x.ready(poll, event, &mut buckets, &packet_queue)
                            .map_err(|e| error!("Error while performing ready() check on connection '{}'", e))
                            .ok(),
                None => {
                    return Err(ErrorKindWrapper::LockingError("Couldn't get lock for connection".to_string()).into())
                }
            };

            if self.connections[&token].is_closed() {
                self.connections.remove(&token);
            }
        }
        Ok(())
    }

    fn cleanup_connections(&mut self, mut poll: &mut Poll) -> ResultExtWrapper<()> {
        if self.mode == P2PNodeMode::BootstrapperMode
           || self.mode == P2PNodeMode::BootstrapperPrivateMode
        {
            for conn in self.connections.values_mut() {
                if conn.last_seen + 300000 < common::get_current_stamp() {
                    conn.close(&mut poll).map_err(|e| error!("{}", e)).ok();
                }
            }
        } else {
            for conn in self.connections.values_mut() {
                if conn.last_seen + 1200000 < common::get_current_stamp()
                   && conn.connection_type == ConnectionType::Node
                {
                    conn.close(&mut poll).map_err(|e| error!("{}", e)).ok();
                }
                if conn.failed_pkts() >= MAX_FAILED_PACKETS_ALLOWED {
                    conn.close(&mut poll).map_err(|e| error!("{}", e)).ok();
                }
            }
            self.unreachable_nodes
                .cleanup(common::get_current_stamp() - MAX_UNREACHABLE_MARK_TIME);
        }

        let closed_ones: Vec<_> = self.connections
                                      .iter()
                                      .filter(|&(_, &ref v)| v.closing)
                                      .map(|(k, _)| k.clone())
                                      .collect();
        for closed in closed_ones {
            if let Some(ref prom) = &self.prometheus_exporter {
                if let Some(ref peer) = self.connections.get(&closed) {
                    if let Some(_) = peer.peer {
                        prom.lock()?.peers_dec().map_err(|e| error!("{}", e)).ok();
                    };
                };
            };

            self.connections.remove(&closed);
        }

        //Kill banned connections
        for peer in &self.banned_peers {
            for conn in self.connections.values_mut() {
                match conn.peer.clone() {
                    Some(ref p) => {
                        if p == peer {
                            conn.close(&mut poll).map_err(|e| error!("{}", e)).ok();
                        }
                    }
                    None => {}
                }
            }
        }
        Ok(())
    }

    fn liveness_check(&mut self) -> ResultExtWrapper<()> {
        for conn in self.connections.values_mut() {
            if conn.last_seen + 120000 < common::get_current_stamp()
               || conn.get_last_ping_sent() + 300000 < common::get_current_stamp()
            {
                let self_peer = conn.get_self_peer().clone();
                conn.serialize_bytes( &NetworkRequest::Ping(self_peer).serialize()).map_err(|e| error!("{}", e))
                                                                                   .ok();
                conn.set_measured_ping_sent();
                conn.set_last_ping_sent();
            }
        }
        Ok(())
    }
}

//Disable certificate verification
pub struct NoCertificateVerification {}

impl ServerCertVerifier for NoCertificateVerification {
    fn verify_server_cert(&self,
                          _roots: &RootCertStore,
                          _presented_certs: &[Certificate],
                          _dns_name: DNSNameRef,
                          _ocsp: &[u8])
                          -> Result<ServerCertVerified, TLSError> {
        Ok(ServerCertVerified::assertion())
    }
}

#[derive(Clone)]
pub struct P2PNode {
    tls_server: Arc<Mutex<TlsServer>>,
    poll: Arc<Mutex<Poll>>,
    id: P2PNodeId,
    buckets: Arc<Mutex<Buckets>>,
    send_queue: Arc<Mutex<VecDeque<Arc<Box<NetworkMessage>>>>>,
    ip: IpAddr,
    port: u16,
    incoming_pkts: mpsc::Sender<Arc<Box<NetworkMessage>>>,
    event_log: Option<mpsc::Sender<P2PEvent>>,
    start_time: Timespec,
    prometheus_exporter: Option<Arc<Mutex<PrometheusServer>>>,
    mode: P2PNodeMode,
    external_ip: IpAddr,
    external_port: u16,
    seen_messages: SeenMessagesList,
    minimum_per_bucket: usize,
}



impl P2PNode {
    pub fn new(supplied_id: Option<String>,
               listen_address: Option<String>,
               listen_port: u16,
               external_ip: Option<String>,
               external_port: Option<u16>,
               pkt_queue: mpsc::Sender<Arc<Box<NetworkMessage>>>,
               event_log: Option<mpsc::Sender<P2PEvent>>,
               mode: P2PNodeMode,
               prometheus_exporter: Option<Arc<Mutex<PrometheusServer>>>,
               networks: Vec<u16>,
               minimum_per_bucket: usize)
               -> P2PNode {
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

        let tlsserv = TlsServer::new(server,
                                     Arc::new(server_conf),
                                     Arc::new(client_conf),
                                     _id.clone(),
                                     event_log.clone(),
                                     self_peer,
                                     mode,
                                     prometheus_exporter.clone(),
                                     networks,
                                     seen_messages.clone());

        P2PNode { tls_server: Arc::new(Mutex::new(tlsserv)),
                  poll: Arc::new(Mutex::new(poll)),
                  id: _id,
                  buckets: Arc::new(Mutex::new(Buckets::new())),
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
                  minimum_per_bucket: minimum_per_bucket, }
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
        if let Some(ref peer) = conn.peer {
            if peer.id() != sender.id() {
                return conn.own_networks.lock().unwrap().contains(network_id);
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
                                            if conn.own_networks.lock().unwrap().contains(network_id) {
                                                if let Some(ref peer) = conn.peer.clone() {
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
                                    if let Some(ref peer) = conn.peer.clone() {
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
                                        if let Some(ref peer) = conn.peer.clone() {
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
                                        if let Some(ref peer) = conn.peer.clone() {
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
                                    if let Some(ref peer) = conn.peer.clone() {
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
                                    if let Some(ref peer) = conn.peer.clone() {
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

    fn get_self_peer(&self) -> P2PPeer {
        P2PPeer::from(ConnectionType::Node,
                      self.get_own_id().clone(),
                      self.get_listening_ip().clone(),
                      self.get_listening_port())
    }

    pub fn get_total_sent(&self) -> u64 {
        TOTAL_MESSAGES_SENT_COUNTER.get() as u64
    }

    pub fn get_total_received(&self) -> u64 {
        TOTAL_MESSAGES_SENT_COUNTER.get() as u64
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
            let mut buckets_ref = self.buckets.lock()?;
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
                                       &mut buckets_ref,
                                       &self.incoming_pkts)
                           .map_err(|e| error!("Error occured while parsing event '{}'", e))
                           .ok();
                }
            }
        }

        {
            let mut tls_ref = self.tls_server.lock()?;
            let mut poll_ref = self.poll.lock()?;
            let mut buckets_ref = self.buckets.lock()?;
            tls_ref.cleanup_connections(&mut poll_ref)?;
            if self.mode == P2PNodeMode::BootstrapperMode
               || self.mode == P2PNodeMode::BootstrapperPrivateMode
            {
                buckets_ref.clean_peers(self.minimum_per_bucket);
            }
        }

        self.process_messages()?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use p2p::*;

    #[test]
    pub fn test_buckets_insert_duplicate_peer_id() {
        let mut bucket = Buckets::new();
        let p2p_self = P2PNodeId::from_ipstring("127.0.0.1:8888".to_string());
        let p2p_node_id = P2PNodeId::from_string(&"c19cd000746763871fae95fcdd4508dfd8bf725f9767be68c3038df183527bb2".to_string()).unwrap();
        let p2p_new_peer = P2PPeer::from(ConnectionType::Node,
                                         p2p_node_id.clone(),
                                         IpAddr::from_str("127.0.0.1").unwrap(),
                                         8888);
        let p2p_new_replacement_peer = P2PPeer::from(ConnectionType::Node,
                                                     p2p_node_id.clone(),
                                                     IpAddr::from_str("127.0.0.1").unwrap(),
                                                     8889);
        bucket.insert_into_bucket(&p2p_new_peer, &p2p_self, vec![]);
        bucket.insert_into_bucket(&p2p_new_replacement_peer, &p2p_self, vec![]);
        assert_eq!(bucket.len(), 1);
    }

    #[test]
    pub fn test_buckets_insert_duplicate_ip_port() {
        let mut bucket = Buckets::new();
        let p2p_self = P2PNodeId::from_ipstring("127.0.0.1:8888".to_string());
        let p2p_node_id = P2PNodeId::from_string(&"c19cd000746763871fae95fcdd4508dfd8bf725f9767be68c3038df183527bb2".to_string()).unwrap();
        let p2p_node_id_2 = P2PNodeId::from_string(&"c19cd000746763871fae95fcdd4508dfd8bf725f9767be68c3038df183527bb3".to_string()).unwrap();
        let p2p_new_peer = P2PPeer::from(ConnectionType::Node,
                                         p2p_node_id.clone(),
                                         IpAddr::from_str("127.0.0.1").unwrap(),
                                         8888);
        let p2p_new_replacement_peer = P2PPeer::from(ConnectionType::Node,
                                                     p2p_node_id_2.clone(),
                                                     IpAddr::from_str("127.0.0.1").unwrap(),
                                                     8888);
        bucket.insert_into_bucket(&p2p_new_peer, &p2p_self, vec![]);
        bucket.insert_into_bucket(&p2p_new_replacement_peer, &p2p_self, vec![]);
        assert_eq!(bucket.len(), 1);
    }
}
