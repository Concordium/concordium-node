use std::sync::{ Arc, Mutex, RwLock };
use std::net::{ IpAddr, SocketAddr };
use mio::net::{ TcpListener, TcpStream };
use mio::{ Token, Poll, Event };
use std::sync::mpsc::Sender;
use std::collections::{ HashMap, HashSet };
use rustls::{ ClientConfig, ServerConfig, ServerSession, ClientSession };
use webpki::{ DNSNameRef };

use prometheus_exporter::PrometheusServer;

use connection::{ 
    Connection, P2PNodeMode, P2PEvent, SeenMessagesList, MessageHandler, 
    MessageManager }; 
use common::{ P2PNodeId, P2PPeer, ConnectionType };
use common;
use p2p::unreachable_nodes::{ UnreachableNodes };
use p2p::peer_statistics::{ PeerStatistic };
use errors::*;
use network::{ NetworkRequest, NetworkMessage, Buckets };

const MAX_UNREACHABLE_MARK_TIME: u64 = 1000 * 60 * 60 * 24;
const MAX_FAILED_PACKETS_ALLOWED: u32 = 50;

pub struct TlsServer {
    server: TcpListener,
    pub connections: HashMap<Token, Connection>,
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
    buckets: Arc< RwLock< Buckets > >,
    message_handler: MessageHandler    
}

impl TlsServer {
    pub fn new(server: TcpListener,
           server_cfg: Arc<ServerConfig>,
           client_cfg: Arc<ClientConfig>,
           own_id: P2PNodeId,
           event_log: Option<Sender<P2PEvent>>,
           self_peer: P2PPeer,
           mode: P2PNodeMode,
           prometheus_exporter: Option<Arc<Mutex<PrometheusServer>>>,
           networks: Vec<u16>,
           seen_messages: SeenMessagesList,
           buckets: Arc< RwLock< Buckets > >
           )
           -> Self {
        let mut mself = TlsServer { server,
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
                    seen_messages: seen_messages, 
                    buckets: buckets,
                    message_handler: MessageHandler::new()
        };

        mself.message_handler = mself.make_default_message_handler();
        mself
    }

    pub fn log_event(&mut self, event: P2PEvent) {
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

    pub fn get_self_peer(&self) -> P2PPeer {
        self.self_peer.clone()
    }

    pub fn remove_network(&mut self, network_id: &u16) -> ResultExtWrapper<()> {
        self.networks.lock()?.retain(|x| x == network_id);
        Ok(())
    }

    pub fn add_network(&mut self, network_id: &u16) -> ResultExtWrapper<()> {
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

    pub fn accept(&mut self, poll: &mut Poll, self_id: P2PPeer) -> ResultExtWrapper<()> {
        match self.server.accept() {
            Ok((socket, addr)) => {
                debug!("Accepting new connection from {:?} to {:?}:{}", addr, self_id.ip(), self_id.port());
                self.log_event(P2PEvent::ConnectEvent(format!("{}", addr.ip()), addr.port()));

                let tls_session = ServerSession::new(&self.server_tls_config);
                let token = Token(self.next_id);
                self.next_id += 1;

                let mut conn = Connection::new(ConnectionType::Node,
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
                                           self.seen_messages.clone(),
                                           self.buckets.clone());
                self.register_message_handlers( &mut conn);

                self.connections.insert(token, conn);
                self.connections[&token].register(poll)
            }
            Err(e) => Err(ErrorKindWrapper::InternalIOError(e).into()),
        }
    }

    pub fn connect(&mut self,
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
                                           self.seen_messages.clone(),
                                           self.buckets.clone());

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

    pub fn find_connection(&mut self, id: P2PNodeId) -> Option<&mut Connection> {
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

    pub fn conn_event(&mut self,
                  poll: &mut Poll,
                  event: &Event,
                  packet_queue: &Sender<Arc<Box<NetworkMessage>>>)
                  -> ResultExtWrapper<()> {
        let token = event.token();
        if self.connections.contains_key(&token) {
            match self.connections.get_mut(&token) {
                Some(x) => x.ready(poll, event, &packet_queue)
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

    pub fn cleanup_connections(&mut self, mut poll: &mut Poll) -> ResultExtWrapper<()> {
        if self.mode == P2PNodeMode::BootstrapperMode
           || self.mode == P2PNodeMode::BootstrapperPrivateMode
        {
            for conn in self.connections.values_mut() {
                if conn.last_seen() + 300000 < common::get_current_stamp() {
                    conn.close(&mut poll).map_err(|e| error!("{}", e)).ok();
                }
            }
        } else {
            for conn in self.connections.values_mut() {
                if conn.last_seen() + 1200000 < common::get_current_stamp()
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

    pub fn liveness_check(&mut self) -> ResultExtWrapper<()> {
        for conn in self.connections.values_mut() {
            if conn.last_seen() + 120000 < common::get_current_stamp()
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

    fn make_default_message_handler(&mut self) -> MessageHandler {
        MessageHandler::new()
    }

    /// It adds all message handler callback to this connection.
    fn register_message_handlers(&self, conn: &mut Connection) {
        let ref mut mh = conn.mut_message_handler();

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

impl MessageManager for TlsServer {
    fn message_handler(&self) -> &MessageHandler {
        & self.message_handler
    }

    fn mut_message_handler(&mut self) -> &mut MessageHandler {
        &mut self.message_handler
    }
}
