use std::sync::{ Arc, Mutex, RwLock };
use std::net::{ IpAddr, SocketAddr };
use std::rc::{ Rc };
use std::cell::{ RefCell };
use mio::net::{ TcpListener, TcpStream };
use mio::{ Token, Poll, Event };
use std::sync::mpsc::Sender;
use rustls::{ ClientConfig, ServerConfig, ServerSession, ClientSession };
use webpki::{ DNSNameRef };

use prometheus_exporter::PrometheusServer;

use connection::{
    Connection, P2PNodeMode, P2PEvent, MessageHandler,
    MessageManager };
use common::{ P2PNodeId, P2PPeer, ConnectionType };
use errors::*;
use network::{ NetworkRequest, NetworkMessage, Buckets };

use p2p::peer_statistics::{ PeerStatistic };
use p2p::tls_server_private::{ TlsServerPrivate };

pub struct TlsServer {
    server: TcpListener,
    next_id: usize,
    server_tls_config: Arc<ServerConfig>,
    client_tls_config: Arc<ClientConfig>,
    own_id: P2PNodeId,
    event_log: Option<Sender<P2PEvent>>,
    self_peer: P2PPeer,
    mode: P2PNodeMode,
    buckets: Arc< RwLock< Buckets > >,
    prometheus_exporter: Option<Arc<Mutex<PrometheusServer>>>,

    message_handler: Arc< RwLock< MessageHandler>>,
    dptr: Rc< RefCell< TlsServerPrivate>>,
    blind_trusted_broadcast: bool,
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
           buckets: Arc< RwLock< Buckets > >,
           blind_trusted_broadcast: bool,
           )
           -> Self {
        let mdptr = Rc::new( RefCell::new(
                TlsServerPrivate::new(
                    networks,
                    prometheus_exporter.clone())));

        let mut mself = TlsServer { server,
                    next_id: 2,
                    server_tls_config: server_cfg,
                    client_tls_config: client_cfg,
                    own_id,
                    event_log,
                    self_peer,
                    mode: mode,
                    prometheus_exporter: prometheus_exporter,
                    buckets: buckets,
                    message_handler: Arc::new( RwLock::new( MessageHandler::new())),
                    dptr: mdptr,
                    blind_trusted_broadcast,
        };

        mself.setup_default_message_handler();
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

    pub fn networks(&self) -> Arc< Mutex< Vec<u16>>> {
        self.dptr.borrow().networks.clone()
    }

    pub fn remove_network(&mut self, network_id: &u16) -> ResultExtWrapper<()> {
        self.dptr.borrow_mut().remove_network( network_id)
    }

    pub fn add_network(&mut self, network_id: &u16) -> ResultExtWrapper<()> {
        self.dptr.borrow_mut().add_network( network_id)
    }

    /// It returns true if `ip` at port `port` is in `unreachable_nodes` list.
    pub fn is_unreachable(&self, ip: IpAddr, port: u16) -> bool {
        self.dptr.borrow().unreachable_nodes.contains( ip, port)
    }

    /// It adds the pair `ip`,`port` to its `unreachable_nodes` list.
    pub fn add_unreachable(&mut self, ip: IpAddr, port: u16) -> bool {
        self.dptr.borrow_mut().unreachable_nodes.insert( ip, port)
    }

    pub fn get_peer_stats(&self, nids: &Vec<u16>) -> Vec<PeerStatistic> {
        self.dptr.borrow().get_peer_stats( nids)
    }

    pub fn ban_node(&mut self, peer: P2PPeer) -> bool {
        self.dptr.borrow_mut().ban_node( peer)
    }

    pub fn unban_node(&mut self, peer: &P2PPeer) -> bool {
        self.dptr.borrow_mut().unban_node( peer)
    }

    pub fn accept(&mut self, poll: &mut Poll, self_id: P2PPeer) -> ResultExtWrapper<()> {
        match self.server.accept() {
            Ok((socket, addr)) => {
                debug!("Accepting new connection from {:?} to {:?}:{}", addr, self_id.ip(), self_id.port());
                self.log_event(P2PEvent::ConnectEvent(format!("{}", addr.ip()), addr.port()));

                let tls_session = ServerSession::new(&self.server_tls_config);
                let token = Token(self.next_id);
                self.next_id += 1;

                let networks = self.dptr.borrow().networks.clone();
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
                                           networks,
                                           self.buckets.clone(),
                                           self.blind_trusted_broadcast,);
                self.register_message_handlers( &mut conn);

                let register_status = conn.register( poll);
                self.dptr.borrow_mut().connections.insert(token, conn);

                register_status
            },
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
        if connection_type == ConnectionType::Node && self.is_unreachable(ip, port) {
            error!("Node marked as unreachable, so not allowing the connection");
            return Err(ErrorKindWrapper::UnreachablePeerError("Peer marked as unreachable, won't try it".to_string()).into());
        }
        let self_peer = self.get_self_peer();
        if self_peer.ip() == ip && self_peer.port() == port {
            return Err(ErrorKindWrapper::DuplicatePeerError("Already connected to peer".to_string()).into());
        }
        for (_, ref conn) in &self.dptr.borrow().connections {
            if let Some(ref peer) = conn.peer() {
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

                let networks = self.dptr.borrow().networks.clone();
                let mut conn = Connection::new(connection_type,
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
                                           networks.clone(),
                                           self.buckets.clone(),
                                           self.blind_trusted_broadcast,);

                self.register_message_handlers( &mut conn);
                conn.register(poll)?;

                self.next_id += 1;
                self.dptr.borrow_mut().connections.insert(token, conn);
                self.log_event(P2PEvent::ConnectEvent(ip.to_string(), port));
                debug!("Requesting handshake from new peer {}:{}",
                       ip.to_string(),
                       port);
                let self_peer = self.get_self_peer().clone();
                if let Some(ref mut conn) = self.dptr.borrow_mut().connections.get_mut(&token) {
                    conn.serialize_bytes(
                        &NetworkRequest::Handshake(self_peer,
                            networks.lock()?.clone(),
                            vec![]).serialize())?;
                    conn.set_measured_handshake_sent();
                }
                Ok(())
            }
            Err(e) => {
                if connection_type == ConnectionType::Node
                   && !self.add_unreachable(ip, port)
                {
                    error!("Can't insert unreachable peer!");
                }
                Err(ErrorKindWrapper::InternalIOError(e).into())
            }
        }
    }

    pub fn conn_event(&mut self,
                  poll: &mut Poll,
                  event: &Event,
                  packet_queue: &Sender<Arc<Box<NetworkMessage>>>)
                  -> ResultExtWrapper<()> {
        self.dptr.borrow_mut().conn_event( poll, event, packet_queue)
    }

    pub fn cleanup_connections(&self, poll: &mut Poll)
            -> ResultExtWrapper<()> {
        self.dptr.borrow_mut().cleanup_connections( self.mode, poll)
    }

    pub fn liveness_check(&self) -> ResultExtWrapper<()> {
        self.dptr.borrow_mut().liveness_check()
    }

    /// It sends `data` message over all filtered connections.
    ///
    /// # Arguments
    /// * `data` - Raw message.
    /// * `filter_conn` - It will send using all connection, where this function returns `true`.
    /// * `send_status` - It will called after each sent, to notify the result of the operation.
    pub fn send_over_all_connections( &self,
            data: &Vec<u8>,
            filter_conn: &Fn( &Connection) -> bool,
            send_status: &Fn( &Connection, ResultExtWrapper<()>))
    {
        self.dptr.borrow_mut()
            .send_over_all_connections( data, filter_conn, send_status)
    }

    /// It setups default message handler at TLSServer level.
    fn setup_default_message_handler(&mut self) {
    }

    /// It adds all message handler callback to this connection.
    fn register_message_handlers(&self, conn: &mut Connection) {
        let mh = self.message_handler.read().expect("Couldn't read when registering message handlers");
        conn.message_handler.merge( &mh);
    }
}

impl MessageManager for TlsServer {
    fn message_handler(&self) -> Arc< RwLock< MessageHandler>> {
        self.message_handler.clone()
    }
}
