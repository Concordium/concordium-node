use byteorder::{NetworkEndian, ReadBytesExt, WriteBytesExt};
use bytes::{BufMut, BytesMut};
use common;
use common::{NetworkMessage, NetworkPacket, NetworkRequest, NetworkResponse, P2PNodeId, P2PPeer};
use get_if_addrs;
use mio::net::TcpListener;
use mio::net::TcpStream;
use mio::*;
use num_bigint::BigUint;
use num_bigint::ToBigUint;
use num_traits::pow;
use rustls::{
    Certificate, ClientConfig, ClientSession, NoClientAuth, PrivateKey, RootCertStore,
    ServerCertVerified, ServerCertVerifier, ServerConfig, ServerSession, Session, TLSError,
};
use std::collections::{HashMap, HashSet, VecDeque};
use std::io;
use std::io::Cursor;
use std::io::Error;
use std::io::ErrorKind;
use std::io::Read;
use std::io::Write;
use std::net::IpAddr;
use std::net::IpAddr::{V4, V6};
use std::net::Ipv4Addr;
use std::net::{Shutdown, SocketAddr};
use std::sync::mpsc;
use std::sync::mpsc::Sender;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;
use time;
use time::Timespec;
use utils;
use webpki::DNSNameRef;

const SERVER: Token = Token(0);
const BUCKET_SIZE: u8 = 20;
const KEY_SIZE: u16 = 256;

pub enum P2PEvent {
    ConnectEvent(String, u16),
    DisconnectEvent(String),
    ReceivedMessageEvent(P2PNodeId),
    SentMessageEvent(P2PNodeId),
    InitiatingConnection(IpAddr, u16),
}

struct Buckets {
    buckets: HashMap<u16, Vec<P2PPeer>>,
}

impl Buckets {
    fn new() -> Buckets {
        let mut buckets = HashMap::new();
        for i in 0..KEY_SIZE {
            buckets.insert(i, Vec::new());
        }

        Buckets { buckets }
    }

    pub fn distance(&self, from: &P2PNodeId, to: &P2PNodeId) -> BigUint {
        from.get_id().clone() ^ to.get_id().clone()
    }

    pub fn insert_into_bucket(&mut self, node: &P2PPeer, own_id: &P2PNodeId) {
        let dist = self.distance(&own_id, &node.id());
        for i in 0..KEY_SIZE {
            if dist >= pow(2_i8.to_biguint().unwrap(), i as usize)
               && dist < pow(2_i8.to_biguint().unwrap(), (i as usize) + 1)
            {
                match self.buckets.get_mut(&i) {
                    Some(x) => {
                        if x.len() >= BUCKET_SIZE as usize {
                            x.remove(0);
                        }

                        x.push(node.clone());
                        break;
                    }
                    None => {
                        error!("Couldn't get bucket as mutable");
                    }
                }
            }
        }
    }

    fn _find_bucket_id(&mut self, own_id: P2PNodeId, id: P2PNodeId) -> Option<u16> {
        let dist = self.distance(&own_id, &id);
        let mut ret: i32 = -1;
        for i in 0..KEY_SIZE {
            if dist >= pow(2_i8.to_biguint().unwrap(), i as usize)
               && dist < pow(2_i8.to_biguint().unwrap(), (i as usize) + 1)
            {
                ret = i as i32;
            }
        }

        if ret == -1 {
            None
        } else {
            Some(ret as u16)
        }
    }

    fn closest_nodes(&self, _id: &P2PNodeId) -> Vec<P2PPeer> {
        let mut ret: Vec<P2PPeer> = Vec::with_capacity(KEY_SIZE as usize);
        let mut count = 0;
        for (_, bucket) in &self.buckets {
            //Fix later to do correctly
            if count < KEY_SIZE {
                for peer in bucket {
                    if count < KEY_SIZE {
                        ret.push(peer.clone());
                        count += 1;
                    } else {
                        break;
                    }
                }
            } else {
                break;
            }
        }

        ret
    }

    pub fn get_all_nodes(&self) -> Vec<P2PPeer> {
        let mut ret: Vec<P2PPeer> = Vec::new();
        for (_, bucket) in &self.buckets {
            for peer in bucket {
                ret.push(peer.clone());
            }
        }

        ret
    }
}

pub struct PeerStatistic {
    pub id: String,
    pub sent: u64,
    pub received: u64,
}

impl PeerStatistic {
    pub fn new(id: String, sent: u64, received: u64) -> PeerStatistic {
        PeerStatistic { id, sent, received }
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
}

impl TlsServer {
    fn new(server: TcpListener,
           server_cfg: Arc<ServerConfig>,
           client_cfg: Arc<ClientConfig>,
           own_id: P2PNodeId,
           event_log: Option<Sender<P2PEvent>>,
           self_peer: P2PPeer)
           -> TlsServer {
        TlsServer { server,
                    connections: HashMap::new(),
                    next_id: 2,
                    server_tls_config: server_cfg,
                    client_tls_config: client_cfg,
                    own_id,
                    event_log,
                    self_peer,
                    banned_peers: HashSet::new(), }
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

    pub fn get_peer_stats(&self) -> Vec<PeerStatistic> {
        let mut ret = vec![];
        for (_, ref conn) in &self.connections {
            match conn.peer {
                Some(ref x) => {
                    ret.push(PeerStatistic::new(x.id().to_string(),
                                                conn.get_messages_sent(),
                                                conn.get_messages_received()));
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

    fn accept(&mut self, poll: &mut Poll, self_id: P2PPeer) -> bool {
        match self.server.accept() {
            Ok((socket, addr)) => {
                debug!("Accepting new connection from {:?}", addr);
                self.log_event(P2PEvent::ConnectEvent(format!("{}", addr.ip()), addr.port()));

                let tls_session = ServerSession::new(&self.server_tls_config);
                let token = Token(self.next_id);
                self.next_id += 1;

                self.connections.insert(token,
                                        Connection::new(socket,
                                                        token,
                                                        Some(tls_session),
                                                        None,
                                                        false,
                                                        self.own_id.clone(),
                                                        self_id.clone(),
                                                        addr.ip().clone(),
                                                        addr.port().clone()));
                self.connections[&token].register(poll);

                true
            }
            Err(e) => {
                error!("encountered error while accepting connection; err={:?}", e);
                false
            }
        }
    }

    fn connect(&mut self, poll: &mut Poll, ip: IpAddr, port: u16, self_id: &P2PPeer) -> bool {
        for (_, ref conn) in &self.connections {
            if let Some(ref peer) = conn.peer {
                if peer.ip() == ip && peer.port() == port {
                    return false;
                }
            } else if conn.ip() == ip && conn.port() == port {
                return false;
            }
        }
        match TcpStream::connect(&SocketAddr::new(ip, port)) {
            Ok(x) => {
                let tls_session =
                    ClientSession::new(&self.client_tls_config,
                                       match DNSNameRef::try_from_ascii_str(&"node.concordium.com")
                                       {
                                           Ok(x) => x,
                                           Err(e) => panic!("The error is: {:?}", e),
                                       });

                let token = Token(self.next_id);
                self.next_id += 1;

                self.connections.insert(token,
                                        Connection::new(x,
                                                        token,
                                                        None,
                                                        Some(tls_session),
                                                        true,
                                                        self.own_id.clone(),
                                                        self_id.clone(),
                                                        ip,
                                                        port));
                self.connections[&token].register(poll);
                self.log_event(P2PEvent::ConnectEvent(ip.to_string(), port));
                debug!("Requesting handshake from new peer!");
                let self_peer = self.get_self_peer().clone();
                if let Some(ref mut conn) = self.connections.get_mut(&token) {
                    serialize_bytes(conn, &NetworkRequest::Handshake(self_peer).serialize())
                }
                true
            }
            Err(e) => {
                error!("encountered error while connecting; err={:?}", e);
                false
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
                  packet_queue: &mpsc::Sender<NetworkMessage>) {
        let token = event.token();
        if self.connections.contains_key(&token) {
            match self.connections.get_mut(&token) {
                Some(x) => x.ready(poll, event, &mut buckets, &packet_queue),
                None => error!("Couldn't get connections mutably"),
            };

            if self.connections[&token].is_closed() {
                self.connections.remove(&token);
            }
        }
    }

    fn cleanup_connections(&mut self, mut poll: &mut Poll) {
        for conn in self.connections.values_mut() {
            if conn.last_seen + 1200000 < common::get_current_stamp() {
                conn.close(&mut poll);
            }
        }

        let closed_ones: Vec<_> = self.connections
                                      .iter()
                                      .filter(|&(_, &ref v)| v.closing)
                                      .map(|(k, _)| k.clone())
                                      .collect();
        for closed in closed_ones {
            self.connections.remove(&closed);
        }

        //Kill banned connections
        for peer in &self.banned_peers {
            for conn in self.connections.values_mut() {
                match conn.peer.clone() {
                    Some(ref p) => {
                        if p == peer {
                            conn.close(&mut poll);
                        }
                    }
                    None => {}
                }
            }
        }
    }

    fn liveness_check(&mut self) {
        for conn in self.connections.values_mut() {
            if conn.last_seen + 300000 < common::get_current_stamp() {
                let self_peer = conn.get_self_peer().clone();
                serialize_bytes(conn, &NetworkRequest::Ping(self_peer).serialize());
            }
        }
    }
}

struct Connection {
    socket: TcpStream,
    token: Token,
    closing: bool,
    closed: bool,
    tls_server_session: Option<ServerSession>,
    tls_client_session: Option<ClientSession>,
    initiated_by_me: bool,
    own_id: P2PNodeId,
    peer: Option<P2PPeer>,
    currently_read: u32,
    peer_ip: IpAddr,
    peer_port: u16,
    expected_size: u32,
    pkt_buffer: Option<BytesMut>,
    last_seen: u64,
    self_peer: P2PPeer,
    messages_sent: u64,
    messages_received: u64,
}

impl Connection {
    fn new(socket: TcpStream,
           token: Token,
           tls_server_session: Option<ServerSession>,
           tls_client_session: Option<ClientSession>,
           initiated_by_me: bool,
           own_id: P2PNodeId,
           self_peer: P2PPeer,
           peer_ip: IpAddr,
           peer_port: u16)
           -> Connection {
        Connection { socket,
                     token,
                     closing: false,
                     closed: false,
                     tls_server_session,
                     tls_client_session,
                     initiated_by_me,
                     own_id,
                     peer: None,
                     currently_read: 0,
                     expected_size: 0,
                     pkt_buffer: None,
                     last_seen: common::get_current_stamp(),
                     self_peer: self_peer,
                     messages_received: 0,
                     messages_sent: 0,
                     peer_ip: peer_ip,
                     peer_port: peer_port, }
    }

    pub fn ip(&self) -> IpAddr {
        self.peer_ip.clone()
    }

    pub fn port(&self) -> u16 {
        self.peer_port.clone()
    }

    fn update_last_seen(&mut self) {
        self.last_seen = common::get_current_stamp();
    }

    fn append_buffer(&mut self, new_data: &[u8]) {
        if let Some(ref mut buf) = self.pkt_buffer {
            buf.reserve(new_data.len());
            buf.put_slice(new_data);
            self.currently_read += new_data.len() as u32;
        }
    }

    fn get_self_peer(&self) -> P2PPeer {
        self.self_peer.clone()
    }

    fn get_messages_received(&self) -> u64 {
        self.messages_received
    }

    fn get_messages_sent(&self) -> u64 {
        self.messages_sent
    }

    fn clear_buffer(&mut self) {
        if let Some(ref mut buf) = self.pkt_buffer {
            buf.clear();
        }
        self.pkt_buffer = None;
    }

    fn setup_buffer(&mut self) {
        self.pkt_buffer = Some(BytesMut::with_capacity(1024));
    }

    fn register(&self, poll: &mut Poll) {
        match poll.register(&self.socket,
                            self.token,
                            self.event_set(),
                            PollOpt::level() | PollOpt::oneshot())
        {
            Ok(_) => {}
            Err(e) => error!("Error registering socket with poll, got error: {:?}", e),
        }
    }

    fn reregister(&self, poll: &mut Poll) {
        match poll.reregister(&self.socket,
                              self.token,
                              self.event_set(),
                              PollOpt::level() | PollOpt::oneshot())
        {
            Ok(_) => {}
            Err(e) => error!("Error reregistering socket with poll, got error: {:?}", e),
        }
    }

    fn event_set(&self) -> Ready {
        let mut _rd = false;
        let mut _wr = false;
        match self.initiated_by_me {
            true => {
                _rd = match self.tls_client_session {
                    Some(ref x) => x.wants_read(),
                    _ => false,
                };
                _wr = match self.tls_client_session {
                    Some(ref x) => x.wants_write(),
                    _ => false,
                };
            }
            false => {
                _rd = match self.tls_server_session {
                    Some(ref x) => x.wants_read(),
                    _ => false,
                };
                _wr = match self.tls_server_session {
                    Some(ref x) => x.wants_write(),
                    _ => false,
                };
            }
        };

        //Don't trust it .. It's broken and inconsistent
        _wr = true;

        if _rd && _wr {
            Ready::readable() | Ready::writable()
        } else if _wr {
            Ready::writable()
        } else {
            Ready::readable()
        }
    }

    fn is_closed(&self) -> bool {
        self.closed
    }

    fn close(&mut self, poll: &mut Poll) {
        self.closing = true;
        match poll.deregister(&self.socket) {
            Ok(_) => {}
            Err(e) => error!("Error deregistering socket with poll, got error: {:?}", e),
        }

        match self.socket.shutdown(Shutdown::Both) {
            Ok(_) => {}
            Err(e) => error!("Error shutting down socket, got error: {:?}", e),
        }
    }

    fn ready(&mut self,
             poll: &mut Poll,
             ev: &Event,
             buckets: &mut Buckets,
             packets_queue: &mpsc::Sender<NetworkMessage>) {
        if ev.readiness().is_readable() {
            self.do_tls_read();
            self.try_plain_read(&packets_queue, buckets);
        }

        if ev.readiness().is_writable() {
            self.do_tls_write();
        }

        if self.closing {
            self.close(poll);
        }

        match self.initiated_by_me {
            true => {
                if self.closing && !match self.tls_client_session {
                    Some(ref x) => x.wants_read(),
                    _ => false,
                } {
                    let _ = self.socket.shutdown(Shutdown::Both);
                    self.closed = true;
                } else {
                    self.reregister(poll);
                }
            }
            false => {
                if self.closing && !match self.tls_server_session {
                    Some(ref x) => x.wants_read(),
                    _ => false,
                } {
                    let _ = self.socket.shutdown(Shutdown::Both);
                    self.closed = true;
                } else {
                    self.reregister(poll);
                }
            }
        };
    }

    fn do_tls_read(&mut self) {
        let rc = match self.initiated_by_me {
            true => {
                match self.tls_client_session {
                    Some(ref mut x) => x.read_tls(&mut self.socket),
                    None => Err(Error::new(ErrorKind::Other, "Couldn't find session!")),
                }
            }
            false => {
                match self.tls_server_session {
                    Some(ref mut x) => x.read_tls(&mut self.socket),
                    None => Err(Error::new(ErrorKind::Other, "Couldn't find session!")),
                }
            }
        };

        if rc.is_err() {
            let err = rc.unwrap_err();

            if let io::ErrorKind::WouldBlock = err.kind() {
                return;
            }

            error!("read error {:?}", err);
            self.closing = true;
            return;
        }

        if rc.unwrap() == 0 {
            debug!("eof");
            self.closing = true;
            return;
        }

        // Process newly-received TLS messages.
        let processed = match self.initiated_by_me {
            true => {
                match self.tls_client_session {
                    Some(ref mut x) => x.process_new_packets(),
                    None => Err(TLSError::General(String::from("Couldn't find session!"))),
                }
            }
            false => {
                match self.tls_server_session {
                    Some(ref mut x) => x.process_new_packets(),
                    None => Err(TLSError::General(String::from("Couldn't find session!"))),
                }
            }
        };

        if processed.is_err() {
            error!("cannot process packet: {:?}", processed);
            self.closing = true;
            return;
        }
    }

    fn try_plain_read(&mut self,
                      packets_queue: &mpsc::Sender<NetworkMessage>,
                      mut buckets: &mut Buckets) {
        // Read and process all available plaintext.
        let mut buf = Vec::new();

        let rc = match self.initiated_by_me {
            true => {
                match self.tls_client_session {
                    Some(ref mut x) => x.read_to_end(&mut buf),
                    None => Err(Error::new(ErrorKind::Other, "Couldn't find session!")),
                }
            }
            false => {
                match self.tls_server_session {
                    Some(ref mut x) => x.read_to_end(&mut buf),
                    None => Err(Error::new(ErrorKind::Other, "Couldn't find session!")),
                }
            }
        };

        if rc.is_err() {
            error!("plaintext read failed: {:?}", rc);
            self.closing = true;
            return;
        }

        if !buf.is_empty() {
            debug!("plaintext read {:?}", buf.len());
            self.incoming_plaintext(&packets_queue, &mut buckets, &buf);
        }
    }

    fn write_all(&mut self, bytes: &[u8]) -> Result<(), Error> {
        match self.initiated_by_me {
            true => {
                match self.tls_client_session {
                    Some(ref mut x) => {
                        self.messages_sent += 1;
                        x.write_all(bytes)
                    }
                    None => Err(Error::new(ErrorKind::Other, "Couldn't find session!")),
                }
            }
            false => {
                match self.tls_server_session {
                    Some(ref mut x) => {
                        self.messages_sent += 1;
                        x.write_all(bytes)
                    }
                    None => Err(Error::new(ErrorKind::Other, "Couldn't find session!")),
                }
            }
        }
    }

    fn process_complete_packet(&mut self,
                               buckets: &mut Buckets,
                               buf: &[u8],
                               packet_queue: &mpsc::Sender<NetworkMessage>) {
        let ref outer = NetworkMessage::deserialize(&buf);
        let self_peer = self.get_self_peer().clone();
        self.messages_received += 1;
        match outer {
            NetworkMessage::NetworkRequest(ref x, _, _) => {
                match x {
                    NetworkRequest::Ping(_) => {
                        //Respond with pong
                        debug!("Got request for ping");
                        self.update_last_seen();
                        serialize_bytes(self, &NetworkResponse::Pong(self_peer).serialize());
                    }
                    NetworkRequest::FindNode(_, x) => {
                        //Return list of nodes
                        debug!("Got request for FindNode");
                        self.update_last_seen();
                        let nodes = buckets.closest_nodes(x);
                        serialize_bytes(self,
                                        &NetworkResponse::FindNode(self_peer, nodes).serialize());
                    }
                    NetworkRequest::BanNode(_, _) => {
                        debug!("Got request for BanNode");
                        self.update_last_seen();
                        match packet_queue.send(outer.clone()) {
                            Ok(_) => {}
                            Err(e) => error!("Couldn't send to packet_queue, {:?}", e),
                        };
                    }
                    NetworkRequest::UnbanNode(_, _) => {
                        debug!("Got request for UnbanNode");
                        self.update_last_seen();
                        match packet_queue.send(outer.clone()) {
                            Ok(_) => {}
                            Err(e) => error!("Couldn't send to packet_queue, {:?}", e),
                        };
                    }
                    NetworkRequest::Handshake(sender) => {
                        debug!("Got request for Handshake");
                        self.update_last_seen();
                        serialize_bytes(self, &NetworkResponse::Handshake(self_peer).serialize());
                        self.peer = Some(sender.clone());
                        buckets.insert_into_bucket(sender, &self.own_id);
                        match packet_queue.send(outer.clone()) {
                            Ok(_) => {}
                            Err(e) => error!("Couldn't send to packet_queue, {:?}", e),
                        };
                    }
                    NetworkRequest::GetPeers(_) => {
                        debug!("Got request for GetPeers");
                        self.update_last_seen();
                        let nodes = buckets.get_all_nodes();
                        serialize_bytes(self,
                                        &NetworkResponse::PeerList(self_peer, nodes).serialize());
                    }
                }
            }
            NetworkMessage::NetworkResponse(ref x, _, _) => {
                match x {
                    NetworkResponse::FindNode(sender, peers) => {
                        debug!("Got response to FindNode");
                        self.update_last_seen();
                        //Process the received node list
                        for peer in peers.iter() {
                            buckets.insert_into_bucket(peer, &self.own_id);
                        }
                        buckets.insert_into_bucket(sender, &self.own_id);
                    }
                    NetworkResponse::Pong(sender) => {
                        debug!("Got response for ping");
                        self.update_last_seen();
                        //Note that node responded back
                        buckets.insert_into_bucket(sender, &self.own_id);
                    }
                    NetworkResponse::PeerList(sender, peers) => {
                        debug!("Got response to FindNode");
                        self.update_last_seen();
                        //Process the received node list
                        for peer in peers.iter() {
                            buckets.insert_into_bucket(peer, &self.own_id);
                        }
                        buckets.insert_into_bucket(sender, &self.own_id);
                        match packet_queue.send(outer.clone()) {
                            Ok(_) => {}
                            Err(e) => error!("Couldn't send to packet_queue, {:?}", e),
                        };
                    }
                    NetworkResponse::Handshake(peer) => {
                        debug!("Got response to Handshake");
                        self.update_last_seen();
                        self.peer = Some(peer.clone());
                        buckets.insert_into_bucket(peer, &self.own_id);
                    }
                }
            }
            NetworkMessage::NetworkPacket(ref x, _, _) => {
                match x {
                    NetworkPacket::DirectMessage(_, _, ref msg) => {
                        self.update_last_seen();
                        debug!("Received direct message of size {}", msg.len());
                        match packet_queue.send(outer.clone()) {
                            Ok(_) => {}
                            Err(e) => error!("Couldn't send to packet_queue, {:?}", e),
                        };
                    }
                    NetworkPacket::BroadcastedMessage(_, ref msg) => {
                        self.update_last_seen();
                        debug!("Received broadcast message of size {}", msg.len());
                        match packet_queue.send(outer.clone()) {
                            Ok(_) => {}
                            Err(e) => error!("Couldn't send to packet_queue, {:?}", e),
                        };
                    }
                }
            }
            NetworkMessage::UnknownMessage => {
                debug!("Unknown message received!");
                self.update_last_seen();
                debug!("Contents were: {:?}",
                       String::from_utf8(buf.to_vec()).unwrap());
            }
            NetworkMessage::InvalidMessage => {
                debug!("Invalid message received!");
                self.update_last_seen();
                debug!("Contents were: {:?}",
                       String::from_utf8(buf.to_vec()).unwrap());
            }
        }
    }

    fn incoming_plaintext(&mut self,
                          packets_queue: &mpsc::Sender<NetworkMessage>,
                          buckets: &mut Buckets,
                          buf: &[u8]) {
        debug!("Received plaintext");
        if self.expected_size > 0 && self.currently_read == self.expected_size {
            debug!("Completed packet with {} size", self.currently_read);
            self.expected_size = 0;
            self.currently_read = 0;
            let mut buffered = Vec::new();
            if let Some(ref mut buf) = self.pkt_buffer {
                buffered = buf[..].to_vec();
            }
            self.process_complete_packet(buckets, &buffered, &packets_queue);
            self.clear_buffer();
            self.incoming_plaintext(packets_queue, buckets, buf);
        } else if self.expected_size > 0
                  && buf.len() <= (self.expected_size as usize - self.currently_read as usize)
        {
            self.append_buffer(&buf);
            if self.expected_size == self.currently_read {
                debug!("Completed packet with {} size", self.currently_read);
                self.expected_size = 0;
                self.currently_read = 0;
                let mut buffered = Vec::new();
                if let Some(ref mut buf) = self.pkt_buffer {
                    buffered = buf[..].to_vec();
                }
                self.process_complete_packet(buckets, &buffered, &packets_queue);
                self.clear_buffer();
            }
        } else if self.expected_size > 0
                  && buf.len() > (self.expected_size as usize - self.currently_read as usize)
        {
            debug!("Got more buffer than needed");
            let to_take = self.expected_size - self.currently_read;
            self.append_buffer(&buf[..to_take as usize]);
            let mut buffered = Vec::new();
            if let Some(ref mut buf) = self.pkt_buffer {
                buffered = buf[..].to_vec();
            }
            self.process_complete_packet(buckets, &buffered, &packets_queue);
            self.clear_buffer();
            self.incoming_plaintext(&packets_queue, buckets, &buf[to_take as usize..]);
        } else if buf.len() >= 4 {
            debug!("Trying to read size");
            let _buf = &buf[..4].to_vec();
            let mut size_bytes = Cursor::new(_buf);
            self.expected_size = size_bytes.read_u32::<NetworkEndian>().unwrap();
            if self.expected_size > 268_435_456 {
                error!("Packet can't be bigger than 256MB");
                self.expected_size = 0;
                self.incoming_plaintext(&packets_queue, buckets, &buf[4..]);
            } else {
                self.setup_buffer();
                if buf.len() > 4 {
                    debug!("Got enough to read it...");
                    self.incoming_plaintext(&packets_queue, buckets, &buf[4..]);
                }
            }
        }
    }

    fn do_tls_write(&mut self) {
        let rc = match self.initiated_by_me {
            true => {
                match self.tls_client_session {
                    Some(ref mut x) => x.write_tls(&mut self.socket),
                    None => Err(Error::new(ErrorKind::Other, "Couldn't find session!")),
                }
            }
            false => {
                match self.tls_server_session {
                    Some(ref mut x) => x.write_tls(&mut self.socket),
                    None => Err(Error::new(ErrorKind::Other, "Couldn't find session!")),
                }
            }
        };

        if rc.is_err() {
            error!("write failed {:?}", rc);
            self.closing = true;
            return;
        }
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
    send_queue: Arc<Mutex<VecDeque<NetworkMessage>>>,
    ip: IpAddr,
    port: u16,
    incoming_pkts: mpsc::Sender<NetworkMessage>,
    event_log: Option<mpsc::Sender<P2PEvent>>,
    start_time: Timespec,
    total_sent: u64,
    total_received: u64,
}

fn serialize_bytes(conn: &mut Connection, pkt: &[u8]) {
    debug!("Serializing data to connection {} bytes", pkt.len());
    let mut size_vec = vec![];
    size_vec.write_u32::<NetworkEndian>(pkt.len() as u32)
            .unwrap();
    match conn.write_all(&size_vec[..]) {
        Ok(_) => {}
        Err(e) => error!("Couldn't write bytes to connection, {:?}", e),
    };
    match conn.write_all(pkt) {
        Ok(_) => {}
        Err(e) => error!("Couldn't write bytes to connection, {:?}", e),
    };
}

impl P2PNode {
    pub fn new(supplied_id: Option<String>,
               port: u16,
               pkt_queue: mpsc::Sender<NetworkMessage>,
               event_log: Option<mpsc::Sender<P2PEvent>>)
               -> P2PNode {
        let addr = format!("0.0.0.0:{}", port).parse().unwrap();

        debug!("Creating new P2PNode");

        //Retrieve IP address octets, format to IP and SHA256 hash it
        let ip = P2PNode::get_ip().unwrap();
        let octets = ip.octets();
        let ip_port = format!("{}.{}.{}.{}:{}",
                              octets[0], octets[1], octets[2], octets[3], port);
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
                utils::to_hex_string(utils::sha256(&format!("{}.{}", instant.sec, instant.nsec)))
            }
        };

        let _id = P2PNodeId::from_string(id.clone()).unwrap();

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

        //TLS Server config
        let mut server_conf = ServerConfig::new(NoClientAuth::new());
        server_conf.set_single_cert(vec![cert], private_key);
        //server_conf.key_log = Arc::new(rustls::KeyLogFile::new());

        let mut client_conf = ClientConfig::new();
        client_conf.dangerous()
                   .set_certificate_verifier(Arc::new(NoCertificateVerification {}));

        let tlsserv = TlsServer::new(server,
                                     Arc::new(server_conf),
                                     Arc::new(client_conf),
                                     _id.clone(),
                                     event_log.clone(),
                                     P2PPeer::from(_id.clone(), IpAddr::V4(ip), port));

        P2PNode { tls_server: Arc::new(Mutex::new(tlsserv)),
                  poll: Arc::new(Mutex::new(poll)),
                  id: _id,
                  buckets: Arc::new(Mutex::new(Buckets::new())),
                  send_queue: Arc::new(Mutex::new(VecDeque::new())),
                  ip: IpAddr::V4(ip),
                  port: port,
                  incoming_pkts: pkt_queue,
                  event_log,
                  start_time: time::get_time(),
                  total_sent: 0,
                  total_received: 0, }
    }

    pub fn spawn(&mut self) -> thread::JoinHandle<()> {
        let mut self_clone = self.clone();
        thread::spawn(move || {
                          let mut events = Events::with_capacity(1024);
                          loop {
                              self_clone.process(&mut events);
                          }
                      })
    }

    pub fn get_version(&self) -> String {
        ::VERSION.to_string()
    }

    pub fn connect(&mut self, ip: IpAddr, port: u16) -> bool {
        self.log_event(P2PEvent::InitiatingConnection(ip.clone(), port));
        match self.tls_server.lock() {
            Ok(mut x) => {
                match self.poll.lock() {
                    Ok(mut y) => {
                        return x.connect(&mut y, ip, port, &self.get_self_peer());
                    }
                    Err(e) => {
                        error!("Couldn't get lock on poll, {:?}", e);
                        return false;
                    }
                };
            }
            Err(e) => {
                error!("Couldn't get lock on tls_server, {:?}", e);
                return false;
            }
        };
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

    pub fn get_nodes(&self) -> Result<Vec<P2PPeer>, Error> {
        match self.buckets.lock() {
            Ok(x) => Ok(x.get_all_nodes()),
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

    pub fn process_messages(&mut self) {
        //Try to send queued messages first
        //Resend queue
        let mut resend_queue = VecDeque::new();
        {
            let mut send_q = self.send_queue.lock().unwrap();
            loop {
                debug!("Processing messages!");
                match send_q.pop_front() {
                    Some(x) => {
                        debug!("Got message to process!");
                        match x.clone() {
                            NetworkMessage::NetworkPacket(NetworkPacket::DirectMessage(me,
                                                                                       receiver,
                                                                                       msg),
                                                          _,
                                                          _) => {
                                //Look up connection associated with ID
                                match self.tls_server
                                          .lock()
                                          .unwrap()
                                          .find_connection(receiver.clone())
                                {
                                    Some(ref mut conn) => {
                                        self.total_sent += 1;
                                        serialize_bytes(conn, &NetworkPacket::DirectMessage(me, receiver,msg).serialize());
                                        debug!("Sent message");
                                    }
                                    _ => {
                                        resend_queue.push_back(x);
                                        debug!("Couldn't find connection, requeuing message!");
                                    }
                                };
                            }
                            NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(me,
                                                                                            msg),
                                                          _,
                                                          _) => {
                                let pkt = Arc::new(NetworkPacket::BroadcastedMessage(me, msg));
                                for (_, mut conn) in
                                    &mut self.tls_server.lock().unwrap().connections
                                {
                                    if conn.peer.is_some() {
                                        self.total_sent += 1;
                                        serialize_bytes(conn, &pkt.clone().serialize());
                                    }
                                }
                            }
                            NetworkMessage::NetworkRequest(NetworkRequest::BanNode(me, id),
                                                           _,
                                                           _) => {
                                let pkt = Arc::new(NetworkRequest::BanNode(me, id));
                                for (_, mut conn) in
                                    &mut self.tls_server.lock().unwrap().connections
                                {
                                    if conn.peer.is_some() {
                                        self.total_sent += 1;
                                        serialize_bytes(conn, &pkt.clone().serialize());
                                    }
                                }
                            }
                            NetworkMessage::NetworkRequest(NetworkRequest::UnbanNode(me, id),
                                                           _,
                                                           _) => {
                                let pkt = Arc::new(NetworkRequest::UnbanNode(me, id));
                                for (_, mut conn) in
                                    &mut self.tls_server.lock().unwrap().connections
                                {
                                    if conn.peer.is_some() {
                                        self.total_sent += 1;
                                        serialize_bytes(conn, &pkt.clone().serialize());
                                    }
                                }
                            }
                            NetworkMessage::NetworkRequest(NetworkRequest::GetPeers(me), _, _) => {
                                let pkt = Arc::new(NetworkRequest::GetPeers(me));
                                for (_, mut conn) in
                                    &mut self.tls_server.lock().unwrap().connections
                                {
                                    if conn.peer.is_some() {
                                        self.total_sent += 1;
                                        serialize_bytes(conn, &pkt.clone().serialize());
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                    None => {
                        send_q.append(&mut resend_queue);
                        break;
                    }
                }
            }
        }
    }

    pub fn send_message(&mut self, id: Option<P2PNodeId>, msg: &[u8], broadcast: bool) {
        debug!("Queueing message!");
        match broadcast {
            true => {
                self.send_queue.lock().unwrap().push_back(NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(self.get_self_peer(), msg.to_vec()), None, None));
            }
            false => {
                match id {
                    Some(x) => {
                        self.send_queue.lock().unwrap().push_back(NetworkMessage::NetworkPacket(NetworkPacket::DirectMessage(self.get_self_peer(), x, msg.to_vec()), None, None));
                    }
                    None => {
                        debug!("Invalid receiver ID for message!");
                    }
                }
            }
        }
    }

    pub fn send_ban(&mut self, id: P2PPeer) {
        self.send_queue
            .lock()
            .unwrap()
            .push_back(NetworkMessage::NetworkRequest(NetworkRequest::BanNode(self.get_self_peer(),
                                                                          id),
                                                  None,
                                                  None));
    }

    pub fn send_unban(&mut self, id: P2PPeer) {
        self.send_queue.lock().unwrap().push_back(NetworkMessage::NetworkRequest(NetworkRequest::UnbanNode(self.get_self_peer(),id), None, None));
    }

    pub fn send_get_peers(&mut self) {
        self.send_queue.lock().unwrap().push_back(NetworkMessage::NetworkRequest(NetworkRequest::GetPeers(self.get_self_peer()), None, None));
    }

    pub fn get_peer_stats(&self) -> Vec<PeerStatistic> {
        match self.tls_server.lock() {
            Ok(x) => x.get_peer_stats(),
            Err(e) => {
                info!("Couldn't lock for tls_server: {:?}", e);
                vec![]
            }
        }
    }

    pub fn get_ip() -> Option<Ipv4Addr> {
        let mut ip: Ipv4Addr = Ipv4Addr::new(127, 0, 0, 1);

        for adapter in get_if_addrs::get_if_addrs().unwrap() {
            match adapter.addr.ip() {
                V4(x) => {
                    if !x.is_loopback()
                       && !x.is_link_local()
                       && !x.is_multicast()
                       && !x.is_broadcast()
                    {
                        ip = x;
                    }
                }
                V6(_) => {
                    //Ignore for now
                }
            };
        }

        if ip == Ipv4Addr::new(127, 0, 0, 1) {
            None
        } else {
            Some(ip)
        }
    }

    fn get_self_peer(&self) -> P2PPeer {
        P2PPeer::from(self.get_own_id().clone(),
                      self.get_listening_ip().clone(),
                      self.get_listening_port())
    }

    pub fn get_total_sent(&self) -> u64 {
        self.total_sent
    }

    pub fn get_total_received(&self) -> u64 {
        self.total_received
    }

    pub fn ban_node(&mut self, peer: P2PPeer) -> bool {
        match self.tls_server.lock() {
            Ok(mut x) => x.ban_node(peer),
            Err(_) => false,
        }
    }

    pub fn unban_node(&mut self, peer: P2PPeer) -> bool {
        match self.tls_server.lock() {
            Ok(mut x) => x.unban_node(peer),
            Err(_) => false,
        }
    }

    pub fn process(&mut self, events: &mut Events) {
        self.poll
            .lock()
            .unwrap()
            .poll(events, Some(Duration::from_millis(500)))
            .unwrap();

        match self.tls_server.lock() {
            Ok(mut x) => x.liveness_check(),
            Err(e) => error!("Couldn't get lock on tls_server, {:?}", e),
        };

        for event in events.iter() {
            match event.token() {
                SERVER => {
                    debug!("Got new connection!");
                    if !self.tls_server
                            .lock()
                            .unwrap()
                            .accept(&mut self.poll.lock().unwrap(), self.get_self_peer().clone())
                    {
                        break;
                    }
                }
                _ => {
                    debug!("Got data!");
                    self.total_received += 1;
                    self.tls_server
                        .lock()
                        .unwrap()
                        .conn_event(&mut self.poll.lock().unwrap(),
                                    &event,
                                    &mut self.buckets.lock().unwrap(),
                                    &self.incoming_pkts);
                }
            }
        }

        self.tls_server
            .lock()
            .unwrap()
            .cleanup_connections(&mut self.poll.lock().unwrap());

        self.process_messages();
    }
}
