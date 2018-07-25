use mio::*;
use mio::net::TcpListener;
use mio::net::TcpStream;
use std::net::{SocketAddr, Shutdown};
use std::io::Error;
use std::io::Write;
use std::io::Read;
use std::io;
use std::io::ErrorKind;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::sync::mpsc::Sender;
use std::time::Duration;
use get_if_addrs;
use std::net::IpAddr;
use std::net::Ipv4Addr;
use std::net::IpAddr::{V4, V6};
use std::thread;
use utils;
use common::{NetworkMessage,NetworkRequest, NetworkPacket, NetworkResponse, P2PPeer, P2PNodeId};
use common;
use num_bigint::BigUint;
use num_bigint::ToBigUint;
use num_traits::pow;
use bytes::{BytesMut, BufMut};
use bincode::{serialize, deserialize};
use time;
use webpki::DNSNameRef;
use std::sync::{Arc,Mutex};
use rustls::{Session, ServerConfig, NoClientAuth, Certificate, PrivateKey, TLSError,
ServerSession, ClientSession, ClientConfig, ServerCertVerifier, RootCertStore, ServerCertVerified};
use std::sync::mpsc;

const SERVER: Token = Token(0);
const BUCKET_SIZE: u8 = 20;
const KEY_SIZE: u16 = 256;

pub enum P2PEvent {
    ConnectEvent(String, u16),
    DisconnectEvent(String),
    ReceivedMessageEvent(P2PNodeId),
    SentMessageEvent(P2PNodeId),
    InitiatingConnection(IpAddr,u16),
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

        Buckets {
            buckets,
        }
    }

    pub fn distance(&self, from: &P2PNodeId, to: &P2PNodeId) -> BigUint {
        from.get_id().clone() ^ to.get_id().clone()
    }

    pub fn insert_into_bucket(&mut self, node: &P2PPeer, own_id: &P2PNodeId) {
        let dist = self.distance(&own_id, &node.id());
        for i in 0..KEY_SIZE {
            if dist >= pow(2_i8.to_biguint().unwrap(),i as usize) && dist < pow(2_i8.to_biguint().unwrap(),(i as usize)+1) {
                let bucket = self.buckets.get_mut(&i).unwrap();
                //Check size
                if bucket.len() >= BUCKET_SIZE as usize {
                    //Check if front element is active
                    bucket.remove(0);
                }
                bucket.push(node.clone());
                break;
            }
        }
    }

    fn _find_bucket_id(&mut self, own_id: P2PNodeId, id: P2PNodeId) -> Option<u16> {
        let dist = self.distance(&own_id, &id);
        let mut ret : i32 = -1;
        for i in 0..KEY_SIZE {
            if dist >= pow(2_i8.to_biguint().unwrap(),i as usize) && dist < pow(2_i8.to_biguint().unwrap(),(i as usize)+1) {
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
        let mut ret : Vec<P2PPeer> = Vec::with_capacity(KEY_SIZE as usize);
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
        let mut ret : Vec<P2PPeer> = Vec::new();
        for (_, bucket) in &self.buckets {
            for peer in bucket {
                ret.push(peer.clone());
            }
        } 

        ret
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
    self_peer: P2PPeer
}

impl TlsServer {
    fn new(server: TcpListener, server_cfg: Arc<ServerConfig>, client_cfg: Arc<ClientConfig>, own_id: P2PNodeId, event_log: Option<Sender<P2PEvent>>, self_peer: P2PPeer) -> TlsServer {
        TlsServer {
            server,
            connections: HashMap::new(),
            next_id: 2,
            server_tls_config: server_cfg,
            client_tls_config: client_cfg,
            own_id,
            event_log,
            self_peer,
        }
    }

    fn log_event(&mut self, event: P2PEvent) {
        match self.event_log {
            Some(ref mut x) => { 
                x.send(event).unwrap(); 
            } ,
            _ => {}, 
        }
    }

    fn get_self_peer(&self) -> P2PPeer {
        self.self_peer.clone()
    }

    fn accept(&mut self, poll: &mut Poll, self_id: P2PPeer) -> bool {
        match self.server.accept() {
            Ok((socket, addr)) => {
                debug!("Accepting new connection from {:?}", addr);
                self.log_event(P2PEvent::ConnectEvent(format!("{}", addr.ip()), addr.port()));

                let tls_session = ServerSession::new(&self.server_tls_config);
                let token = Token(self.next_id);
                self.next_id += 1;

                self.connections.insert(token, Connection::new(socket, token, Some(tls_session), None, false, self.own_id.clone(), self_id.clone()));
                self.connections[&token].register(poll);

                true
            },
            Err(e) => {
                error!("encountered error while accepting connection; err={:?}", e);
                false
            }
        }
    }

    fn connect(&mut self, poll: &mut Poll, ip: IpAddr, port: u16, self_id: &P2PPeer) -> bool {
        match TcpStream::connect(&SocketAddr::new(ip, port)) {
            Ok(x) => {
                let tls_session = ClientSession::new(&self.client_tls_config, match DNSNameRef::try_from_ascii_str(&"node.concordium.com") {
                    Ok(x) => {
                        x
                    },
                    Err(e) => {
                        panic!("The error is: {:?}", e)
                    }
                });

                let token = Token(self.next_id);
                self.next_id += 1;

                self.connections.insert(token, Connection::new(x, token, None, Some(tls_session), true, self.own_id.clone(), self_id.clone()));
                self.connections[&token].register(poll);
                self.log_event(P2PEvent::ConnectEvent(ip.to_string(), port));
                debug!("Requesting handshake from new peer!");
                let self_peer = self.get_self_peer().clone();
                if let Some(ref mut conn) = self.connections.get_mut(&token) {
                    serialize_bytes( conn, NetworkRequest::Handshake(self_peer).serialize() )
                }
                true
            },
            Err(e) => {
                error!("encountered error while connecting; err={:?}", e);
                false
            }
        }
    } 

    fn find_connection(&mut self, id: P2PNodeId) -> Option<&mut Connection> {
        let mut tok = Token(0);
        for (token,mut connection) in &self.connections {
            match connection.peer {
                Some(ref x) => {
                    if x.id() == id {
                        tok = *token;
                    } else {
                        break;
                    }
                },
                _ => {
                    break;
                }
            }
        }

        if tok == Token(0) {
            None
        } else {
            Some(self.connections.get_mut(&tok).unwrap())
        }
    }

    fn conn_event(&mut self, poll: &mut Poll, event: &Event, mut buckets: &mut Buckets, packet_queue: &mpsc::Sender<NetworkMessage>) {
        let token = event.token();
        if self.connections.contains_key(&token) {
            self.connections
                .get_mut(&token)
                .unwrap()
                .ready(poll, event, &mut buckets, &packet_queue);

            if self.connections[&token].is_closed() {
                self.connections.remove(&token);
            }
        }
    }

    fn cleanup_connections(&mut self, mut poll: &mut Poll) {
        for conn in self.connections.values_mut() {
            if conn.last_seen + 300000 < common::get_current_stamp() {
                conn.close(&mut poll);
            }
        }

        let closed_ones :  Vec<_> = self.connections
                                        .iter()
                                        .filter(|&(_, &ref v)| v.closing)
                                        .map(|(k, _)| k.clone())
                                        .collect();
        for closed in closed_ones {
            self.connections.remove(&closed);
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
    currently_read: u64,
    expected_size: u64,
    pkt_buffer: Option<BytesMut>,
    last_seen: u64,
    self_peer: P2PPeer,
}

impl Connection {
    fn new(socket: TcpStream, token: Token, tls_server_session: Option<ServerSession>, tls_client_session: Option<ClientSession>, initiated_by_me: bool, own_id: P2PNodeId, self_peer: P2PPeer) -> Connection {
        Connection {
            socket,
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
        }
    }

    fn update_last_seen(&mut self) {
        self.last_seen = common::get_current_stamp();
    }

    fn append_buffer(&mut self, new_data: &[u8] ) {
        if let Some(ref mut buf) = self.pkt_buffer {
            buf.reserve(new_data.len());
            buf.put_slice(new_data);
            self.currently_read += new_data.len() as u64;
        }
    }

    fn get_self_peer(&self) -> P2PPeer {
        self.self_peer.clone()
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
                      PollOpt::level()| PollOpt::oneshot()) {
                            Ok(_) => {},
                            Err(e) => error!("Error registering socket with poll, got error: {:?}", e),
                      }
    }

    fn reregister(&self, poll: &mut Poll) {
        match poll.reregister(&self.socket,
                        self.token,
                        self.event_set(),
                        PollOpt::level() | PollOpt::oneshot()){
                            Ok(_) => {},
                            Err(e) => error!("Error reregistering socket with poll, got error: {:?}", e),
        }
    }

    fn event_set(&self) -> Ready {
        let mut _rd = false;
        let mut _wr = false;
        match self.initiated_by_me {
            true => {
                _rd = match self.tls_client_session {
                    Some(ref x) => {
                        x.wants_read()
                    },
                    _ => false,
                };
                _wr = match self.tls_client_session {
                    Some(ref x) => {
                        x.wants_write()
                    },
                    _ => false,
                };
            },
            false => {
                _rd = match self.tls_server_session {
                    Some(ref x) => {
                        x.wants_read()
                    },
                    _ => false,
                };
                _wr = match self.tls_server_session {
                    Some(ref x) => {
                        x.wants_write()
                    },
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
            Ok(_) => {},
            Err(e) => error!("Error deregistering socket with poll, got error: {:?}", e)
        }

        match self.socket.shutdown(Shutdown::Both) {
            Ok(_) => {},
            Err(e) => error!("Error shutting down socket, got error: {:?}", e)
        }
    }

    fn ready(&mut self, poll: &mut Poll, ev: &Event, buckets: &mut Buckets, packets_queue: &mpsc::Sender<NetworkMessage>) {
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
                    Some(ref x) => {
                        x.wants_read()
                    },
                    _ => false,
                } {
                    let _ = self.socket.shutdown(Shutdown::Both);
                    self.closed = true;
                } else {
                    self.reregister(poll);
                }
            },
            false => {
                if self.closing && !match self.tls_server_session {
                    Some(ref x) => {
                        x.wants_read()
                    },
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
                    Some(ref mut x) => {
                        x.read_tls(&mut self.socket)
                    },
                    None => {
                        Err(Error::new(ErrorKind::Other, "Couldn't find session!"))
                    }
                }
            },
            false => {
                match self.tls_server_session {
                    Some(ref mut x) => {
                        x.read_tls(&mut self.socket)
                    },
                    None => {
                        Err(Error::new(ErrorKind::Other, "Couldn't find session!"))
                    }
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
                    Some(ref mut x) => {
                        x.process_new_packets()
                    },
                    None => {
                        Err(TLSError::General(String::from("Couldn't find session!")))
                    }
                }
            },
            false => {
                match self.tls_server_session {
                    Some(ref mut x) => {
                        x.process_new_packets()
                    },
                    None => {
                        Err(TLSError::General(String::from("Couldn't find session!")))
                    }
                }
            }
        };

        if processed.is_err() {
            error!("cannot process packet: {:?}", processed);
            self.closing = true;
            return;
        }
    }

    fn try_plain_read(&mut self, packets_queue: &mpsc::Sender<NetworkMessage>, mut buckets: &mut Buckets) {
        // Read and process all available plaintext.
        let mut buf = Vec::new();

        let rc = match self.initiated_by_me {
            true => {
                match self.tls_client_session {
                    Some(ref mut x) => {
                        x.read_to_end(&mut buf)
                    },
                    None => {
                        Err(Error::new(ErrorKind::Other, "Couldn't find session!"))
                    }
                }
            },
            false => {
                match self.tls_server_session {
                    Some(ref mut x) => {
                        x.read_to_end(&mut buf)
                    },
                    None => {
                        Err(Error::new(ErrorKind::Other, "Couldn't find session!"))
                    }
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

    fn write_all(&mut self, bytes: &[u8]) -> Result<(), Error>{
        match self.initiated_by_me {
            true => {
                match self.tls_client_session {
                    Some(ref mut x) => {
                        x.write_all(bytes)
                    },
                    None => {
                        Err(Error::new(ErrorKind::Other, "Couldn't find session!"))
                    }
                }
            },
            false => {
                match self.tls_server_session {
                    Some(ref mut x) => {
                        x.write_all(bytes)
                    },
                    None => {
                        Err(Error::new(ErrorKind::Other, "Couldn't find session!"))
                    }
                }
            }
        }
    }

    fn process_complete_packet(&mut self, buckets: &mut Buckets, buf: &[u8], packet_queue: &mpsc::Sender<NetworkMessage>) {
        let ref outer =  NetworkMessage::deserialize(&String::from_utf8(buf.to_vec()).unwrap().trim_matches(char::from(0)));
        let self_peer = self.get_self_peer().clone();
        match outer {
            NetworkMessage::NetworkRequest(x,_,_) => {
                match x {
                    NetworkRequest::Ping(_) => {
                        //Respond with pong
                        debug!("Got request for ping");
                        self.update_last_seen();
                        serialize_bytes(self, NetworkResponse::Pong(self_peer).serialize());
                    },
                    NetworkRequest::FindNode(_, x) => {
                        //Return list of nodes
                        debug!("Got request for FindNode");
                        self.update_last_seen();
                        let nodes = buckets.closest_nodes(x);
                        serialize_bytes(self, NetworkResponse::FindNode(self_peer, nodes).serialize());
                    },
                    NetworkRequest::BanNode(_, _) => {
                        debug!("Got request for BanNode");
                        self.update_last_seen();
                        packet_queue.send(outer.clone()).unwrap();
                    },
                    NetworkRequest::UnbanNode(_, _) => {
                        debug!("Got request for UnbanNode");
                        self.update_last_seen();
                        packet_queue.send(outer.clone()).unwrap()
                    },
                    NetworkRequest::Handshake(sender) => {
                        debug!("Got request for Handshake");
                        self.update_last_seen();
                        serialize_bytes(self, NetworkResponse::Handshake(self_peer).serialize());
                        self.peer = Some(sender.clone());
                        buckets.insert_into_bucket(sender, &self.own_id);
                        packet_queue.send(outer.clone()).unwrap();
                    },
                    NetworkRequest::GetPeers(_) => {
                        debug!("Got request for GetPeers");
                        self.update_last_seen();
                        let nodes = buckets.get_all_nodes();
                        serialize_bytes(self, NetworkResponse::PeerList(self_peer, nodes).serialize());
                    }
                }
            },
            NetworkMessage::NetworkResponse(x, _,_) => {
                match x {
                    NetworkResponse::FindNode(sender, peers) => {
                        debug!("Got response to FindNode");
                        self.update_last_seen();
                        //Process the received node list
                        for peer in peers.iter() {
                            buckets.insert_into_bucket(peer, &self.own_id);
                        }
                        buckets.insert_into_bucket(sender, &self.own_id);
                    },
                    NetworkResponse::Pong(sender) => {
                        debug!("Got response for ping");
                        self.update_last_seen();
                        //Note that node responded back
                        buckets.insert_into_bucket(sender, &self.own_id);
                    },
                    NetworkResponse::PeerList(sender, peers) => {
                        debug!("Got response to FindNode");
                        self.update_last_seen();
                        //Process the received node list
                        for peer in peers.iter() {
                            buckets.insert_into_bucket(peer, &self.own_id);
                        }
                        buckets.insert_into_bucket(sender, &self.own_id);
                    },
                    NetworkResponse::Handshake(peer) => {
                        debug!("Got response to Handshake");
                        self.update_last_seen();
                        self.peer = Some(peer.clone());
                        buckets.insert_into_bucket(peer, &self.own_id);
                    }
                }
            },
            NetworkMessage::NetworkPacket(x, _,_ ) => {
                match x {
                    NetworkPacket::DirectMessage(_,_, msg) => {
                        self.update_last_seen();
                        debug!("Received {} of size {} as a direct message", msg, msg.len());
                        packet_queue.send(outer.clone()).unwrap();
                    },
                    NetworkPacket::BroadcastedMessage(_,msg) => {
                        self.update_last_seen();
                        debug!("Received {} of size {} as a broadcast message", msg, msg.len());
                        packet_queue.send(outer.clone()).unwrap();
                    }
                }
            },
            NetworkMessage::UnknownMessage => {
                debug!("Unknown message received!");
                self.update_last_seen();
                debug!("Contents were: {:?}", String::from_utf8(buf.to_vec()).unwrap());
            },
            NetworkMessage::InvalidMessage => {
                debug!("Invalid message received!");
                self.update_last_seen();
                debug!("Contents were: {:?}", String::from_utf8(buf.to_vec()).unwrap());
            }                                        
        }
    }

    fn incoming_plaintext(&mut self, packets_queue: &mpsc::Sender<NetworkMessage>, buckets: &mut Buckets, buf: &[u8]) {
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
            self.incoming_plaintext(packets_queue,buckets,buf);
        } else if self.expected_size > 0  && buf.len() <= (self.expected_size as usize-self.currently_read as usize) {
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
        } else if self.expected_size > 0 && buf.len() > (self.expected_size as usize-self.currently_read as usize) {
            debug!("Got more buffer than needed");
            let to_take = self.expected_size-self.currently_read;
            self.append_buffer(&buf[..to_take as usize]);
            let mut buffered = Vec::new();
            if let Some(ref mut buf) = self.pkt_buffer {
                buffered = buf[..].to_vec();
            }
            self.process_complete_packet(buckets, &buffered, &packets_queue);
            self.clear_buffer();
            self.incoming_plaintext(&packets_queue, buckets, &buf[to_take as usize..]);
        } else if buf.len() >= 8 {
            debug!("Trying to read size");
            self.expected_size = deserialize(&buf[..8]).unwrap();
            self.setup_buffer();
            if buf.len() > 8 {
                debug!("Got enough to read it...");
                self.incoming_plaintext(&packets_queue,buckets,&buf[8..]);
            } 
        }
    }

    fn do_tls_write(&mut self) {
        let rc = match self.initiated_by_me {
            true => {
                match self.tls_client_session {
                    Some(ref mut x) => {
                        x.write_tls(&mut self.socket)
                    },
                    None => {
                        Err(Error::new(ErrorKind::Other, "Couldn't find session!"))
                    }
                }
            },
            false => {
                match self.tls_server_session {
                    Some(ref mut x) => {
                        x.write_tls(&mut self.socket)
                    },
                    None => {
                        Err(Error::new(ErrorKind::Other, "Couldn't find session!"))
                    }
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
                          _ocsp: &[u8]) -> Result<ServerCertVerified, TLSError> {
        Ok(ServerCertVerified::assertion())
    }
}

#[derive(Clone)]
pub struct P2PNode {
    tls_server: Arc<Mutex<TlsServer>>,
    poll: Arc<Mutex<Poll>>,
    id: P2PNodeId,
    buckets: Arc<Mutex<Buckets>>,
    send_queue: Arc<Mutex<VecDeque<NetworkPacket>>>,
    ip: IpAddr,
    port: u16,
    incoming_pkts: mpsc::Sender<NetworkMessage>,
    event_log: Option<mpsc::Sender<P2PEvent>>
}

fn serialize_bytes(conn: &mut Connection, pkt: String ) {
    let serialized = pkt.as_bytes();
     debug!("Serializing data to connection {} bytes", serialized.len());
    conn.write_all(&serialize(&serialized.len()).unwrap()).unwrap();
    conn.write_all(serialized).unwrap();
}

impl P2PNode {
    pub fn new(supplied_id: Option<String>, port: u16, pkt_queue: mpsc::Sender<NetworkMessage>, event_log: Option<mpsc::Sender<P2PEvent>>) -> P2PNode {
        let addr = format!("0.0.0.0:{}", port).parse().unwrap();
        
        debug!("Creating new P2PNode");
        
        //Retrieve IP address octets, format to IP and SHA256 hash it
        let ip = P2PNode::get_ip().unwrap();
        let octets = ip.octets();
        let ip_port = format!("{}.{}.{}.{}:{}", octets[0], octets[1], octets[2], octets[3], port);
        debug!("Listening on {:?}", ip_port);

        let id = match supplied_id {
            Some(x) => {
                if x.chars().count() != 64 {
                    panic!("Incorrect ID specified.. Should be a sha256 value or 64 characters long!");
                }
                x
            },
            _ => {
                let instant = time::get_time();
                utils::to_hex_string(utils::sha256(&format!("{}.{}", instant.sec, instant.nsec)))
            }
        };

        let _id = P2PNodeId::from_string(id.clone());

        let poll = match Poll::new() {
            Ok(x) => x,
            _ => panic!("Couldn't create poll")
        };

        let server = match TcpListener::bind(&addr) {
            Ok(x) => x,
            _ => panic!("Couldn't listen on port!")
        };

        match poll.register(&server, SERVER, Ready::readable(), PollOpt::edge()) {
            Ok(_x) => (),
            _ => panic!("Couldn't register server with poll!")
        };

        //Generate key pair and cert
        let (cert, private_key) = match utils::generate_certificate(id) {
            Ok(x) => {
                match x.x509.to_der() {
                    Ok(der) => {
                        match x.private_key.private_key_to_der() {
                            Ok(private_part) => {
                                (Certificate(der), PrivateKey(private_part))
                            },
                            Err(e) => {
                                panic!("Couldn't convert certificate to DER! {:?}", e);
                            }
                        }
                    },
                    Err(e) => {
                        panic!("Couldn't convert certificate to DER! {:?}", e);
                    }
                }
            },
            Err(e) => {
                panic!("Couldn't create certificate! {:?}", e);
            }
        };

        //TLS Server config
        let mut server_conf = ServerConfig::new(NoClientAuth::new());
        server_conf.set_single_cert(vec![cert], private_key);
        //server_conf.key_log = Arc::new(rustls::KeyLogFile::new());

        let mut client_conf = ClientConfig::new();
        client_conf.dangerous().set_certificate_verifier(Arc::new(NoCertificateVerification {}));

        let tlsserv = TlsServer::new(server, Arc::new(server_conf), Arc::new(client_conf), _id.clone(), event_log.clone(), P2PPeer::from(_id.clone(), IpAddr::V4(ip), port));

        P2PNode {
            tls_server: Arc::new(Mutex::new(tlsserv)),
            poll: Arc::new(Mutex::new(poll)),
            id: _id,
            buckets: Arc::new(Mutex::new(Buckets::new())),
            send_queue: Arc::new(Mutex::new(VecDeque::new())),
            ip: IpAddr::V4(ip),
            port: port,
            incoming_pkts: pkt_queue,
            event_log,
        }

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

    pub fn connect(&mut self, ip: IpAddr, port: u16) {
        self.log_event(P2PEvent::InitiatingConnection(ip.clone(),port));
        self.tls_server.lock().unwrap().connect(&mut self.poll.lock().unwrap(), ip, port, &self.get_self_peer());
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

    fn log_event(&mut self, event: P2PEvent) {
        match self.event_log {
            Some(ref mut x) => { 
                x.send(event).unwrap(); 
            } ,
            _ => {}, 
        }
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
                            NetworkPacket::DirectMessage(me, receiver, msg) => {
                                //Look up connection associated with ID
                                match self.tls_server.lock().unwrap().find_connection(receiver.clone()) {
                                    Some(ref mut conn) => {
                                        serialize_bytes(conn, NetworkPacket::DirectMessage(me, receiver,msg).serialize());
                                        debug!("Sent message");
                                    },
                                    _ => {
                                        resend_queue.push_back(x);
                                        debug!("Couldn't find connection, requeuing message!");
                                    },
                                };                          
                            },
                            NetworkPacket::BroadcastedMessage(me, msg) => {
                                let pkt = Arc::new(NetworkPacket::BroadcastedMessage(me,msg));
                                for (_,mut conn) in &mut self.tls_server.lock().unwrap().connections {
                                    serialize_bytes(conn, pkt.clone().serialize());
                                }
                            }
                        }
                    },
                    None => {
                        send_q.append(&mut resend_queue);
                        break;
                    }
                }
            }
        }
        
    }

    pub fn send_message(&mut self, id: Option<P2PNodeId>, msg: String, broadcast: bool) {
        debug!("Queueing message!");
        match broadcast {
            true => {
                self.send_queue.lock().unwrap().push_back(NetworkPacket::BroadcastedMessage(self.get_self_peer(), msg));
            },
            false => {
                match id {
                    Some(x) => {
                        self.send_queue.lock().unwrap().push_back(NetworkPacket::DirectMessage(self.get_self_peer(), x, msg));
                    },
                    None => {
                        debug!("Invalid receiver ID for message!");
                    }
                }
            }
        }
        //self.send_queue.push_back(P2PMessage::new(id, msg));
    }

    pub fn get_ip() -> Option<Ipv4Addr>{
        let mut ip : Ipv4Addr = Ipv4Addr::new(127,0,0,1);

        for adapter in get_if_addrs::get_if_addrs().unwrap() {
            match adapter.addr.ip() {
                V4(x) => {
                    if !x.is_loopback() && !x.is_link_local() && !x.is_multicast() && !x.is_broadcast() {
                        ip = x;
                    }
                    
                },
                V6(_) => {
                    //Ignore for now
                }
            };
            
        }

        if ip == Ipv4Addr::new(127,0,0,1) {
            None
        } else {
            Some(ip)
        }
    }

    fn get_self_peer(&self) -> P2PPeer {
        P2PPeer::from(
            self.get_own_id().clone(),
            self.get_listening_ip().clone(),
            self.get_listening_port())
    }

    pub fn process(&mut self, events: &mut Events) {
        self.poll.lock().unwrap().poll(events, Some(Duration::from_millis(500))).unwrap();

        self.process_messages();

        self.tls_server.lock().unwrap().cleanup_connections(&mut self.poll.lock().unwrap());


        for event in events.iter() {
            match event.token() {
                SERVER => {
                    debug!("Got new connection!");
                    if !self.tls_server.lock().unwrap().accept(&mut self.poll.lock().unwrap(),self.get_self_peer().clone()) {
                        break;
                    }
                },
                _ => {
                    debug!("Got data!");
                    self.tls_server.lock().unwrap().conn_event(&mut self.poll.lock().unwrap(), &event, &mut self.buckets.lock().unwrap(), &self.incoming_pkts);
                }
            }

        }
    }
}