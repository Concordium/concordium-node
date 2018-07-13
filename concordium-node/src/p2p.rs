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
use std::sync::mpsc::Receiver;
use std::sync::mpsc::Sender;
use std::time::Duration;
use get_if_addrs;
use std::net::IpAddr;
use std::net::Ipv4Addr;
use std::net::IpAddr::{V4, V6};
use std::str::FromStr;
use utils;
use common::{NetworkMessage,NetworkRequest, NetworkPacket, NetworkResponse, P2PPeer, P2PNodeId};
use num_bigint::BigUint;
use num_traits::Num;
use num_bigint::ToBigUint;
use num_traits::pow;
use bytes::{Bytes, BytesMut, Buf, BufMut};
use bincode::{serialize, deserialize};
use time;
use rustls;
use webpki::DNSNameRef;
use std::sync::Arc;
use rustls::{Session, ServerConfig, NoClientAuth, Certificate, PrivateKey, TLSError,
ServerSession, ClientSession, ClientConfig, ServerCertVerifier, RootCertStore, ServerCertVerified};

use env_logger;
#[macro_use]
use log;

const SERVER: Token = Token(0);
const BUCKET_SIZE: u8 = 20;
const KEY_SIZE: u16 = 256;

// REMOVE WHEN REPLACED!!!
pub fn get_self_peer() -> P2PPeer {
    P2PPeer::new(IpAddr::from_str("10.10.10.10").unwrap(), 9999)
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

    pub fn insert_into_bucket(&mut self, node: P2PPeer, own_id: &P2PNodeId) {
        let dist = self.distance(&own_id, &node.id());
        for i in 0..KEY_SIZE {
            if dist >= pow(2_i8.to_biguint().unwrap(),i as usize) && dist < pow(2_i8.to_biguint().unwrap(),(i as usize)+1) {
                let bucket = self.buckets.get_mut(&i).unwrap();
                //Check size
                if bucket.len() >= BUCKET_SIZE as usize {
                    //Check if front element is active
                    bucket.remove(0);
                }
                bucket.push(node);
                break;
            }
        }
    }

    pub fn find_bucket_id(&mut self, own_id: P2PNodeId, id: P2PNodeId) -> Option<u16> {
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

    pub fn closest_nodes(&self, id: P2PNodeId) -> Vec<P2PPeer> {
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
}

struct TlsServer {
    server: TcpListener,
    connections: HashMap<Token, Connection>,
    next_id: usize,
    server_tls_config: Arc<ServerConfig>,
    client_tls_config: Arc<ClientConfig>,
    own_id: P2PNodeId,
}

impl TlsServer {
    fn new(server: TcpListener, server_cfg: Arc<ServerConfig>, client_cfg: Arc<ClientConfig>, own_id: P2PNodeId) -> TlsServer {
        TlsServer {
            server,
            connections: HashMap::new(),
            next_id: 2,
            server_tls_config: server_cfg,
            client_tls_config: client_cfg,
            own_id,
        }
    }

    fn accept(&mut self, poll: &mut Poll) -> bool {
        match self.server.accept() {
            Ok((socket, addr)) => {
                debug!("Accepting new connection from {:?}", addr);

                let tls_session = ServerSession::new(&self.server_tls_config);
                let token = Token(self.next_id);
                self.next_id += 1;

                self.connections.insert(token, Connection::new(socket, token, Some(tls_session), None, false, self.own_id.clone()));
                self.connections[&token].register(poll);

                true
            },
            Err(e) => {
                println!("encountered error while accepting connection; err={:?}", e);
                false
            }
        }
    }

    fn connect(&mut self, poll: &mut Poll, ip: IpAddr, port: u16) -> bool {
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

                self.connections.insert(token, Connection::new(x, token, None, Some(tls_session), true, self.own_id.clone()));
                self.connections[&token].register(poll);

                true

            },
            Err(e) => {
                println!("encountered error while connecting; err={:?}", e);
                false
            }
        }
    } 

    fn conn_event(&mut self, poll: &mut Poll, event: &Event, mut buckets: &mut Buckets) {
        let token = event.token();
        if self.connections.contains_key(&token) {
            self.connections
                .get_mut(&token)
                .unwrap()
                .ready(poll, event, &mut buckets);

            if self.connections[&token].is_closed() {
                self.connections.remove(&token);
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
}

impl Connection {
    fn new(socket: TcpStream, token: Token, tls_server_session: Option<ServerSession>, tls_client_session: Option<ClientSession>, initiated_by_me: bool, own_id: P2PNodeId) -> Connection {
        Connection {
            socket,
            token,
            closing: false,
            closed: false,
            tls_server_session,
            tls_client_session,
            initiated_by_me,
            own_id
        }
    }

    fn register(&self, poll: &mut Poll) {
        poll.register(&self.socket,
                      self.token,
                      self.event_set(),
                      PollOpt::level() | PollOpt::oneshot())
            .unwrap();
    }

    fn reregister(&self, poll: &mut Poll) {
        poll.reregister(&self.socket,
                        self.token,
                        self.event_set(),
                        PollOpt::level() | PollOpt::oneshot())
            .unwrap();
    }

    fn event_set(&self) -> Ready {
        let mut rd = false;
        let mut wr = false;
        match self.initiated_by_me {
            true => {
                rd = match self.tls_client_session {
                    Some(ref x) => {
                        x.wants_read()
                    },
                    _ => false,
                };
                wr = match self.tls_client_session {
                    Some(ref x) => {
                        x.wants_write()
                    },
                    _ => false,
                };
            },
            false => {
                rd = match self.tls_server_session {
                    Some(ref x) => {
                        x.wants_read()
                    },
                    _ => false,
                };
                wr = match self.tls_server_session {
                    Some(ref x) => {
                        x.wants_write()
                    },
                    _ => false,
                };
            }
        };

        if rd && wr {
            Ready::readable() | Ready::writable()
        } else if wr {
            Ready::writable()
        } else {
            Ready::readable()
        }
    }

    fn is_closed(&self) -> bool {
        self.closed
    }

    fn ready(&mut self, poll: &mut Poll, ev: &Event, mut buckets: &mut Buckets) {
        if ev.readiness().is_readable() {
            self.do_tls_read();
            self.try_plain_read(buckets);
        }

        if ev.readiness().is_writable() {
            self.do_tls_write();
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

    fn try_plain_read(&mut self, mut buckets: &mut Buckets) {
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
            self.incoming_plaintext(&mut buckets, &buf);
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

    fn incoming_plaintext(&mut self, buckets: &mut Buckets, buf: &[u8]) {
        info!("Received plaintext!"); 
        match NetworkMessage::deserialize(&String::from_utf8(buf.to_vec()).unwrap().trim_matches(char::from(0))) {
            NetworkMessage::NetworkRequest(x) => {
                match x {
                    NetworkRequest::Ping(sender) => {
                        //Respond with pong
                        info!("Got request for ping");
                        self.write_all(NetworkResponse::Pong(get_self_peer()).serialize().as_bytes());
                    },
                    NetworkRequest::FindNode(sender, x) => {
                        //Return list of nodes
                        info!("Got request for FindNode");
                        let nodes = buckets.closest_nodes(x);
                        self.write_all(NetworkResponse::FindNode(get_self_peer(), nodes).serialize().as_bytes());
                    },
                    NetworkRequest::BanNode(sender, x) => {
                        info!("Got request for BanNode");
                        self.write_all(NetworkResponse::BanNode(get_self_peer(), true).serialize().as_bytes());
                    }
                }
            },
            NetworkMessage::NetworkResponse(x) => {
                match x {
                    NetworkResponse::FindNode(sender, peers) => {
                        info!("Got response to FindNode");
                        //Process the received node list
                        for peer in peers.iter() {
                            buckets.insert_into_bucket(peer.clone(), &self.own_id);
                        }
                        buckets.insert_into_bucket(sender, &self.own_id);
                    },
                    NetworkResponse::Pong(sender) => {
                        info!("Got response for ping");
                        //Note that node responded back
                        buckets.insert_into_bucket(sender, &self.own_id);
                    },
                    NetworkResponse::BanNode(sender, ok) => {
                        info!("Got response to BanNode");
                    }
                }
            },
            NetworkMessage::NetworkPacket(x) => {
                match x {
                    NetworkPacket::DirectMessage(sender, msg) => {
                        info!("Got {} size direct message", msg.len());
                    },
                    NetworkPacket::BroadcastedMessage(sender,msg) => {
                        info!("Got {} size broadcasted packet", msg.len());
                    }
                }
            },
            NetworkMessage::UnknownMessage => {
                info!("Unknown message received!");
                info!("Contents were: {:?}", String::from_utf8(buf.to_vec()).unwrap());
            },
            NetworkMessage::InvalidMessage => {
                info!("Invalid message received!");
                info!("Contents were: {:?}", String::from_utf8(buf.to_vec()).unwrap());
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

pub struct P2PMessage {
    pub addr: P2PNodeId,
    pub msg: Vec<u8>,
}

impl P2PMessage {
    pub fn new(addr: P2PNodeId, msg: Vec<u8>) -> Self {
        P2PMessage {
            addr,
            msg
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


pub struct P2PNode {
    tls_server: TlsServer,
    poll: Poll,
    id: P2PNodeId,
    buckets: Buckets,
    map: HashMap<BigUint, Token>,
    send_queue: VecDeque<P2PMessage>,
}

impl P2PNode {
    pub fn new(supplied_id: Option<String>, port: u16) -> P2PNode {
        let addr = format!("0.0.0.0:{}", port).parse().unwrap();
        
        info!("Creating new P2PNode");
        
        //Retrieve IP address octets, format to IP and SHA256 hash it
        let octets = P2PNode::get_ip().unwrap().octets();
        let ip_port = format!("{}.{}.{}.{}:{}", octets[0], octets[1], octets[2], octets[3], port);
        info!("Listening on {:?}", ip_port);

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
        println!("Got ID: {}", _id.clone().to_string());

        let poll = match Poll::new() {
            Ok(x) => x,
            _ => panic!("Couldn't create poll")
        };

        let server = match TcpListener::bind(&addr) {
            Ok(x) => x,
            _ => panic!("Couldn't listen on port!")
        };

        match poll.register(&server, SERVER, Ready::readable(), PollOpt::edge()) {
            Ok(x) => (),
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

        let tlsserv = TlsServer::new(server, Arc::new(server_conf), Arc::new(client_conf), _id.clone());

        P2PNode {
            tls_server: tlsserv,
            poll,
            id: _id,
            buckets: Buckets::new(),
            map: HashMap::new(),
            send_queue: VecDeque::new(),
        }

    }

    pub fn connect(&mut self, ip: IpAddr, port: u16) {
        self.tls_server.connect(&mut self.poll, ip, port);
    }

    pub fn process_messages(&mut self) {
        //Try to send queued messages first
        loop {
            match self.send_queue.pop_front() {
                Some(x) => {
                    //We have messages to send
                    info!("Trying to send queued message to {:?}", x.addr);
                    match self.buckets.find_bucket_id(self.id.clone(), x.addr.clone()) {
                        Some(y) => {
                            //We found the appropriate bucket where the peer should be
                            let bucket = &self.buckets.buckets[&y];
                            let node = P2PPeer::from(x.addr, "127.0.0.1".parse().unwrap(), 8888);
                            if bucket.contains(&node) {
                                //Send directly to peer
                                match bucket.binary_search(&node) {
                                    Ok(idx) => {
                                        let peer = &bucket[idx];
                                        //let mut socket = &self.peers[self.map.get(peer.id().get_id()).unwrap()].handle;
                                        //socket.write(&x.msg).unwrap();
                                    },
                                    Err(_) => {
                                        panic!("Should never happen ..");
                                    }
                                }
                            } else {

                            }
                        },
                        None => {
                            //Send find_node to neighbors
                            //Let's just pick one at first
                            let node = P2PPeer::from(x.addr.clone(), "127.0.0.1".parse().unwrap(), 8888);
                            let nodes = &self.buckets.closest_nodes(node.id());
                            if nodes.len() > 0 {
                                let neighbor = &self.buckets.closest_nodes(node.id())[0];
                                //let mut socket = &self.peers[self.map.get(neighbor.id().get_id()).unwrap()].handle;
                                //socket.write(&NetworkRequest::FindNode(get_self_peer(), node.id()).serialize().as_bytes()).unwrap();
                            } else {
                                info!("Don't have any friends, so not sending message :(");
                            }

                            //Readd message
                            info!("Requeuing message for {:?}!", x.addr);
                            self.send_queue.push_back(x);
                        }
                    }
                },
                None => {
                    break;
                }
            }
        }
    }

    pub fn send_message(&mut self, id: P2PNodeId, msg: Vec<u8>) {
        info!("Queueing message!");
        self.send_queue.push_back(P2PMessage::new(id, msg));
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

    pub fn process(&mut self, events: &mut Events) {
        loop {
            self.poll.poll(events, Some(Duration::from_millis(500))).unwrap();

            self.send_message(P2PNodeId::from_string(String::from("c19cd000746763871fae95fcdd4508dfd8bf725f9767be68c3038df183527bb2")), String::from("Hello world!").as_bytes().to_vec());

            self.process_messages();

            for event in events.iter() {
                match event.token() {
                    SERVER => {
                        info!("Got new connection!");
                        if !self.tls_server.accept(&mut self.poll) {
                            break;
                        }
                    },
                    _ => {
                        info!("Got data!");
                        self.tls_server.conn_event(&mut self.poll, &event, &mut self.buckets);
                    }
                }
            }
        }
    }
}