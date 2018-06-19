use mio::*;
use mio::net::TcpListener;
use mio::net::TcpStream;
use std::net::SocketAddr;
use std::io::Error;
use std::io::Write;
use std::io::Read;
use std::io;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::sync::mpsc::Receiver;
use std::sync::mpsc::Sender;
use std::time::Duration;
use get_if_addrs;
use std::net::IpAddr;
use std::net::Ipv4Addr;
use std::net::IpAddr::{V4, V6};
use utils;
use num_bigint::BigUint;
use num_traits::Num;
use num_bigint::ToBigUint;
use num_traits::pow;

const SERVER: Token = Token(0);
const BUCKET_SIZE: u8 = 20;
const KEY_SIZE: u16 = 256;

pub enum Message {
    Request(Request),
    Reply(Reply),
}

pub enum Request {
    Ping,
    FindNode(String),
}

pub enum Reply {
    Ping,
    FindNode(Vec<P2PPeer>),
}

#[derive(Debug, Clone)]
pub struct P2PPeer {
    ip: IpAddr,
    port: u16,
    id: P2PNodeId,
}

#[derive(Debug, Clone)]
pub struct P2PNodeId {
    id: BigUint,
}

impl P2PNodeId {
    pub fn from_string(sid: String) -> P2PNodeId {
        P2PNodeId {
            id: match BigUint::from_str_radix(&sid, 16) {
                Ok(x) => {
                    x
                },
                Err(e) => {
                    panic!("Couldn't convert ID from hex to biguint")
                }
            }
        }
    }

    pub fn to_string(self) -> String {
        format!("{:x}", self.id)
    }
}

impl P2PPeer {
    pub fn new(ip: IpAddr, port: u16) -> Self {
        let ip_port = format!("{}:{}", ip, port);
        P2PPeer {
            ip,
            port,
            id: P2PNodeId::from_string(utils::to_hex_string(utils::sha256(&ip_port))),
        }
    }
}

pub struct P2PMessage {
    pub token: Token,
    pub msg: Vec<u8>,
}

impl P2PMessage {
    pub fn new(token: Token, msg: Vec<u8>) -> Self {
        P2PMessage {
            token,
            msg
        }
    }
}

pub struct P2PNode {
    listener: TcpListener,
    poll: Poll,
    token_counter: usize,
    peers: HashMap<Token, TcpStream>,
    out_rx: Receiver<P2PMessage>,
    in_tx: Sender<P2PMessage>,
    id: BigUint,
    buckets: HashMap<u16, VecDeque<P2PPeer>>,
    map: HashMap<BigUint, Token>,
}

impl P2PNode {
    pub fn new(out_rx: Receiver<P2PMessage>, in_tx: Sender<P2PMessage>) -> Self {
        let addr = "0.0.0.0:8888".parse().unwrap();

        println!("Creating new P2PNode");

        //Retrieve IP address octets, format to IP and SHA256 hash it
        let octets = P2PNode::get_ip().unwrap().octets();
        let ip_port = format!("{}.{}.{}.{}:{}", octets[0], octets[1], octets[2], octets[3], 8888);
        println!("IP: {:?}", ip_port);

        let id = BigUint::from_str_radix(&utils::to_hex_string(utils::sha256(&ip_port)), 16).unwrap();
        println!("Got ID: {:x}", id);

        let poll = Poll::new().unwrap();

        let server = TcpListener::bind(&addr).unwrap();
        let res = poll.register(&server, SERVER, Ready::readable(), PollOpt::edge());

        let mut buckets = HashMap::new();
        for i in 0..KEY_SIZE {
            buckets.insert(i, VecDeque::new());
        }

        match res {
            Ok(_) => {
                P2PNode {
                    listener: server,
                    poll,
                    token_counter: 1,
                    peers: HashMap::new(),
                    out_rx,
                    in_tx,
                    id: id,
                    buckets,
                    map: HashMap::new(),
                }
            },
            Err(x) => {
                panic!("Couldn't create server! Error: {:?}", x)
            }
        }

        
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

    pub fn distance(&self, to: &P2PPeer) -> BigUint {
        self.id.clone() ^ to.id.id.clone()
    }

    pub fn insert_into_bucket(&mut self, node: P2PPeer) {
        let dist = self.distance(&node);
        for i in 0..KEY_SIZE {
            if dist >= pow(2_i8.to_biguint().unwrap(),i as usize) && dist < pow(2_i8.to_biguint().unwrap(),(i as usize)+1) {
                let bucket = self.buckets.get_mut(&i).unwrap();
                //Check size
                if bucket.len() >= BUCKET_SIZE as usize {
                    //Check if front element is active
                    bucket.pop_front();
                }
                bucket.push_back(node);
                break;
            }
        }
    }

    pub fn handle_request(&mut self, req: Request, src: P2PPeer) -> Reply {
        self.insert_into_bucket(src);
        match req {
            Request::Ping => {
                Reply::Ping
            },

            Request::FindNode(id) => {
                Reply::FindNode(self.closest_nodes(id))
            }
        }
    }

    pub fn closest_nodes(&self, id: String) -> Vec<P2PPeer> {
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

    pub fn connect(&mut self, peer: P2PPeer) -> Result<Token, Error> {
        let stream = TcpStream::connect(&SocketAddr::new(peer.ip, peer.port));
        match stream {
            Ok(x) => {
                let token = Token(self.token_counter);
                let res = self.poll.register(&x, token, Ready::readable() | Ready::writable(), PollOpt::edge());
                match res {
                    Ok(_) => {
                        self.peers.insert(token, x);
                        self.insert_into_bucket(peer);
                        println!("Inserting connection");
                        self.token_counter += 1;
                        Ok(token)
                    },
                    Err(x) => {
                        Err(x)
                    }
                }
            },
            Err(e) => {
                Err(e)
            }
        }
        
        
    }

    pub fn process(&mut self, events: &mut Events, channel: &mut Receiver<P2PPeer>) {
        loop {
            //Check if we have messages to receive
            match channel.try_recv() {
                Ok(x) => {
                    match self.connect(x) {
                        Ok(_) => {

                        },
                        Err(e) => {
                            println!("Error connecting: {}", e);
                        }
                    }
                },
                _ => {

                }
            }

            //Try and write out messages
            match self.out_rx.try_recv() {
                Ok(x) => {
                    let peer = self.peers.get_mut(&x.token).unwrap();
                    match peer.write(&x.msg) {
                        Ok(_) => {
                            
                        },
                        Err(_) => {
                            println!("Couldn't write message out to {}", peer.peer_addr().unwrap());
                        }
                    };
                },
                _ => {}
            }

            self.poll.poll(events, Some(Duration::from_millis(500))).unwrap();

            for event in events.iter() {
                
                match event.token() {
                    SERVER => {
                        loop {
                            match self.listener.accept() {
                                Ok((mut socket, _)) => {
                                    let token = Token(self.token_counter);
                                    println!("Accepting connection from {}", socket.peer_addr().unwrap());
                                    self.poll.register(&socket, token, Ready::readable() | Ready::writable(), PollOpt::edge()).unwrap();

                                    self.peers.insert(token, socket);

                                    self.token_counter += 1;
                                }
                                Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
                                    break;
                                }
                                e => panic!("err={:?}", e),
                            }
                            
                        }
                    }
                    x => {
                        loop {
                            let mut buf = [0; 256];
                            let y = x;
                            match self.peers.get_mut(&x).unwrap().read(&mut buf) {
                                Ok(0) => {
                                    println!("Closing connection!");
                                    self.peers.remove(&x);
                                    break;
                                }
                                Ok(_) => {
                                    match self.in_tx.send(P2PMessage::new(y, buf.to_vec())) {
                                        Ok(_) => {

                                        },
                                        Err(e) => println!("Error sending message into channel {}", e)
                                    };
                                    
                                },
                                Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
                                    break;
                                }
                                e => panic!("err={:?}", e),
                            }
                        }
                    }
                }
            }
        }
    }
}