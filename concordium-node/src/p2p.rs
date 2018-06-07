use mio::*;
use mio::net::TcpListener;
use mio::net::TcpStream;
use std::net::SocketAddr;
use std::io::Error;
use std::io::Write;
use std::io::Read;
use std::io;
use std::collections::HashMap;
use std::sync::mpsc::TryRecvError;
use std::sync::mpsc::Receiver;
use std::sync::mpsc::Sender;
use std::time::Duration;

const SERVER: Token = Token(0);

pub struct P2PPeer {
    socket: TcpStream,
}

impl P2PPeer {
    pub fn new(socket: TcpStream) -> Self {
        P2PPeer {
            socket,
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
    peers: HashMap<Token, P2PPeer>,
    out_rx: Receiver<P2PMessage>,
    in_tx: Sender<P2PMessage>,
}

impl P2PNode {
    pub fn new(out_rx: Receiver<P2PMessage>, in_tx: Sender<P2PMessage>) -> Self {
        let addr = "127.0.0.1:8888".parse().unwrap();
        let poll = Poll::new().unwrap();

        let server = TcpListener::bind(&addr).unwrap();
        let res = poll.register(&server, SERVER, Ready::readable(), PollOpt::edge());

        match res {
            Ok(_) => {
                P2PNode {
                    listener: server,
                    poll,
                    token_counter: 1,
                    peers: HashMap::new(),
                    out_rx,
                    in_tx,
                }
            },
            Err(x) => {
                panic!("Couldn't create server! Error: {:?}", x)
            }
        }

        
    }

    pub fn connect(&mut self, addr: SocketAddr) -> Result<Token, Error> {
        let stream = TcpStream::connect(&addr);
        match stream {
            Ok(x) => {
                let token = Token(self.token_counter);
                let res = self.poll.register(&x, token, Ready::readable() | Ready::writable(), PollOpt::edge());
                match res {
                    Ok(_) => {
                        self.peers.insert(token, P2PPeer::new(x));
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

    pub fn process(&mut self, events: &mut Events, channel: &mut Receiver<SocketAddr>) {
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
                    match peer.socket.write(&x.msg) {
                        Ok(x) => {
                            
                        },
                        Err(_) => {
                            println!("Couldn't write message out to {}", peer.socket.peer_addr().unwrap());
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

                                    self.peers.insert(token, P2PPeer::new(socket));

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
                            match self.peers.get_mut(&x).unwrap().socket.read(&mut buf) {
                                Ok(0) => {
                                    println!("Closing connection!");
                                    self.peers.remove(&x);
                                    break;
                                }
                                Ok(_) => {
                                    match self.in_tx.send(P2PMessage::new(y, buf.to_vec())) {
                                        Ok(y) => {

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