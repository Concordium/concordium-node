use mio::*;
use mio::net::TcpListener;
use mio::net::TcpStream;
use std::net::SocketAddr;
use std::io::Error;
use std::io::Write;
use std::io::Read;
use std::io;
use std::collections::HashMap;
use std::thread;
use std::sync::mpsc::Receiver;
use std::sync::mpsc::Sender;
use std::sync::mpsc::TryRecvError;
use std::sync::mpsc;
use std::time::Duration;


const SERVER: Token = Token(0);

struct P2PPeer {
    socket: TcpStream,
    rx: Receiver<[u8; 256]>,
    tx: Sender<[u8; 256]>,
}

impl P2PPeer {
    fn new(socket: TcpStream, rx: Receiver<[u8; 256]>, tx: Sender<[u8; 256]>) -> Self {
        P2PPeer {
            socket,
            rx,
            tx
        }
    }
}

struct P2PNode {
    listener: TcpListener,
    poll: Poll,
    token_counter: usize,
    peers: HashMap<Token, P2PPeer>,
}

impl P2PNode {
    fn new() -> Self {
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
                }
            },
            Err(x) => {
                panic!("Couldn't create server! Error: {:?}", x)
            }
        }

        
    }

    fn connect(&mut self, addr: SocketAddr) -> Result<(), Error> {
        let stream = TcpStream::connect(&addr).unwrap();
        let token = Token(self.token_counter);
        let res = self.poll.register(&stream, token, Ready::readable() | Ready::writable(), PollOpt::edge());
        match res {
            Ok(x) => {
                let (tx, rx) = mpsc::channel();
                self.peers.insert(token, P2PPeer::new(stream, rx, tx));
                println!("Inserting connection");
                self.token_counter += 1;
                Ok(x)
            },
            Err(x) => {
                Err(x)
            }
        }
    }

    fn send(&mut self, receiver: Token, msg: [u8; 256]) {
        let peer = self.peers.get_mut(&receiver).unwrap();
        peer.tx.send(msg).unwrap();
    }

    fn receive(&mut self, sender: Token) -> Result<[u8; 256], TryRecvError>{
        let peer = self.peers.get_mut(&sender).unwrap();
        match peer.rx.try_recv() {
            Ok(x) => {
                Ok(x)
            },
            Err(e) => {
                Err(e)
            }
        }
    }

    fn process(&mut self, events: &mut Events, channel: &mut Receiver<SocketAddr>) {
        loop {
            //Check if we have messages to receive
            match channel.try_recv() {
                Ok(x) => {
                    self.connect(x).unwrap();
                },
                _ => {

                }
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

                                    let (tx, rx) = mpsc::channel();

                                    self.peers.insert(token, P2PPeer::new(socket, rx, tx));

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
                                Ok(x) => {
                                    //let socket = self.peers.get_mut(&token);
                                    let peer = self.peers.get_mut(&y).unwrap();
                                    peer.tx.send(buf).unwrap();
                                    
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

fn main() {
    let mut node = P2PNode::new();

    let (connect_send, mut connect_recv) = mpsc::channel();

    let th = thread::spawn(move || {
        let mut events = Events::with_capacity(1024);
        node.process(&mut events, &mut connect_recv);
    });

    th.join().unwrap();

}