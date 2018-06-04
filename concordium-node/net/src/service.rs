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
use std::sync::mpsc;


const SERVER: Token = Token(0);

struct P2PNode {
    listener: TcpListener,
    poll: Poll,
    token_counter: usize,
    peers: HashMap<Token, TcpStream>,
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
        let res = self.poll.register(&stream, Token::from(self.token_counter), Ready::readable() | Ready::writable(), PollOpt::edge());
        match res {
            Ok(x) => {
                self.peers.insert(Token::from(self.token_counter), stream);
                println!("Inserting connection");
                self.token_counter += 1;
                Ok(x)
            },
            Err(x) => {
                Err(x)
            }
        }
    }

    fn process(&mut self, events: &mut Events, channel: &mut Receiver<SocketAddr>) {
        let mut buf = [0; 256];
        loop {
            //Check if we have messages to receive
            match channel.try_recv() {
                Ok(x) => {
                    self.connect(x).unwrap();
                },
                _ => {

                }
            }

            self.poll.poll(events, None).unwrap();

            for event in events.iter() {
                match event.token() {
                    SERVER => {
                        loop {
                            match self.listener.accept() {
                                Ok((socket, _)) => {
                                    let token = Token::from(self.token_counter);
                                    println!("Accepting connection from {}", socket.peer_addr().unwrap());
                                    self.poll.register(&socket, token, Ready::readable(), PollOpt::edge()).unwrap();
                                }
                                Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
                                    break;
                                }
                                e => panic!("err={:?}", e),
                            }
                            
                        }
                    }
                    token => {
                        loop {
                            match self.peers.get_mut(&token).unwrap().read(&mut buf) {
                                Ok(0) => {
                                    self.peers.remove(&token);
                                    break;
                                }
                                Ok(_) => {
                                    //let socket = self.peers.get_mut(&token);
                                    println!("Received was: {:?}", String::from_utf8_lossy(&buf));
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

    connect_send.send("127.0.0.1:8888".parse().unwrap()).unwrap();
    connect_send.send("127.0.0.1:8888".parse().unwrap()).unwrap();
    connect_send.send("127.0.0.1:8888".parse().unwrap()).unwrap();
    connect_send.send("127.0.0.1:8888".parse().unwrap()).unwrap();

    th.join().unwrap();

}