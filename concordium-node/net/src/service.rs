use tokio::io;
use tokio::net::{TcpListener, TcpStream};
use tokio::prelude::*;
use tokio::*;
use futures::sync::mpsc;
use futures::future::{self, Either};
use futures::Map;
use bytes::{BytesMut, Bytes, BufMut};
use mio::Token;
use tokio::runtime::Runtime;

use std::collections::HashMap;
use std::net::SocketAddr;
use std::sync::{Arc, Mutex};

const SERVER_TOKEN: Token = Token(0);

struct P2PNode {
    token_counter: usize,
    peers: HashMap<Token, Arc<TcpStream>>,
    runtime: Runtime,

}

impl P2PNode {
    fn new() -> P2PNode {
        let mut rt = Runtime::new().unwrap();

        P2PNode {
            token_counter: 0,
            peers: HashMap::new(),
            runtime: rt
        }
    }

    //Connect to new peer
    fn connect(&mut self, addr: &String) {
        let addr = addr.parse::<SocketAddr>().unwrap();
        let connect_future = TcpStream::connect(&addr);
   
        //MPSC tunnel for communicating inside closure?

        let task = connect_future.and_then(move |stream| {
            let stream = stream;
            println!("Connected successfully!");
            match stream.peer_addr() {
                Err(e) => println!("Error: {}", e),
                Ok(x) => println!("Remote Address is {}", x)
            }
            Ok(())
        })
        .map_err(|e| println!("Failed to connect; {:?}", e));

        //self.peers.insert(Token(self.token_counter), stream);

        self.runtime.spawn(task);
    }

    fn process(&mut self, socket: TcpStream) {
        match socket.peer_addr() {
            Ok(x) => println!("Address is {}", x),
            Err(y) => println!("Error: {}",y ),
        };
    }
}

fn main() {
    let addr = "127.0.0.1:8888".parse().unwrap();
    let listener = TcpListener::bind(&addr).unwrap();

    let server = listener.incoming()
        .map_err(|e| println!("Error= {}", e))
        .for_each(|socket| {
            future::lazy(move || {
                println!("Handling socket");
                match socket.peer_addr() {
                    Err(e) => println!("Error: {}", e),
                    Ok(x) => println!("Remote Address is {}", x)
                }
                Ok(())
        })
    });
    
    let mut node = P2PNode::new();

    node.runtime.spawn(server);


    node.connect(&String::from("127.0.0.1:8888"));
    node.connect(&String::from("127.0.0.1:8888"));
    node.connect(&String::from("127.0.0.1:8888"));
    node.connect(&String::from("127.0.0.1:8888"));
    

    node.runtime.shutdown_on_idle()
        .wait().unwrap();
}