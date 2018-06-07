extern crate p2p_client;
#[macro_use] extern crate structopt;
extern crate bytes;
extern crate mio;
use std::sync::mpsc;
use std::thread;
use p2p_client::p2p::*;
use mio::Events;

fn main() {
    let (out_tx, out_rx) = mpsc::channel();
    let (in_tx, in_rx) = mpsc::channel();

    let mut node = P2PNode::new(out_rx, in_tx);

    let (connect_send, mut connect_recv) = mpsc::channel();

    let tok1 = node.connect("127.0.0.1:8888".parse().unwrap()).unwrap();
    println!("First token is: {:?}", tok1);
    let tok2 = node.connect("127.0.0.1:8888".parse().unwrap()).unwrap();
    let tok3 = node.connect("127.0.0.1:8888".parse().unwrap()).unwrap();

    let th = thread::spawn(move || {
        let mut events = Events::with_capacity(1024);
        node.process(&mut events, &mut connect_recv);
    });

    let th2 = thread::spawn(move || {
        loop {
            match in_rx.try_recv() {
                Ok(x) => {
                    println!("Received: {:?} from {:?}", String::from_utf8_lossy(&x.msg), x.token);
                },
                _ => {

                }
            };
        }
    });
    out_tx.send(P2PMessage::new(tok1, String::from("Foo").into_bytes()));
    out_tx.send(P2PMessage::new(tok1, String::from("Foo").into_bytes()));
    out_tx.send(P2PMessage::new(tok1, String::from("Foo").into_bytes()));
    out_tx.send(P2PMessage::new(tok1, String::from("Foo").into_bytes()));
    out_tx.send(P2PMessage::new(tok1, String::from("Foo").into_bytes()));
    out_tx.send(P2PMessage::new(tok1, String::from("Foo").into_bytes()));
    out_tx.send(P2PMessage::new(tok1, String::from("Foo").into_bytes()));

    th.join().unwrap();
    th2.join().unwrap();

}