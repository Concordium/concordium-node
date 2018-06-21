extern crate p2p_client;
#[macro_use] extern crate structopt;
extern crate bytes;
extern crate mio;
#[macro_use]
extern crate log;
extern crate env_logger;
use std::sync::mpsc;
use std::thread;
use p2p_client::p2p::*;
use p2p_client::configuration;
use mio::Events;

fn main() {
    env_logger::init().ok().expect("Failed to initialize logger");

    let conf = configuration::parse_config();
    let node_ip = match conf.remote_ip {
        Some(x) => { x },
        _ => { String::from("127.0.0.1") }
    };

    let node_port = match conf.remote_port {
        Some(x) => x,
        _ => 8888,
    };

    let listen_port = match conf.listen_port {
        Some(x) => x,
        _ => 8888,
    };

    let (out_tx, out_rx) = mpsc::channel();
    let (in_tx, in_rx) = mpsc::channel();

    let mut node = P2PNode::new(listen_port, out_rx, in_tx);

    let (connect_send, mut connect_recv) = mpsc::channel();

    let tok1 = node.connect(P2PPeer::new(node_ip.parse().unwrap(), 8888)).unwrap();

    let th = thread::spawn(move || {
        let mut events = Events::with_capacity(1024);
        node.process(&mut events, &mut connect_recv);
    });

    let th2 = thread::spawn(move || {
        loop {
            match in_rx.try_recv() {
                Ok(x) => {
                    println!("Received: {:?} from {:?}", String::from_utf8(x.msg).unwrap(), x.token);
                },
                _ => {

                }
            };
        }
    });

    th.join().unwrap();
    th2.join().unwrap();

}