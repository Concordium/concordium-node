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
use p2p_client::common::{P2PPeer,P2PNodeId};
use mio::Events;

fn main() {
    env_logger::init();
    info!("Starting up!");

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

    let mut node = P2PNode::new(conf.id, listen_port);

    //let tok1 = node.connect(P2PPeer::new("10.0.82.68".parse().unwrap(), 8888)).unwrap();

    let th = thread::spawn(move || {
        let mut events = Events::with_capacity(1024);
        node.process(&mut events);
    });

    th.join().unwrap();

}