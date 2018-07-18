extern crate p2p_client;
#[macro_use] extern crate structopt;
extern crate bytes;
extern crate mio;
#[macro_use]
extern crate log;
extern crate env_logger;
use std::sync::mpsc;
use p2p_client::p2p::*;
use p2p_client::configuration;
use p2p_client::common::{P2PPeer,P2PNodeId,NetworkRequest,NetworkPacket,NetworkMessage};
use mio::Events;
use std::sync::{Arc,Mutex};
use std::{thread, time};

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

    info!("Debuging enabled {}", conf.debug);

    let (pkt_in,pkt_out) = mpsc::channel();

    let _guard_pkt = thread::spawn(move|| {
        loop {
            if let Ok(msg) = pkt_out.recv() {
                match msg {
                    NetworkMessage::NetworkPacket(NetworkPacket::DirectMessage(sender,receiver, msg),sent,received) => info!( "DirectMessage with text {} received", msg),
                    NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(sender,msg),sent,received) => info!("BroadcastedMessage with text {} received", msg),
                    NetworkMessage::NetworkRequest(NetworkRequest::BanNode(sender, x),sent,received)  => info!("Ban node request for {:x}", x.get_id()),
                    NetworkMessage::NetworkRequest(NetworkRequest::UnbanNode(sender, x), sent, received) => info!("Unban node requets for {:x}", x.get_id()), 
                    _ => {}
                }
            }
        }
    });

    let mut node = if conf.debug {
        let (sender, receiver) = mpsc::channel();
        let _guard = thread::spawn(move|| {
            loop {
                if let Ok(msg) = receiver.recv() {
                    match msg {
                        P2PEvent::ConnectEvent(ip, port) => info!("Received connection from {}:{}", ip, port),
                        P2PEvent::DisconnectEvent(msg) => info!("Received disconnect for {}", msg),
                        P2PEvent::ReceivedMessageEvent(nodeId) => info!("Received message from {:?}", nodeId),
                        P2PEvent::SentMessageEvent(nodeId) => info!("Sent message to {:?}", nodeId),
                        _ => error!("Received unknown event!")
                    }
                }
            }
        });
        P2PNode::new(conf.id, listen_port, pkt_in, Some(sender))
    } else {
        P2PNode::new(conf.id, listen_port, pkt_in, None)
    };

    //let tok1 = node.connect(P2PPeer::new("10.0.82.68".parse().unwrap(), 8888)).unwrap();
    node.connect("127.0.0.1".parse().unwrap(), 8889);

    let th = thread::spawn(move || {
        let mut events = Events::with_capacity(1024);
        node.process(&mut events);
    });

    th.join().unwrap();
}