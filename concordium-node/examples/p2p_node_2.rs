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
use p2p_client::common::{P2PNodeId,NetworkRequest,NetworkPacket,NetworkMessage};
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
        _ => 8889,
    };

    let listen_port = match conf.listen_port {
        Some(x) => x,
        _ => 8889,
    };

    info!("Debuging enabled {}", conf.debug);

    let (pkt_in,pkt_out) = mpsc::channel();

    let _guard_pkt = thread::spawn(move|| {
        loop {
            if let Ok(msg) = pkt_out.recv() {
                match msg {
                    NetworkMessage::NetworkPacket(NetworkPacket::DirectMessage(_,_, msg),_,_) => info!( "DirectMessage with text {} received", msg),
                    NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(_,msg),_,_) => info!("BroadcastedMessage with text {} received", msg),
                    NetworkMessage::NetworkRequest(NetworkRequest::BanNode(_, x),_,_)  => info!("Ban node request for {:x}", x.get_id()),
                    NetworkMessage::NetworkRequest(NetworkRequest::UnbanNode(_, x), _, _) => info!("Unban node requets for {:x}", x.get_id()), 
                    _ => {}
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
                        P2PEvent::ReceivedMessageEvent(node_id) => info!("Received message from {:?}", node_id),
                        P2PEvent::SentMessageEvent(node_id) => info!("Sent message to {:?}", nodeId),
                        P2PEvent::InitiatingConnection(ip,port) => info!("Initiating connection to {}:{}", ip, port),
                        _ => error!("Received unknown event!")
                    }
                }
            }
        });
        P2PNode::new(conf.id, listen_port, pkt_in, Some(sender))
    } else {
        P2PNode::new(conf.id, listen_port, pkt_in, None)
    };

    node.connect("127.0.0.1".parse().unwrap(), 8888);
    let mut events = Events::with_capacity(1024);

    loop {
        node.process(&mut events);
        node.send_message(Some(P2PNodeId::from_string("c19cd000746763871fae95fcdd4508dfd8bf725f9767be68c3038df183527bb2".to_string())), "Hello world!".to_string(), false);
    }

}