extern crate p2p_client;
extern crate bytes;
extern crate mio;
#[macro_use]
extern crate log;
extern crate env_logger;
use std::sync::mpsc;
use p2p_client::p2p::*;
use p2p_client::configuration;
use p2p_client::common::{NetworkRequest,NetworkPacket,NetworkMessage};
use std::thread;
use env_logger::Env;

fn main() {
    let conf = configuration::parse_config();
    
    let env = if conf.debug {
        Env::default()
            .filter_or("MY_LOG_LEVEL", "debug")
    } else {
        Env::default()
            .filter_or("MY_LOG_LEVEL","info")
    };
    
    env_logger::init_from_env(env);
    info!("Starting up {} version {}!", p2p_client::APPNAME, p2p_client::VERSION);

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
                    NetworkMessage::NetworkPacket(NetworkPacket::DirectMessage(_,_, msg),_,_) => info!( "DirectMessage with {:?} received", msg),
                    NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(_,msg),_,_) => info!("BroadcastedMessage with {:?} received", msg),
                    NetworkMessage::NetworkRequest(NetworkRequest::BanNode(_, x),_,_)  => info!("Ban node request for {:x}", x.get_id()),
                    NetworkMessage::NetworkRequest(NetworkRequest::UnbanNode(_, x), _, _) => info!("Unban node requets for {:x}", x.get_id()), 
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
                        P2PEvent::SentMessageEvent(node_id) => info!("Sent message to {:?}", node_id),
                        P2PEvent::InitiatingConnection(ip,port) => info!("Initiating connection to {}:{}", ip, port),
                    }
                }
            }
        });
        P2PNode::new(conf.id, listen_port, pkt_in, Some(sender))
    } else {
        P2PNode::new(conf.id, listen_port, pkt_in, None)
    };

    let _th = node.spawn();

    _th.join().unwrap();

}