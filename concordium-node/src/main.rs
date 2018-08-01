#![feature(plugin, use_extern_macros, proc_macro_path_invoc)]
#![plugin(tarpc_plugins)]
extern crate p2p_client;
#[macro_use]
extern crate log;
extern crate env_logger;
extern crate bytes;
extern crate mio;
use p2p_client::configuration;
use std::sync::mpsc;
use std::thread;
use p2p_client::p2p::*;
use p2p_client::rpc::RpcServer;
use p2p_client::common::{NetworkRequest,NetworkPacket,NetworkMessage};
extern crate tarpc;
use env_logger::Env;
use p2p_client::utils;

fn main() {
    let conf = configuration::parse_config();

    let listen_port = match conf.listen_port {
        Some(x) => x,
        _ => 8888,
    };

    let env = if conf.debug {
        Env::default()
            .filter_or("MY_LOG_LEVEL", "debug")
    } else {
        Env::default()
            .filter_or("MY_LOG_LEVEL","info")
    };
    
    env_logger::init_from_env(env);
    info!("Starting up {} version {}!", p2p_client::APPNAME, p2p_client::VERSION);

    info!("Debuging enabled {}", conf.debug);

    let (pkt_in,pkt_out) = mpsc::channel();

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

    let mut _node_self_clone = node.clone();

    let _guard_pkt = thread::spawn(move|| {
        loop {
            if let Ok(msg) = pkt_out.recv() {
                match msg {
                    NetworkMessage::NetworkPacket(NetworkPacket::DirectMessage(_,_, msg),_,_) => info!( "DirectMessage with text {:?} received", msg),
                    NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(_,msg),_,_) => { 
                        info!("BroadcastedMessage with text {:?} received", msg);
                        _node_self_clone.send_message(None,&msg,true);
                    },
                    NetworkMessage::NetworkRequest(NetworkRequest::BanNode(_, x),_,_)  => info!("Ban node request for {:x}", x.get_id()),
                    NetworkMessage::NetworkRequest(NetworkRequest::UnbanNode(_, x), _, _) => info!("Unban node requets for {:x}", x.get_id()), 
                    _ => {}
                }
            }
        }
    });

    info!("Concordium P2P layer. Network disabled: {}", conf.no_network);

    if !conf.no_rpc_server {
        let mut serv = RpcServer::new(node.clone(), conf.rpc_server_addr, conf.rpc_server_port);
        let _th_rpc = serv.spawn();
    }

    let _node_th = node.spawn();

    if conf.connect_to.is_some() {
        let connect_to = conf.connect_to.unwrap();
        match utils::parse_ip_port(&connect_to) {
            Some((ip,port)) => {
                info!("Connecting to peer {}", &connect_to);
                node.connect(ip, port);
            },
            _ => {}
        }
    }

    _node_th.join().unwrap();
}