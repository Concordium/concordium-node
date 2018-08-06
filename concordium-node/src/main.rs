extern crate p2p_client;
#[macro_use]
extern crate log;
extern crate env_logger;
extern crate bytes;
extern crate mio;
extern crate grpcio;

use p2p_client::configuration;
use std::sync::mpsc;
use std::thread;
use p2p_client::p2p::*;
use p2p_client::rpc::RpcServerImpl;
use p2p_client::common::{NetworkRequest,NetworkPacket,NetworkMessage};
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

    let mut rpc_serv:Option<RpcServerImpl> = None;
    if !conf.no_rpc_server {
        let mut serv = RpcServerImpl::new(node.clone(), conf.rpc_server_addr, conf.rpc_server_port, conf.rpc_server_token);
        serv.start_server();
        rpc_serv = Some(serv);
    }

    let mut _node_self_clone = node.clone();

    let mut _rpc_clone = rpc_serv.clone();
    let _guard_pkt = thread::spawn(move|| {
        loop {
            if let Ok(ref mut full_msg) = pkt_out.recv() {
                match full_msg {
                    NetworkMessage::NetworkPacket(NetworkPacket::DirectMessage(_,_, ref msg),_,_) => {
                        if let Some(ref mut rpc) = _rpc_clone {
                            rpc.queue_message(full_msg);
                        }
                        info!( "DirectMessage with text {:?} received", msg) ;
                    }
                    NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(_,ref msg),_,_) => { 
                        if let Some(ref mut rpc) = _rpc_clone {
                            rpc.queue_message(full_msg);
                        }
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
    if let Some(ref mut serv) = rpc_serv {
        serv.stop_server();
    }
}