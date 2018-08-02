extern crate p2p_client;
extern crate bytes;
extern crate mio;
#[macro_use]
extern crate log;
extern crate env_logger;
extern crate tarpc;

#[cfg(test)]
mod tests {
    use std::sync::mpsc;
    use std::{thread,time};
    use p2p_client::p2p::*;
    use p2p_client::common::{NetworkPacket,NetworkMessage, NetworkRequest};
    use p2p_client::rpc::RpcServer;
    use tarpc::sync::client;
    use tarpc::sync::client::ClientExt;
    use p2p_client::rpc::SyncClient;
    use env_logger;

    #[test]
    pub fn rpc_test_000() {
        let (pkt_in,pkt_out) = mpsc::channel();

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
        let mut node = P2PNode::new(None, 8888, pkt_in, Some(sender));

        let mut _node_self_clone = node.clone();

        let _guard_pkt = thread::spawn(move|| {
            loop {
                if let Ok(msg) = pkt_out.recv() {
                    match msg {
                        NetworkMessage::NetworkPacket(NetworkPacket::DirectMessage(_,_, msg),_,_) => info!( "DirectMessage with {:?} received", msg),
                        NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(_,msg),_,_) => { 
                            info!("BroadcastedMessage with {:?} received", msg);
                            _node_self_clone.send_message(None,&msg,true);
                        },
                        NetworkMessage::NetworkRequest(NetworkRequest::BanNode(_, x),_,_)  => info!("Ban node request for {:x}", x.get_id()),
                        NetworkMessage::NetworkRequest(NetworkRequest::UnbanNode(_, x), _, _) => info!("Unban node requets for {:x}", x.get_id()), 
                        _ => {}
                    }
                }
            }
        });

        env_logger::init();

        let mut serv = RpcServer::new(node.clone(), String::from("127.0.0.1"), 10000, None);
        let _th_rpc = serv.spawn();

        let _node_th = node.spawn();

        thread::sleep(time::Duration::from_secs(3));

        let client = SyncClient::connect("127.0.0.1:10000", client::Options::default()).unwrap();

        assert_eq!(client.get_version().unwrap(), env!("CARGO_PKG_VERSION").to_string() );

    }
}