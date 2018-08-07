extern crate p2p_client;
extern crate bytes;
extern crate mio;
#[macro_use]
extern crate log;
extern crate grpcio;

#[cfg(test)]
mod tests {
    use std::sync::mpsc;
    use p2p_client::p2p::*;
    use p2p_client::common::{NetworkPacket,NetworkMessage, NetworkRequest};
    use p2p_client::rpc::RpcServerImpl;
    use std::sync::Arc;
    use grpcio::{ChannelBuilder, EnvBuilder};
    use p2p_client::proto::*;
    use std::thread;
    use grpcio::RpcStatusCode;

    #[test]
    pub fn test_grpc_version() {
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
        let node = P2PNode::new(None, 8888, pkt_in, Some(sender));

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

        let mut rpc_serv = RpcServerImpl::new(node.clone(), "127.0.0.1".to_string(), 10002, "rpcadmin".to_string());
        rpc_serv.start_server();

        let env = Arc::new(EnvBuilder::new().build());
        let ch = ChannelBuilder::new(env).connect("127.0.0.1:10002");

        let client = P2PClient::new(ch);
        
        let mut req_meta_builder = ::grpcio::MetadataBuilder::new();
        req_meta_builder.add_str("Authentication", "rpcadmin").unwrap();
        let meta_data = req_meta_builder.build();
       
        let call_options = ::grpcio::CallOption::default().headers(meta_data);
        let reply = client.peer_version_opt(&Empty::new(), call_options).expect("rpc");

        assert_eq!(reply.get_value(), env!("CARGO_PKG_VERSION").to_string());

        rpc_serv.stop_server();
    }

    #[test]
    pub fn test_grpc_noauth() {
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
        let node = P2PNode::new(None, 8889, pkt_in, Some(sender));

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

        let mut rpc_serv = RpcServerImpl::new(node.clone(), "127.0.0.1".to_string(), 10001, "rpcadmin".to_string());
        rpc_serv.start_server();

        let env = Arc::new(EnvBuilder::new().build());
        let ch = ChannelBuilder::new(env).connect("127.0.0.1:10001");

        let client = P2PClient::new(ch);
        
        match client.peer_version(&Empty::new()) {
            Err(::grpcio::Error::RpcFailure(ref x)) => assert_eq!(x.status, RpcStatusCode::Unauthenticated),
            _ => panic!("Wrong rejection")
        }

        rpc_serv.stop_server();
    }
}