#![feature(box_syntax, box_patterns)]
extern crate bytes;
extern crate mio;
extern crate p2p_client;
#[macro_use]
extern crate log;
extern crate grpcio;

#[cfg(test)]
mod tests {
    use grpcio::RpcStatusCode;
    use grpcio::{ChannelBuilder, EnvBuilder};
    use p2p_client::common::{NetworkMessage, NetworkPacket, NetworkRequest};
    use p2p_client::p2p::*;
    use p2p_client::proto::*;
    use p2p_client::rpc::RpcServerImpl;
    use std::sync::mpsc;
    use std::sync::Arc;
    use std::thread;

    #[test]
    pub fn test_grpc_version() {
        let (pkt_in, pkt_out) = mpsc::channel::<Arc<Box<NetworkMessage>>>();

        let (sender, receiver) = mpsc::channel();
        let _guard =
            thread::spawn(move || {
                              loop {
                                  if let Ok(msg) = receiver.recv() {
                                      match msg {
                                          P2PEvent::ConnectEvent(ip, port) => {
                                              info!("Received connection from {}:{}", ip, port)
                                          }
                                          P2PEvent::DisconnectEvent(msg) => {
                                              info!("Received disconnect for {}", msg)
                                          }
                                          P2PEvent::ReceivedMessageEvent(node_id) => {
                                              info!("Received message from {:?}", node_id)
                                          }
                                          P2PEvent::SentMessageEvent(node_id) => {
                                              info!("Sent message to {:?}", node_id)
                                          }
                                          P2PEvent::InitiatingConnection(ip, port) => {
                                              info!("Initiating connection to {}:{}", ip, port)
                                          }
                                          P2PEvent::JoinedNetwork(peer,network_id) => {
                                              info!("Peer {} joined network {}", peer.id().to_string(), network_id);
                                          }
                                          P2PEvent::LeftNetwork(peer,network_id) => {
                                              info!("Peer {} left network {}", peer.id().to_string(), network_id);
                                          }
                                      }
                                  }
                              }
                          });
        let node = P2PNode::new(None,
                                Some("127.0.0.1".to_string()),
                                8888,
                                None,
                                None,
                                pkt_in,
                                Some(sender),
                                P2PNodeMode::NormalPrivateMode,
                                None,
                                vec![]);

        let mut _node_self_clone = node.clone();

        let _guard_pkt = thread::spawn(move || {
                                           loop {
                                               if let Ok(ref outer_msg) = pkt_out.recv() {
                                                   match *outer_msg.clone() {
                                                   box NetworkMessage::NetworkPacket(NetworkPacket::DirectMessage(_, _, ref nid, ref msg), _, _) => info!("DirectMessage/{} with {:?} received", nid, msg),
                                                   box NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(_, ref nid, ref msg), _, _) => {
                                                       info!("BroadcastedMessage/{} with {:?} received", nid, msg);
                                                       _node_self_clone.send_message(None, *nid, &msg, true).map_err(|e| panic!(e)).ok();
                                                   }
                                                   box NetworkMessage::NetworkRequest(NetworkRequest::BanNode(_, ref x), _, _) => info!("Ban node request for {:?}", x),
                                                   box NetworkMessage::NetworkRequest(NetworkRequest::UnbanNode(_, ref x), _, _) => info!("Unban node requets for {:?}", x),
                                                   _ => {}
                                               }
                                               }
                                           }
                                       });

        let mut rpc_serv = RpcServerImpl::new(node.clone(),
                                              None,
                                              "127.0.0.1".to_string(),
                                              10002,
                                              "rpcadmin".to_string());
        rpc_serv.start_server().expect("rpc");

        let env = Arc::new(EnvBuilder::new().build());
        let ch = ChannelBuilder::new(env).connect("127.0.0.1:10002");

        let client = P2PClient::new(ch);

        let mut req_meta_builder = ::grpcio::MetadataBuilder::new();
        req_meta_builder.add_str("Authentication", "rpcadmin")
                        .unwrap();
        let meta_data = req_meta_builder.build();

        let call_options = ::grpcio::CallOption::default().headers(meta_data);
        let reply = client.peer_version_opt(&Empty::new(), call_options)
                          .expect("rpc");

        assert_eq!(reply.get_value(), env!("CARGO_PKG_VERSION").to_string());

        rpc_serv.stop_server().expect("rpc");
    }

    #[test]
    pub fn test_grpc_noauth() {
        let (pkt_in, pkt_out) = mpsc::channel::<Arc<Box<NetworkMessage>>>();

        let (sender, receiver) = mpsc::channel();
        let _guard =
            thread::spawn(move || {
                              loop {
                                  if let Ok(msg) = receiver.recv() {
                                      match msg {
                                          P2PEvent::ConnectEvent(ip, port) => {
                                              info!("Received connection from {}:{}", ip, port)
                                          }
                                          P2PEvent::DisconnectEvent(msg) => {
                                              info!("Received disconnect for {}", msg)
                                          }
                                          P2PEvent::ReceivedMessageEvent(node_id) => {
                                              info!("Received message from {:?}", node_id)
                                          }
                                          P2PEvent::SentMessageEvent(node_id) => {
                                              info!("Sent message to {:?}", node_id)
                                          }
                                          P2PEvent::InitiatingConnection(ip, port) => {
                                              info!("Initiating connection to {}:{}", ip, port)
                                          }
                                          P2PEvent::JoinedNetwork(peer,network_id) => {
                                              info!("Peer {} joined network {}", peer.id().to_string(), network_id);
                                          }
                                          P2PEvent::LeftNetwork(peer,network_id) => {
                                              info!("Peer {} left network {}", peer.id().to_string(), network_id);
                                          }
                                      }
                                  }
                              }
                          });
        let node = P2PNode::new(None,
                                Some("127.0.0.1".to_string()),
                                8889,
                                None,
                                None,
                                pkt_in,
                                Some(sender),
                                P2PNodeMode::NormalPrivateMode,
                                None,
                                vec![]);

        let mut _node_self_clone = node.clone();

        let _guard_pkt = thread::spawn(move || {
                                           loop {
                                               if let Ok(ref outer_msg) = pkt_out.recv() {
                                                   match *outer_msg.clone() {
                                                   box NetworkMessage::NetworkPacket(NetworkPacket::DirectMessage(_, _, ref nid, ref msg), _, _) => info!("DirectMessage/{} with {:?} received", nid, msg),
                                                   box NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(_,ref nid, ref msg), _, _) => {
                                                       info!("BroadcastedMessage/{} with {:?} received", nid, msg);
                                                       _node_self_clone.send_message(None, *nid, &msg, true).map_err(|e| panic!(e)).ok();
                                                   }
                                                   box NetworkMessage::NetworkRequest(NetworkRequest::BanNode(_, ref x), _, _) => info!("Ban node request for {:?}", x),
                                                   box NetworkMessage::NetworkRequest(NetworkRequest::UnbanNode(_, ref x), _, _) => info!("Unban node requets for {:?}", x),
                                                   _ => {}
                                               }
                                               }
                                           }
                                       });

        let mut rpc_serv = RpcServerImpl::new(node.clone(),
                                              None,
                                              "127.0.0.1".to_string(),
                                              10001,
                                              "rpcadmin".to_string());
        rpc_serv.start_server().expect("rpc");

        let env = Arc::new(EnvBuilder::new().build());
        let ch = ChannelBuilder::new(env).connect("127.0.0.1:10001");

        let client = P2PClient::new(ch);

        match client.peer_version(&Empty::new()) {
            Err(::grpcio::Error::RpcFailure(ref x)) => {
                assert_eq!(x.status, RpcStatusCode::Unauthenticated)
            }
            _ => panic!("Wrong rejection"),
        }

        rpc_serv.stop_server().expect("rpc");
    }
}
