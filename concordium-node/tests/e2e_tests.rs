#![feature(box_syntax, box_patterns)]
extern crate bytes;
extern crate mio;
extern crate p2p_client;
#[macro_use]
extern crate log;
extern crate env_logger;

#[cfg(test)]
mod tests {
    use p2p_client::common::{NetworkMessage, NetworkPacket, NetworkRequest};
    use p2p_client::p2p::*;
    use std::sync::mpsc;
    use std::{thread, time};

    #[test]
    pub fn e2e_000_two_nodes() {
        let (pkt_in_1, pkt_out_1) = mpsc::channel();
        let (pkt_in_2, _pkt_out_2) = mpsc::channel();

        let (sender, receiver) = mpsc::channel();
        let _guard = thread::spawn(move || loop {
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
                                           }
                                       }
                                   });

        let mut node_1 = P2PNode::new(None,
                                      Some("127.0.0.1".to_string()),
                                      8888,
                                      None,
                                      None,
                                      pkt_in_1,
                                      Some(sender),
                                      P2PNodeMode::NormalPrivateMode,
                                      None);

        let mut _th_1 = node_1.spawn();

        let mut node_2 = P2PNode::new(None,
                                      Some("127.0.0.1".to_string()),
                                      8889,
                                      None,
                                      None,
                                      pkt_in_2,
                                      None,
                                      P2PNodeMode::NormalPrivateMode,
                                      None);

        let _th_2 = node_2.spawn();

        let msg = String::from("Hello other brother!");

        node_2.connect("127.0.0.1".parse().unwrap(), 8888).unwrap();

        thread::sleep(time::Duration::from_secs(5));

        node_2.send_message(Some(node_1.get_own_id()), &msg.as_bytes().to_vec(), false)
              .map_err(|e| panic!(e))
              .ok();

        thread::sleep(time::Duration::from_secs(5));

        match pkt_out_1.try_recv() {
            Ok(ref x) => match *x.clone() {
                box NetworkMessage::NetworkRequest(NetworkRequest::Handshake(_), _, _) => {}
                _ => panic!("Didn't get handshake"),
            },
            _ => {
                panic!("Didn't get handshake");
            }
        }

        thread::sleep(time::Duration::from_secs(5));

        match pkt_out_1.try_recv() {
            Ok(ref outer) => match *outer.clone() {
                box NetworkMessage::NetworkPacket(NetworkPacket::DirectMessage(_,
                                                                               _,
                                                                               ref recv_msg),
                                                  _,
                                                  _) => {
                    assert_eq!(msg.as_bytes().to_vec(), *recv_msg);
                }
                ref x => {
                    panic!("Didn't get message from node_2, but got {:?}", x);
                }
            },
            x => {
                panic!("Didn't get message from node_2, but got {:?}", x);
            }
        }
    }

    #[test]
    pub fn e2e_001_trust_broadcast() {
        let (pkt_in_1, _pkt_out_1) = mpsc::channel();
        let (pkt_in_2, pkt_out_2) = mpsc::channel();
        let (pkt_in_3, pkt_out_3) = mpsc::channel();

        let (sender, receiver) = mpsc::channel();
        let _guard = thread::spawn(move || loop {
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
                                           }
                                       }
                                   });

        let mut node_1 = P2PNode::new(None,
                                      Some("127.0.0.1".to_string()),
                                      8898,
                                      None,
                                      None,
                                      pkt_in_1,
                                      Some(sender),
                                      P2PNodeMode::NormalPrivateMode,
                                      None);

        let mut _th_1 = node_1.spawn();

        let mut node_2 = P2PNode::new(None,
                                      Some("127.0.0.1".to_string()),
                                      8899,
                                      None,
                                      None,
                                      pkt_in_2,
                                      None,
                                      P2PNodeMode::NormalPrivateMode,
                                      None);

        let _th_2 = node_2.spawn();

        let mut _2_node = node_2.clone();

        let _guard_2 = thread::spawn(move || loop {
                                         if let Ok(ref outer_msg) = pkt_out_2.recv() {
                                             match *outer_msg.clone() {
                                                 box NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(_, ref msg), _, _) => {
                                                     _2_node.send_message(None, &msg, true).map_err(|e| panic!(e)).ok();
                                                 }
                                                 _ => {}
                                             }
                                         }
                                     });

        let mut node_3 = P2PNode::new(None,
                                      Some("127.0.0.1".to_string()),
                                      8900,
                                      None,
                                      None,
                                      pkt_in_3,
                                      None,
                                      P2PNodeMode::NormalPrivateMode,
                                      None);

        let _th_3 = node_3.spawn();

        let msg = String::from("Hello other brother!");

        node_2.connect("127.0.0.1".parse().unwrap(), 8898).unwrap();

        node_3.connect("127.0.0.1".parse().unwrap(), 8899).unwrap();

        thread::sleep(time::Duration::from_secs(5));

        node_1.send_message(None, &msg.as_bytes().to_vec(), true)
              .map_err(|e| panic!(e))
              .ok();

        thread::sleep(time::Duration::from_secs(5));

        match pkt_out_3.try_recv() {
            Ok(ref mut outer_msg) => match *outer_msg.clone() {
                box NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(_,
                                                                                    ref recv_msg),
                                                  _,
                                                  _) => {
                    assert_eq!(&msg.as_bytes().to_vec(), recv_msg);
                }
                ref x => {
                    panic!("Didn't get message from node_1 on node_3, but got {:?}", x);
                }
            },
            x => {
                panic!("Didn't get message from node_1 on node_3, but got {:?}", x);
            }
        }
    }
}
