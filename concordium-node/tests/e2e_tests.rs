#![feature(box_syntax, box_patterns)]
extern crate bytes;
extern crate mio;
extern crate p2p_client;
#[macro_use]
extern crate log;
extern crate atomic_counter;
extern crate env_logger;

#[cfg(test)]
mod tests {
    use atomic_counter::AtomicCounter;
    use atomic_counter::RelaxedCounter;
    use p2p_client::common::{ConnectionType, NetworkMessage, NetworkPacket, NetworkRequest};
    use p2p_client::p2p::*;
    use p2p_client::prometheus_exporter::{PrometheusMode, PrometheusServer};
    use std::sync::mpsc;
    use std::sync::{Arc, Mutex};
    use std::{thread, time};

    #[test]
    pub fn e2e_000_two_nodes() {
        let test_port_added = 0;
        let (pkt_in_1, pkt_out_1) = mpsc::channel();
        let (pkt_in_2, _pkt_out_2) = mpsc::channel();

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
                                          P2PEvent::JoinedNetwork(peer, network_id) => {
                                              info!("Peer {} joined network {}",
                                                    peer.id().to_string(),
                                                    network_id);
                                          }
                                          P2PEvent::LeftNetwork(peer, network_id) => {
                                              info!("Peer {} left network {}",
                                                    peer.id().to_string(),
                                                    network_id);
                                          }
                                      }
                                  }
                              }
                          });

        let mut node_1 = P2PNode::new(None,
                                      Some("127.0.0.1".to_string()),
                                      8888 + test_port_added,
                                      None,
                                      None,
                                      pkt_in_1,
                                      Some(sender),
                                      P2PNodeMode::NormalPrivateMode,
                                      None,
                                      vec![100]);

        let mut _th_1 = node_1.spawn();

        let mut node_2 = P2PNode::new(None,
                                      Some("127.0.0.1".to_string()),
                                      8889 + test_port_added,
                                      None,
                                      None,
                                      pkt_in_2,
                                      None,
                                      P2PNodeMode::NormalPrivateMode,
                                      None,
                                      vec![100]);

        let _th_2 = node_2.spawn();

        let msg = String::from("Hello other brother!");

        node_2.connect(ConnectionType::Node,
                       "127.0.0.1".parse().unwrap(),
                       8888 + test_port_added)
              .unwrap();

        thread::sleep(time::Duration::from_secs(5));

        node_2.send_message(Some(node_1.get_own_id()),
                            100,
                            None,
                            &msg.as_bytes().to_vec(),
                            false)
              .map_err(|e| panic!(e))
              .ok();

        thread::sleep(time::Duration::from_secs(5));

        match pkt_out_1.try_recv() {
            Ok(ref x) => {
                match *x.clone() {
                    box NetworkMessage::NetworkRequest(NetworkRequest::Handshake(_, _, _),
                                                       _,
                                                       _) => {}
                    _ => panic!("Didn't get handshake"),
                }
            }
            _ => {
                panic!("Didn't get handshake");
            }
        }

        thread::sleep(time::Duration::from_secs(5));

        match pkt_out_1.try_recv() {
            Ok(ref outer) => {
                match *outer.clone() {
                    box NetworkMessage::NetworkPacket(NetworkPacket::DirectMessage(_,
                                                                                   _,
                                                                                   _,
                                                                                   _,
                                                                                   ref recv_msg),
                                                      _,
                                                      _) => {
                        assert_eq!(msg.as_bytes().to_vec(), *recv_msg);
                    }
                    ref x => {
                        panic!("Didn't get message from node_2, but got {:?}", x);
                    }
                }
            }
            x => {
                panic!("Didn't get message from node_2, but got {:?}", x);
            }
        }
    }

    #[test]
    pub fn e2e_000_two_nodes_wrong_net() {
        let test_port_added = 100;
        let (pkt_in_1, pkt_out_1) = mpsc::channel();
        let (pkt_in_2, _pkt_out_2) = mpsc::channel();

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
                                          P2PEvent::JoinedNetwork(peer, network_id) => {
                                              info!("Peer {} joined network {}",
                                                    peer.id().to_string(),
                                                    network_id);
                                          }
                                          P2PEvent::LeftNetwork(peer, network_id) => {
                                              info!("Peer {} left network {}",
                                                    peer.id().to_string(),
                                                    network_id);
                                          }
                                      }
                                  }
                              }
                          });

        let mut node_1 = P2PNode::new(None,
                                      Some("127.0.0.1".to_string()),
                                      8888 + test_port_added,
                                      None,
                                      None,
                                      pkt_in_1,
                                      Some(sender),
                                      P2PNodeMode::NormalPrivateMode,
                                      None,
                                      vec![100]);

        let mut _th_1 = node_1.spawn();

        let mut node_2 = P2PNode::new(None,
                                      Some("127.0.0.1".to_string()),
                                      8889 + test_port_added,
                                      None,
                                      None,
                                      pkt_in_2,
                                      None,
                                      P2PNodeMode::NormalPrivateMode,
                                      None,
                                      vec![200]);

        let _th_2 = node_2.spawn();

        let msg = String::from("Hello other brother!");

        node_2.connect(ConnectionType::Node,
                       "127.0.0.1".parse().unwrap(),
                       8888 + test_port_added)
              .unwrap();

        thread::sleep(time::Duration::from_secs(5));

        node_2.send_message(Some(node_1.get_own_id()),
                            100,
                            None,
                            &msg.as_bytes().to_vec(),
                            false)
              .map_err(|e| panic!(e))
              .ok();

        thread::sleep(time::Duration::from_secs(5));

        match pkt_out_1.try_recv() {
            Ok(ref x) => {
                match *x.clone() {
                    box NetworkMessage::NetworkRequest(NetworkRequest::Handshake(_, _, _),
                                                       _,
                                                       _) => {}
                    _ => panic!("Didn't get handshake"),
                }
            }
            _ => {
                panic!("Didn't get handshake");
            }
        }

        thread::sleep(time::Duration::from_secs(5));

        match pkt_out_1.try_recv() {
            Ok(ref outer) => {
                match *outer.clone() {
                    box NetworkMessage::NetworkPacket(NetworkPacket::DirectMessage(_,
                                                                                   _,
                                                                                   _,
                                                                                   _,
                                                                                   _),
                                                      _,
                                                      _) => {
                        panic!("Got a message, this shouldn't happen!");
                    }
                    ref x => {
                        panic!("Didn't get message from node_2, but got {:?}", x);
                    }
                }
            }
            _ => {}
        }
    }

    #[test]
    pub fn e2e_002_trust_broadcast() {
        let test_port_added = 200;
        let (pkt_in_1, _pkt_out_1) = mpsc::channel();
        let (pkt_in_2, pkt_out_2) = mpsc::channel();
        let (pkt_in_3, pkt_out_3) = mpsc::channel();

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
                                          P2PEvent::JoinedNetwork(peer, network_id) => {
                                              info!("Peer {} joined network {}",
                                                    peer.id().to_string(),
                                                    network_id);
                                          }
                                          P2PEvent::LeftNetwork(peer, network_id) => {
                                              info!("Peer {} left network {}",
                                                    peer.id().to_string(),
                                                    network_id);
                                          }
                                      }
                                  }
                              }
                          });

        let mut node_1 = P2PNode::new(None,
                                      Some("127.0.0.1".to_string()),
                                      8888 + test_port_added,
                                      None,
                                      None,
                                      pkt_in_1,
                                      Some(sender),
                                      P2PNodeMode::NormalPrivateMode,
                                      None,
                                      vec![100]);

        let mut _th_1 = node_1.spawn();

        let mut node_2 = P2PNode::new(None,
                                      Some("127.0.0.1".to_string()),
                                      8889 + test_port_added,
                                      None,
                                      None,
                                      pkt_in_2,
                                      None,
                                      P2PNodeMode::NormalPrivateMode,
                                      None,
                                      vec![100]);

        let _th_2 = node_2.spawn();

        let mut _2_node = node_2.clone();

        let _guard_2 = thread::spawn(move || {
                                         loop {
                                             if let Ok(ref outer_msg) = pkt_out_2.recv() {
                                                 match *outer_msg.clone() {
                                                 box NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(_, ref msgid,  ref nid, ref msg), _, _) => {
                                                     _2_node.send_message(None, *nid, Some(msgid.clone()), &msg, true).map_err(|e| panic!(e)).ok();
                                                 }
                                                 _ => {}
                                             }
                                             }
                                         }
                                     });

        let mut node_3 = P2PNode::new(None,
                                      Some("127.0.0.1".to_string()),
                                      8890 + test_port_added,
                                      None,
                                      None,
                                      pkt_in_3,
                                      None,
                                      P2PNodeMode::NormalPrivateMode,
                                      None,
                                      vec![100]);

        let _th_3 = node_3.spawn();

        let msg = String::from("Hello other brother!");

        node_2.connect(ConnectionType::Node,
                       "127.0.0.1".parse().unwrap(),
                       8888 + test_port_added)
              .unwrap();

        node_3.connect(ConnectionType::Node,
                       "127.0.0.1".parse().unwrap(),
                       8889 + test_port_added)
              .unwrap();

        thread::sleep(time::Duration::from_secs(5));

        node_1.send_message(None, 100, None, &msg.as_bytes().to_vec(), true)
              .map_err(|e| panic!(e))
              .ok();

        thread::sleep(time::Duration::from_secs(5));

        match pkt_out_3.try_recv() {
            Ok(ref mut outer_msg) => {
                match *outer_msg.clone() {
                    box NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(_,
                                                                                        _,
                                                                                        _,
                                                                                        ref recv_msg),
                                                      _,
                                                      _) => {
                        assert_eq!(&msg.as_bytes().to_vec(), recv_msg);
                    }
                    ref x => {
                        panic!("Didn't get message from node_1 on node_3, but got {:?}", x);
                    }
                }
            }
            x => {
                panic!("Didn't get message from node_1 on node_3, but got {:?}", x);
            }
        }
    }

    #[test]
    pub fn e2e_002_trust_broadcast_wrong_net() {
        let test_port_added = 300;
        let (pkt_in_1, _pkt_out_1) = mpsc::channel();
        let (pkt_in_2, pkt_out_2) = mpsc::channel();
        let (pkt_in_3, pkt_out_3) = mpsc::channel();

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
                                          P2PEvent::JoinedNetwork(peer, network_id) => {
                                              info!("Peer {} joined network {}",
                                                    peer.id().to_string(),
                                                    network_id);
                                          }
                                          P2PEvent::LeftNetwork(peer, network_id) => {
                                              info!("Peer {} left network {}",
                                                    peer.id().to_string(),
                                                    network_id);
                                          }
                                      }
                                  }
                              }
                          });

        let mut node_1 = P2PNode::new(None,
                                      Some("127.0.0.1".to_string()),
                                      8888 + test_port_added,
                                      None,
                                      None,
                                      pkt_in_1,
                                      Some(sender),
                                      P2PNodeMode::NormalPrivateMode,
                                      None,
                                      vec![100]);

        let mut _th_1 = node_1.spawn();

        let mut node_2 = P2PNode::new(None,
                                      Some("127.0.0.1".to_string()),
                                      8889 + test_port_added,
                                      None,
                                      None,
                                      pkt_in_2,
                                      None,
                                      P2PNodeMode::NormalPrivateMode,
                                      None,
                                      vec![200]);

        let _th_2 = node_2.spawn();

        let mut _2_node = node_2.clone();

        let _guard_2 = thread::spawn(move || {
                                         loop {
                                             if let Ok(ref outer_msg) = pkt_out_2.recv() {
                                                 match *outer_msg.clone() {
                                                 box NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(_, ref msgid, ref nid, ref msg), _, _) => {
                                                     _2_node.send_message(None, *nid, Some(msgid.clone()), &msg, true).map_err(|e| panic!(e)).ok();
                                                 }
                                                 _ => {}
                                             }
                                             }
                                         }
                                     });

        let mut node_3 = P2PNode::new(None,
                                      Some("127.0.0.1".to_string()),
                                      8890 + test_port_added,
                                      None,
                                      None,
                                      pkt_in_3,
                                      None,
                                      P2PNodeMode::NormalPrivateMode,
                                      None,
                                      vec![200]);

        let _th_3 = node_3.spawn();

        let msg = String::from("Hello other brother!");

        node_2.connect(ConnectionType::Node,
                       "127.0.0.1".parse().unwrap(),
                       8888 + test_port_added)
              .unwrap();

        node_3.connect(ConnectionType::Node,
                       "127.0.0.1".parse().unwrap(),
                       8889 + test_port_added)
              .unwrap();

        thread::sleep(time::Duration::from_secs(5));

        node_1.send_message(None, 100, None, &msg.as_bytes().to_vec(), true)
              .map_err(|e| panic!(e))
              .ok();

        thread::sleep(time::Duration::from_secs(5));

        match pkt_out_3.try_recv() {
            Ok(ref mut outer_msg) => {
                match *outer_msg.clone() {
                    box NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(_,
                                                                                        _,
                                                                                        _,
                                                                                        _),
                                                      _,
                                                      _) => {
                        panic!("Got message this should not happen!");
                    }
                    ref x => {
                        panic!("Didn't get message from node_1 on node_3, but got {:?}", x);
                    }
                }
            }
            _ => {}
        }
    }

    #[test]
    pub fn e2e_003_small_mesh_net() {
        let test_port_added = 400;
        let mesh_node_count = 15;

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
                                          P2PEvent::JoinedNetwork(peer, network_id) => {
                                              info!("Peer {} joined network {}",
                                                    peer.id().to_string(),
                                                    network_id);
                                          }
                                          P2PEvent::LeftNetwork(peer, network_id) => {
                                              info!("Peer {} left network {}",
                                                    peer.id().to_string(),
                                                    network_id);
                                          }
                                      }
                                  }
                              }
                          });

        let mut peers: Vec<(usize, thread::JoinHandle<()>, P2PNode, PrometheusServer)> = vec![];
        let mut peer_ports: Vec<usize> = vec![];

        let message_counter = Arc::new(RelaxedCounter::new(0));
        let message_count_estimated = mesh_node_count - 1;

        let mut peer = 0;

        for instance_port in (test_port_added + 8888)..(test_port_added + 8888 + mesh_node_count) {
            let (inner_sender, inner_receiver) = mpsc::channel();
            let prometheus = PrometheusServer::new(PrometheusMode::NodeMode);
            let mut node = P2PNode::new(None,
                                        Some("127.0.0.1".to_string()),
                                        instance_port as u16,
                                        None,
                                        None,
                                        inner_sender,
                                        Some(sender.clone()),
                                        P2PNodeMode::NormalPrivateMode,
                                        Some(Arc::new(Mutex::new(prometheus.clone()))),
                                        vec![100]);
            let mut _node_self_clone = node.clone();
            let _msg_counter = message_counter.clone();
            let _guard_pkt = thread::spawn(move || {
                loop {
                    if let Ok(full_msg) = inner_receiver.recv() {
                        match *full_msg.clone() {
                            box NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(_, ref msgid, ref nid, ref msg), _, _) => {
                                info!("BroadcastedMessage/{}/{} with size {} received", nid, msgid, msg.len());
                                _node_self_clone.send_message(None, *nid, Some(msgid.clone()),&msg, true).map_err(|e| error!("Error sending message {}", e)).ok();
                                _msg_counter.inc();
                            }
                            _ => { /*ignored for test*/ }
                        }
                    }
                }
            });
            let th = node.spawn();
            if peer > 0 {
                for i in 0..peer {
                    node.connect(ConnectionType::Node,
                                 "127.0.0.1".parse().unwrap(),
                                 (instance_port - 1 - (i)) as u16)
                        .ok();
                }
            }
            peer += 1;
            peers.push((instance_port, th, node, prometheus));
            peer_ports.push(instance_port);
        }

        thread::sleep(time::Duration::from_secs(5));

        let msg = "Hello other mother's brother".to_string();

        if let Some((_, _, ref mut node_sender_ref, _)) = peers.get_mut(0) {
            node_sender_ref.send_message(None, 100, None, &msg.as_bytes().to_vec(), true)
                           .map_err(|e| panic!(e))
                           .ok();
        };

        thread::sleep(time::Duration::from_secs(5));

        for peer in &peers {
            match peer.3.queue_size() {
                Ok(size) => assert_eq!(0, size),
                _ => panic!("Can't read queue size!"),
            }
        }

        assert_eq!(message_count_estimated, message_counter.get());
    }

    macro_rules! islands_mesh_test {
        ($test_port_added:expr, $island_size: expr, $islands_count: expr) => {
            let test_port_added = $test_port_added;
            let island_size = $island_size;
            let islands_count = $islands_count;
            let message_counter = Arc::new(RelaxedCounter::new(0));
            let message_count_estimated = (island_size*islands_count-1)*islands_count;

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
                                            P2PEvent::JoinedNetwork(peer, network_id) => {
                                                info!("Peer {} joined network {}",
                                                        peer.id().to_string(),
                                                        network_id);
                                            }
                                            P2PEvent::LeftNetwork(peer, network_id) => {
                                                info!("Peer {} left network {}",
                                                        peer.id().to_string(),
                                                        network_id);
                                            }
                                        }
                                    }
                                }
                            });

            let mut islands: Vec<(Vec<usize>, Vec<(usize,
                                        thread::JoinHandle<()>,
                                        P2PNode,
                                        PrometheusServer)>)> = vec![];

            for island in 0..islands_count {
                let mut peers_island: Vec<(usize,
                                        thread::JoinHandle<()>,
                                        P2PNode,
                                        PrometheusServer)> = vec![];
                let mut peer_ports_island: Vec<usize> = vec![];

                let mut peer = 0;

                for instance_port in (test_port_added + 8888 + (island_size * island))
                                    ..(test_port_added + 8888 + island_size + (island_size * island))
                {
                    let _inner_msg_counter = message_counter.clone();
                    let (inner_sender, inner_receiver) = mpsc::channel();
                    let prometheus = PrometheusServer::new(PrometheusMode::NodeMode);
                    let mut node = P2PNode::new(None,
                                                Some("127.0.0.1".to_string()),
                                                instance_port as u16,
                                                None,
                                                None,
                                                inner_sender,
                                                Some(sender.clone()),
                                                P2PNodeMode::NormalPrivateMode,
                                                Some(Arc::new(Mutex::new(prometheus.clone()))),
                                                vec![100]);
                    let mut _node_self_clone = node.clone();
                    let _guard_pkt = thread::spawn(move || {
                        loop {
                            if let Ok(full_msg) = inner_receiver.recv() {
                                match *full_msg.clone() {
                                    box NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(_, ref msgid, ref nid, ref msg), _, _) => {
                                        info!("BroadcastedMessage/{}/{} with size {} received", nid, msgid, msg.len());
                                        _inner_msg_counter.inc();
                                        _node_self_clone.send_message(None, *nid, Some(msgid.clone()),&msg, true).map_err(|e| error!("Error sending message {}", e)).ok();
                                    }
                                    _ => { /*ignored for test*/ }
                                }
                            }
                        }
                    });
                    let th = node.spawn();
                    if peer > 0 {
                        for i in 0..peer {
                            node.connect(ConnectionType::Node, "127.0.0.1".parse().unwrap(), (instance_port-1-(i)) as u16).ok();
                        }
                    }
                    peer += 1;
                    peers_island.push((instance_port, th, node, prometheus));
                    peer_ports_island.push(instance_port);
                }
                islands.push((peer_ports_island, peers_island));
            }

            thread::sleep(time::Duration::from_secs(5));

            if let Some((_,ref mut peers)) = islands.get_mut(0) {
                if let Some((_,_, ref mut central_peer,_)) = peers.get_mut(0) {
                    for i in 1..islands_count {
                        central_peer.connect(ConnectionType::Node,"127.0.0.1".parse().unwrap(), (test_port_added+8888+(island_size*i)) as u16) .map_err(|e| println!("{}", e)).ok();
                    }
                };
            };

            thread::sleep(time::Duration::from_secs(5));

            let msg = "Hello other mother's brother".to_string();

            for island in &mut islands {
                let (_,ref mut peers) = island;
                if let Some((_, _, ref mut node_sender_ref, _)) = peers.get_mut(0) {
                node_sender_ref.send_message(None, 100, None, &msg.as_bytes().to_vec(), true)
                            .map_err(|e| panic!(e))
                            .ok();
                };
            }

            thread::sleep(time::Duration::from_secs(10));

            for island in &islands {
                for peer in &island.1 {
                    match peer.3.queue_size() {
                        Ok(size) => assert_eq!(0, size),
                        _ => panic!("Can't read queue size!"),
                    }
                }
            }

            assert_eq!(message_count_estimated, message_counter.get());
        }
    }

    #[test]
    pub fn e2e_003_small_mesh_five_islands_net() {
        islands_mesh_test!(500, 3, 5);
    }

    #[test]
    pub fn e2e_003_big_mesh_five_islands_net() {
        islands_mesh_test!(600, 5, 5);
    }

}
