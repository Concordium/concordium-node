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
    use p2p_client::common::{ ConnectionType };
    use p2p_client::network::{ NetworkMessage, NetworkPacket, NetworkRequest };
    use p2p_client::connection::{ P2PEvent, P2PNodeMode };
    use p2p_client::p2p::p2p_node::{ P2PNode };
    use p2p_client::prometheus_exporter::{PrometheusMode, PrometheusServer};
    use std::sync::mpsc;
    use std::sync::{Arc, Mutex, Once, ONCE_INIT};
    use std::sync::atomic::{ AtomicUsize, Ordering, ATOMIC_USIZE_INIT};
    use std::{thread, time};

    static INIT: Once = ONCE_INIT;
    static PORT_OFFSET: AtomicUsize = ATOMIC_USIZE_INIT;

    #[derive(Debug, Clone)]
    pub enum NetworkStep {
        Handshake(u16),
        Broadcast(u16),
    }

    /// It initializes the global logger with a `env_logger`, but just once. 
    fn setup() {
        INIT.call_once( || {
            env_logger::init()
        });
    }

    /// It returns next port available and it ensures that next `slot_size` ports will be 
    /// available too.
    ///
    /// # Arguments
    /// * `slot_size` - Size of blocked ports. It 
    ///
    /// # Example
    /// ```
    /// let port_range_1 = next_port_offset( 10);   // It will return 0, you can use from 0..9
    /// let port_range_2 = next_port_offset( 20);   // It will return 10, you can use from 10..19
    /// let port_range_3 = next_port_offset( 100);  // It will return 30, you can use from 20..129
    /// let port_range_4 = next_port_offset( 130);
    /// ```
    fn next_port_offset( slot_size: usize) -> u16 {
        PORT_OFFSET.fetch_add( slot_size, Ordering::SeqCst) as u16
    }

    #[test]
    #[ignore]
    pub fn e2e_000_two_nodes() {
        setup();
        let test_port_added = next_port_offset( 10);
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
                                      vec![100],
                                      100);

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
                                      vec![100],
                                      100);

        let _th_2 = node_2.spawn();

        let msg = String::from("Hello other brother!");

        node_2.connect(ConnectionType::Node,
                       "127.0.0.1".parse().unwrap(),
                       8888 + test_port_added,
                       None)
              .unwrap();

        thread::sleep(time::Duration::from_secs(10));

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
    #[ignore]
    pub fn e2e_000_two_nodes_wrong_net() {
        setup();
        let test_port_added = next_port_offset( 10);
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
                                      vec![100],
                                      100);

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
                                      vec![200],
                                      100);

        let _th_2 = node_2.spawn();

        let msg = String::from("Hello other brother!");

        node_2.connect(ConnectionType::Node,
                       "127.0.0.1".parse().unwrap(),
                       8888 + test_port_added,
                       None)
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
    pub fn e2e_001_trust_broadcast() {
        setup();
        let test_port_added = next_port_offset( 10);
        
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
                                      vec![100],
                                      100);

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
                                      vec![100],
                                      100);

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
                                      vec![100],
                                      100);

        let _th_3 = node_3.spawn();

        let msg = String::from("Hello other brother!");

        node_2.connect(ConnectionType::Node,
                       "127.0.0.1".parse().unwrap(),
                       8888 + test_port_added,
                       None)
              .unwrap();

        node_3.connect(ConnectionType::Node,
                       "127.0.0.1".parse().unwrap(),
                       8889 + test_port_added,
                       None)
              .unwrap();

        thread::sleep(time::Duration::from_secs(5));

        node_1.send_message(None, 100, None, &msg.as_bytes().to_vec(), true)
              .map_err(|e| panic!(e))
              .ok();

        let pkt_out_3_timeout = time::Duration::from_secs( 30);
        let mut pkt_out_3_msg = pkt_out_3.recv_timeout( pkt_out_3_timeout);

        match pkt_out_3_msg {
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
    #[ignore]
    pub fn e2e_001_trust_broadcast_wrong_net() {
        setup();
        let test_port_added = next_port_offset( 10);
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
                                      vec![100],
                                      100);

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
                                      vec![200],
                                      100);

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
                                      vec![200],
                                      100);

        let _th_3 = node_3.spawn();

        let msg = String::from("Hello other brother!");

        node_2.connect(ConnectionType::Node,
                       "127.0.0.1".parse().unwrap(),
                       8888 + test_port_added,
                       None)
              .unwrap();

        node_3.connect(ConnectionType::Node,
                       "127.0.0.1".parse().unwrap(),
                       8889 + test_port_added,
                       None)
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
    #[ignore]
    pub fn e2e_002_small_mesh_net() {
        setup();
        let test_port_added = next_port_offset( 50);

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
                                        vec![100],
                                        100);
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
                                 (instance_port - 1 - (i)) as u16,
                                 None)
                        .ok();
                }
            }
            peer += 1;
            peers.push((instance_port as usize, th, node, prometheus));
            peer_ports.push(instance_port as usize);
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

        assert_eq!(message_count_estimated as usize, message_counter.get());
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
                                                vec![100],
                                                100);
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
                            node.connect(ConnectionType::Node, "127.0.0.1".parse().unwrap(), (instance_port-1-(i)) as u16, None).ok();
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
                        central_peer.connect(ConnectionType::Node,"127.0.0.1".parse().unwrap(), (test_port_added+8888+(island_size*i)) as u16, None) .map_err(|e| println!("{}", e)).ok();
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
    #[ignore]
    pub fn e2e_002_small_mesh_three_islands_net() {
        islands_mesh_test!(500, 3, 3);
    }

    #[test]
    #[ignore]
    pub fn e2e_003_big_mesh_three_islands_net() {
        islands_mesh_test!(600, 5, 3);
    }

    #[test]
    /// This test call `no_relay_broadcast_so_sender` test using 2 nodes.
    pub fn e2e_004_2_no_relay_broadcast_to_sender() {
        no_relay_broadcast_so_sender( 2); 
    }
    
    #[test]
    /// This test call `no_relay_broadcast_so_sender` test using 8 nodes.
    pub fn e2e_004_8_no_relay_broadcast_to_sender() {
        no_relay_broadcast_so_sender( 8); 
    }
    
    #[test]
    /// This test call `no_relay_broadcast_so_sender` test using 20 nodes.
    pub fn e2e_004_20_no_relay_broadcast_to_sender() {
        no_relay_broadcast_so_sender( 20); 
    }

    /// It creates `num_nodes` nodes. All nodes from `1..num_nodes` will be connected to node `0`.
    /// After all handshakes, node 0 will send a broadcast message.
    /// This test checks that number of broadcast messages is what we expected.
    fn no_relay_broadcast_so_sender( num_nodes: usize) { 
        setup();
        let test_port_added = next_port_offset( num_nodes + 1);
        let network_id: u16 = 100;

        let (tx, rx) = mpsc::channel();

        let mut nodes: Vec<P2PNode> = Vec::new();
        let mut node_threads = vec![];

        // 0. Create P2PNodes
        for n in 0..num_nodes {
            let tx_i = tx.clone();
            
            let mut node = P2PNode::new( 
                    None, Some("127.0.0.1".to_string()), 8888 + test_port_added + n as u16,
                    None, None, tx_i, None, P2PNodeMode::NormalPrivateMode, None, 
                    vec![100], 100);

            if n > 0 {
                let root = &nodes[0];
                node.connect(
                        ConnectionType::Node, root.get_listening_ip(),
                        root.get_listening_port(), None).unwrap();
            }

            node_threads.push( node.spawn());
            nodes.push( node);
        }
        
        let root: &mut P2PNode = nodes.first_mut().unwrap();
        let broadcast_msg: &str = "Hello broadcasted!";

        // 0. Gather agent checks that others P2PNode 
        let (net_tx, net_rx) = mpsc::channel();

        let ga_exp_num_nodes: u16 = (num_nodes -1) as u16;
        let ga_root_id = root.get_own_id();
        let ga_network_id = network_id;

        let gather_agent = thread::spawn( move || {
            let mut exp_broadcast: i32 = ga_exp_num_nodes as i32;
            let mut exp_handshake: i32 = ga_exp_num_nodes as i32;

            for received in rx {
                match *received{
                    box NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(ref sender, ref _msgid, ref nid, ref msg), _,_) => { 
                        exp_broadcast -= 1;
                        assert!( exp_broadcast >= 0);

                        if exp_broadcast == 0 {
                            debug!( "Broadcast has been confirmed by {} nodes", ga_exp_num_nodes);
                            net_tx.send( NetworkStep::Broadcast( ga_exp_num_nodes)).unwrap();
                        }
                        assert_eq!( *msg, broadcast_msg.as_bytes().to_vec());
                        assert_eq!( ga_root_id, sender.id());
                        assert_eq!( ga_network_id, *nid);
                    },
                    box NetworkMessage::NetworkRequest( NetworkRequest::Handshake(_,_,_), _,_) => {
                        exp_handshake -= 1;
                        assert!( exp_handshake >= 0);

                        if exp_handshake == 0 {
                            debug!( "Handshake has been confirmed by {} nodes", ga_exp_num_nodes);
                            net_tx.send( NetworkStep::Handshake( ga_exp_num_nodes)).unwrap();
                        }
                    },
                    ref other_msg => { debug!( "No forward message: {:?}", other_msg); }
                }

                if exp_broadcast == 0 && exp_handshake == 0 {
                    break;
                }
            }
        });

        // 1. Wait until all nodes make their handshake.
        let net_rx_msg_1 = net_rx.recv_timeout( time::Duration::from_secs(20))
            .expect("Unexpected message while handshake waiting");
        
        match net_rx_msg_1 {
            NetworkStep::Handshake(count) =>  assert_eq!( (num_nodes -1) as u16, count),
            ref e => panic!( "Unexpected message while handshake waiting: {:?}", e)
        };

        // 2. Send broadcast message.
        let root_id = Some(root.get_own_id());
        root.send_message( 
                root_id, network_id, None, 
                &broadcast_msg.as_bytes().to_vec(), true)
            .map_err( |e| panic!(e))
            .ok();
        debug!( "Broadcast message from root({}) node to others!", root.get_listening_port());

        // 3. Wait until all broadcast messages are received.
        let net_rx_msg_2 = net_rx.recv_timeout( time::Duration::from_secs(10))
            .expect( "Unexpected message while broadcast waiting");

        match net_rx_msg_2 {
            NetworkStep::Broadcast( count ) => assert_eq!( (num_nodes -1) as u16, count),
            ref e => panic!( "Unexpected message while broadcast waiting: {:?}", e)
        };

        gather_agent.join().expect( "Gather agent has failed");
    }
}
