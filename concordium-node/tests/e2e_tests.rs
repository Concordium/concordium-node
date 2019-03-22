#![feature(box_syntax, box_patterns)]
#[macro_use] extern crate p2p_client;
#[macro_use]
extern crate log;

#[cfg(test)]
mod tests {
    use atomic_counter::AtomicCounter;
    use atomic_counter::RelaxedCounter;
    use p2p_client::common::{ ConnectionType };
    use p2p_client::network::{ NetworkMessage, NetworkPacket, NetworkRequest };
    use p2p_client::connection::{ P2PEvent, P2PNodeMode, MessageManager };
    use p2p_client::p2p::p2p_node::{ P2PNode };
    use p2p_client::prometheus_exporter::{PrometheusMode, PrometheusServer};
    use std::sync::mpsc;
    use std::sync::{Arc, Mutex };
    use std::sync::atomic::{ AtomicUsize, Ordering};
    use std::cell::{ RefCell };
    use std::{thread, time};
    use rand::{ Rng };
    use failure::{ Fallible  };

    #[derive(Debug, Clone)]
    pub enum NetworkStep {
        Handshake(u16),
        Broadcast(u16),
    }

    mod utils
    {
        use std::sync::mpsc::{ Receiver };
        use std::sync::{Arc, Mutex, Once, ONCE_INIT };
        use std::cell::{ RefCell };
        use std::sync::atomic::{ AtomicUsize, Ordering};
        use std::time;
        use failure::{ Fallible };

        use p2p_client::common::{ ConnectionType };
        use p2p_client::p2p::p2p_node::{ P2PNode };
        use p2p_client::connection::{ P2PNodeMode, MessageManager };
        use p2p_client::network::{ NetworkMessage, NetworkPacket, NetworkResponse };

        static INIT: Once = ONCE_INIT;
        static PORT_OFFSET: AtomicUsize = AtomicUsize::new(0);
        static PORT_START: u16 = 8888;

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
        pub fn next_port_offset( slot_size: usize) -> u16 {
            PORT_OFFSET.fetch_add( slot_size, Ordering::SeqCst) as u16 + PORT_START
        }

        /// It initializes the global logger with a `env_logger`, but just once.
        pub fn setup() {
            INIT.call_once( || {
                env_logger::init()
            });

            /*
            // @note It adds thread ID to each message.
            INIT.call_once( || {
            let mut builder = env_logger::Builder::from_default_env();
            builder.format(
            |buf, record| {
            let curr_thread = thread::current();
            writeln!( buf, "{}@{:?} {}", record.level(), curr_thread.id(), record.args())
            })
            .init();
            });*/
        }

        #[cfg( debug_assertions)]
        pub fn max_recv_timeout() -> std::time::Duration {
            time::Duration::from_secs( 5 * 60) // 5 minutes
        }

        #[cfg( not( debug_assertions))]
        pub fn max_recv_timeout() -> std::time::Duration {
            time::Duration::from_secs( 60) // 1 minutes
        }

        /// It makes a list of nodes using `make_node_and_sync`.
        ///
        /// # Arguments
        /// * `port` - Initial port. Each node will use the port `port` + `i` where `i` is `[0,
        /// count)`.
        /// * `count` - Number of nodes to be generated.
        /// * `networks` - Networks added to new nodes.
        ///
        /// # Return
        /// As `make_node_and_sync`, this returns a tuple but it contains list of objects instead of
        /// just one.
        pub fn make_nodes_from_port( port: u16, count: usize, networks: &Vec<u16>)
            -> Fallible<(Vec<RefCell<P2PNode>>, Vec<Receiver<NetworkMessage>>)>
        {
            let mut nodes = Vec::with_capacity( count);
            let mut receivers = Vec::with_capacity( count);

            for i in 0..count {
                let (node, receiver) = make_node_and_sync( port + i as u16, networks, true)?;

                nodes.push( RefCell::new(node));
                receivers.push( receiver);
            }

            Ok((nodes, receivers))
        }

        /// It creates a pair of `P2PNode` and a `Receiver` which can be used to wait for specific
        /// messages.
        /// Using this approach protocol tests will be easier and cleaner.
        pub fn make_node_and_sync(
                port: u16,
                networks: &Vec<u16>,
                blind_trusted_broadcast: bool ) -> Fallible<(P2PNode, Receiver<NetworkMessage>)>
        {
            let (net_tx, _) = std::sync::mpsc::channel();
            let (msg_wait_tx, msg_wait_rx) = std::sync::mpsc::channel();

            let mut node = P2PNode::new(
                None, Some("127.0.0.1".to_string()), port,
                None, None, net_tx, None, P2PNodeMode::NormalPrivateMode, None,
                networks.clone(), 100, blind_trusted_broadcast);

            let mh = node.message_handler();
            safe_write!(mh)?.add_callback(
                make_atomic_callback!( move |m: &NetworkMessage|{
                    into_err!(msg_wait_tx.send(m.clone()))}));

            node.spawn();
            Ok((node, msg_wait_rx))
        }

        /// It connects `source` and `target` nodes, and it waits until `receiver` receive a
        /// `handshake` response packet.
        /// Other messages are ignored.
        pub fn connect_and_wait_handshake(
            source: &mut P2PNode,
            target: &P2PNode,
            receiver: &Receiver<NetworkMessage>) -> Fallible<()>
        {
            source.connect(
                ConnectionType::Node,
                target.get_listening_ip(),
                target.get_listening_port(), None)?;

            // Wait for Handshake response on source node
            loop {
                match receiver.recv()? {
                    NetworkMessage::NetworkResponse( NetworkResponse::Handshake(_, _, _) , _, _) => break,
                    _ => {}
                }
            }

            Ok(())
        }

        pub fn wait_broadcast_message( waiter: &Receiver<NetworkMessage>) -> Fallible<Vec<u8>>
        {
            let payload;
            loop
            {
                let msg = waiter.recv()?;
                match msg {
                    NetworkMessage::NetworkPacket( NetworkPacket::BroadcastedMessage(_, _, _, data), _, _) => {
                        payload = data;
                        break;
                    },
                    _ => { }
                };
            }

            Ok(payload)
        }

        pub fn wait_direct_message( waiter: &Receiver<NetworkMessage>) -> Fallible<Vec<u8>>
        {
            let payload;
            loop
            {
                let msg = waiter.recv()?;
                match msg {
                    NetworkMessage::NetworkPacket( NetworkPacket::DirectMessage(_, _, _, _, data) , _, _) => {
                        payload = data;
                        break;
                    },
                    _ => { }
                };
            }

            Ok(payload)
        }



        pub fn wait_direct_message_timeout(
                waiter: &Receiver<NetworkMessage>,
                timeout: std::time::Duration) -> Option<Vec<u8>>
        {
            let mut payload = None;
            loop
            {
                match waiter.recv_timeout( timeout)
                {
                    Ok(msg) => match msg {
                        NetworkMessage::NetworkPacket( NetworkPacket::DirectMessage(_, _, _, _, data) , _, _) => {
                            payload = Some(data);
                            break;
                        },
                        _ => { }
                    },
                    Err(_timeout_error) => break
                }
            }

            payload
        }

        #[allow(dead_code)]
        pub fn consume_pending_messages( waiter: &Receiver<NetworkMessage>)
        {
            let max_wait_time = time::Duration::from_millis(250);
            loop {
                if let Err(_) = waiter.recv_timeout(max_wait_time) {
                    break
                }
            }

        }
    }

    /// Counter implementation
    #[derive(Clone)]
    pub struct Counter(pub Arc<AtomicUsize>);

    impl Counter {
        /// Creates a new `Counter` starting with the given value.
        pub fn new(value: usize) -> Self {
            Counter(Arc::new(AtomicUsize::new(value)))
        }

        /// Retrieves the current counter value.
        pub fn get(&self) -> usize {
            self.0.load(Ordering::SeqCst)
        }

        /// Increase the current counter by `ticks`.
        pub fn tick(&self, ticks: usize) {
            self.0.fetch_add(ticks, Ordering::SeqCst);
        }
    }



    #[test]
    pub fn e2e_000_two_nodes() -> Fallible<()> {
        utils::setup();

        let msg = b"Hello other brother!".to_vec();
        let port = utils::next_port_offset(2);
        let networks = vec![100 as u16];

        let (mut node_1, msg_waiter_1) = utils::make_node_and_sync( port, &networks, true)?;
        let (mut node_2, _msg_waiter_2) = utils::make_node_and_sync( port + 1, &networks, true)?;
        utils::connect_and_wait_handshake( &mut node_1, &node_2, &msg_waiter_1)?;
        utils::consume_pending_messages( &msg_waiter_1);

        node_2.send_message(Some(node_1.get_own_id()), 100, None, &msg, false)?;
        let msg_recv = utils::wait_direct_message( &msg_waiter_1)?;
        assert_eq!( msg, msg_recv);

        Ok(())
    }

    #[test]
    pub fn e2e_001_two_nodes_wrong_net() -> Fallible<()> {
        utils::setup();

        let port = utils::next_port_offset(5);
        let networks_1 = vec![100 as u16];
        let networks_2 = vec![200 as u16];
        let msg = b"Hello other brother!".to_vec();

        let (mut node_1, msg_waiter_1) = utils::make_node_and_sync( port, &networks_1, true)?;
        let (mut node_2, _) = utils::make_node_and_sync( port+1, &networks_2, true)?;
        utils::connect_and_wait_handshake( &mut node_1, &node_2, &msg_waiter_1)?;
        utils::consume_pending_messages( &msg_waiter_1);

        // Send msg
        node_2.send_message(
                Some(node_1.get_own_id()), 100, None, &msg, false)?;
        let received_msg = utils::wait_direct_message_timeout(
                &msg_waiter_1,
                utils::max_recv_timeout());
        assert_eq!( received_msg, Some(msg));

        Ok(())
    }

    #[test]
    pub fn e2e_001_trust_broadcast() -> Fallible<()>
    {
        utils::setup();

        let msg = b"Hello other brother!".to_vec();
        let port = utils::next_port_offset(3);
        let networks = vec![100 as u16];

        let (mut node_1, _msg_waiter_1) = utils::make_node_and_sync( port, &networks, true)?;
        let (mut node_2, msg_waiter_2) = utils::make_node_and_sync( port+1, &networks, true)?;
        let (mut node_3, msg_waiter_3) = utils::make_node_and_sync( port+2, &networks, true)?;

        utils::connect_and_wait_handshake( &mut node_2, &node_1, &msg_waiter_2)?;
        utils::connect_and_wait_handshake( &mut node_3, &node_2, &msg_waiter_3)?;

        node_1.send_message(None, 100, None, &msg, true)?;
        let msg_broadcast = utils::wait_broadcast_message( &msg_waiter_3)?;
        assert_eq!( msg_broadcast, msg);
        Ok(())
    }

    #[test]
    pub fn e2e_001_trust_broadcast_wrong_net() -> Fallible<()>
    {
        utils::setup();

        let msg = b"Hello other brother!".to_vec();
        let port = utils::next_port_offset(3);
        let networks_1 = vec![100 as u16];
        let networks_2 = vec![200 as u16];

        let (mut node_1, _msg_waiter_1) = utils::make_node_and_sync( port, &networks_1, true)?;
        let (mut node_2, msg_waiter_2) = utils::make_node_and_sync( port+1, &networks_2, true)?;
        let (mut node_3, msg_waiter_3) = utils::make_node_and_sync( port+2, &networks_2, true)?;

        utils::connect_and_wait_handshake( &mut node_2, &node_1, &msg_waiter_2)?;
        utils::connect_and_wait_handshake( &mut node_3, &node_2, &msg_waiter_3)?;
        utils::consume_pending_messages( &msg_waiter_3);

        node_1.send_message(None, 100, None, &msg, true)?;
        if let Ok(msg) = msg_waiter_3.recv_timeout( time::Duration::from_secs(5))
        {
            match msg
            {
                NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(_, _, _, _), _, _) => {
                    panic!("Got message this should not happen!");
                }
                ref x => {
                    panic!("Didn't get message from node_1 on node_3, but got {:?}", x);
                }
            }
        }
        Ok(())
    }

    #[test]
    pub fn e2e_002_small_mesh_net() {
        utils::setup();
        let test_port_added = utils::next_port_offset( 20);

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
        let message_count_estimated = mesh_node_count;

        let mut peer = 0;

        for instance_port in test_port_added..(test_port_added + mesh_node_count) {
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
                                        100,
                                        false);
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

        thread::sleep(time::Duration::from_secs(30));

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
            let message_counter = Counter::new(0);
            let message_count_estimated = island_size*islands_count*islands_count;

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

                for instance_port in (test_port_added + (island_size * island))
                                    ..(test_port_added + island_size + (island_size * island))
                {
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
                                                100,
                                                false);
                    let mut _node_self_clone = node.clone();

                    let _inner_counter = message_counter.clone();
                    let _guard_pkt = thread::spawn(move || {
                        loop {
                            if let Ok(full_msg) = inner_receiver.recv() {
                                match *full_msg.clone() {
                                    box NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(_, ref msgid, ref nid, ref msg), _, _) => {
                                        info!("BroadcastedMessage/{}/{} with size {} received", nid, msgid, msg.len());
                                        _inner_counter.tick(1);
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
                        central_peer.connect(ConnectionType::Node,
                                             "127.0.0.1".parse().unwrap(),
                                             (test_port_added+(island_size*i)) as u16, None)
                            .map_err(|e| println!("{}", e)).ok();
                    }
                };
            };

            thread::sleep(time::Duration::from_secs(30));

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
    pub fn e2e_002_small_mesh_three_islands_net() {
        islands_mesh_test!(utils::next_port_offset( 10) as usize, 3, 3);
    }

    #[test]
    pub fn e2e_003_big_mesh_three_islands_net() {
        islands_mesh_test!(utils::next_port_offset( 20) as usize, 5, 3);
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
        utils::setup();
        let test_port_added = utils::next_port_offset( num_nodes + 1);
        let network_id: u16 = 100;

        let (tx, rx) = mpsc::channel();

        let mut nodes: Vec<P2PNode> = Vec::new();
        let mut node_threads = vec![];

        // 0. Create P2PNodes
        for n in 0..num_nodes {
            let tx_i = tx.clone();

            let mut node = P2PNode::new(
                    None, Some("127.0.0.1".to_string()), test_port_added + n as u16,
                    None, None, tx_i, None, P2PNodeMode::NormalPrivateMode, None,
                    vec![100], 100, true);

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
        let net_rx_msg_1 = net_rx.recv_timeout( utils::max_recv_timeout())
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
        let net_rx_msg_2 = net_rx.recv_timeout( utils::max_recv_timeout())
            .expect( "Unexpected message while broadcast waiting");

        match net_rx_msg_2 {
            NetworkStep::Broadcast( count ) => assert_eq!( (num_nodes -1) as u16, count),
            ref e => panic!( "Unexpected message while broadcast waiting: {:?}", e)
        };

        gather_agent.join().expect( "Gather agent has failed");
    }

    /// This test has been used in
    #[test]
    fn e2e_006_rustls_ready_writeable() -> Fallible<()>
    {
        utils::setup();
        let msg = b"Direct message between nodes".to_vec();
        let networks = vec![100 as u16];
        let port = utils::next_port_offset(2);

        // 1. Create and connect nodes
        let (mut node_1, msg_waiter_1) = utils::make_node_and_sync( port, &networks, true)?;
        let (mut node_2, msg_waiter_2) = utils::make_node_and_sync( port +1, &networks, true)?;
        utils::connect_and_wait_handshake( &mut node_1, &node_2, &msg_waiter_1)?;

        // 2. Send message from n1 to n2.
        node_1.send_message( Some(node_2.get_own_id()), 100, None, &msg, false)?;
        let msg_1 = utils::wait_direct_message( &msg_waiter_2)?;
        assert_eq!( msg_1, msg);

        node_2.send_message( Some(node_1.get_own_id()), 100, None, &msg, false)?;
        let msg_2 = utils::wait_direct_message( &msg_waiter_1)?;
        assert_eq!( msg_2, msg);

        node_1.send_message( Some(node_2.get_own_id()), 102, None, &msg, false)?;
        let msg_3 = utils::wait_direct_message( &msg_waiter_2)?;
        assert_eq!( msg_3, msg);

        Ok(())
    }

    /// It creates a network tree and tries to broadcast a message. Only one node
    /// is connected to upper level. In this way, we force to forward broadcast
    /// messages between nodes which are not *directly* connected.
    ///
    /// # Arguments
    /// * `levels` - Number of levels for network. It should be greater than 1.
    /// * `min_node_per_level` - Minimum number of nodes per level.
    /// * `max_node_per_level` - Maximum number of nodes per level. This upper
    ///     bound is exclusive.
    fn no_relay_broadcast_to_sender_on_tree_network(
            levels: usize,
            min_node_per_level: usize,
            max_node_per_level: usize) -> Fallible<()> {
        let network_id = 100 as u16;
        let networks = vec![network_id];
        let test_port_added = utils::next_port_offset( levels * max_node_per_level);
        let mut rng = rand::thread_rng();

        // 1. Create network: all nodes, per level.
        // At first level, only one node is generated.
        let mut nodes_per_level = Vec::with_capacity( levels);
        let mut conn_waiters_per_level = Vec::with_capacity( levels);

        // 1.1. Root node adds callback for receive last broadcast packet.
        let (node, conn_waiter) = utils::make_node_and_sync( test_port_added, &networks, true)?;
        let (bcast_tx, bcast_rx) = std::sync::mpsc::channel();
        {
            let mh = node.message_handler();
            mh.write().unwrap().add_packet_callback(
                make_atomic_callback!( move |pac: &NetworkPacket| {
                    debug!( "Root node is forwarding Packet to channel");
                    into_err!(bcast_tx.send( pac.clone()))
                }));
        }

        nodes_per_level.push( vec![ RefCell::new(node)]);
        conn_waiters_per_level.push( vec![conn_waiter]);

        // 1.2. Create each level
        for level in 1..levels {
            // 1.2.1. Make nodes
            let count_nodes: usize = rng.gen_range( min_node_per_level, max_node_per_level);
            debug!( "Creating level {} with {} nodes", level, count_nodes);
            let (nodes, conn_waiters) =  utils::make_nodes_from_port(
                    test_port_added + (level * max_node_per_level) as u16,
                    count_nodes,
                    &networks)?;

            nodes_per_level.push( nodes);
            conn_waiters_per_level.push( conn_waiters);

            // 1.2.2 Connect one to previous level.
            let root_previous_level_idx: usize = rng.gen_range( 0, nodes_per_level[level-1].len());
            let root_curr_level_idx: usize = rng.gen_range( 0, count_nodes);

            let target_node = nodes_per_level[level-1][root_previous_level_idx].clone();
            {
                let src_node = &nodes_per_level[level][root_curr_level_idx];
                let src_waiter = &conn_waiters_per_level[level][root_curr_level_idx];
                utils::connect_and_wait_handshake( &mut *src_node.borrow_mut(),
                               &*target_node.borrow(),
                               src_waiter)?;
            }
            debug!( "Connected to previous level: Node[{}][{}] to Node[{}][{}]",
                    level, root_curr_level_idx, level-1, root_previous_level_idx);

            // 1.2.3. Connect all nodes in that level to this.
            for i in 0..count_nodes  {
                if i != root_curr_level_idx {
                    let src_node = &nodes_per_level[level][i];
                    let src_waiter = &conn_waiters_per_level[level][i];
                    let tgt_node = &nodes_per_level[level][root_curr_level_idx];
                    utils::connect_and_wait_handshake(
                            &mut *src_node.borrow_mut(),
                            &*tgt_node.borrow(),
                            src_waiter)?;
                }
            }
        }

        // 2. Log network structure into debug.
        let mut debug_level_str = String::new();
        for level in 0.. levels {
            debug_level_str.push_str( format!( "\n\t[{}]: ", level).as_str());
            for idx in 0..nodes_per_level[level].len() {
                debug_level_str.push_str(
                    format!( "{}, ",
                    nodes_per_level[level][idx].borrow().get_listening_port()).as_str());
            }
        }
        debug!("Network has been created with {} levels: {}", levels, debug_level_str);

        let mut wait_loop = true;
        while wait_loop {

            // 3. Select random node in last level and send a broadcast.
            let bcast_content = "Hello from last level node";
            {
                let src_idx = rng.gen_range( 0, nodes_per_level[levels-1].len());
                let src_node = &mut nodes_per_level[levels-1][src_idx];
                let src_node_id = Some(src_node.borrow().get_own_id());

                debug!( "Send message from {} in broadcast",
                        src_node.borrow().get_listening_port());

                src_node.borrow_mut().send_message(
                    src_node_id, network_id, None,
                    &bcast_content.as_bytes().to_vec(), true)
                    .map_err( |e| panic!(e))
                    .ok();
            }

            // 4. Wait broadcast in root.
            debug!( "Waiting broadcast message");
            if let Ok(bcast_msg) = bcast_rx.recv_timeout( time::Duration::from_secs(2)) {
                debug!( "Waiting broadcast message - finished");
                match bcast_msg {
                    NetworkPacket::BroadcastedMessage(ref _sender, ref _msgid, ref _nid, ref msg) => {
                        let str_msg = std::str::from_utf8( msg).unwrap();
                        assert_eq!( str_msg, bcast_content);
                        wait_loop = false;
                    },
                    _ => {}
                }
            }
        }

        Ok(())
    }

    /// It creates a linear network structure of 3 levels, using just one node per level.
    /// Network for this test is:
    ///     (Node 1) <--- (Node 2) <--- (Node 3).
    /// Test sends a broadcast message from `Node 3`, and it double-checks that
    /// `Node 1` will receive that message.
    ///
    #[test]
    pub fn e2e_005_001_no_relay_broadcast_to_sender_on_linear_network() -> Fallible<()> {
        utils::setup();
        no_relay_broadcast_to_sender_on_tree_network( 3, 1, 2)
    }


    /// It creates a tree network structure of 3 levels, using between 2 and 3 nodes
    /// per level.
    #[test]
    pub fn e2e_005_002_no_relay_broadcast_to_sender_on_tree_network() -> Fallible<()> {
        utils::setup();
        no_relay_broadcast_to_sender_on_tree_network( 3, 2, 4)
    }

    /// It create a *complex* network structure of 5 levels, using between 4 and 9 nodes
    /// per level.
    #[test]
    pub fn e2e_005_003_no_relay_broadcast_to_sender_on_complex_tree_network() -> Fallible<()> {
        utils::setup();
        no_relay_broadcast_to_sender_on_tree_network( 5, 4, 10)
    }
}
