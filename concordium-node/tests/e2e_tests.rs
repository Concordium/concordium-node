#[macro_use]
extern crate p2p_client;
#[macro_use]
extern crate log;

#[cfg(test)]
mod tests {
    use failure::{bail, Fallible};
    use p2p_client::{
        common::{PeerType, UCursor},
        configuration::Config,
        connection::{MessageManager, P2PEvent},
        network::{NetworkId, NetworkMessage, NetworkPacket, NetworkPacketType},
        p2p::{banned_nodes::BannedNode, p2p_node::P2PNode},
        prometheus_exporter::{PrometheusMode, PrometheusServer},
    };
    use rand::{distributions::Standard, thread_rng, Rng};
    use std::{
        cell::RefCell,
        sync::{
            atomic::{AtomicUsize, Ordering},
            mpsc, Arc, RwLock,
        },
        thread, time,
    };

    mod utils {
        use failure::Fallible;
        use std::{
            cell::RefCell,
            sync::{
                atomic::{AtomicUsize, Ordering},
                mpsc::Receiver,
                Arc, Once, RwLock, ONCE_INIT,
            },
            time,
        };

        use p2p_client::{
            common::{PeerType, UCursor},
            configuration::Config,
            connection::MessageManager,
            network::{NetworkMessage, NetworkPacketType, NetworkResponse},
            p2p::p2p_node::P2PNode,
        };

        static INIT: Once = ONCE_INIT;
        static PORT_OFFSET: AtomicUsize = AtomicUsize::new(0);
        static PORT_START: u16 = 8888;

        /// It returns next port available and it ensures that next `slot_size`
        /// ports will be available too.
        ///
        /// # Arguments
        /// * `slot_size` - Size of blocked ports. It
        ///
        /// # Example
        /// ```
        /// let port_range_1 = next_port_offset(10); // It will return 0, you can use from 0..9
        /// let port_range_2 = next_port_offset(20); // It will return 10, you can use from 10..19
        /// let port_range_3 = next_port_offset(100); // It will return 30, you can use from 20..129
        /// let port_range_4 = next_port_offset(130);
        /// ```
        pub fn next_port_offset(slot_size: usize) -> u16 {
            PORT_OFFSET.fetch_add(slot_size, Ordering::SeqCst) as u16 + PORT_START
        }

        /// It initializes the global logger with a `env_logger`, but just once.
        pub fn setup() {
            INIT.call_once(|| env_logger::init());

            // @note It adds thread ID to each message.
            // INIT.call_once( || {
            // let mut builder = env_logger::Builder::from_default_env();
            // builder.format(
            // |buf, record| {
            // let curr_thread = thread::current();
            // writeln!( buf, "{}@{:?} {}", record.level(), curr_thread.id(), record.args())
            // })
            // .init();
            // });
        }

        #[cfg(debug_assertions)]
        pub fn max_recv_timeout() -> std::time::Duration {
            time::Duration::from_secs(5 * 60) // 5 minutes
        }

        #[cfg(not(debug_assertions))]
        pub fn max_recv_timeout() -> std::time::Duration {
            time::Duration::from_secs(60) // 1 minutes
        }

        /// It makes a list of nodes using `make_node_and_sync`.
        ///
        /// # Arguments
        /// * `port` - Initial port. Each node will use the port `port` + `i`
        ///   where `i` is `[0,
        /// count)`.
        /// * `count` - Number of nodes to be generated.
        /// * `networks` - Networks added to new nodes.
        ///
        /// # Return
        /// As `make_node_and_sync`, this returns a tuple but it contains list
        /// of objects instead of just one.
        pub fn make_nodes_from_port(
            port: u16,
            count: usize,
            networks: Vec<u16>,
        ) -> Fallible<Vec<(RefCell<P2PNode>, Receiver<NetworkMessage>)>> {
            let mut nodes_and_receivers = Vec::with_capacity(count);

            for i in 0..count {
                let (node, receiver) = make_node_and_sync(port + i as u16, networks.clone(), true)?;

                nodes_and_receivers.push((RefCell::new(node), receiver));
            }

            Ok(nodes_and_receivers)
        }

        /// It creates a pair of `P2PNode` and a `Receiver` which can be used to
        /// wait for specific messages.
        /// Using this approach protocol tests will be easier and cleaner.
        pub fn make_node_and_sync(
            port: u16,
            networks: Vec<u16>,
            blind_trusted_broadcast: bool,
        ) -> Fallible<(P2PNode, Receiver<NetworkMessage>)> {
            let (net_tx, _) = std::sync::mpsc::channel();
            let (msg_wait_tx, msg_wait_rx) = std::sync::mpsc::channel();

            let mut config = Config::new(Some("127.0.0.1".to_owned()), port, networks, 100);
            config.connection.no_trust_broadcasts = blind_trusted_broadcast;

            let mut node = P2PNode::new(None, &config, net_tx, None, PeerType::Node, None);

            let mh = node.message_handler();
            safe_write!(mh)?.add_callback(make_atomic_callback!(move |m: &NetworkMessage| {
                into_err!(msg_wait_tx.send(m.clone()))
            }));

            node.spawn();
            Ok((node, msg_wait_rx))
        }

        /// It connects `source` and `target` nodes, and it waits until
        /// `receiver` receive a `handshake` response packet.
        /// Other messages are ignored.
        pub fn connect_and_wait_handshake(
            source: &mut P2PNode,
            target: &P2PNode,
            receiver: &Receiver<NetworkMessage>,
        ) -> Fallible<()> {
            source.connect(
                PeerType::Node,
                target.get_listening_ip(),
                target.get_listening_port(),
                None,
            )?;

            // Wait for Handshake response on source node
            loop {
                if let NetworkMessage::NetworkResponse(NetworkResponse::Handshake(..), ..) =
                    receiver.recv()?
                {
                    break;
                }
            }

            Ok(())
        }

        pub fn wait_broadcast_message(waiter: &Receiver<NetworkMessage>) -> Fallible<UCursor> {
            let payload;
            loop {
                let msg = waiter.recv()?;
                if let NetworkMessage::NetworkPacket(ref pac, ..) = msg {
                    if let NetworkPacketType::BroadcastedMessage = pac.packet_type {
                        payload = pac.message.clone();
                        break;
                    }
                }
            }

            Ok(payload)
        }

        pub fn wait_direct_message(waiter: &Receiver<NetworkMessage>) -> Fallible<UCursor> {
            let payload;
            loop {
                let msg = waiter.recv()?;
                if let NetworkMessage::NetworkPacket(ref pac, ..) = msg {
                    if let NetworkPacketType::DirectMessage(..) = pac.packet_type {
                        payload = pac.message.clone();
                        break;
                    }
                }
            }

            Ok(payload)
        }

        pub fn wait_direct_message_timeout(
            waiter: &Receiver<NetworkMessage>,
            timeout: std::time::Duration,
        ) -> Option<UCursor> {
            let mut payload = None;
            loop {
                match waiter.recv_timeout(timeout) {
                    Ok(msg) => {
                        if let NetworkMessage::NetworkPacket(ref pac, ..) = msg {
                            if let NetworkPacketType::DirectMessage(..) = pac.packet_type {
                                payload = Some(pac.message.clone());
                                break;
                            }
                        }
                    }
                    Err(_timeout_error) => break,
                }
            }

            payload
        }

        #[allow(dead_code)]
        pub fn consume_pending_messages(waiter: &Receiver<NetworkMessage>) {
            let max_wait_time = time::Duration::from_millis(250);
            loop {
                if waiter.recv_timeout(max_wait_time).is_err() {
                    break;
                }
            }
        }
    }

    /// Counter implementation
    #[derive(Clone)]
    pub struct Counter(pub Arc<AtomicUsize>);

    impl Counter {
        /// Creates a new `Counter` starting with the given value.
        pub fn new(value: usize) -> Self { Counter(Arc::new(AtomicUsize::new(value))) }

        /// Retrieves the current counter value.
        pub fn get(&self) -> usize { self.0.load(Ordering::SeqCst) }

        /// Increase the current counter by `ticks`.
        pub fn tick(&self, ticks: usize) { self.0.fetch_add(ticks, Ordering::SeqCst); }
    }

    #[test]
    pub fn e2e_000_two_nodes() -> Fallible<()> {
        utils::setup();

        let msg = b"Hello other brother!".to_vec();
        let port = utils::next_port_offset(2);
        let networks = vec![100];

        let (mut node_1, msg_waiter_1) = utils::make_node_and_sync(port, networks.clone(), true)?;
        let (mut node_2, _msg_waiter_2) = utils::make_node_and_sync(port + 1, networks, true)?;
        utils::connect_and_wait_handshake(&mut node_1, &node_2, &msg_waiter_1)?;
        utils::consume_pending_messages(&msg_waiter_1);

        node_2.send_message(
            Some(node_1.id()),
            NetworkId::from(100),
            None,
            msg.clone(),
            false,
        )?;
        let mut msg_recv = utils::wait_direct_message(&msg_waiter_1)?;
        assert_eq!(msg.as_slice(), msg_recv.read_all_into_view()?.as_slice());

        Ok(())
    }

    #[test]
    pub fn e2e_001_two_nodes_wrong_net() -> Fallible<()> {
        utils::setup();

        let port = utils::next_port_offset(5);
        let networks_1 = vec![100];
        let networks_2 = vec![200];
        let msg = b"Hello other brother!".to_vec();

        let (mut node_1, msg_waiter_1) = utils::make_node_and_sync(port, networks_1, true)?;
        let (mut node_2, _) = utils::make_node_and_sync(port + 1, networks_2, true)?;
        utils::connect_and_wait_handshake(&mut node_1, &node_2, &msg_waiter_1)?;
        utils::consume_pending_messages(&msg_waiter_1);

        // Send msg
        node_2.send_message(
            Some(node_1.id()),
            NetworkId::from(100),
            None,
            msg.clone(),
            false,
        )?;
        let received_msg =
            utils::wait_direct_message_timeout(&msg_waiter_1, utils::max_recv_timeout());
        assert_eq!(received_msg, Some(UCursor::from(msg)));

        Ok(())
    }

    #[test]
    pub fn e2e_001_trust_broadcast() -> Fallible<()> {
        utils::setup();

        let msg = b"Hello other brother!".to_vec();
        let port = utils::next_port_offset(3);
        let networks = vec![100];

        let (mut node_1, _msg_waiter_1) = utils::make_node_and_sync(port, networks.clone(), true)?;
        let (mut node_2, msg_waiter_2) =
            utils::make_node_and_sync(port + 1, networks.clone(), true)?;
        let (mut node_3, msg_waiter_3) = utils::make_node_and_sync(port + 2, networks, true)?;

        utils::connect_and_wait_handshake(&mut node_2, &node_1, &msg_waiter_2)?;
        utils::connect_and_wait_handshake(&mut node_3, &node_2, &msg_waiter_3)?;

        node_1.send_message(None, NetworkId::from(100), None, msg.clone(), true)?;
        let msg_broadcast = utils::wait_broadcast_message(&msg_waiter_3)?.read_all_into_view()?;
        assert_eq!(msg_broadcast.as_slice(), msg.as_slice());
        Ok(())
    }

    #[test]
    pub fn e2e_001_trust_broadcast_wrong_net() -> Fallible<()> {
        utils::setup();

        let msg = b"Hello other brother!".to_vec();
        let port = utils::next_port_offset(3);
        let networks_1 = vec![100];
        let networks_2 = vec![200];

        let (mut node_1, _msg_waiter_1) = utils::make_node_and_sync(port, networks_1, true)?;
        let (mut node_2, msg_waiter_2) =
            utils::make_node_and_sync(port + 1, networks_2.clone(), true)?;
        let (mut node_3, msg_waiter_3) = utils::make_node_and_sync(port + 2, networks_2, true)?;

        utils::connect_and_wait_handshake(&mut node_2, &node_1, &msg_waiter_2)?;
        utils::connect_and_wait_handshake(&mut node_3, &node_2, &msg_waiter_3)?;
        utils::consume_pending_messages(&msg_waiter_3);

        node_1.send_message(None, NetworkId::from(100), None, msg, true)?;
        if let Ok(msg) = msg_waiter_3.recv_timeout(time::Duration::from_secs(5)) {
            match msg {
                NetworkMessage::NetworkPacket(ref pac, ..) => {
                    if let NetworkPacketType::BroadcastedMessage = pac.packet_type {
                        panic!("Got a message; this should not happen!");
                    }
                }
                x => {
                    panic!(
                        "Didn't get a message from node_1 on node_3, but got {:?}",
                        x
                    );
                }
            }
        }
        Ok(())
    }

    #[test]
    pub fn e2e_002_small_mesh_net() {
        utils::setup();
        let test_port_added = utils::next_port_offset(20);

        let mesh_node_count = 15;

        let (sender, receiver) = mpsc::channel();

        let _guard = thread::spawn(move || loop {
            if let Ok(msg) = receiver.recv() {
                match msg {
                    P2PEvent::ConnectEvent(ip, port) => {
                        info!("Received connection from {}:{}", ip, port)
                    }
                    P2PEvent::DisconnectEvent(msg) => info!("Received disconnect for {}", msg),
                    P2PEvent::ReceivedMessageEvent(node_id) => {
                        info!("Received message from {:?}", node_id)
                    }
                    P2PEvent::SentMessageEvent(node_id) => info!("Sent message to {:?}", node_id),
                    P2PEvent::InitiatingConnection(ip, port) => {
                        info!("Initiating connection to {}:{}", ip, port)
                    }
                    P2PEvent::JoinedNetwork(peer, network_id) => {
                        info!(
                            "Peer {} joined network {}",
                            peer.id().to_string(),
                            network_id
                        );
                    }
                    P2PEvent::LeftNetwork(peer, network_id) => {
                        info!("Peer {} left network {}", peer.id().to_string(), network_id);
                    }
                }
            }
        });

        let message_count_estimated = mesh_node_count;
        let mut peers: Vec<(usize, P2PNode, PrometheusServer)> =
            Vec::with_capacity(mesh_node_count);
        let mut peer_ports: Vec<usize> = Vec::with_capacity(mesh_node_count);

        let message_counter = Arc::new(AtomicUsize::new(0));

        let mut peer = 0;

        for instance_port in test_port_added..(test_port_added + mesh_node_count as u16) {
            let (inner_sender, inner_receiver) = mpsc::channel();
            let prometheus = PrometheusServer::new(PrometheusMode::NodeMode);

            let config = Config::new(
                Some("127.0.0.1".to_owned()),
                instance_port as u16,
                vec![100],
                100,
            );

            let mut node = P2PNode::new(
                None,
                &config,
                inner_sender,
                Some(sender.clone()),
                PeerType::Node,
                Some(Arc::new(RwLock::new(prometheus.clone()))),
            );

            let mut _node_self_clone = node.clone();
            let _msg_counter = message_counter.clone();
            let _guard_pkt = thread::spawn(move || loop {
                if let Ok(full_msg) = inner_receiver.recv() {
                    if let NetworkMessage::NetworkPacket(ref pac, ..) = *full_msg {
                        if let NetworkPacketType::BroadcastedMessage = pac.packet_type {
                            info!(
                                "BroadcastedMessage/{}/{} with size {} received",
                                pac.network_id,
                                pac.message_id,
                                pac.message.len()
                            );
                            _node_self_clone
                                .send_message_from_cursor(
                                    None,
                                    pac.network_id,
                                    Some(pac.message_id.clone()),
                                    pac.message.clone(),
                                    true,
                                )
                                .map_err(|e| error!("Error sending message {}", e))
                                .ok();
                            _msg_counter.fetch_add(1, Ordering::Relaxed);
                        }
                    }
                }
            });
            node.spawn();
            if peer > 0 {
                let localhost = "127.0.0.1".parse().unwrap();
                for i in 0..peer {
                    node.connect(
                        PeerType::Node,
                        localhost,
                        (instance_port - 1 - i) as u16,
                        None,
                    )
                    .ok();
                }
            }
            peer += 1;
            peers.push((instance_port as usize, node, prometheus));
            peer_ports.push(instance_port as usize);
        }

        thread::sleep(time::Duration::from_secs(5));

        let msg = b"Hello other mother's brother".to_vec();
        if let Some((.., ref mut node_sender_ref, _)) = peers.get_mut(0) {
            node_sender_ref
                .send_message(None, NetworkId::from(100), None, msg, true)
                .map_err(|e| panic!(e))
                .ok();
        };

        thread::sleep(time::Duration::from_secs(30));

        for peer in &peers {
            match peer.2.queue_size() {
                Ok(size) => assert_eq!(0, size),
                _ => panic!("Can't read queue size!"),
            }
        }

        assert_eq!(
            message_count_estimated as usize,
            message_counter.load(Ordering::Relaxed)
        );
    }

    fn islands_mesh_test(test_port_added: usize, island_size: usize, islands_count: usize) {
        let message_counter = Counter::new(0);
        let message_count_estimated = island_size * islands_count * islands_count;

        let (sender, receiver) = mpsc::channel();

        let _guard = thread::spawn(move || loop {
            if let Ok(msg) = receiver.recv() {
                match msg {
                    P2PEvent::ConnectEvent(ip, port) => {
                        info!("Received connection from {}:{}", ip, port)
                    }
                    P2PEvent::DisconnectEvent(msg) => info!("Received disconnect for {}", msg),
                    P2PEvent::ReceivedMessageEvent(node_id) => {
                        info!("Received message from {:?}", node_id)
                    }
                    P2PEvent::SentMessageEvent(node_id) => info!("Sent message to {:?}", node_id),
                    P2PEvent::InitiatingConnection(ip, port) => {
                        info!("Initiating connection to {}:{}", ip, port)
                    }
                    P2PEvent::JoinedNetwork(peer, network_id) => {
                        info!(
                            "Peer {} joined network {}",
                            peer.id().to_string(),
                            network_id
                        );
                    }
                    P2PEvent::LeftNetwork(peer, network_id) => {
                        info!("Peer {} left network {}", peer.id().to_string(), network_id);
                    }
                }
            }
        });

        let mut islands: Vec<Vec<(usize, P2PNode, PrometheusServer, usize)>> =
            Vec::with_capacity(islands_count);
        let localhost = "127.0.0.1".parse().unwrap();

        for island in 0..islands_count {
            let mut peers_islands_and_ports: Vec<(usize, P2PNode, PrometheusServer, usize)> =
                Vec::with_capacity(island_size);

            let mut peer = 0;

            for instance_port in (test_port_added + (island_size * island))
                ..(test_port_added + (island_size * island) + island_size)
            {
                let (inner_sender, inner_receiver) = mpsc::channel();
                let prometheus = PrometheusServer::new(PrometheusMode::NodeMode);

                let config = Config::new(
                    Some("127.0.0.1".to_owned()),
                    instance_port as u16,
                    vec![100],
                    100,
                );

                let mut node = P2PNode::new(
                    None,
                    &config,
                    inner_sender,
                    Some(sender.clone()),
                    PeerType::Node,
                    Some(Arc::new(RwLock::new(prometheus.clone()))),
                );
                let mut _node_self_clone = node.clone();

                let _inner_counter = message_counter.clone();
                let _guard_pkt = thread::spawn(move || loop {
                    if let Ok(full_msg) = inner_receiver.recv() {
                        if let NetworkMessage::NetworkPacket(ref pac, ..) = *full_msg {
                            if let NetworkPacketType::BroadcastedMessage = pac.packet_type {
                                info!(
                                    "BroadcastedMessage/{}/{} with size {} received",
                                    pac.network_id,
                                    pac.message_id,
                                    pac.message.len()
                                );
                                _inner_counter.tick(1);
                                _node_self_clone
                                    .send_message_from_cursor(
                                        None,
                                        pac.network_id,
                                        Some(pac.message_id.clone()),
                                        pac.message.clone(),
                                        true,
                                    )
                                    .map_err(|e| error!("Error sending message {}", e))
                                    .ok();
                            }
                        }
                    }
                });
                node.spawn();
                if peer > 0 {
                    for i in 0..peer {
                        node.connect(
                            PeerType::Node,
                            localhost,
                            (instance_port - 1 - (i)) as u16,
                            None,
                        )
                        .ok();
                    }
                }
                peer += 1;
                peers_islands_and_ports.push((instance_port, node, prometheus, instance_port));
            }
            islands.push(peers_islands_and_ports);
        }

        thread::sleep(time::Duration::from_secs(3));

        if let Some(ref mut peers) = islands.get_mut(0) {
            if let Some((.., ref mut central_peer, _, _)) = peers.get_mut(0) {
                for i in 1..islands_count {
                    central_peer
                        .connect(
                            PeerType::Node,
                            localhost,
                            (test_port_added + (island_size * i)) as u16,
                            None,
                        )
                        .map_err(|e| println!("{}", e))
                        .ok();
                }
            };
        };

        thread::sleep(time::Duration::from_secs(3));

        let msg = b"Hello other mother's brother".to_vec();

        for island in &mut islands {
            if let Some((.., ref mut node_sender_ref, _, _)) = island.get_mut(0) {
                node_sender_ref
                    .send_message(None, NetworkId::from(100), None, msg.clone(), true)
                    .map_err(|e| panic!(e))
                    .ok();
            };
        }

        thread::sleep(time::Duration::from_secs(3));

        for island in islands {
            for peer in &island {
                match peer.2.queue_size() {
                    Ok(size) => assert_eq!(0, size),
                    _ => panic!("Can't read queue size!"),
                }
            }
        }

        assert_eq!(message_count_estimated, message_counter.get());
    }

    #[test]
    pub fn e2e_002_small_mesh_three_islands_net() {
        islands_mesh_test(utils::next_port_offset(10) as usize, 3, 3);
    }

    #[ignore]
    #[test]
    pub fn e2e_003_big_mesh_three_islands_net() {
        islands_mesh_test(utils::next_port_offset(20) as usize, 5, 3);
    }

    #[test]
    /// This test calls `no_relay_broadcast_so_sender` test using 2 nodes.
    pub fn e2e_004_2_no_relay_broadcast_to_sender() {
        assert!(no_relay_broadcast_to_sender(2).is_ok());
    }

    #[test]
    /// This test call `no_relay_broadcast_so_sender` test using 8 nodes.
    pub fn e2e_004_8_no_relay_broadcast_to_sender() {
        assert!(no_relay_broadcast_to_sender(8).is_ok());
    }

    #[test]
    /// This test call `no_relay_broadcast_so_sender` test using 20 nodes.
    pub fn e2e_004_20_no_relay_broadcast_to_sender() {
        assert!(no_relay_broadcast_to_sender(20).is_ok());
    }

    /// Creates a star shaped network with `num_nodes - 1` nodes conected to
    /// node 0. It sends a broadcast message from node 0 and counts the
    /// number of received messages that match the original one.
    fn no_relay_broadcast_to_sender(num_nodes: usize) -> Fallible<()> {
        utils::setup();
        let network_id = 100;
        let test_port_added = utils::next_port_offset(num_nodes);

        // 1.1. Root node adds callback for receive last broadcast packet.
        let mut nodes = vec![];
        let mut waiters = vec![];
        let (bcast_tx, bcast_rx) = std::sync::mpsc::channel();
        for i in 0..num_nodes {
            let tx_i = bcast_tx.clone();
            let (node, conn_waiter) =
                utils::make_node_and_sync(test_port_added + (i as u16), vec![network_id], true)?;
            let mh = node.message_handler();
            safe_write!(mh)?.add_packet_callback(make_atomic_callback!(
                move |pac: &NetworkPacket| { into_err!(tx_i.send(pac.clone())) }
            ));
            nodes.push(RefCell::new(node));
            waiters.push(conn_waiter);

            if i != 0 {
                let src_node = &nodes[i];
                let src_waiter = &waiters[i];
                let tgt_node = &nodes[0];
                utils::connect_and_wait_handshake(
                    &mut *src_node.borrow_mut(),
                    &*tgt_node.borrow(),
                    src_waiter,
                )?;
            }
        }

        let mut ack_count = 0;
        while ack_count < num_nodes - 1 {
            // 3. Select random node in last level and send a broadcast.
            let broadcast_msg = b"Hello broadcasted!".to_vec();
            {
                let src_node = &mut nodes[0];
                let src_node_id = Some(src_node.borrow().id());

                debug!(
                    "Send message from {} in broadcast",
                    src_node.borrow().get_listening_port()
                );

                src_node
                    .borrow_mut()
                    .send_message(
                        src_node_id,
                        NetworkId::from(network_id),
                        None,
                        broadcast_msg.clone(),
                        true,
                    )
                    .map_err(|e| panic!(e))
                    .ok();
            }

            // 4. Wait broadcast in root.
            if let Ok(ref mut pac) = bcast_rx.recv_timeout(time::Duration::from_secs(2)) {
                match pac.packet_type {
                    NetworkPacketType::BroadcastedMessage => {
                        let msg = pac.message.read_all_into_view()?;
                        assert_eq!(msg.as_slice(), broadcast_msg.as_slice());
                        ack_count += 1;
                    }
                    _ => {}
                }
            }
        }

        Ok(())
    }

    /// This test has been used in
    #[test]
    fn e2e_006_rustls_ready_writeable() -> Fallible<()> {
        utils::setup();
        let msg = UCursor::from(b"Direct message between nodes".to_vec());
        let networks = vec![100];
        let port = utils::next_port_offset(2);

        // 1. Create and connect nodes
        let (mut node_1, msg_waiter_1) = utils::make_node_and_sync(port, networks.clone(), true)?;
        let (mut node_2, msg_waiter_2) = utils::make_node_and_sync(port + 1, networks, true)?;
        utils::connect_and_wait_handshake(&mut node_1, &node_2, &msg_waiter_1)?;

        // 2. Send message from n1 to n2.
        node_1.send_message_from_cursor(
            Some(node_2.id()),
            NetworkId::from(100),
            None,
            msg.clone(),
            false,
        )?;
        let msg_1 = utils::wait_direct_message(&msg_waiter_2)?;
        assert_eq!(msg_1, msg);

        node_2.send_message_from_cursor(
            Some(node_1.id()),
            NetworkId::from(100),
            None,
            msg.clone(),
            false,
        )?;
        let msg_2 = utils::wait_direct_message(&msg_waiter_1)?;
        assert_eq!(msg_2, msg);

        node_1.send_message_from_cursor(
            Some(node_2.id()),
            NetworkId::from(102),
            None,
            msg.clone(),
            false,
        )?;
        let msg_3 = utils::wait_direct_message(&msg_waiter_2)?;
        assert_eq!(msg_3, msg);

        Ok(())
    }

    #[test]
    fn e2e_007_rustls_write_would_block() -> Fallible<()> {
        utils::setup();

        let msg_content: Vec<u8> = thread_rng()
            .sample_iter(&Standard)
            .take(16 * 1024 * 1024)
            .collect();
        let msg = UCursor::from(msg_content);
        let networks = vec![100];
        let port = utils::next_port_offset(2);

        // 1. Create and connect nodes
        let (mut node_1, msg_waiter_1) = utils::make_node_and_sync(port, networks.clone(), true)?;
        let (node_2, msg_waiter_2) = utils::make_node_and_sync(port + 1, networks, true)?;
        utils::connect_and_wait_handshake(&mut node_1, &node_2, &msg_waiter_1)?;

        // 2. Send message from n1 to n2.
        node_1.send_message_from_cursor(
            Some(node_2.id()),
            NetworkId::from(100),
            None,
            msg.clone(),
            false,
        )?;
        let msg_1 = utils::wait_direct_message(&msg_waiter_2)?;
        assert_eq!(msg_1, msg);

        Ok(())
    }

    /// It creates a network tree and tries to broadcast a message. Only one
    /// node is connected to upper level. In this way, we force to forward
    /// broadcast messages between nodes which are not *directly* connected.
    ///
    /// # Arguments
    /// * `levels` - Number of levels for network. It should be greater than 1.
    /// * `min_node_per_level` - Minimum number of nodes per level.
    /// * `max_node_per_level` - Maximum number of nodes per level. This upper
    ///   bound is exclusive.
    fn no_relay_broadcast_to_sender_on_tree_network(
        levels: usize,
        min_node_per_level: usize,
        max_node_per_level: usize,
    ) -> Fallible<()> {
        let network_id = 100;
        let test_port_added = utils::next_port_offset(levels * max_node_per_level);
        let mut rng = rand::thread_rng();

        // 1. Create network: all nodes, per level.
        // At first level, only one node is generated.
        let mut nodes_per_level = Vec::with_capacity(levels);
        let mut conn_waiters_per_level = Vec::with_capacity(levels);

        // 1.1. Root node adds callback for receive last broadcast packet.
        let (node, conn_waiter) =
            utils::make_node_and_sync(test_port_added, vec![network_id], true)?;
        let (bcast_tx, bcast_rx) = std::sync::mpsc::channel();
        {
            let mh = node.message_handler();
            mh.write()
                .unwrap()
                .add_packet_callback(make_atomic_callback!(move |pac: &NetworkPacket| {
                    debug!("Root node is forwarding Packet to channel");
                    into_err!(bcast_tx.send(pac.clone()))
                }));
        }

        nodes_per_level.push(vec![RefCell::new(node)]);
        conn_waiters_per_level.push(vec![conn_waiter]);

        // 1.2. Create each level
        for level in 1..levels {
            // 1.2.1. Make nodes
            let count_nodes: usize = rng.gen_range(min_node_per_level, max_node_per_level);
            debug!("Creating level {} with {} nodes", level, count_nodes);
            let nodes_and_conn_waiters = utils::make_nodes_from_port(
                test_port_added + (level * max_node_per_level) as u16,
                count_nodes,
                vec![network_id],
            )?;

            let (nodes, waiters) = nodes_and_conn_waiters.into_iter().unzip();
            nodes_per_level.push(nodes);
            conn_waiters_per_level.push(waiters);

            // 1.2.2 Connect one to previous level.
            let root_previous_level_idx: usize = rng.gen_range(0, nodes_per_level[level - 1].len());
            let root_curr_level_idx: usize = rng.gen_range(0, count_nodes);

            let target_node = &nodes_per_level[level - 1][root_previous_level_idx];
            {
                let src_node = &nodes_per_level[level][root_curr_level_idx];
                let src_waiter = &conn_waiters_per_level[level][root_curr_level_idx];
                utils::connect_and_wait_handshake(
                    &mut *src_node.borrow_mut(),
                    &*target_node.borrow(),
                    src_waiter,
                )?;
            }
            debug!(
                "Connected to previous level: Node[{}][{}] to Node[{}][{}]",
                level,
                root_curr_level_idx,
                level - 1,
                root_previous_level_idx
            );

            // 1.2.3. Connect all nodes in that level to this.
            for i in 0..count_nodes {
                if i != root_curr_level_idx {
                    let src_node = &nodes_per_level[level][i];
                    let src_waiter = &conn_waiters_per_level[level][i];
                    let tgt_node = &nodes_per_level[level][root_curr_level_idx];
                    utils::connect_and_wait_handshake(
                        &mut *src_node.borrow_mut(),
                        &*tgt_node.borrow(),
                        src_waiter,
                    )?;
                }
            }
        }

        // 2. Log network structure into debug.
        let mut debug_level_str = String::new();
        for level in 0..levels {
            debug_level_str.push_str(format!("\n\t[{}]: ", level).as_str());
            for idx in 0..nodes_per_level[level].len() {
                debug_level_str.push_str(
                    format!(
                        "{}, ",
                        nodes_per_level[level][idx].borrow().get_listening_port()
                    )
                    .as_str(),
                );
            }
        }
        debug!(
            "Network has been created with {} levels: {}",
            levels, debug_level_str
        );

        let mut wait_loop = true;
        while wait_loop {
            // 3. Select random node in last level and send a broadcast.
            let bcast_content = b"Hello from last level node".to_vec();
            {
                let src_idx = rng.gen_range(0, nodes_per_level[levels - 1].len());
                let src_node = &mut nodes_per_level[levels - 1][src_idx];
                let src_node_id = Some(src_node.borrow().id());

                debug!(
                    "Send message from {} in broadcast",
                    src_node.borrow().get_listening_port()
                );

                src_node
                    .borrow_mut()
                    .send_message(
                        src_node_id,
                        NetworkId::from(network_id),
                        None,
                        bcast_content.clone(),
                        true,
                    )
                    .map_err(|e| panic!(e))
                    .ok();
            }

            // 4. Wait broadcast in root.
            debug!("Waiting broadcast message");
            if let Ok(ref mut pac) = bcast_rx.recv_timeout(time::Duration::from_secs(2)) {
                debug!("Waiting broadcast message - finished");
                if let NetworkPacketType::BroadcastedMessage = pac.packet_type {
                    let msg = pac.message.read_all_into_view()?;
                    assert_eq!(msg.as_slice(), bcast_content.as_slice());
                    wait_loop = false;
                }
            }
        }

        Ok(())
    }

    /// It creates a linear network structure of 3 levels, using just one node
    /// per level. Network for this test is:
    ///     (Node 1) <--- (Node 2) <--- (Node 3).
    /// Test sends a broadcast message from `Node 3`, and it double-checks that
    /// `Node 1` will receive that message.
    #[test]
    pub fn e2e_005_001_no_relay_broadcast_to_sender_on_linear_network() -> Fallible<()> {
        utils::setup();
        no_relay_broadcast_to_sender_on_tree_network(3, 1, 2)
    }

    /// It creates a tree network structure of 3 levels, using between 2 and 3
    /// nodes per level.
    #[test]
    pub fn e2e_005_002_no_relay_broadcast_to_sender_on_tree_network() -> Fallible<()> {
        utils::setup();
        no_relay_broadcast_to_sender_on_tree_network(3, 2, 4)
    }

    /// It create a *complex* network structure of 5 levels, using between 4 and
    /// 9 nodes per level.
    #[test]
    pub fn e2e_005_003_no_relay_broadcast_to_sender_on_complex_tree_network() -> Fallible<()> {
        utils::setup();
        no_relay_broadcast_to_sender_on_tree_network(5, 4, 10)
    }

    #[test]
    pub fn e2e_008_drop_on_ban() -> Fallible<()> {
        utils::setup();

        let port = utils::next_port_offset(3);
        let networks = vec![100];

        let (mut node_1, msg_waiter_1) = utils::make_node_and_sync(port, networks.clone(), true)?;
        let (node_2, _msg_waiter_2) = utils::make_node_and_sync(port + 1, networks.clone(), true)?;
        utils::connect_and_wait_handshake(&mut node_1, &node_2, &msg_waiter_1)?;
        utils::consume_pending_messages(&msg_waiter_1);

        let to_ban = BannedNode::ById(node_2.id());

        node_1.ban_node(to_ban)?;
        let mut reply = node_1.get_peer_stats(&vec![])?;

        let t1 = time::Instant::now();
        while reply.len() == 1 {
            reply = node_1.get_peer_stats(&vec![])?;
            if time::Instant::now().duration_since(t1).as_secs() > 30 {
                bail!("timeout");
            }
        }

        Ok(())
    }

}
