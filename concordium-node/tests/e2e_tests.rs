#[macro_use]
extern crate p2p_client;
#[macro_use]
extern crate log;

#[cfg(test)]
mod tests {
    use failure::{bail, Fallible};
    use p2p_client::{
        common::{
            functor::{FilterFunctor, Functorable},
            PeerType, UCursor,
        },
        configuration::Config,
        connection::MessageManager,
        network::{NetworkId, NetworkMessage, NetworkPacket, NetworkPacketType},
        p2p::{banned_nodes::BannedNode, p2p_node::P2PNode},
    };
    use rand::{distributions::Standard, thread_rng, Rng};
    use std::{
        cell::RefCell,
        sync::{
            atomic::{AtomicUsize, Ordering},
            Arc, RwLock,
        },
        time,
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
            common::{
                functor::{FilterFunctor, Functorable},
                PeerType, UCursor,
            },
            configuration::Config,
            connection::MessageManager,
            network::{NetworkMessage, NetworkPacketType, NetworkRequest, NetworkResponse},
            p2p::p2p_node::P2PNode,
            stats_export_service::{StatsExportService, StatsServiceMode},
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

            let export_service = Arc::new(RwLock::new(
                StatsExportService::new(StatsServiceMode::NodeMode).unwrap(),
            ));
            let mut node = P2PNode::new(
                None,
                &config,
                net_tx,
                None,
                PeerType::Node,
                Some(export_service),
                Arc::new(FilterFunctor::new("Broadcasting_checks")),
            );

            let mh = node.message_handler();
            safe_write!(mh)?.add_callback(make_atomic_callback!(move |m: &NetworkMessage| {
                // It is safe to ignore error.
                let _ = msg_wait_tx.send(m.clone());
                Ok(())
            }));

            let _ = node.spawn();
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
            source.connect(PeerType::Node, target.internal_addr, None)?;

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

        pub fn consume_pending_messages(waiter: &Receiver<NetworkMessage>) {
            let max_wait_time = time::Duration::from_millis(250);
            loop {
                if waiter.recv_timeout(max_wait_time).is_err() {
                    break;
                }
            }
        }

        /// Helper handler to log as `info` the secuence of packets received by
        /// node.
        ///
        /// # Example
        /// ```
        /// let (mut node, waiter) = make_node_and_sync(5555, vec![100], true).unwrap();
        /// let node_id_and_port = format!("{}(port={})", node.id(), 5555);
        ///
        /// node.message_handler()
        ///     .write()
        ///     .unwrap()
        ///     .add_callback(make_atomic_callback!(move |m: &NetworkMessage| {
        ///         let id = node_id_and_port.clone();
        ///         log_any_message_handler(id, m);
        ///         Ok(())
        ///     }));
        /// ```
        pub fn log_any_message_handler<T>(id: T, message: &NetworkMessage)
        where
            T: std::fmt::Display, {
            let msg_type: String = match message {
                NetworkMessage::NetworkRequest(ref request, ..) => match request {
                    NetworkRequest::Ping(ref peer, ..) => format!("Request::Ping({})", peer.id()),
                    NetworkRequest::FindNode(ref peer, ..) => {
                        format!("Request::FindNode({})", peer.id())
                    }
                    NetworkRequest::BanNode(ref peer, ..) => {
                        format!("Request::BanNode({})", peer.id())
                    }
                    NetworkRequest::Handshake(ref peer, ..) => {
                        format!("Request::Handshake({})", peer.id())
                    }
                    NetworkRequest::GetPeers(ref peer, ..) => {
                        format!("Request::GetPeers({})", peer.id())
                    }
                    NetworkRequest::UnbanNode(ref peer, ..) => {
                        format!("Request::UnbanNode({})", peer.id())
                    }
                    NetworkRequest::JoinNetwork(ref peer, ..) => {
                        format!("Request::JoinNetwork({})", peer.id())
                    }
                    NetworkRequest::LeaveNetwork(ref peer, ..) => {
                        format!("Request::LeaveNetwork({})", peer.id())
                    }
                    NetworkRequest::Retransmit(ref peer, ..) => {
                        format!("Request::Retransmit({})", peer.id())
                    }
                },
                NetworkMessage::NetworkResponse(ref response, ..) => match response {
                    NetworkResponse::Pong(..) => "Response::Pong".to_owned(),
                    NetworkResponse::FindNode(..) => "Response::FindNode".to_owned(),
                    NetworkResponse::PeerList(..) => "Response::PeerList".to_owned(),
                    NetworkResponse::Handshake(..) => "Response::Handshake".to_owned(),
                },
                NetworkMessage::NetworkPacket(ref packet, ..) => match packet.packet_type {
                    NetworkPacketType::BroadcastedMessage => {
                        format!("Packet::Broadcast(size={})", packet.message.len())
                    }
                    NetworkPacketType::DirectMessage(src_node_id, ..) => format!(
                        "Packet::Direct(from={},size={})",
                        src_node_id,
                        packet.message.len()
                    ),
                },
                NetworkMessage::UnknownMessage => "Unknown".to_owned(),
                NetworkMessage::InvalidMessage => "Invalid".to_owned(),
            };
            info!("Message at {}: {}", id, msg_type);
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

    #[ignore]
    #[test]
    pub fn e2e_002_small_mesh_net() -> Fallible<()> {
        const MESH_NODE_COUNT: usize = 15;
        utils::setup();

        let port_base: u16 = utils::next_port_offset(MESH_NODE_COUNT);
        let message_counter = Counter::new(0);
        let mut peers: Vec<(P2PNode, _)> = Vec::with_capacity(MESH_NODE_COUNT);

        // Create mesh net
        for node_idx in 0..MESH_NODE_COUNT {
            let instance_port = node_idx as u16 + port_base;
            let inner_counter = message_counter.clone();

            let (mut node, waiter) = utils::make_node_and_sync(instance_port, vec![100], false)?;

            safe_write!(node.message_handler())?.add_packet_callback(make_atomic_callback!(
                move |pac: &NetworkPacket| {
                    if let NetworkPacketType::BroadcastedMessage = pac.packet_type {
                        inner_counter.tick(1);
                        info!(
                            "BroadcastedMessage/{}/{} at {} with size {} received, ticks {}",
                            pac.network_id,
                            pac.message_id,
                            instance_port,
                            pac.message.len(),
                            inner_counter.get()
                        );
                    }
                    Ok(())
                }
            ));

            for (tgt_node, tgt_waiter) in &peers {
                utils::connect_and_wait_handshake(&mut node, tgt_node, &waiter)?;
                utils::consume_pending_messages(&waiter);
                utils::consume_pending_messages(&tgt_waiter);
            }

            peers.push((node, waiter));
        }

        // Send broadcast message from 0 node
        let msg = b"Hello other mother's brother";
        if let Some((ref mut node, _)) = peers.get_mut(0) {
            node.send_message(None, NetworkId::from(100), None, msg.to_vec(), true)?;
        }

        // Wait for broadcast message from 1..MESH_NODE_COUNT
        // and close and join to all nodes (included first node).
        for (node, waiter) in peers.iter_mut().nth(1) {
            let msg_recv = utils::wait_broadcast_message(&waiter)?.read_all_into_view()?;
            assert_eq!(msg_recv.as_slice(), msg);
            assert_eq!(true, node.close_and_join().is_ok());
        }
        if let Some((ref mut node, _)) = peers.get_mut(0) {
            assert_eq!(true, node.close_and_join().is_ok());
        }

        // Check counter.
        let local_message_counter = message_counter.get();
        debug!("Check message counter: {}", local_message_counter);
        assert_eq!(MESH_NODE_COUNT - 1, local_message_counter);
        Ok(())
    }

    fn islands_mesh_test(
        test_port_added: usize,
        island_size: usize,
        islands_count: usize,
    ) -> Fallible<()> {
        utils::setup();
        let message_counter = Counter::new(0);
        let message_count_estimated = (island_size - 1) * islands_count;

        let mut islands: Vec<Vec<(P2PNode, _)>> = Vec::with_capacity(islands_count);
        let networks = vec![100];

        // Create island of nodes. Each node (in each island) is connected to all
        // previous created nodes.
        for island in 0..islands_count {
            let mut peers_islands_and_ports: Vec<(P2PNode, _)> = Vec::with_capacity(island_size);
            let island_init_port = test_port_added + (island_size * island);

            for island_idx in 0..island_size {
                let inner_counter = message_counter.clone();
                let instance_port: u16 = (island_init_port + island_idx) as u16;

                let (mut node, waiter) =
                    utils::make_node_and_sync(instance_port, networks.clone(), false)?;
                let port = node.internal_addr.port();

                safe_write!(node.message_handler())?
                    .add_packet_callback(make_atomic_callback!(move |pac: &NetworkPacket| {
                        if let NetworkPacketType::BroadcastedMessage = pac.packet_type {
                            inner_counter.tick(1);
                            info!(
                                "BroadcastedMessage/{}/{} at {} with size {} received, ticks {}",
                                pac.network_id,
                                pac.message_id,
                                port,
                                pac.message.len(),
                                inner_counter.get()
                            );
                        }
                        Ok(())
                    }))
                    .add_callback(make_atomic_callback!(move |m: &NetworkMessage| {
                        utils::log_any_message_handler(port, m);
                        Ok(())
                    }));

                // Connect to previous nodes and clean any pending message in waiters
                for (tgt_node, tgt_waiter) in &peers_islands_and_ports {
                    utils::connect_and_wait_handshake(&mut node, tgt_node, &waiter)?;
                    utils::consume_pending_messages(&waiter);
                    utils::consume_pending_messages(&tgt_waiter);
                }
                peers_islands_and_ports.push((node, waiter));
            }
            islands.push(peers_islands_and_ports);
        }

        // Send broadcast message in each island.
        let msg = b"Hello other mother's brother";
        for island in &mut islands {
            if let Some((ref mut node_sender_ref, _)) = island.get_mut(0) {
                node_sender_ref.send_message(
                    None,
                    NetworkId::from(100),
                    None,
                    msg.to_vec(),
                    true,
                )?;
            };
        }

        // Wait reception of that broadcast message.
        for island in islands.iter_mut() {
            for (node, waiter) in island.iter_mut().nth(1) {
                let msg_recv = utils::wait_broadcast_message(&waiter)?.read_all_into_view()?;
                assert_eq!(msg_recv.as_slice(), msg);
                assert_eq!(true, node.close_and_join().is_ok());
            }
        }

        let local_message_counter: usize = message_counter.get();
        assert_eq!(message_count_estimated, local_message_counter);
        Ok(())
    }

    #[test]
    pub fn e2e_002_small_mesh_three_islands_net() -> Fallible<()> {
        islands_mesh_test(utils::next_port_offset(10) as usize, 3, 3)
    }

    #[test]
    pub fn e2e_003_big_mesh_three_islands_net() -> Fallible<()> {
        islands_mesh_test(utils::next_port_offset(20) as usize, 5, 3)
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
                move |pac: &NetworkPacket| {
                    // It is safe to ignore error.
                    let _ = tx_i.send(pac.clone());
                    Ok(())
                }
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
                    src_node.borrow().internal_addr.port()
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
                    // It is safe to ignore error.
                    let _ = bcast_tx.send(pac.clone());
                    Ok(())
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
                        nodes_per_level[level][idx].borrow().internal_addr.port()
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
                    src_node.borrow().internal_addr.port()
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
    pub fn e2e_006_01_close_and_join_on_not_spawned_node() -> Fallible<()> {
        utils::setup();
        let port = utils::next_port_offset(1);

        let (net_tx, _) = std::sync::mpsc::channel();
        let config = Config::new(Some("127.0.0.1".to_owned()), port, vec![100], 100);
        let mut node = P2PNode::new(
            None,
            &config,
            net_tx,
            None,
            PeerType::Node,
            None,
            Arc::new(FilterFunctor::new("Broadcasting_checks")),
        );

        assert_eq!(true, node.close_and_join().is_err());
        assert_eq!(true, node.close_and_join().is_err());
        assert_eq!(true, node.close_and_join().is_err());
        Ok(())
    }

    #[test]
    pub fn e2e_006_02_close_and_join_on_spawned_node() -> Fallible<()> {
        utils::setup();
        let port = utils::next_port_offset(2);

        let (mut node_1, waiter_1) = utils::make_node_and_sync(port, vec![100], true)?;
        let (node_2, waiter_2) = utils::make_node_and_sync(port + 1, vec![100], true)?;
        utils::connect_and_wait_handshake(&mut node_1, &node_2, &waiter_1)?;

        let msg = b"Hello";
        node_1.send_message(
            Some(node_2.id()),
            NetworkId::from(100),
            None,
            msg.to_vec(),
            false,
        )?;
        node_1.close_and_join()?;

        let node_2_msg = utils::wait_direct_message(&waiter_2)?.read_all_into_view()?;
        assert_eq!(node_2_msg.as_slice(), msg);
        Ok(())
    }

    #[test]
    pub fn e2e_006_03_close_from_inside_spawned_node() -> Fallible<()> {
        utils::setup();
        let port = utils::next_port_offset(2);

        let (mut node_1, waiter_1) = utils::make_node_and_sync(port, vec![100], true)?;
        let (node_2, waiter_2) = utils::make_node_and_sync(port + 1, vec![100], true)?;

        let node_2_cloned = RefCell::new(node_2.clone());
        safe_write!(node_2.message_handler())?.add_packet_callback(make_atomic_callback!(
            move |_pac: &NetworkPacket| {
                let join_status = node_2_cloned.borrow_mut().close_and_join();
                assert_eq!(join_status.is_err(), true);
                Ok(())
            }
        ));
        utils::connect_and_wait_handshake(&mut node_1, &node_2, &waiter_1)?;

        let msg = b"Hello";
        node_1.send_message(
            Some(node_2.id()),
            NetworkId::from(100),
            None,
            msg.to_vec(),
            false,
        )?;

        let node_2_msg = utils::wait_direct_message(&waiter_2)?.read_all_into_view()?;
        assert_eq!(node_2_msg.as_slice(), msg);
        Ok(())
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

        node_1.ban_node(to_ban);
        let mut reply = node_1.get_peer_stats(&vec![]);

        let t1 = time::Instant::now();
        while reply.len() == 1 {
            reply = node_1.get_peer_stats(&vec![]);
            if time::Instant::now().duration_since(t1).as_secs() > 30 {
                bail!("timeout");
            }
        }

        Ok(())
    }

}
