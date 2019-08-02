extern crate p2p_client;

#[cfg(test)]
mod tests {
    use concordium_common::{make_atomic_callback, safe_write, UCursor};
    use failure::{bail, Fallible};
    use p2p_client::{
        common::PeerType,
        connection::network_handler::message_processor::MessageManager,
        network::{NetworkId, NetworkMessage, NetworkPacket, NetworkPacketType},
        p2p::{banned_nodes::BannedNode, p2p_node::*},
        test_utils::{
            await_handshake, connect, consume_pending_messages, get_test_config,
            make_node_and_sync, make_nodes_from_port, max_recv_timeout, next_available_port,
            setup_logger, wait_broadcast_message, wait_direct_message, wait_direct_message_timeout,
        },
    };

    use log::{debug, info};
    use rand::{distributions::Standard, thread_rng, Rng};
    use std::{
        cell::RefCell,
        collections::hash_map::DefaultHasher,
        hash::Hasher,
        sync::{
            atomic::{AtomicUsize, Ordering},
            Arc, RwLock,
        },
        time,
    };

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
        setup_logger();

        let msg = b"Hello other brother!".to_vec();
        let networks = vec![100];

        let (mut node_1, msg_waiter_1) =
            make_node_and_sync(next_available_port(), networks.clone(), PeerType::Node)?;
        let (node_2, _msg_waiter_2) =
            make_node_and_sync(next_available_port(), networks, PeerType::Node)?;
        connect(&mut node_1, &node_2)?;
        await_handshake(&msg_waiter_1)?;
        consume_pending_messages(&msg_waiter_1);

        send_direct_message(
            &node_2,
            Some(node_1.id()),
            NetworkId::from(100),
            None,
            msg.clone(),
        )?;
        let mut msg_recv = wait_direct_message(&msg_waiter_1)?;
        assert_eq!(msg.as_slice(), msg_recv.read_all_into_view()?.as_slice());

        Ok(())
    }

    #[test]
    pub fn e2e_001_two_nodes_wrong_net() -> Fallible<()> {
        setup_logger();

        let networks_1 = vec![100];
        let networks_2 = vec![200];
        let msg = b"Hello other brother!".to_vec();

        let (mut node_1, msg_waiter_1) =
            make_node_and_sync(next_available_port(), networks_1, PeerType::Node)?;
        let (node_2, _) = make_node_and_sync(next_available_port(), networks_2, PeerType::Node)?;
        connect(&mut node_1, &node_2)?;
        await_handshake(&msg_waiter_1)?;
        consume_pending_messages(&msg_waiter_1);
        // Send msg
        send_direct_message(
            &node_2,
            Some(node_1.id()),
            NetworkId::from(100),
            None,
            msg.clone(),
        )?;
        let received_msg = wait_direct_message_timeout(&msg_waiter_1, max_recv_timeout());
        assert_eq!(received_msg, Some(UCursor::from(msg)));

        Ok(())
    }

    #[test]
    pub fn e2e_001_trust_broadcast() -> Fallible<()> {
        setup_logger();

        let msg = b"Hello other brother!".to_vec();
        let networks = vec![100];

        let (node_1, _msg_waiter_1) =
            make_node_and_sync(next_available_port(), networks.clone(), PeerType::Node)?;
        let (mut node_2, msg_waiter_2) =
            make_node_and_sync(next_available_port(), networks.clone(), PeerType::Node)?;
        let (mut node_3, msg_waiter_3) =
            make_node_and_sync(next_available_port(), networks, PeerType::Node)?;

        connect(&mut node_2, &node_1)?;
        await_handshake(&msg_waiter_2)?;

        connect(&mut node_3, &node_2)?;
        await_handshake(&msg_waiter_3)?;

        send_broadcast_message(&node_1, None, NetworkId::from(100), None, msg.clone())?;
        let msg_broadcast = wait_broadcast_message(&msg_waiter_3)?.read_all_into_view()?;
        assert_eq!(msg_broadcast.as_slice(), msg.as_slice());
        Ok(())
    }

    #[test]
    pub fn e2e_001_trust_broadcast_wrong_net() -> Fallible<()> {
        setup_logger();

        let msg = b"Hello other brother!".to_vec();
        let networks_1 = vec![100];
        let networks_2 = vec![200];

        let (node_1, _msg_waiter_1) =
            make_node_and_sync(next_available_port(), networks_1, PeerType::Node)?;
        let (mut node_2, msg_waiter_2) =
            make_node_and_sync(next_available_port(), networks_2.clone(), PeerType::Node)?;
        let (mut node_3, msg_waiter_3) =
            make_node_and_sync(next_available_port(), networks_2, PeerType::Node)?;

        connect(&mut node_2, &node_1)?;
        await_handshake(&msg_waiter_2)?;

        connect(&mut node_3, &node_2)?;
        await_handshake(&msg_waiter_3)?;

        consume_pending_messages(&msg_waiter_3);

        send_broadcast_message(&node_1, None, NetworkId::from(100), None, msg)?;
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
    pub fn e2e_002_small_mesh_net() -> Fallible<()> {
        const MESH_NODE_COUNT: usize = 15;
        setup_logger();
        let message_counter = Counter::new(0);
        let mut peers: Vec<(P2PNode, _)> = Vec::with_capacity(MESH_NODE_COUNT);

        let msg = b"Hello other mother's brother";
        // Create mesh net
        for _node_idx in 0..MESH_NODE_COUNT {
            let inner_counter = message_counter.clone();

            let (mut node, waiter) =
                make_node_and_sync(next_available_port(), vec![100], PeerType::Node)?;
            let port = node.internal_addr().port();
            node.message_processor()
                .add_notification(make_atomic_callback!(move |m: &NetworkMessage| {
                    if let NetworkMessage::NetworkPacket(pac, _, _) = m {
                        if let NetworkPacketType::BroadcastedMessage = pac.packet_type {
                            inner_counter.tick(1);
                            info!(
                                "BroadcastedMessage/{}/{:?} at {} with size {} received, ticks {}",
                                pac.network_id,
                                pac.message_id,
                                port,
                                pac.message.len(),
                                inner_counter.get()
                            );
                        }
                    }
                    Ok(())
                }));

            for (tgt_node, tgt_waiter) in &peers {
                connect(&mut node, &tgt_node)?;
                await_handshake(&waiter)?;
                consume_pending_messages(&waiter);
                consume_pending_messages(&tgt_waiter);
            }

            peers.push((node, waiter));
        }

        // Send broadcast message from 0 node
        if let Some((ref node, _)) = peers.get_mut(0) {
            send_broadcast_message(node, None, NetworkId::from(100), None, msg.to_vec())?;
        }

        // Wait for broadcast message from 1..MESH_NODE_COUNT
        // and close and join to all nodes (included first node).
        for (node, waiter) in peers.iter_mut().skip(1) {
            let msg_recv = wait_broadcast_message(&waiter)?.read_all_into_view()?;
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

    fn islands_mesh_test(island_size: usize, islands_count: usize) -> Fallible<()> {
        setup_logger();

        let message_counter = Counter::new(0);
        let message_count_estimated = (island_size - 1) * islands_count;

        let mut islands: Vec<Vec<(P2PNode, _)>> = Vec::with_capacity(islands_count);
        let networks = vec![100];

        let msg = b"Hello other mother's brother";
        // Create island of nodes. Each node (in each island) is connected to all
        // previous created nodes.
        for _island in 0..islands_count {
            let mut peers_islands_and_ports: Vec<(P2PNode, _)> = Vec::with_capacity(island_size);

            for _island_idx in 0..island_size {
                let inner_counter = message_counter.clone();

                let (mut node, waiter) =
                    make_node_and_sync(next_available_port(), networks.clone(), PeerType::Node)?;
                let port = node.internal_addr().port();

                node.message_processor()
                    .add_notification(make_atomic_callback!(move |m: &NetworkMessage| {
                        if let NetworkMessage::NetworkPacket(pac, _, _) = m {
                            if let NetworkPacketType::BroadcastedMessage = pac.packet_type {
                                inner_counter.tick(1);
                                info!(
                                    "BroadcastedMessage/{}/{:?} at {} with size {} received, \
                                     ticks {}",
                                    pac.network_id,
                                    pac.message_id,
                                    port,
                                    pac.message.len(),
                                    inner_counter.get()
                                );
                            }
                        }
                        Ok(())
                    }));

                // Connect to previous nodes and clean any pending message in waiters
                for (tgt_node, tgt_waiter) in &peers_islands_and_ports {
                    connect(&mut node, &tgt_node)?;
                    await_handshake(&waiter)?;
                    consume_pending_messages(&waiter);
                    consume_pending_messages(&tgt_waiter);
                }
                peers_islands_and_ports.push((node, waiter));
            }
            islands.push(peers_islands_and_ports);
        }

        // Send broadcast message in each island.

        for island in &mut islands {
            if let Some((ref node_sender_ref, _)) = island.get_mut(0) {
                send_broadcast_message(
                    node_sender_ref,
                    None,
                    NetworkId::from(100),
                    None,
                    msg.to_vec(),
                )?;
            };
        }

        // Wait reception of that broadcast message.
        for island in islands.iter_mut() {
            for (node, waiter) in island.iter_mut().skip(1) {
                let msg_recv = wait_broadcast_message(&waiter)?.read_all_into_view()?;
                assert_eq!(msg_recv.as_slice(), msg);
                assert_eq!(true, node.close_and_join().is_ok());
            }
        }

        let local_message_counter: usize = message_counter.get();
        assert_eq!(message_count_estimated, local_message_counter);
        Ok(())
    }

    #[test]
    pub fn e2e_002_small_mesh_three_islands_net() -> Fallible<()> { islands_mesh_test(3, 3) }

    #[test]
    pub fn e2e_003_big_mesh_three_islands_net() -> Fallible<()> { islands_mesh_test(5, 3) }

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
        setup_logger();
        let network_id = 100;

        // 1.1. Root node adds callback for receive last broadcast packet.
        let mut nodes = vec![];
        let mut waiters = vec![];
        let (bcast_tx, bcast_rx) = std::sync::mpsc::sync_channel(64);
        for i in 0..num_nodes {
            let tx_i = bcast_tx.clone();
            let port = next_available_port();

            let (node, conn_waiter) = make_node_and_sync(port, vec![network_id], PeerType::Node)?;

            let mh = node.message_processor();
            mh.add_packet_action(make_atomic_callback!(move |pac: &NetworkPacket| {
                // It is safe to ignore error.
                let _ = tx_i.send(pac.clone());
                Ok(())
            }));

            nodes.push(RefCell::new(node));
            waiters.push(conn_waiter);

            if i != 0 {
                let src_node = &nodes[i];
                let src_waiter = &waiters[i];
                let tgt_node = &nodes[0];
                connect(&mut *src_node.borrow_mut(), &*tgt_node.borrow())?;
                await_handshake(src_waiter)?;
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
                    src_node.borrow().internal_addr().port()
                );

                send_broadcast_message(
                    &src_node.borrow(),
                    src_node_id,
                    NetworkId::from(network_id),
                    None,
                    broadcast_msg.clone(),
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
    fn e2e_006_noise_ready_writeable() -> Fallible<()> {
        setup_logger();
        let msg = UCursor::from(b"Direct message between nodes".to_vec());
        let networks = vec![100];

        // 1. Create and connect nodes
        let (mut node_1, msg_waiter_1) =
            make_node_and_sync(next_available_port(), networks.clone(), PeerType::Node)?;
        let (node_2, msg_waiter_2) =
            make_node_and_sync(next_available_port(), networks, PeerType::Node)?;
        connect(&mut node_1, &node_2)?;
        await_handshake(&msg_waiter_1)?;

        // 2. Send message from n1 to n2.
        send_message_from_cursor(
            &node_1,
            Some(node_2.id()),
            NetworkId::from(100),
            None,
            msg.clone(),
            false,
        )?;
        let msg_1 = wait_direct_message(&msg_waiter_2)?;
        assert_eq!(msg_1, msg);

        send_message_from_cursor(
            &node_2,
            Some(node_1.id()),
            NetworkId::from(100),
            None,
            msg.clone(),
            false,
        )?;
        let msg_2 = wait_direct_message(&msg_waiter_1)?;
        assert_eq!(msg_2, msg);

        send_message_from_cursor(
            &node_1,
            Some(node_2.id()),
            NetworkId::from(102),
            None,
            msg.clone(),
            false,
        )?;
        let msg_3 = wait_direct_message(&msg_waiter_2)?;
        assert_eq!(msg_3, msg);

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
        setup_logger();

        let network_id = 100;
        let mut rng = rand::thread_rng();

        // 1. Create network: all nodes, per level.
        // At first level, only one node is generated.
        let mut nodes_per_level = Vec::with_capacity(levels);
        let mut conn_waiters_per_level = Vec::with_capacity(levels);

        // 1.1. Root node adds callback for receive last broadcast packet.
        let (node, conn_waiter) =
            make_node_and_sync(next_available_port(), vec![network_id], PeerType::Node)?;
        let (bcast_tx, bcast_rx) = std::sync::mpsc::sync_channel(64);
        {
            node.message_processor()
                .add_packet_action(make_atomic_callback!(move |pac: &NetworkPacket| {
                    debug!("Root node is forwarding Packet to channel");
                    // It is safe to ignore error.
                    bcast_tx.send(pac.clone()).map_err(failure::Error::from)
                }));
        }

        nodes_per_level.push(vec![RefCell::new(node)]);
        conn_waiters_per_level.push(vec![conn_waiter]);

        // 1.2. Create each level
        for level in 1..levels {
            // 1.2.1. Make nodes
            let count_nodes: usize = rng.gen_range(min_node_per_level, max_node_per_level);
            debug!("Creating level {} with {} nodes", level, count_nodes);
            let nodes_and_conn_waiters = make_nodes_from_port(count_nodes, vec![network_id])?;

            // Create nodes and add logger for each message
            let (nodes, waiters): (Vec<RefCell<P2PNode>>, _) =
                nodes_and_conn_waiters.into_iter().unzip();

            nodes_per_level.push(nodes);
            conn_waiters_per_level.push(waiters);

            // 1.2.2 Connect one to previous level.
            let root_previous_level_idx: usize = rng.gen_range(0, nodes_per_level[level - 1].len());
            let root_curr_level_idx: usize = rng.gen_range(0, count_nodes);

            let target_node = &nodes_per_level[level - 1][root_previous_level_idx];
            {
                let src_node = &nodes_per_level[level][root_curr_level_idx];
                let src_waiter = &conn_waiters_per_level[level][root_curr_level_idx];
                connect(&mut *src_node.borrow_mut(), &*target_node.borrow())?;
                await_handshake(src_waiter)?;
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
                    connect(&mut *src_node.borrow_mut(), &*tgt_node.borrow())?;
                    await_handshake(src_waiter)?;
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
                        nodes_per_level[level][idx].borrow().internal_addr().port()
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
                    src_node.borrow().internal_addr().port()
                );

                send_broadcast_message(
                    &src_node.borrow(),
                    src_node_id,
                    NetworkId::from(network_id),
                    None,
                    bcast_content.clone(),
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
        no_relay_broadcast_to_sender_on_tree_network(3, 1, 2)
    }

    /// It creates a tree network structure of 3 levels, using between 2 and 3
    /// nodes per level.
    #[test]
    pub fn e2e_005_002_no_relay_broadcast_to_sender_on_tree_network() -> Fallible<()> {
        no_relay_broadcast_to_sender_on_tree_network(3, 2, 4)
    }

    /// It create a *complex* network structure of 5 levels, using between 4 and
    /// 9 nodes per level.
    #[test]
    pub fn e2e_005_003_no_relay_broadcast_to_sender_on_complex_tree_network() -> Fallible<()> {
        no_relay_broadcast_to_sender_on_tree_network(5, 4, 10)
    }

    #[test]
    pub fn e2e_006_01_close_and_join_on_not_spawned_node() -> Fallible<()> {
        setup_logger();

        let (net_tx, _) = std::sync::mpsc::sync_channel(64);
        let (rpc_tx, _) = std::sync::mpsc::sync_channel(64);
        let mut node = P2PNode::new(
            None,
            &get_test_config(next_available_port(), vec![100]),
            net_tx,
            None,
            PeerType::Node,
            None,
            rpc_tx,
        );

        assert_eq!(true, node.close_and_join().is_err());
        assert_eq!(true, node.close_and_join().is_err());
        assert_eq!(true, node.close_and_join().is_err());
        Ok(())
    }

    #[test]
    pub fn e2e_006_02_close_and_join_on_spawned_node() -> Fallible<()> {
        setup_logger();

        let (mut node_1, waiter_1) =
            make_node_and_sync(next_available_port(), vec![100], PeerType::Node)?;
        let (node_2, waiter_2) =
            make_node_and_sync(next_available_port(), vec![100], PeerType::Node)?;
        connect(&mut node_1, &node_2)?;
        await_handshake(&waiter_1)?;

        let msg = b"Hello";
        send_direct_message(
            &node_1,
            Some(node_2.id()),
            NetworkId::from(100),
            None,
            msg.to_vec(),
        )?;
        node_1.close_and_join()?;

        let node_2_msg = wait_direct_message(&waiter_2)?.read_all_into_view()?;
        assert_eq!(node_2_msg.as_slice(), msg);
        Ok(())
    }

    #[test]
    pub fn e2e_006_03_close_from_inside_spawned_node() -> Fallible<()> {
        setup_logger();

        let (mut node_1, waiter_1) =
            make_node_and_sync(next_available_port(), vec![100], PeerType::Node)?;
        let (node_2, waiter_2) =
            make_node_and_sync(next_available_port(), vec![100], PeerType::Node)?;

        let node_2_cloned = RwLock::new(node_2.clone());
        node_2
            .message_processor()
            .add_packet_action(make_atomic_callback!(move |_pac: &NetworkPacket| {
                let join_status = safe_write!(node_2_cloned)?.close_and_join();
                assert_eq!(join_status.is_err(), true);
                Ok(())
            }));
        connect(&mut node_1, &node_2)?;
        await_handshake(&waiter_1)?;

        let msg = b"Hello";
        send_direct_message(
            &node_1,
            Some(node_2.id()),
            NetworkId::from(100),
            None,
            msg.to_vec(),
        )?;

        let node_2_msg = wait_direct_message(&waiter_2)?.read_all_into_view()?;
        assert_eq!(node_2_msg.as_slice(), msg);
        Ok(())
    }

    #[test]
    pub fn e2e_008_drop_on_ban() -> Fallible<()> {
        setup_logger();

        let networks = vec![100];

        let (mut node_1, msg_waiter_1) =
            make_node_and_sync(next_available_port(), networks.clone(), PeerType::Node)?;
        let (node_2, _msg_waiter_2) =
            make_node_and_sync(next_available_port(), networks.clone(), PeerType::Node)?;
        connect(&mut node_1, &node_2)?;
        await_handshake(&msg_waiter_1)?;
        consume_pending_messages(&msg_waiter_1);

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

    #[test]
    pub fn e2e_009_network_direct_128k() { p2p_net(128 * 1024); }

    #[test]
    pub fn e2e_009_network_direct_8m() { p2p_net(8 * 1024 * 1024); }

    #[test]
    pub fn e2e_009_network_direct_32m() { p2p_net(32 * 1024 * 1024); }

    #[test]
    pub fn e2e_009_network_direct_128m() { p2p_net(128 * 1024 * 1024); }

    fn p2p_net(size: usize) {
        setup_logger();

        // Create nodes and connect them.
        let (mut node_1, msg_waiter_1) =
            make_node_and_sync(next_available_port(), vec![100], PeerType::Node).unwrap();
        let (node_2, msg_waiter_2) =
            make_node_and_sync(next_available_port(), vec![100], PeerType::Node).unwrap();
        connect(&mut node_1, &node_2).unwrap();
        await_handshake(&msg_waiter_1).unwrap();

        // let mut msg = make_direct_message_into_disk().unwrap();
        let msg = thread_rng()
            .sample_iter(&Standard)
            .take(size)
            .collect::<Vec<u8>>();
        let mut uc = UCursor::from(msg);
        let net_id = NetworkId::from(100);

        // Send.
        send_message_from_cursor(&node_1, Some(node_2.id()), net_id, None, uc.clone(), false)
            .unwrap();
        let mut msg_recv = wait_direct_message(&msg_waiter_2).unwrap();
        assert_eq!(uc.len(), msg_recv.len());

        // Get content hash.
        let content_hash_list = [
            uc.read_all_into_view().unwrap(),
            msg_recv.read_all_into_view().unwrap(),
        ]
        .into_iter()
        .map(|view| {
            let mut hasher = DefaultHasher::new();
            hasher.write(view.as_slice());
            hasher.finish()
        })
        .collect::<Vec<u64>>();

        assert_eq!(content_hash_list[0], content_hash_list[1]);
    }
}
