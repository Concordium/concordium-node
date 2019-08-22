extern crate p2p_client;

#[cfg(test)]
mod tests {
    use concordium_common::{make_atomic_callback, safe_write};
    use failure::{bail, Fallible};
    use p2p_client::{
        common::PeerType,
        connection::network_handler::message_processor::MessageManager,
        network::{NetworkId, NetworkMessage, NetworkPacket, NetworkPacketType},
        p2p::{banned_nodes::BannedNode, p2p_node::*},
        test_utils::{
            await_broadcast_message, await_direct_message, await_direct_message_with_timeout,
            await_handshake, await_handshake_with_timeout, await_peerlist_with_timeout,
            await_ping_with_timeout, connect, consume_pending_messages, get_test_config,
            make_node_and_sync, max_recv_timeout, next_available_port, setup_logger,
        },
    };

    use log::{debug, info};
    use rand::{distributions::Standard, thread_rng, Rng};
    use std::{
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

        let msg = b"Hello other brother!";
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
            msg.to_vec(),
        )?;
        let mut msg_recv = await_direct_message(&msg_waiter_1)?;
        assert_eq!(&msg[..], &msg_recv.remaining_bytes()?[..]);

        Ok(())
    }

    #[test]
    pub fn e2e_001_two_nodes_wrong_net() -> Fallible<()> {
        setup_logger();

        let networks_1 = vec![100];
        let networks_2 = vec![200];
        let msg = b"Hello other brother!";

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
            msg.to_vec(),
        )?;
        let received_msg = await_direct_message_with_timeout(&msg_waiter_1, max_recv_timeout());
        assert_eq!(
            received_msg.map(|mut hb| hb.remaining_bytes().unwrap()),
            Some(msg.to_vec())
        );

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
                        if let NetworkPacketType::BroadcastedMessage(..) = pac.packet_type {
                            inner_counter.tick(1);
                            info!(
                                "BroadcastedMessage/{}/{:?} at {} with size {} received, ticks {}",
                                pac.network_id,
                                pac.message_id,
                                port,
                                pac.message.len()?,
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
            send_broadcast_message(node, vec![], NetworkId::from(100), None, msg.to_vec())?;
        }

        // Wait for broadcast message from 1..MESH_NODE_COUNT
        // and close and join to all nodes (included first node).
        for (node, waiter) in peers.iter_mut().skip(1) {
            let mut msg_recv = await_broadcast_message(&waiter)?;
            assert_eq!(&msg_recv.remaining_bytes()?[..], msg);
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
        // Create an island of nodes. Each node (in each island) is connected to all
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
                            if let NetworkPacketType::BroadcastedMessage(..) = pac.packet_type {
                                inner_counter.tick(1);
                                info!(
                                    "BroadcastedMessage/{}/{:?} at {} with size {} received, \
                                     ticks {}",
                                    pac.network_id,
                                    pac.message_id,
                                    port,
                                    pac.message.len()?,
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
                    vec![],
                    NetworkId::from(100),
                    None,
                    msg.to_vec(),
                )?;
            };
        }

        // Wait reception of that broadcast message.
        for island in islands.iter_mut() {
            for (node, waiter) in island.iter_mut().skip(1) {
                let mut msg_recv = await_broadcast_message(&waiter)?;
                assert_eq!(&msg_recv.remaining_bytes()?[..], msg);
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
    fn e2e_004_noise_ready_writeable() -> Fallible<()> {
        setup_logger();
        let msg = b"Direct message between nodes";
        let networks = vec![100];

        // 1. Create and connect nodes
        let (mut node_1, msg_waiter_1) =
            make_node_and_sync(next_available_port(), networks.clone(), PeerType::Node)?;
        let (node_2, msg_waiter_2) =
            make_node_and_sync(next_available_port(), networks, PeerType::Node)?;
        connect(&mut node_1, &node_2)?;
        await_handshake(&msg_waiter_1)?;

        // 2. Send message from n1 to n2.
        send_direct_message(
            &node_1,
            Some(node_2.id()),
            NetworkId::from(100),
            None,
            msg.to_vec(),
        )?;
        let mut msg_1 = await_direct_message(&msg_waiter_2)?;
        assert_eq!(&msg_1.remaining_bytes()?[..], msg);

        send_direct_message(
            &node_2,
            Some(node_1.id()),
            NetworkId::from(100),
            None,
            msg.to_vec(),
        )?;
        let mut msg_2 = await_direct_message(&msg_waiter_1)?;
        assert_eq!(&msg_2.remaining_bytes()?[..], msg);

        send_direct_message(
            &node_1,
            Some(node_2.id()),
            NetworkId::from(102),
            None,
            msg.to_vec(),
        )?;
        let mut msg_3 = await_direct_message(&msg_waiter_2)?;
        assert_eq!(&msg_3.remaining_bytes()?[..], msg);

        Ok(())
    }

    #[test]
    pub fn e2e_004_01_close_and_join_on_not_spawned_node() -> Fallible<()> {
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
    pub fn e2e_004_02_close_and_join_on_spawned_node() -> Fallible<()> {
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

        let mut node_2_msg = await_direct_message(&waiter_2)?;
        assert_eq!(&node_2_msg.remaining_bytes()?[..], msg);
        Ok(())
    }

    #[test]
    pub fn e2e_004_03_close_from_inside_spawned_node() -> Fallible<()> {
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

        let mut node_2_msg = await_direct_message(&waiter_2)?;
        assert_eq!(&node_2_msg.remaining_bytes()?[..], msg);
        Ok(())
    }

    #[test]
    pub fn e2e_005_drop_on_ban() -> Fallible<()> {
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
        let mut reply = node_1.get_peer_stats(&[]);

        let t1 = time::Instant::now();
        while reply.len() == 1 {
            if time::Instant::now().duration_since(t1).as_secs()
                > 2 * node_1.config.housekeeping_interval
            {
                bail!("timeout");
            }
            std::thread::sleep(std::time::Duration::from_millis(50));
            reply = node_1.get_peer_stats(&[]);
        }

        Ok(())
    }

    #[test]
    pub fn e2e_005_network_direct_128k() { p2p_net(128 * 1024); }

    #[test]
    pub fn e2e_005_network_direct_8m() { p2p_net(8 * 1024 * 1024); }

    #[test]
    pub fn e2e_005_network_direct_32m() { p2p_net(32 * 1024 * 1024); }

    fn p2p_net(size: usize) {
        setup_logger();

        // Create nodes and connect them.
        let (mut node_1, msg_waiter_1) =
            make_node_and_sync(next_available_port(), vec![100], PeerType::Node).unwrap();
        let (node_2, msg_waiter_2) =
            make_node_and_sync(next_available_port(), vec![100], PeerType::Node).unwrap();
        connect(&mut node_1, &node_2).unwrap();
        await_handshake(&msg_waiter_1).unwrap();

        let msg = thread_rng()
            .sample_iter(&Standard)
            .take(size)
            .collect::<Vec<u8>>();
        let net_id = NetworkId::from(100);

        // Send.
        send_direct_message(&node_1, Some(node_2.id()), net_id, None, msg.clone()).unwrap();
        let mut msg_recv = await_direct_message(&msg_waiter_2).unwrap();
        assert_eq!(msg.len() as u64, msg_recv.remaining_len().unwrap());

        // Get content hash.
        let content_hash_list = [msg, msg_recv.remaining_bytes().unwrap()]
            .into_iter()
            .map(|view| {
                let mut hasher = DefaultHasher::new();
                hasher.write(view.as_slice());
                hasher.finish()
            })
            .collect::<Vec<u64>>();

        assert_eq!(content_hash_list[0], content_hash_list[1]);
    }

    #[test]
    #[ignore]
    pub fn e2e_006_bootstrapper_load_test() -> Fallible<()> {
        use std::{net::SocketAddr, thread, time::Duration};

        const BOOTSTRAPPER_CONN_COUNT: usize = 400;
        const ARTIFICIAL_DELAY_BETWEEN_PEERS: u64 = 10;
        const WAIT_TIME: std::time::Duration = Duration::from_millis(10000);
        setup_logger();
        let bootstrapped_counter = Arc::new(AtomicUsize::new(0));

        let (mut bootstrapper_node, _) =
            make_node_and_sync(next_available_port(), vec![100], PeerType::Bootstrapper)?;

        let node_runner = |node_idx: usize,
                           bootstrapper: SocketAddr,
                           counter: Arc<AtomicUsize>|
         -> Fallible<()> {
            debug!("Attempting node #{}", node_idx);
            let (mut node, waiter) =
                make_node_and_sync(next_available_port(), vec![100], PeerType::Node)?;
            node.connect(PeerType::Bootstrapper, bootstrapper, None)?;
            await_handshake_with_timeout(&waiter, WAIT_TIME)?;
            await_ping_with_timeout(&waiter, WAIT_TIME)?;
            await_peerlist_with_timeout(&waiter, WAIT_TIME)?;
            let _ = node.close_and_join()?;
            let _ = counter.fetch_add(1, Ordering::SeqCst);
            Ok(())
        };

        // Create nodes
        let mut threads = vec![];
        for node_idx in 0..BOOTSTRAPPER_CONN_COUNT {
            let bootstrapper_addr = bootstrapper_node.internal_addr();
            let counter_clone = Arc::clone(&bootstrapped_counter);
            threads.push(thread::spawn(move || {
                node_runner(node_idx, bootstrapper_addr, counter_clone)
            }));
            thread::sleep(Duration::from_millis(ARTIFICIAL_DELAY_BETWEEN_PEERS));
        }

        for thread in threads {
            if thread.join().is_err() {
                bail!("Thread failed!");
            }
        }

        // Check counter.
        let bootstrapped_counter_count = bootstrapped_counter.load(Ordering::SeqCst);
        assert_eq!(BOOTSTRAPPER_CONN_COUNT, bootstrapped_counter_count);
        bootstrapper_node.close_and_join()?;
        Ok(())
    }
}
