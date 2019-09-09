extern crate p2p_client;

#[cfg(test)]
mod tests {
    use concordium_common::hybrid_buf::HybridBuf;
    use failure::{bail, Fallible};
    use p2p_client::{
        common::PeerType,
        network::NetworkId,
        p2p::{banned_nodes::BannedNode, p2p_node::*},
        test_utils::{
            await_direct_message, await_direct_message_with_timeout, await_handshake, connect,
            consume_pending_messages, get_test_config, make_node_and_sync, max_recv_timeout,
            next_available_port, setup_logger,
        },
    };

    use rand::{distributions::Standard, thread_rng, Rng};
    use std::{collections::hash_map::DefaultHasher, convert::TryFrom, hash::Hasher, time};

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
        await_handshake(&node_1)?;
        // consume_pending_messages(&msg_waiter_1);

        send_direct_message(
            &node_2,
            Some(node_1.id()),
            NetworkId::from(100),
            None,
            HybridBuf::try_from(&msg[..])?,
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
        await_handshake(&node_1)?;
        consume_pending_messages(&msg_waiter_1);
        // Send msg
        send_direct_message(
            &node_2,
            Some(node_1.id()),
            NetworkId::from(100),
            None,
            HybridBuf::try_from(&msg[..])?,
        )?;
        let received_msg = await_direct_message_with_timeout(&msg_waiter_1, max_recv_timeout());
        assert_eq!(
            received_msg.map(|mut hb| hb.remaining_bytes().unwrap().into_owned()),
            Some(msg.to_vec())
        );

        Ok(())
    }

    #[test]
    pub fn e2e_004_01_close_and_join_on_not_spawned_node() -> Fallible<()> {
        setup_logger();

        let (net_tx, _) = std::sync::mpsc::sync_channel(64);
        let (rpc_tx, _) = std::sync::mpsc::sync_channel(64);
        let (mut node, _receivers) = P2PNode::new(
            None,
            &get_test_config(next_available_port(), vec![100]),
            net_tx,
            None,
            PeerType::Node,
            None,
            rpc_tx,
            None,
        );

        assert_eq!(true, node.close_and_join().is_err());
        assert_eq!(true, node.close_and_join().is_err());
        assert_eq!(true, node.close_and_join().is_err());
        Ok(())
    }

    #[test]
    pub fn e2e_004_02_close_and_join_on_spawned_node() -> Fallible<()> {
        setup_logger();

        let (mut node_1, _) = make_node_and_sync(next_available_port(), vec![100], PeerType::Node)?;
        let (node_2, waiter_2) =
            make_node_and_sync(next_available_port(), vec![100], PeerType::Node)?;
        connect(&mut node_1, &node_2)?;
        await_handshake(&node_1)?;

        let msg = b"Hello";
        send_direct_message(
            &node_1,
            Some(node_2.id()),
            NetworkId::from(100),
            None,
            HybridBuf::try_from(&msg[..])?,
        )?;
        node_1.close_and_join()?;

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
        await_handshake(&node_1)?;
        consume_pending_messages(&msg_waiter_1);

        let to_ban = BannedNode::ById(node_2.id());

        node_1.ban_node(to_ban);
        let mut reply = node_1.get_peer_stats();

        let t1 = time::Instant::now();
        while reply.len() == 1 {
            if time::Instant::now().duration_since(t1).as_secs()
                > 2 * node_1.config.housekeeping_interval
            {
                bail!("timeout");
            }
            std::thread::sleep(std::time::Duration::from_millis(50));
            reply = node_1.get_peer_stats();
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
        let (mut node_1, _) =
            make_node_and_sync(next_available_port(), vec![100], PeerType::Node).unwrap();
        let (node_2, msg_waiter_2) =
            make_node_and_sync(next_available_port(), vec![100], PeerType::Node).unwrap();
        connect(&mut node_1, &node_2).unwrap();
        await_handshake(&node_1).unwrap();

        let msg = thread_rng()
            .sample_iter(&Standard)
            .take(size)
            .collect::<Vec<u8>>();
        let net_id = NetworkId::from(100);

        // Send.
        send_direct_message(
            &node_1,
            Some(node_2.id()),
            net_id,
            None,
            HybridBuf::try_from(msg.clone()).unwrap(),
        )
        .unwrap();
        let mut msg_recv = await_direct_message(&msg_waiter_2).unwrap();
        assert_eq!(msg.len() as u64, msg_recv.remaining_len().unwrap());

        // Get content hash.
        let content_hash_list = [msg, msg_recv.remaining_bytes().unwrap().into_owned()]
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
