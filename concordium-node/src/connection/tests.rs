use itertools::Itertools;

use crate::{
    common::PeerType,
    consensus_ffi::helpers::PacketType,
    network::NetworkId,
    p2p::connectivity::send_broadcast_message,
    test_utils::{
        await_handshakes, connect, dummy_regenesis_blocks, make_node_and_sync, next_available_port,
    },
};

use std::sync::Arc;

const NID: u16 = 100;
const NODE_COUNT: usize = 10;

#[test]
fn basic_connectivity() {
    // start up test nodes
    let mut nodes = Vec::with_capacity(NODE_COUNT);
    for _ in 0..NODE_COUNT {
        nodes.push(
            make_node_and_sync(
                next_available_port(),
                vec![NID],
                PeerType::Node,
                dummy_regenesis_blocks(),
            )
            .unwrap(),
        );
    }

    // obtain a list of possible connections
    let mut possible_connections = (0..NODE_COUNT).permutations(2).collect::<Vec<_>>();
    for pair in &mut possible_connections {
        pair.sort();
    }
    possible_connections.sort();
    possible_connections.dedup();
    assert_eq!(possible_connections.len(), NODE_COUNT * (NODE_COUNT - 1) / 2);

    // connect the nodes in a mesh
    for pair in &possible_connections {
        connect(&nodes[pair[0]], &nodes[pair[1]]);
    }

    // test the handshake (both low- and high-level)
    for node in &nodes {
        await_handshakes(node);
    }

    // send a test broadcast from each node
    for node in &nodes {
        send_broadcast_message(
            node,
            vec![],
            NetworkId::from(NID),
            Arc::from(&[PacketType::Block as u8][..]), // an empty Block packet
        );
    }
}
