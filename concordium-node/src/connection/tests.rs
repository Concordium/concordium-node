use itertools::Itertools;

use crate::{
    common::PeerType,
    network::NetworkId,
    p2p::connectivity::send_broadcast_message,
    test_utils::{connect, make_node_and_sync, next_available_port},
};
use concordium_common::PacketType;

use std::{sync::Arc, thread, time::Duration};

const NID: u16 = 100;
const NODE_COUNT: usize = 8;

#[test]
fn basic_connectivity() {
    // start up test nodes
    let mut nodes = Vec::with_capacity(NODE_COUNT);
    for _ in 0..NODE_COUNT {
        nodes.push(make_node_and_sync(next_available_port(), vec![NID], PeerType::Node).unwrap());
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
        connect(&nodes[pair[0]], &nodes[pair[1]]).unwrap();
    }

    // test the handshake (both low- and high-level)
    let mut done;
    loop {
        done = true;
        'outer: for node in &nodes {
            let conns = read_or_die!(node.connections());
            if conns.len() != NODE_COUNT - 1 {
                done = false;
                break;
            }
            for conn in conns.values() {
                if !conn.is_post_handshake() {
                    done = false;
                    break 'outer;
                }
            }
        }
        if done {
            break;
        }
        thread::sleep(Duration::from_millis(100));
    }

    // send a test broadcast from each node
    for node in &nodes {
        send_broadcast_message(
            node,
            vec![],
            NetworkId::from(NID),
            Arc::from(&[PacketType::Block as u8][..]), // an empty Block packet
        )
        .unwrap()
    }
}
