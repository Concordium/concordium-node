#![allow(dead_code)]

use env_logger::{Builder, Env};
use failure::Fallible;
use log::LevelFilter;
use rand::{thread_rng, Rng};

use concordium_node::{
    common::PeerType,
    connection::Connection,
    network::NetworkId,
    p2p::{connectivity::send_broadcast_message, P2PNode},
    test_utils::{
        connect, dummy_regenesis_blocks, generate_random_data, make_node_and_sync,
        next_available_port, stop_node_delete_dirs,
    },
};

use std::{sync::Arc, thread, time::Duration};

const KIB: usize = 1024;
const MIB: usize = 1024 * 1024;

const CNT: usize = 1_000_000;
const MIN_MSG_SIZE: usize = 12; // current minimum possible message size
const MIN_PKT_SIZE: usize = 2; // current minimum possible packet size
const MAX: usize = 6 * MIB;

fn main() -> Fallible<()> {
    let env = Env::default().filter_or("LOG_LEVEL", "trace");
    let mut log_builder = Builder::from_env(env);
    // disregard invalid packet type errors
    log_builder.filter_module("concordium_node::connection::message_handlers", LevelFilter::Off);
    // hide module paths
    log_builder.format_module_path(false);
    // hide the timestamps
    log_builder.format_timestamp(None);
    log_builder.init();

    // create 2 nodes and connect them as peers
    let (node_1, dp_1) = make_node_and_sync(
        next_available_port(),
        vec![100],
        PeerType::Node,
        dummy_regenesis_blocks(),
    )?;
    let (node_2, dp_2) = make_node_and_sync(
        next_available_port(),
        vec![100],
        PeerType::Node,
        dummy_regenesis_blocks(),
    )?;
    connect(&node_1, &node_2);

    // send fuzzed packets from node 2
    let node_2_ref = Arc::clone(&node_2);
    thread::spawn(move || {
        for _ in 0..CNT {
            send_fuzzed_packet(&node_2_ref, MIN_PKT_SIZE, MAX)
        }
        node_2_ref.close_and_join().unwrap();
    });

    // send fuzzed packets from node 1
    let node_1_ref = Arc::clone(&node_1);
    thread::spawn(move || {
        for _ in 0..CNT {
            send_fuzzed_packet(&node_1_ref, MIN_PKT_SIZE, MAX)
        }
        node_1_ref.close_and_join().unwrap();
    });

    // wait until the handshakes are done
    thread::sleep(Duration::from_secs(5));

    // send a few invalid network messages from 5 faulty nodes
    let node_1_ref = Arc::clone(&node_1);
    let node_2_ref = Arc::clone(&node_2);
    thread::spawn(move || {
        let mut faulty_nodes = vec![];

        for i in 0..5 {
            let (faulty_node, fn_dp) = make_node_and_sync(
                next_available_port(),
                vec![100],
                PeerType::Node,
                dummy_regenesis_blocks(),
            )
            .unwrap();
            if i % 2 == 0 {
                connect(&node_1_ref, &faulty_node);
            } else {
                connect(&node_2_ref, &faulty_node);
            }
            faulty_nodes.push((faulty_node, fn_dp));
        }
        thread::sleep(Duration::from_secs(5));

        for (faulty_node, dp) in faulty_nodes {
            send_zeroes(&faulty_node);
            thread::sleep(Duration::from_secs(1));
            stop_node_delete_dirs(dp, faulty_node);
        }
    });

    // wait until all the messages are expected to be processed and cleanup.
    thread::sleep(Duration::from_secs(5));
    stop_node_delete_dirs(dp_1, node_1);
    stop_node_delete_dirs(dp_2, node_2);

    println!("\n*** stress test complete ***\n");

    Ok(())
}

/// Sends a broadcast with a `NetworkPacket` containing between `min` and `max`
/// random bytes as its payload.
fn send_fuzzed_packet(source: &P2PNode, min: usize, max: usize) {
    send_broadcast_message(
        &source,
        vec![],
        NetworkId::from(100),
        Arc::from(generate_random_data(thread_rng().gen_range(min, max))),
    );
}

/// Sends a broadcast with between `min` and `max` random raw bytes.
fn send_fuzzed_message(source: &P2PNode, min: usize, max: usize) {
    let filter = |_: &Connection| true;
    let msg = generate_random_data(thread_rng().gen_range(min, max));
    source.send_over_all_connections(&msg, &filter);
}

/// Sends a broadcast with an empty payload (which the low-level network layer
/// prepends with a zero as the buffer size).
fn send_zeroes(source: &P2PNode) {
    let filter = |_: &Connection| true;
    source.send_over_all_connections(&[], &filter);
}
