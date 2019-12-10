#![allow(dead_code)]

use env_logger::{Builder, Env};
use failure::Fallible;
use log::LevelFilter;
use rand::{thread_rng, Rng};

use concordium_common::hybrid_buf::HybridBuf;

use p2p_client::{
    common::PeerType,
    connection::Connection,
    network::NetworkId,
    p2p::p2p_node::{send_broadcast_message, P2PNode},
    test_utils::{connect, generate_random_data, make_node_and_sync, next_available_port},
};

use std::{convert::TryFrom, sync::Arc, thread};

const KIB: usize = 1024;
const MIB: usize = 1024 * 1024;

const CNT: usize = 1_000_000;
const MIN_MSG_SIZE: usize = 12; // current minimum possible message size
const MIN_PKT_SIZE: usize = 2; // current minimum possible packet size
const MAX: usize = 6 * MIB;

fn main() -> Fallible<()> {
    let env = Env::default().filter_or("LOG_LEVEL", "warn");
    let mut log_builder = Builder::from_env(env);
    // disregard invalid packet type errors
    log_builder.filter_module("p2p_client::connection::message_handlers", LevelFilter::Off);
    // hide mnodule paths
    log_builder.format_module_path(false);
    // hide the timestamps
    log_builder.format_timestamp(None);
    log_builder.init();

    let node_1 = make_node_and_sync(next_available_port(), vec![100], PeerType::Node)?;
    let node_2 = make_node_and_sync(next_available_port(), vec![100], PeerType::Node)?;
    connect(&node_1, &node_2)?;

    let node_2_ref = Arc::clone(&node_2);
    thread::spawn(move || {
        for _ in 0..CNT {
            send_fuzzed_packet(&node_2_ref, MIN_PKT_SIZE, MAX)
        }
        node_2_ref.close_and_join().unwrap();
    });

    let node_1_ref = Arc::clone(&node_1);
    thread::spawn(move || {
        for _ in 0..CNT {
            send_fuzzed_packet(&node_1_ref, MIN_PKT_SIZE, MAX)
        }
        node_1_ref.close_and_join().unwrap();
    });

    node_1.join().unwrap();
    node_2.join().unwrap();

    println!("\n*** stress test complete ***\n");

    Ok(())
}

fn send_fuzzed_packet(source: &P2PNode, min: usize, max: usize) {
    send_broadcast_message(
        &source,
        source.self_peer.id,
        vec![],
        NetworkId::from(100),
        HybridBuf::try_from(generate_random_data(thread_rng().gen_range(min, max))).unwrap(),
    )
    .unwrap()
}

fn send_fuzzed_message(source: &P2PNode, min: usize, max: usize) {
    let filter = |_: &Connection| true;
    source
        .send_over_all_connections(
            generate_random_data(thread_rng().gen_range(min, max)),
            &filter,
        )
        .unwrap();
}
