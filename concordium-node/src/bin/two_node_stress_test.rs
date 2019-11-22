#![allow(dead_code)]

use env_logger::{Builder, Env};
use failure::Fallible;
use log::LevelFilter;
use rand::{thread_rng, Rng};

use concordium_common::hybrid_buf::HybridBuf;

use p2p_client::{
    common::PeerType,
    network::NetworkId,
    p2p::p2p_node::send_direct_message,
    test_utils::{connect, generate_random_data, make_node_and_sync, next_available_port},
};

use std::{convert::TryFrom, sync::Arc, thread};

const KIB: usize = 1024;
const MIB: usize = 1024 * 1024;

const CNT: usize = 100_000;
const MIN: usize = 2; // minimum packet size (packet type ID)
const MAX: usize = 8 * MIB;

fn main() -> Fallible<()> {
    let env = Env::default().filter_or("LOG_LEVEL", "trace");
    let mut log_builder = Builder::from_env(env);
    // disregard invalid packet type errors
    log_builder.filter_module("p2p_client::connection::message_handlers", LevelFilter::Off);
    log_builder.init();

    let node_1 = make_node_and_sync(next_available_port(), vec![100], PeerType::Node)?;
    let node_2 = make_node_and_sync(next_available_port(), vec![100], PeerType::Node)?;
    connect(&node_1, &node_2)?;

    let node_1_id = node_1.id();
    let node_2_ref = Arc::clone(&node_2);
    thread::spawn(move || {
        for _ in 0..CNT {
            let _ = send_direct_message(
                &node_2_ref.clone(),
                node_2_ref.self_peer.id,
                Some(node_1_id),
                NetworkId::from(100),
                HybridBuf::try_from(generate_random_data(thread_rng().gen_range(MIN, MAX)))
                    .unwrap(),
            );
        }
        node_2_ref.close_and_join().unwrap();
    });

    let node_2_id = node_2.id();
    let node_1_ref = Arc::clone(&node_1);
    thread::spawn(move || {
        for _ in 0..CNT {
            let _ = send_direct_message(
                &node_1_ref.clone(),
                node_1_ref.self_peer.id,
                Some(node_2_id),
                NetworkId::from(100),
                HybridBuf::try_from(generate_random_data(thread_rng().gen_range(MIN, MAX)))
                    .unwrap(),
            );
        }
        node_1_ref.close_and_join().unwrap();
    });

    node_1.join().unwrap();
    node_2.join().unwrap();

    println!("\n*** stress test complete ***\n");

    Ok(())
}
