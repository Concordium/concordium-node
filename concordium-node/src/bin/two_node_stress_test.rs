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

use std::convert::TryFrom;

fn main() -> Fallible<()> {
    let env = Env::default().filter_or("LOG_LEVEL", "trace");
    let mut log_builder = Builder::from_env(env);
    // disregard invalid packet type errors
    log_builder.filter_module("p2p_client::connection::message_handlers", LevelFilter::Off);
    log_builder.init();

    let mut node_1 = make_node_and_sync(next_available_port(), vec![100], PeerType::Node)?;
    let node_2 = make_node_and_sync(next_available_port(), vec![100], PeerType::Node)?;
    connect(&mut node_1, &node_2)?;

    for _ in 0..1000 {
        // let msg_size: usize = thread_rng().gen_range(4 * 1024 * 1024, 8 * 1024 *
        // 1024);
        let msg_size = 11 * 1024 * 1024 - 1;
        let msg = generate_random_data(msg_size);

        send_direct_message(
            &node_2,
            node_2.self_peer.id,
            Some(node_1.id()),
            NetworkId::from(100),
            HybridBuf::try_from(msg)?,
        )?;
    }

    Ok(())
}
