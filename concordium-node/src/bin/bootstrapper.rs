#![recursion_limit = "1024"]

// Force the system allocator on every platform
use std::{alloc::System, sync::Arc};
#[global_allocator]
static A: System = System;

use anyhow::{ensure, Context};
use concordium_node::{
    common::PeerType,
    consensus_ffi::{blockchain_types::BlockHash, consensus::Regenesis},
    p2p::{maintenance::spawn, *},
    stats_export_service::instantiate_stats_export_engine,
    utils::get_config_and_logging_setup,
};

#[cfg(feature = "instrumentation")]
use concordium_node::stats_export_service::start_push_gateway;

fn main() -> anyhow::Result<()> {
    let (mut conf, app_prefs) = get_config_and_logging_setup()?;
    conf.connection.max_allowed_nodes = Some(conf.bootstrapper.max_nodes);
    let data_dir_path = app_prefs.get_data_dir();

    let stats_export_service = instantiate_stats_export_engine(&conf)?;

    let fname = conf
        .bootstrapper
        .regenesis_block_hashes
        .clone()
        .unwrap_or_else(|| data_dir_path.join(std::path::Path::new("genesis_hash")));
    let regenesis_hashes_bytes = std::fs::read(&fname)
        .context(format!("Could not open file {} with genesis hashes.", fname.to_string_lossy()))?;
    let regenesis_blocks: Vec<BlockHash> = serde_json::from_slice(&regenesis_hashes_bytes)
        .context("Could not parse genesis hashes.")?;
    let regenesis_arc: Arc<Regenesis> = Arc::new(Regenesis::from_blocks(regenesis_blocks));

    ensure!(
        regenesis_arc.blocks.read().unwrap().len() > 0,
        "Bootstrapper can't run without specifying genesis hashes."
    );

    // todo: decide how to manage the private key
    let kp = ed25519_dalek::Keypair::generate(&mut rand::rngs::OsRng);
    let (node, server, poll) =
        P2PNode::new(kp, &conf, PeerType::Bootstrapper, stats_export_service, regenesis_arc)
            .context("Failed to create the network node.")?;

    #[cfg(feature = "instrumentation")]
    start_push_gateway(&conf.prometheus, &node.stats, node.id());

    spawn(&node, server, poll, None);

    node.join().expect("Node thread panicked!");

    Ok(())
}
