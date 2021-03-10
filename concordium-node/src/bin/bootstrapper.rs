#![recursion_limit = "1024"]

// Force the system allocator on every platform
use std::{
    alloc::System,
    sync::{Arc, RwLock},
};
#[global_allocator]
static A: System = System;
use serde_json;

use concordium_node::{
    common::{P2PNodeId, PeerType},
    consensus_ffi::blockchain_types::BlockHash,
    p2p::{maintenance::spawn, *},
    stats_export_service::instantiate_stats_export_engine,
    utils::get_config_and_logging_setup,
};
use failure::{ensure, Error};

#[cfg(feature = "instrumentation")]
use concordium_node::stats_export_service::start_push_gateway;

fn main() -> Result<(), Error> {
    let (mut conf, app_prefs) = get_config_and_logging_setup()?;
    conf.connection.max_allowed_nodes = Some(conf.bootstrapper.max_nodes);
    let data_dir_path = app_prefs.get_user_app_dir();

    let stats_export_service = instantiate_stats_export_engine(&conf)?;

    let regenesis_blocks: Vec<BlockHash> = match &conf.bootstrapper.regenesis_block_hashes {
        None => {
            let mut regenesis_path = data_dir_path.clone();
            regenesis_path.push("/genesis_hash");
            serde_json::from_str(&std::fs::read_to_string(regenesis_path)?)?
        }
        Some(p) => serde_json::from_str(&std::fs::read_to_string(p)?)?,
    };
    let regenesis_arc: Arc<RwLock<Vec<BlockHash>>> = Arc::new(RwLock::new(regenesis_blocks));

    ensure!(
        regenesis_arc.read().unwrap().len() > 0,
        "Bootstrapper can't run without specifying genesis hashes."
    );

    let id = match conf.common.id {
        Some(ref x) => x.to_owned(),
        _ => P2PNodeId::default().to_string(),
    };

    let (node, poll) = P2PNode::new(
        Some(id),
        &conf,
        PeerType::Bootstrapper,
        stats_export_service,
        Some(data_dir_path),
        regenesis_arc,
    );

    #[cfg(feature = "instrumentation")]
    start_push_gateway(&conf.prometheus, &node.stats, node.id());

    spawn(&node, poll, None);

    node.join().expect("Node thread panicked!");

    Ok(())
}
