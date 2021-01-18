#![recursion_limit = "1024"]

// Force the system allocator on every platform
use std::alloc::System;
#[global_allocator]
static A: System = System;

use concordium_node::{
    common::{P2PNodeId, PeerType},
    p2p::{maintenance::spawn, *},
    stats_export_service::instantiate_stats_export_engine,
    utils::get_config_and_logging_setup,
};
use failure::Error;

#[cfg(feature = "instrumentation")]
use concordium_node::stats_export_service::start_push_gateway;

fn main() -> Result<(), Error> {
    let (mut conf, app_prefs) = get_config_and_logging_setup()?;
    conf.connection.max_allowed_nodes = Some(conf.bootstrapper.max_nodes);
    let data_dir_path = app_prefs.get_user_app_dir();

    let stats_export_service = instantiate_stats_export_engine(&conf)?;

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
    );

    #[cfg(feature = "instrumentation")]
    start_push_gateway(&conf.prometheus, &node.stats, node.id());

    spawn(&node, poll, None);

    node.join().expect("Node thread panicked!");

    Ok(())
}
