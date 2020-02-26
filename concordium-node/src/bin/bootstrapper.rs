#![recursion_limit = "1024"]
#[macro_use]
extern crate log;

// Force the system allocator on every platform
use std::alloc::System;
#[global_allocator]
static A: System = System;

use failure::Error;
use p2p_client::{
    common::{P2PNodeId, PeerType},
    p2p::{maintenance::spawn, *},
    stats_export_service::{instantiate_stats_export_engine, StatsServiceMode},
    utils::get_config_and_logging_setup,
};

#[cfg(feature = "instrumentation")]
use p2p_client::stats_export_service::start_push_gateway;

fn main() -> Result<(), Error> {
    let (mut conf, app_prefs) = get_config_and_logging_setup()?;
    conf.connection.max_allowed_nodes = Some(conf.bootstrapper.max_nodes);
    let data_dir_path = app_prefs.get_user_app_dir();

    if conf.common.print_config {
        info!("Config {:?}", conf);
    }

    info!("Starting up {}-bootstrapper version {}!", p2p_client::APPNAME, p2p_client::VERSION);
    info!("Application data directory: {:?}", app_prefs.get_user_app_dir());
    info!("Application config directory: {:?}", app_prefs.get_user_config_dir());

    let stats_export_service =
        instantiate_stats_export_engine(&conf, StatsServiceMode::BootstrapperMode)?;

    info!("Debugging enabled: {}", conf.common.debug);

    let id = match conf.common.id {
        Some(ref x) => x.to_owned(),
        _ => format!("{}", P2PNodeId::default()),
    };

    let node = P2PNode::new(
        Some(id),
        &conf,
        PeerType::Bootstrapper,
        stats_export_service,
        Some(data_dir_path),
    );

    #[cfg(feature = "instrumentation")]
    start_push_gateway(&conf.prometheus, &node.stats, node.id());

    info!("Concordium P2P layer. Network disabled: {}", conf.cli.no_network);

    spawn(&node);

    node.join().expect("Node thread panicked!");

    Ok(())
}
