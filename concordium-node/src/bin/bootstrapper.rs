#![recursion_limit = "1024"]
#[macro_use]
extern crate log;
#[macro_use]
extern crate concordium_common;

// Explicitly defining allocator to avoid future reintroduction of jemalloc
use std::alloc::System;
#[global_allocator]
static A: System = System;

use concordium_common::QueueMsg::Relay;
use crossbeam_channel;
use failure::Error;
use p2p_client::{
    common::{P2PNodeId, PeerType},
    configuration as config,
    p2p::*,
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
        // Print out the configuration
        info!("Config {:?}", conf);
    }

    info!("Starting up {}-bootstrapper version {}!", p2p_client::APPNAME, p2p_client::VERSION);
    info!("Application data directory: {:?}", app_prefs.get_user_app_dir());
    info!("Application config directory: {:?}", app_prefs.get_user_config_dir());

    // Instantiate stats export engine
    let stats_export_service =
        instantiate_stats_export_engine(&conf, StatsServiceMode::BootstrapperMode).unwrap();

    info!("Debugging enabled: {}", conf.common.debug);

    let id = match conf.common.id {
        Some(ref x) => x.to_owned(),
        _ => format!("{}", P2PNodeId::default()),
    };

    let node = if conf.common.debug {
        let (sender, receiver) = crossbeam_channel::bounded(config::EVENT_LOG_QUEUE_DEPTH);
        let _guard = spawn_or_die!("Log loop", move || loop {
            if let Ok(Relay(msg)) = receiver.recv() {
                info!("{}", msg);
            }
        });
        P2PNode::new(
            Some(id),
            &conf,
            Some(sender),
            PeerType::Bootstrapper,
            stats_export_service,
            Some(data_dir_path),
        )
    } else {
        P2PNode::new(
            Some(id),
            &conf,
            None,
            PeerType::Bootstrapper,
            stats_export_service,
            Some(data_dir_path),
        )
    };

    #[cfg(feature = "instrumentation")]
    // Start push gateway to prometheus
    start_push_gateway(&conf.prometheus, &node.stats, node.id());

    info!("Concordium P2P layer. Network disabled: {}", conf.cli.no_network);

    node.spawn();

    node.join().expect("Node thread panicked!");

    Ok(())
}
