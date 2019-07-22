#![recursion_limit = "1024"]
#[cfg(not(target_os = "windows"))]
extern crate grpciounix as grpcio;
#[cfg(target_os = "windows")]
extern crate grpciowin as grpcio;
#[macro_use]
extern crate log;
extern crate p2p_client;
#[macro_use]
extern crate concordium_common;

// Explicitly defining allocator to avoid future reintroduction of jemalloc
use std::alloc::System;
#[global_allocator]
static A: System = System;

use concordium_common::{
    stats_export_service::StatsServiceMode, RelayOrStopEnvelope, RelayOrStopReceiver,
};
use env_logger::{Builder, Env};
use failure::Error;
use p2p_client::{
    client::utils as client_utils,
    common::{P2PNodeId, PeerType},
    configuration,
    network::{NetworkMessage, NetworkRequest},
    p2p::*,
    utils::{self, load_bans},
};
use rkv::{Manager, Rkv};
use std::sync::{mpsc, Arc, RwLock};

fn main() -> Result<(), Error> {
    let conf = configuration::parse_config()?;

    let app_prefs = configuration::AppPreferences::new(
        conf.common.config_dir.to_owned(),
        conf.common.data_dir.to_owned(),
    );
    let data_dir_path = app_prefs.get_user_app_dir();

    let env = if conf.common.trace {
        Env::default().filter_or("LOG_LEVEL", "trace")
    } else if conf.common.debug {
        Env::default().filter_or("LOG_LEVEL", "debug")
    } else {
        Env::default().filter_or("LOG_LEVEL", "info")
    };

    let mut log_builder = Builder::from_env(env);
    if conf.common.no_log_timestamp {
        log_builder.default_format_timestamp(false);
    }
    log_builder.init();

    p2p_client::setup_panics();

    if conf.common.print_config {
        // Print out the configuration
        info!("Config {:?}", conf);
    }

    info!(
        "Starting up {}-bootstrapper version {}!",
        p2p_client::APPNAME,
        p2p_client::VERSION
    );
    info!(
        "Application data directory: {:?}",
        app_prefs.get_user_app_dir()
    );
    info!(
        "Application config directory: {:?}",
        app_prefs.get_user_config_dir()
    );

    // Instantiate stats export engine
    let stats_export_service =
        client_utils::instantiate_stats_export_engine(&conf, StatsServiceMode::BootstrapperMode)
            .unwrap_or_else(|e| {
                error!(
                    "I was not able to instantiate an stats export service: {}",
                    e
                );
                None
            });

    info!("Debugging enabled: {}", conf.common.debug);

    let id = match conf.common.id {
        Some(ref x) => x.to_owned(),
        _ => format!("{}", P2PNodeId::default()),
    };

    let (pkt_in, pkt_out) = mpsc::sync_channel::<RelayOrStopEnvelope<Arc<NetworkMessage>>>(64);
    let (rpc_tx, _) = std::sync::mpsc::sync_channel(64);

    let mut node = if conf.common.debug {
        let (sender, receiver) = mpsc::sync_channel(64);
        let _guard = spawn_or_die!("Log loop", move || loop {
            if let Ok(msg) = receiver.recv() {
                info!("{}", msg);
            }
        });
        P2PNode::new(
            Some(id),
            &conf,
            pkt_in,
            Some(sender),
            PeerType::Bootstrapper,
            stats_export_service.clone(),
            rpc_tx,
        )
    } else {
        P2PNode::new(
            Some(id),
            &conf,
            pkt_in,
            None,
            PeerType::Bootstrapper,
            stats_export_service.clone(),
            rpc_tx,
        )
    };

    #[cfg(feature = "instrumentation")]
    // Start push gateway to prometheus
    client_utils::start_push_gateway(&conf.prometheus, &stats_export_service, node.id())?;

    // Create the key-value store environment
    let kvs_handle = Manager::singleton()
        .write()
        .unwrap()
        .get_or_create(data_dir_path.as_path(), Rkv::new)
        .unwrap();

    // Load and apply existing bans
    if let Err(e) = load_bans(&mut node, &kvs_handle) {
        error!("{}", e);
    };

    // Connect outgoing messages to be forwarded into the baker and RPC streams.
    //
    // Thread #4: Read P2PNode output
    setup_process_output(&node, kvs_handle, &conf, pkt_out);

    {
        node.max_nodes = Some(conf.bootstrapper.max_nodes);
        node.print_peers = true;
        node.spawn();
    }

    node.join().expect("Node thread panicked!");

    // Close stats server export if present
    client_utils::stop_stats_export_engine(&conf, &stats_export_service);

    Ok(())
}

fn setup_process_output(
    node: &P2PNode,
    kvs_handle: Arc<RwLock<Rkv>>,
    conf: &configuration::Config,
    pkt_out: RelayOrStopReceiver<Arc<NetworkMessage>>,
) {
    let mut _node_self_clone = node.clone();
    let _no_trust_bans = conf.common.no_trust_bans;
    let _guard_pkt = spawn_or_die!("Higher queue processing", move || {
        while let Ok(RelayOrStopEnvelope::Relay(full_msg)) = pkt_out.recv() {
            match *full_msg {
                NetworkMessage::NetworkRequest(
                    NetworkRequest::BanNode(ref peer, peer_to_ban),
                    ..
                ) => {
                    utils::ban_node(
                        &mut _node_self_clone,
                        peer,
                        peer_to_ban,
                        &kvs_handle,
                        _no_trust_bans,
                    );
                }
                NetworkMessage::NetworkRequest(
                    NetworkRequest::UnbanNode(ref peer, peer_to_ban),
                    ..
                ) => {
                    utils::unban_node(
                        &mut _node_self_clone,
                        peer,
                        peer_to_ban,
                        &kvs_handle,
                        _no_trust_bans,
                    );
                }
                _ => {}
            }
        }
    });

    info!(
        "Concordium P2P layer. Network disabled: {}",
        conf.cli.no_network
    );
}
