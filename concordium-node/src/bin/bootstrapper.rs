#![recursion_limit = "1024"]
#[cfg(not(target_os = "windows"))]
extern crate grpciounix as grpcio;
#[cfg(target_os = "windows")]
extern crate grpciowin as grpcio;
#[macro_use]
extern crate log;
#[macro_use]
extern crate p2p_client;

// Explicitly defining allocator to avoid future reintroduction of jemalloc
use std::alloc::System;
#[global_allocator]
static A: System = System;

use env_logger::{Builder, Env};
use failure::Error;
use p2p_client::{
    client::utils as client_utils,
    common::{
        functor::{FilterFunctor, Functorable},
        P2PNodeId, PeerType,
    },
    configuration,
    connection::MessageManager,
    db::P2PDB,
    network::{NetworkMessage, NetworkRequest},
    p2p::*,
    safe_read, spawn_or_die,
    stats_export_service::StatsServiceMode,
    utils,
};
use std::sync::{mpsc, Arc, RwLock};

fn main() -> Result<(), Error> {
    let conf = configuration::parse_config();

    let app_prefs = configuration::AppPreferences::new(
        conf.common.config_dir.to_owned(),
        conf.common.data_dir.to_owned(),
    );

    let env = if conf.common.trace {
        Env::default().filter_or("MY_LOG_LEVEL", "trace")
    } else if conf.common.debug {
        Env::default().filter_or("MY_LOG_LEVEL", "debug")
    } else {
        Env::default().filter_or("MY_LOG_LEVEL", "info")
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

    let mut db_path = app_prefs.get_user_app_dir();
    db_path.push("p2p.db");

    let db = P2PDB::new(db_path.as_path());

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

    let arc_stats_export_service = if let Some(ref service) = stats_export_service {
        Some(Arc::clone(service))
    } else {
        None
    };

    info!("Debugging enabled {}", conf.common.debug);

    let id = match conf.common.id {
        Some(ref x) => x.to_owned(),
        _ => format!("{}", P2PNodeId::default()),
    };

    let (pkt_in, _pkt_out) = mpsc::channel::<Arc<NetworkMessage>>();

    let broadcasting_checks = Arc::new(FilterFunctor::new("Broadcasting_checks"));

    let node = if conf.common.debug {
        let (sender, receiver) = mpsc::channel();
        let _guard = spawn_or_die!("Log loop", move || loop {
            if let Ok(msg) = receiver.recv() {
                info!("{}", msg);
            }
        });
        Arc::new(RwLock::new(P2PNode::new(
            Some(id),
            &conf,
            pkt_in,
            Some(sender),
            PeerType::Bootstrapper,
            arc_stats_export_service,
            Arc::clone(&broadcasting_checks),
        )))
    } else {
        Arc::new(RwLock::new(P2PNode::new(
            Some(id),
            &conf,
            pkt_in,
            None,
            PeerType::Bootstrapper,
            arc_stats_export_service,
            Arc::clone(&broadcasting_checks),
        )))
    };

    match db.get_banlist() {
        Some(nodes) => {
            info!("Found existing banlist, loading up!");
            let mut locked_node = write_or_die!(node);
            for n in nodes {
                locked_node.ban_node(n);
            }
        }
        None => {
            info!("Couldn't find existing banlist. Creating new!");
            db.create_banlist();
        }
    };

    let cloned_node = Arc::clone(&node);
    let _no_trust_bans = conf.common.no_trust_bans;

    // Register handles for ban & unban requests.
    let message_handler = read_or_die!(node).message_handler();
    safe_write!(message_handler)?.add_request_callback(make_atomic_callback!(
        move |msg: &NetworkRequest| {
            match msg {
                NetworkRequest::BanNode(ref peer, x) => {
                    let mut locked_cloned_node = write_or_die!(cloned_node);
                    utils::ban_node(&mut locked_cloned_node, peer, *x, &db, _no_trust_bans);
                }
                NetworkRequest::UnbanNode(ref peer, x) => {
                    let mut locked_cloned_node = write_or_die!(cloned_node);
                    utils::unban_node(&mut locked_cloned_node, peer, *x, &db, _no_trust_bans);
                }
                _ => {}
            };
            Ok(())
        }
    ));

    #[cfg(feature = "instrumentation")]
    // Start push gateway to prometheus
    client_utils::start_push_gateway(
        &conf.prometheus,
        &stats_export_service,
        safe_read!(node)?.id(),
    )?;

    {
        let mut locked_node = safe_write!(node)?;
        locked_node.max_nodes = Some(conf.bootstrapper.max_nodes);
        locked_node.print_peers = true;
        locked_node.spawn();
    }

    write_or_die!(node).join().expect("Node thread panicked!");

    // Close stats server export if present
    client_utils::stop_stats_export_engine(&conf, &stats_export_service);

    Ok(())
}
