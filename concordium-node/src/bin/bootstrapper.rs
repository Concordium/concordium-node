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

use chrono::prelude::Utc;
use env_logger::{Builder, Env};
use failure::Error;
use p2p_client::{
    common::PeerType,
    configuration,
    connection::MessageManager,
    db::P2PDB,
    network::{NetworkMessage, NetworkRequest},
    p2p::*,
    safe_read,
    stats_export_service::{StatsExportService, StatsServiceMode},
    utils,
};
use std::{
    sync::{mpsc, Arc, RwLock},
    thread,
};

#[cfg(feature = "instrumentation")]
use failure::Fallible;

#[cfg(feature = "instrumentation")]
use p2p_client::common::P2PNodeId;

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

    #[cfg(feature = "instrumentation")]
    let stats_export_service = if conf.prometheus.prometheus_server {
        use std::net::SocketAddr;
        info!("Enabling prometheus server");
        let mut srv = StatsExportService::new(StatsServiceMode::BootstrapperMode);
        srv.start_server(SocketAddr::new(
            conf.prometheus.prometheus_listen_addr.parse()?,
            conf.prometheus.prometheus_listen_port,
        ));
        Some(Arc::new(RwLock::new(srv)))
    } else if let Some(ref gateway) = conf.prometheus.prometheus_push_gateway {
        info!("Enabling prometheus push gateway at {}", gateway);
        let srv = StatsExportService::new(StatsServiceMode::BootstrapperMode);
        Some(Arc::new(RwLock::new(srv)))
    } else {
        None
    };

    #[cfg(not(feature = "instrumentation"))]
    let stats_export_service = Some(Arc::new(RwLock::new(StatsExportService::new(
        StatsServiceMode::NodeMode,
    ))));

    let arc_stats_export_service = if let Some(ref p) = stats_export_service {
        Some(Arc::clone(p))
    } else {
        None
    };

    info!("Debugging enabled {}", conf.common.debug);

    let id = match conf.common.id {
        Some(ref x) => x.to_owned(),
        _ => {
            let current_time = Utc::now();
            base64::encode(&utils::sha256(&format!(
                "{}.{}",
                current_time.timestamp(),
                current_time.timestamp_subsec_nanos()
            )))
        }
    };

    let (pkt_in, _pkt_out) = mpsc::channel::<Arc<NetworkMessage>>();

    let node = if conf.common.debug {
        let (sender, receiver) = mpsc::channel();
        let _guard = thread::spawn(move || loop {
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
        )))
    } else {
        Arc::new(RwLock::new(P2PNode::new(
            Some(id),
            &conf,
            pkt_in,
            None,
            PeerType::Bootstrapper,
            arc_stats_export_service,
        )))
    };

    match db.get_banlist() {
        Some(nodes) => {
            info!("Found existing banlist, loading up!");
            let mut locked_node = safe_write!(node)?;
            for n in nodes {
                locked_node.ban_node(n)?;
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
    let message_handler = safe_read!(node)?.message_handler();
    safe_write!(message_handler)?.add_request_callback(make_atomic_callback!(
        move |msg: &NetworkRequest| {
            match msg {
                NetworkRequest::BanNode(ref peer, x) => {
                    let mut locked_cloned_node = safe_write!(cloned_node)?;
                    utils::ban_node(&mut locked_cloned_node, peer, *x, &db, _no_trust_bans);
                }
                NetworkRequest::UnbanNode(ref peer, x) => {
                    let mut locked_cloned_node = safe_write!(cloned_node)?;
                    utils::unban_node(&mut locked_cloned_node, peer, *x, &db, _no_trust_bans);
                }
                _ => {}
            };
            Ok(())
        }
    ));

    #[cfg(feature = "instrumentation")]
    // Start push gateway to prometheus
    start_push_gateway(
        &conf.prometheus,
        &stats_export_service,
        node.read().unwrap().id(),
    )?;

    {
        let mut locked_node = safe_write!(node)?;
        locked_node.max_nodes = Some(conf.bootstrapper.max_nodes);
        locked_node.print_peers = true;
        locked_node.spawn()?;
    }

    safe_write!(node)?.join().expect("Node thread panicked!");

    Ok(())
}

#[cfg(feature = "instrumentation")]
fn start_push_gateway(
    conf: &configuration::PrometheusConfig,
    stats_export_service: &Option<Arc<RwLock<StatsExportService>>>,
    id: P2PNodeId,
) -> Fallible<()> {
    if let Some(ref service) = stats_export_service {
        if let Some(ref prom_push_addy) = conf.prometheus_push_gateway {
            let instance_name = if let Some(ref instance_id) = conf.prometheus_instance_name {
                instance_id.clone()
            } else {
                id.to_string()
            };
            safe_read!(service)?.start_push_to_gateway(
                prom_push_addy.clone(),
                conf.prometheus_push_interval,
                conf.prometheus_job_name.clone(),
                instance_name,
                conf.prometheus_push_username.clone(),
                conf.prometheus_push_password.clone(),
            )
        }
    }
    Ok(())
}
