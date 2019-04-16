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
    configuration,
    connection::{MessageManager, P2PEvent, P2PNodeMode},
    db::P2PDB,
    network::{NetworkMessage, NetworkRequest},
    p2p::*,
    prometheus_exporter::{PrometheusMode, PrometheusServer},
    safe_read, utils,
};
use std::{
    rc::Rc,
    sync::{mpsc, Arc, RwLock},
    thread,
};

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

    let prometheus = if conf.prometheus.prometheus_server {
        info!("Enabling prometheus server");
        let mut srv = PrometheusServer::new(PrometheusMode::BootstrapperMode);
        srv.start_server(
            &conf.prometheus.prometheus_listen_addr,
            conf.prometheus.prometheus_listen_port,
        )
        .map_err(|e| error!("{}", e))
        .ok();
        Some(Arc::new(RwLock::new(srv)))
    } else if let Some(ref gateway) = conf.prometheus.prometheus_push_gateway {
        info!("Enabling prometheus push gateway at {}", gateway);
        let srv = PrometheusServer::new(PrometheusMode::BootstrapperMode);
        Some(Arc::new(RwLock::new(srv)))
    } else {
        None
    };

    let arc_prometheus = if let Some(ref p) = prometheus {
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
                match msg {
                    P2PEvent::ConnectEvent(ip, port) => {
                        info!("Received connection from {}:{}", ip, port)
                    }
                    P2PEvent::DisconnectEvent(msg) => info!("Received disconnect for {}", msg),
                    P2PEvent::ReceivedMessageEvent(node_id) => {
                        info!("Received message from {:?}", node_id)
                    }
                    P2PEvent::SentMessageEvent(node_id) => info!("Sent message to {:?}", node_id),
                    P2PEvent::InitiatingConnection(ip, port) => {
                        info!("Initiating connection to {}:{}", ip, port)
                    }
                    P2PEvent::JoinedNetwork(peer, network_id) => {
                        info!(
                            "Peer {} joined network {}",
                            peer.id().to_string(),
                            network_id
                        );
                    }
                    P2PEvent::LeftNetwork(peer, network_id) => {
                        info!("Peer {} left network {}", peer.id().to_string(), network_id);
                    }
                }
            }
        });
        Arc::new(RwLock::new(P2PNode::new(
            Some(id),
            &conf,
            pkt_in,
            Some(sender),
            P2PNodeMode::BootstrapperMode,
            arc_prometheus,
        )))
    } else {
        Arc::new(RwLock::new(P2PNode::new(
            Some(id),
            &conf,
            pkt_in,
            None,
            P2PNodeMode::BootstrapperMode,
            arc_prometheus,
        )))
    };

    match db.get_banlist() {
        Some(nodes) => {
            info!("Found existing banlist, loading up!");
            let mut locked_node = safe_write!(node)?;
            for n in nodes {
                locked_node.ban_node(n.to_peer())?;
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
                NetworkRequest::BanNode(ref peer, ref x) => {
                    info!("Ban node request for {:?}", x);
                    let mut locked_cloned_node = safe_write!(cloned_node)?;
                    let ban = locked_cloned_node
                        .ban_node(x.to_owned())
                        .map_err(|e| error!("{}", e));
                    if ban.is_ok() {
                        db.insert_ban(&peer.id().to_string(), &peer.ip().to_string(), peer.port());
                        if !_no_trust_bans {
                            locked_cloned_node.send_ban(x.to_owned())?;
                        }
                    }
                }
                NetworkRequest::UnbanNode(ref peer, ref x) => {
                    info!("Unban node requets for {:?}", x);
                    let req = safe_write!(cloned_node)?
                        .unban_node(x.to_owned())
                        .map_err(|e| error!("{}", e));
                    if req.is_ok() {
                        db.delete_ban(peer.id().to_string(), peer.ip().to_string(), peer.port());
                        if !_no_trust_bans {
                            safe_write!(cloned_node)?.send_unban(x.to_owned())?;
                        }
                    }
                }
                _ => {}
            };
            Ok(())
        }
    ));

    if let Some(ref prom) = prometheus {
        if let Some(ref prom_push_addy) = conf.prometheus.prometheus_push_gateway {
            let instance_name =
                if let Some(ref instance_id) = conf.prometheus.prometheus_instance_name {
                    instance_id.to_owned()
                } else {
                    safe_write!(node)?.id().to_string()
                };
            safe_read!(prom)?
                .start_push_to_gateway(
                    prom_push_addy.to_owned(),
                    conf.prometheus.prometheus_push_interval,
                    conf.prometheus.prometheus_job_name,
                    instance_name,
                    conf.prometheus.prometheus_push_username,
                    conf.prometheus.prometheus_push_password,
                )
                .map_err(|e| error!("{}", e))
                .ok();
        }
    }

    {
        let mut locked_node = safe_write!(node)?;
        locked_node.max_nodes = Some(conf.bootstrapper.max_nodes);
        locked_node.print_peers = true;
        locked_node.spawn()?;
    }

    safe_write!(node)?.join().expect("Node thread panicked!");

    Ok(())
}
