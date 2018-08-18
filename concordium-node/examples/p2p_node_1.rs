#![feature(box_syntax, box_patterns)]
#![recursion_limit = "1024"]
#[macro_use]
extern crate error_chain;
extern crate bytes;
extern crate mio;
extern crate p2p_client;
#[macro_use]
extern crate log;
extern crate env_logger;
use env_logger::Env;
use p2p_client::common::{NetworkMessage, NetworkPacket, NetworkRequest};
use p2p_client::configuration;
use p2p_client::p2p::*;
use std::sync::mpsc;
use std::sync::{Arc,Mutex};
use p2p_client::db::P2PDB;
use std::thread;
use p2p_client::errors::*;
use p2p_client::utils;
use p2p_client::prometheus_exporter::PrometheusServer;

quick_main!(run);

fn run() -> ResultExtWrapper<()> {
    let conf = configuration::parse_cli_config();
    let app_prefs = configuration::AppPreferences::new();

    let env = if conf.debug {
        Env::default().filter_or("MY_LOG_LEVEL", "debug")
    } else {
        Env::default().filter_or("MY_LOG_LEVEL", "info")
    };

    env_logger::init_from_env(env);
    info!("Starting up {} version {}!",
          p2p_client::APPNAME,
          p2p_client::VERSION);

    info!("Application data directory: {:?}",
          app_prefs.get_user_app_dir());
    info!("Application config directory: {:?}",
          app_prefs.get_user_config_dir());

    let prometheus = if conf.prometheus_server {
        info!("Enabling prometheus server");
        let mut srv = PrometheusServer::new();
        srv.start_server(&conf.prometheus_listen_addr, conf.prometheus_listen_port).map_err(|e| error!("{}", e)).ok();
        Some(Arc::new(Mutex::new(srv)))
    } else if conf.prometheus_push_gateway.is_some() {
        info!("Enabling prometheus push gateway at {}", &conf.prometheus_push_gateway.clone().unwrap());
        let mut srv = PrometheusServer::new();
        Some(Arc::new(Mutex::new(srv)))
    } else {
        None
    };

    info!("Debuging enabled {}", conf.debug);

    let mut db_path = app_prefs.get_user_app_dir().clone();
    db_path.push("p2p.db");

    let db = P2PDB::new(db_path.as_path());

    let bootstrap_nodes = utils::get_bootstrap_nodes(conf.require_dnssec, conf.bootstrap_server.clone());

    let (pkt_in, pkt_out) = mpsc::channel::<Arc<Box<NetworkMessage>>>();

    let _guard_pkt = thread::spawn(move || loop {
        if let Ok(ref outer_msg) = pkt_out.recv() {
            match *outer_msg.clone() {
                box NetworkMessage::NetworkPacket(NetworkPacket::DirectMessage(_, _, ref msg),
                                                  _,
                                                  _) => {
                    info!("DirectMessage with {:?} received", msg)
                }
                box NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(_,
                                                                                    ref msg),
                                                  _,
                                                  _) => {
                    info!("BroadcastedMessage with {:?} received", msg)
                }
                box NetworkMessage::NetworkRequest(NetworkRequest::BanNode(_, ref x), _, _) => {
                    info!("Ban node request for {:?}", x)
                }
                box NetworkMessage::NetworkRequest(NetworkRequest::UnbanNode(_, ref x), _, _) => {
                    info!("Unban node requets for {:?}", x)
                }
                _ => {}
            }
        }
    });

    let mut node = if conf.debug {
        let (sender, receiver) = mpsc::channel();
        let _guard = thread::spawn(move || loop {
                                       if let Ok(msg) = receiver.recv() {
                                           match msg {
                                               P2PEvent::ConnectEvent(ip, port) => {
                                                   info!("Received connection from {}:{}", ip, port)
                                               }
                                               P2PEvent::DisconnectEvent(msg) => {
                                                   info!("Received disconnect for {}", msg)
                                               }
                                               P2PEvent::ReceivedMessageEvent(node_id) => {
                                                   info!("Received message from {:?}", node_id)
                                               }
                                               P2PEvent::SentMessageEvent(node_id) => {
                                                   info!("Sent message to {:?}", node_id)
                                               }
                                               P2PEvent::InitiatingConnection(ip, port) => {
                                                   info!("Initiating connection to {}:{}", ip, port)
                                               }
                                           }
                                       }
                                   });
        P2PNode::new(conf.id, conf.listen_address, conf.listen_port, conf.external_ip, conf.external_port, pkt_in, Some(sender),P2PNodeMode::NormalMode, prometheus.clone())
    } else {
        P2PNode::new(conf.id, conf.listen_address, conf.listen_port, conf.external_ip, conf.external_port, pkt_in, None,P2PNodeMode::NormalMode, prometheus.clone())
    };

    if let Some(ref prom ) = prometheus {
        if let Some(ref prom_push_addy) = conf.prometheus_push_gateway {
            let instance_name = if let Some(ref instance_id) = conf.prometheus_instance_name {
                instance_id.clone()
            } else {
                node.get_own_id().to_string()
            };
            prom.lock()?.start_push_to_gateway( prom_push_addy.clone(), conf.prometheus_job_name, instance_name, 
                conf.prometheus_push_username, conf.prometheus_push_password).map_err(|e| error!("{}", e)).ok();
        }
    }

    let _th = node.spawn();

    info!("Attempting to bootstrap via DNS");
    match bootstrap_nodes {
        Ok(nodes) => {
            for (ip, port) in nodes {
                info!("Found bootstrap node IP: {} and port: {}", ip, port);
                node.connect(ip, port).map_err(|e| error!("{}", e)).ok();
            }
        }
        Err(e) => error!("Couldn't retrieve bootstrap node list! {:?}", e),
    };

    match db.get_banlist() {
        Some(nodes) => {
            info!("Found existing banlist, loading up!");
            for n in nodes {
                node.ban_node(n.to_peer())?;
            }
        }
        None => {
            info!("Couldn't find existing banlist. Creating new!");
            db.create_banlist();
        }
    };

    _th.join().unwrap();

    Ok(())
}
