#![feature(box_syntax, box_patterns)]
#![recursion_limit = "1024"]
#[cfg(not(target_os = "windows"))]
extern crate grpciounix as grpcio;
#[cfg(target_os = "windows")]
extern crate grpciowin as grpcio;
#[macro_use] extern crate log;

// Explicitly defining allocator to avoid future reintroduction of jemalloc
use std::alloc::System;
#[global_allocator]
static A: System = System;

use env_logger::{Builder, Env};
use p2p_client::network::{NetworkMessage, NetworkRequest};
use p2p_client::configuration;
use p2p_client::db::P2PDB;
use failure::Error;
use p2p_client::p2p::*;
use p2p_client::safe_lock;
use p2p_client::connection::{ P2PEvent, P2PNodeMode };
use p2p_client::prometheus_exporter::{PrometheusMode, PrometheusServer};
use std::sync::mpsc;
use std::sync::{Arc, Mutex};
use std::rc::Rc;
use std::thread;
use timer::Timer;

fn main() -> Result<(), Error> {
    let conf = configuration::parse_bootstrapper_config();
    let app_prefs =
        configuration::AppPreferences::new(conf.config_dir.clone(), conf.data_dir.clone());

    let env = if conf.trace {
        Env::default().filter_or("MY_LOG_LEVEL", "trace")
    } else if conf.debug {
        Env::default().filter_or("MY_LOG_LEVEL", "debug")
    } else {
        Env::default().filter_or("MY_LOG_LEVEL", "info")
    };

    let mut log_builder = Builder::from_env(env);
    if conf.no_log_timestamp {
        log_builder.default_format_timestamp(false);
    }
    log_builder.init();

    p2p_client::setup_panics();

    info!("Starting up {}-bootstrapper version {}!",
          p2p_client::APPNAME,
          p2p_client::VERSION);
    info!("Application data directory: {:?}",
          app_prefs.get_user_app_dir());
    info!("Application config directory: {:?}",
          app_prefs.get_user_config_dir());

    let mut db_path = app_prefs.get_user_app_dir();
    db_path.push("p2p.db");

    let db = P2PDB::new(db_path.as_path());

    let prometheus = if conf.prometheus_server {
        info!("Enabling prometheus server");
        let mut srv = PrometheusServer::new(PrometheusMode::BootstrapperMode);
        srv.start_server(&conf.prometheus_listen_addr, conf.prometheus_listen_port)
           .map_err(|e| error!("{}", e))
           .ok();
        Some(Arc::new(Mutex::new(srv)))
    } else if let Some(ref gateway) = conf.prometheus_push_gateway {
        info!("Enabling prometheus push gateway at {}", gateway);
        let srv = PrometheusServer::new(PrometheusMode::BootstrapperMode);
        Some(Arc::new(Mutex::new(srv)))
    } else {
        None
    };

    info!("Debugging enabled {}", conf.debug);

    let (pkt_in, pkt_out) = mpsc::channel::<Arc<Box<NetworkMessage>>>();

    let mode_type = if conf.private_node {
        P2PNodeMode::BootstrapperPrivateMode
    } else {
        P2PNodeMode::BootstrapperMode
    };

    let mut node = if conf.debug {
        let (sender, receiver) = mpsc::channel();
        let _guard =
            thread::spawn(move || {
                              loop {
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
                                          P2PEvent::JoinedNetwork(peer, network_id) => {
                                              info!("Peer {} joined network {}",
                                                    peer.id().to_string(),
                                                    network_id);
                                          }
                                          P2PEvent::LeftNetwork(peer, network_id) => {
                                              info!("Peer {} left network {}",
                                                    peer.id().to_string(),
                                                    network_id);
                                          }
                                      }
                                  }
                              }
                          });
        P2PNode::new(Some(conf.id),
                     conf.listen_address,
                     conf.listen_port,
                     conf.external_ip,
                     conf.external_port,
                     pkt_in,
                     Some(sender),
                     mode_type,
                     prometheus.clone(),
                     conf.network_ids,
                     conf.min_peers_bucket,
                     false)
    } else {
        P2PNode::new(Some(conf.id),
                     conf.listen_address,
                     conf.listen_port,
                     conf.external_ip,
                     conf.external_port,
                     pkt_in,
                     None,
                     mode_type,
                     prometheus.clone(),
                     conf.network_ids,
                     conf.min_peers_bucket,
                     false)
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

    let mut _node_self_clone = node.clone();
    let _no_trust_bans = conf.no_trust_bans;

    let _guard_pkt = thread::spawn(move || {
        loop {
            if let Ok(full_msg) = pkt_out.recv() {
                match *full_msg.clone() {
                    box NetworkMessage::NetworkRequest(NetworkRequest::BanNode(ref peer,
                                                                               ref x),
                                                       _,
                                                       _) => {
                        info!("Ban node request for {:?}", x);
                        let ban = _node_self_clone.ban_node(x.clone())
                                                  .map_err(|e| error!("{}", e));
                        if ban.is_ok() {
                            db.insert_ban(peer.id().to_string(),
                                          peer.ip().to_string(),
                                          peer.port());
                            if !_no_trust_bans {
                                _node_self_clone.send_ban(x.clone())
                                                .map_err(|e| error!("{}", e))
                                                .ok();
                            }
                        }
                    }
                    box NetworkMessage::NetworkRequest(NetworkRequest::UnbanNode(ref peer,
                                                                                 ref x),
                                                       _,
                                                       _) => {
                        info!("Unban node requets for {:?}", x);
                        let req = _node_self_clone.unban_node(x.clone())
                                                  .map_err(|e| error!("{}", e));
                        if req.is_ok() {
                            db.delete_ban(peer.id().to_string(),
                                          peer.ip().to_string(),
                                          peer.port());
                            if !_no_trust_bans {
                                _node_self_clone.send_unban(x.clone())
                                                .map_err(|e| error!("{}", e))
                                                .ok();
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    });

    if let Some(ref prom) = prometheus {
        if let Some(ref prom_push_addy) = conf.prometheus_push_gateway {
            let instance_name = if let Some(ref instance_id) = conf.prometheus_instance_name {
                instance_id.clone()
            } else {
                node.get_own_id().to_string()
            };
            safe_lock!(prom)?
                .start_push_to_gateway(prom_push_addy.clone(),
                                       conf.prometheus_push_interval,
                                       conf.prometheus_job_name,
                                       instance_name,
                                       conf.prometheus_push_username,
                                       conf.prometheus_push_password)
                .map_err(|e| error!("{}", e))
                .ok();
        }
    }

    node.spawn();

    let _node_th = Rc::try_unwrap(node.process_th().unwrap()).ok().unwrap().into_inner();

    let timer = Timer::new();

    let _max_nodes = conf.max_nodes;

    let _guard_timer = timer.schedule_repeating(chrono::Duration::seconds(30), move || {
        match node.get_peer_stats(&[]) {
            Ok(x) => info!("I currently have {}/{} nodes!", x.len(), _max_nodes),
            Err(e) => error!("Couldn't get node list, {:?}", e),
        };
    });

    _node_th.join().expect("Node thread panicked!");

    Ok(())
}
