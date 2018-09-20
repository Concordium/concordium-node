#![feature(box_syntax, box_patterns)]
#![recursion_limit = "1024"]
extern crate p2p_client;
#[macro_use]
extern crate log;
extern crate bytes;
extern crate chrono;
extern crate env_logger;
extern crate grpcio;
extern crate mio;
extern crate timer;
#[macro_use]
extern crate error_chain;
use env_logger::Env;
use p2p_client::common::{NetworkMessage, NetworkRequest};
use p2p_client::configuration;
use p2p_client::db::P2PDB;
use p2p_client::errors::*;
use p2p_client::p2p::*;
use p2p_client::prometheus_exporter::{PrometheusMode, PrometheusServer};
use std::sync::mpsc;
use std::sync::{Arc, Mutex};
use std::thread;
use timer::Timer;

quick_main!(run);

fn run() -> ResultExtWrapper<()> {
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

    p2p_client::setup_panics();

    env_logger::init_from_env(env);
    info!("Starting up {}-bootstrapper version {}!",
          p2p_client::APPNAME,
          p2p_client::VERSION);
    info!("Application data directory: {:?}",
          app_prefs.get_user_app_dir());
    info!("Application config directory: {:?}",
          app_prefs.get_user_config_dir());

    let mut db_path = app_prefs.get_user_app_dir().clone();
    db_path.push("p2p.db");

    let db = P2PDB::new(db_path.as_path());

    let prometheus = if conf.prometheus_server {
        info!("Enabling prometheus server");
        let mut srv = PrometheusServer::new(PrometheusMode::NodeMode);
        srv.start_server(&conf.prometheus_listen_addr, conf.prometheus_listen_port)
           .map_err(|e| error!("{}", e))
           .ok();
        Some(Arc::new(Mutex::new(srv)))
    } else if conf.prometheus_push_gateway.is_some() {
        info!("Enabling prometheus push gateway at {}",
              &conf.prometheus_push_gateway.clone().unwrap());
        let mut srv = PrometheusServer::new(PrometheusMode::NodeMode);
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
                     conf.network_ids)
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
                     conf.network_ids)
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
                                          format!("{}", peer.ip()),
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
                                          format!("{}", peer.ip()),
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
            prom.lock()?
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

    let _node_th = node.spawn();

    let timer = Timer::new();

    let _max_nodes = conf.max_nodes;

    let _guard_timer = timer.schedule_repeating(chrono::Duration::seconds(30), move || {
                                match node.get_nodes() {
                                    Ok(x) => {
                                        info!("I currently have {}/{} nodes!", x.len(), _max_nodes);
                                    }
                                    Err(e) => error!("Couldn't get node list, {:?}", e),
                                };
                            });

    _node_th.join().unwrap();

    Ok(())
}
