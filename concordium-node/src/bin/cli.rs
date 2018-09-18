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
use p2p_client::common::{
    ConnectionType, NetworkMessage, NetworkPacket, NetworkRequest, NetworkResponse,
};
use p2p_client::configuration;
use p2p_client::db::P2PDB;
use p2p_client::errors::*;
use p2p_client::p2p::*;
use p2p_client::prometheus_exporter::{PrometheusMode, PrometheusServer};
use p2p_client::rpc::RpcServerImpl;
use p2p_client::utils;
use std::sync::mpsc;
use std::sync::{Arc, Mutex};
use std::thread;
use timer::Timer;

quick_main!(run);

fn run() -> ResultExtWrapper<()> {
    let conf = configuration::parse_cli_config();
    let mut app_prefs = configuration::AppPreferences::new();

    let bootstrap_nodes = utils::get_bootstrap_nodes(conf.bootstrap_server.clone());

    let env = if conf.trace {
        Env::default().filter_or("MY_LOG_LEVEL", "trace")
    } else if conf.debug {
        Env::default().filter_or("MY_LOG_LEVEL", "debug")
    } else {
        Env::default().filter_or("MY_LOG_LEVEL", "info")
    };

    p2p_client::setup_panics();

    env_logger::init_from_env(env);
    info!("Starting up {} version {}!",
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

    let mode_type = if conf.private_node {
        P2PNodeMode::NormalPrivateMode
    } else {
        P2PNodeMode::NormalMode
    };

    let (pkt_in, pkt_out) = mpsc::channel::<Arc<Box<NetworkMessage>>>();

    let external_ip = if conf.external_ip.is_some() {
        conf.external_ip
    } else if conf.ip_discovery_service {
        match utils::discover_external_ip(&conf.ip_discovery_service_host) {
            Ok(ip) => Some(ip.to_string()),
            Err(_) => None,
        }
    } else {
        None
    };

    let node_id = if conf.id.is_some() {
        conf.id.clone()
    } else {
        app_prefs.get_config(configuration::APP_PREFERENCES_PERSISTED_NODE_ID)
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
        P2PNode::new(node_id,
                     conf.listen_address,
                     conf.listen_port,
                     external_ip,
                     conf.external_port,
                     pkt_in,
                     Some(sender),
                     mode_type,
                     prometheus.clone(),
                     conf.network_ids)
    } else {
        P2PNode::new(node_id,
                     conf.listen_address,
                     conf.listen_port,
                     external_ip,
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

    if !app_prefs.set_config(configuration::APP_PREFERENCES_PERSISTED_NODE_ID,
                             Some(node.get_own_id().to_string()))
    {
        error!("Failed to persist own node id");
    }

    let mut rpc_serv: Option<RpcServerImpl> = None;
    if !conf.no_rpc_server {
        let mut serv = RpcServerImpl::new(node.clone(),
                                          Some(db.clone()),
                                          conf.rpc_server_addr,
                                          conf.rpc_server_port,
                                          conf.rpc_server_token);
        serv.start_server()?;
        rpc_serv = Some(serv);
    }

    let mut _node_self_clone = node.clone();

    let _no_trust_bans = conf.no_trust_bans;
    let _no_trust_broadcasts = conf.no_trust_broadcasts;
    let mut _rpc_clone = rpc_serv.clone();
    let _desired_nodes_clone = conf.desired_nodes;
    let _guard_pkt = thread::spawn(move || {
                                       loop {
                                           if let Ok(full_msg) = pkt_out.recv() {
                                               match *full_msg.clone() {
                                               box NetworkMessage::NetworkPacket(NetworkPacket::DirectMessage(_, ref msgid, _, ref nid, ref msg), _, _) => {
                                                   if let Some(ref mut rpc) = _rpc_clone {
                                                       rpc.queue_message(&full_msg).map_err(|e| error!("Couldn't queue message {}", e)).ok();
                                                   }
                                                   info!("DirectMessage/{}/{} with size {} received", nid, msgid, msg.len());
                                               }
                                               box NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(_, ref msgid, ref nid, ref msg), _, _) => {
                                                   if let Some(ref mut rpc) = _rpc_clone {
                                                       rpc.queue_message(&full_msg).map_err(|e| error!("Couldn't queue message {}", e)).ok();
                                                   }
                                                   if !_no_trust_broadcasts {
                                                       info!("BroadcastedMessage/{}/{} with size {} received", nid, msgid, msg.len());
                                                       _node_self_clone.send_message(None, *nid, Some(msgid.clone()), &msg, true).map_err(|e| error!("Error sending message {}", e)).ok();
                                                   }
                                               }

                                               box NetworkMessage::NetworkRequest(NetworkRequest::BanNode(ref peer, ref x), _, _) => {
                                                   info!("Ban node request for {:?}", x);
                                                   let ban = _node_self_clone.ban_node(x.clone()).map_err(|e| error!("{}", e));
                                                   if ban.is_ok() {
                                                       db.insert_ban(peer.id().to_string(), format!("{}", peer.ip()), peer.port());
                                                       if !_no_trust_bans {
                                                           _node_self_clone.send_ban(x.clone()).map_err(|e| error!("{}", e)).ok();
                                                       }
                                                   }
                                               }
                                               box NetworkMessage::NetworkRequest(NetworkRequest::UnbanNode(ref peer, ref x), _, _) => {
                                                   info!("Unban node requets for {:?}", x);
                                                   let req = _node_self_clone.unban_node(x.clone()).map_err(|e| error!("{}", e));
                                                   if req.is_ok() {
                                                       db.delete_ban(peer.id().to_string(), format!("{}", peer.ip()), peer.port());
                                                       if !_no_trust_bans {
                                                           _node_self_clone.send_unban(x.clone()).map_err(|e| error!("{}", e)).ok();
                                                       }
                                                   }
                                               }
                                               box NetworkMessage::NetworkResponse(NetworkResponse::PeerList(_, ref peers), _, _) => {
                                                   info!("Received PeerList response, attempting to satisfy desired peers");
                                                   let mut new_peers = 0;
                                                   match _node_self_clone.get_nodes() {
                                                       Ok(x) => {
                                                           for peer_node in peers {
                                                               if _node_self_clone.connect(ConnectionType::Node, peer_node.ip(), peer_node.port()).map_err(|e| error!("{}", e)).is_ok() {
                                                                   new_peers += 1;
                                                               }
                                                               if new_peers + x.len() as u8 >= _desired_nodes_clone {
                                                                   break;
                                                               }
                                                           }
                                                       }
                                                       _ => {
                                                           error!("Can't get nodes - so not trying to connect to new peers!");
                                                       }
                                                   }
                                               }
                                               _ => {}
                                           }
                                           }
                                       }
                                   });

    info!("Concordium P2P layer. Network disabled: {}",
          conf.no_network);

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

    if !conf.no_network {
        for connect_to in conf.connect_to {
            match utils::parse_ip_port(&connect_to) {
                Some((ip, port)) => {
                    info!("Connecting to peer {}", &connect_to);
                    node.connect(ConnectionType::Node, ip, port)
                        .map_err(|e| error!("{}", e))
                        .ok();
                }
                None => error!("Can't parse IP to connect to '{}'", &connect_to),
            }
        }
    }

    if !conf.no_network && !conf.no_boostrap_dns {
        info!("Attempting to bootstrap via DNS");
        match bootstrap_nodes {
            Ok(nodes) => {
                for (ip, port) in nodes {
                    info!("Found bootstrap node IP: {} and port: {}", ip, port);
                    node.connect(ConnectionType::Bootstrapper, ip, port)
                        .map_err(|e| error!("{}", e))
                        .ok();
                }
            }
            Err(e) => error!("Couldn't retrieve bootstrap node list! {:?}", e),
        };
    }

    let timer = Timer::new();

    let _desired_nodes_count = conf.desired_nodes;
    let _no_net_clone = conf.no_network;
    let _bootstrappers_conf = conf.bootstrap_server;
    let _guard_timer =
        timer.schedule_repeating(chrono::Duration::seconds(30), move || {
                 match node.get_nodes() {
                     Ok(ref x) => {
                         info!("I currently have {}/{} nodes!",
                               x.len(),
                               _desired_nodes_count);
                         let mut count = 0;
                         for i in x {
                             info!("Peer {}: {}/{}:{}",
                                   count,
                                   i.id().to_string(),
                                   i.ip().to_string(),
                                   i.port());
                             count += 1;
                         }
                         if !_no_net_clone && _desired_nodes_count > x.len() as u8 {
                             if x.len() == 0 {
                                 info!("No nodes at all - retrying bootstrapping");
                                 match utils::get_bootstrap_nodes(_bootstrappers_conf.clone()) {
                                     Ok(nodes) => {
                                         for (ip, port) in nodes {
                                             info!("Found bootstrap node IP: {} and port: {}",
                                                   ip, port);
                                             node.connect(ConnectionType::Bootstrapper, ip, port)
                                                 .map_err(|e| error!("{}", e))
                                                 .ok();
                                         }
                                     }
                                     _ => error!("Can't find any bootstrap nodes - check DNS!"),
                                 }
                             } else {
                                 info!("Not enough nodes, sending GetPeers requests");
                                 node.send_get_peers().map_err(|e| error!("{}", e)).ok();
                             }
                         }
                     }
                     Err(e) => error!("Couldn't get node list, {:?}", e),
                 };
             });

    _node_th.join().unwrap();
    if let Some(ref mut serv) = rpc_serv {
        serv.stop_server()?;
    }

    Ok(())
}
