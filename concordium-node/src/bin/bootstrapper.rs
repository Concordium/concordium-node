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
use p2p_client::p2p::*;
use std::sync::mpsc;
use std::sync::{Arc,Mutex};
use std::thread;
use timer::Timer;
use p2p_client::errors::*;
use p2p_client::prometheus_exporter::PrometheusServer;

quick_main!( run );

fn run() -> ResultExtWrapper<()>{
    let conf = configuration::parse_bootstrapper_config();
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

    let mut db_path = app_prefs.get_user_app_dir().clone();
    db_path.push("p2p.db");

    let db = P2PDB::new(db_path.as_path());

    let prometheus = if conf.prometheus {
        let mut srv = PrometheusServer::new();
        srv.start_server(&conf.prometheus_listen_addr, conf.prometheus_listen_port).map_err(|e| error!("{}", e)).ok();
        Some(Arc::new(Mutex::new(srv)))
    } else {
        None
    };

    info!("Debugging enabled {}", conf.debug);

    let (pkt_in, pkt_out) = mpsc::channel::<Arc<Box<NetworkMessage>>>();

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
        P2PNode::new(Some(conf.id), conf.listen_address, conf.listen_port, conf.external_ip, conf.external_port, pkt_in, Some(sender),P2PNodeMode::BootstrapperMode, prometheus)
    } else {
        P2PNode::new(Some(conf.id), conf.listen_address, conf.listen_port, conf.external_ip, conf.external_port, pkt_in, None, P2PNodeMode::BootstrapperMode, prometheus)
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

     let _guard_pkt = thread::spawn(move || loop {
        if let Ok(full_msg) = pkt_out.recv() {
            match *full_msg.clone() {
                box NetworkMessage::NetworkRequest(NetworkRequest::BanNode(ref peer, ref x),
                                                   _,
                                                   _) => {
                    info!("Ban node request for {:?}", x);
                    let ban = _node_self_clone.ban_node(x.clone()).map_err(|e| error!("{}", e));
                    if ban.is_ok() {
                        db.insert_ban(peer.id().to_string(), format!("{}", peer.ip()), peer.port());
                        if !_no_trust_bans {
                            _node_self_clone.send_ban(x.clone()).map_err(|e| error!("{}", e)).ok();
                        }
                    }
                }
                box NetworkMessage::NetworkRequest(NetworkRequest::UnbanNode(ref peer, ref x),
                                                   _,
                                                   _) => {
                    info!("Unban node requets for {:?}", x);
                    let req = _node_self_clone.unban_node(x.clone()).map_err(|e| error!("{}", e));
                    if req.is_ok() {
                        db.delete_ban(peer.id().to_string(), format!("{}", peer.ip()), peer.port());
                        if !_no_trust_bans {
                            _node_self_clone.send_unban(x.clone()).map_err(|e| error!("{}", e)).ok();
                        }
                    }
                }
                _ => {}
            }
        }
    });

    let _node_th = node.spawn();

    let timer = Timer::new();

    let _max_nodes = conf.max_nodes;

    let _guard_timer = timer.schedule_repeating(chrono::Duration::seconds(30), move || {
                                match node.get_nodes() {
                                    Ok(x) => {
                                        info!("I currently have {}/{} nodes!",
                                              x.len(),
                                              _max_nodes);
                                    }
                                    Err(e) => error!("Couldn't get node list, {:?}", e),
                                };
                            });

    _node_th.join().unwrap();

    Ok(())
}