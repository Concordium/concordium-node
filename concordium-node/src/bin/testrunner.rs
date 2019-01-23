#![feature(box_syntax, box_patterns)]
#![recursion_limit = "1024"]
extern crate iron;
extern crate p2p_client;
extern crate router;
#[macro_use]
extern crate error_chain;
extern crate chrono;
#[macro_use]
extern crate log;
extern crate env_logger;
#[macro_use]
extern crate serde_derive;
extern crate serde;
#[macro_use]
extern crate serde_json;
extern crate rand;
extern crate timer;

use std::alloc::System;
#[global_allocator]
static A: System = System;

use env_logger::{Builder, Env};
use iron::headers::ContentType;
use iron::prelude::*;
use iron::status;
use p2p_client::common;
use p2p_client::common::{ ConnectionType };
use p2p_client::network::{ NetworkMessage, NetworkPacket, NetworkRequest, NetworkResponse };
use p2p_client::configuration;
use p2p_client::db::P2PDB;
use p2p_client::errors::*;
use p2p_client::p2p::*;
use p2p_client::connection::{ P2PEvent, P2PNodeMode };
use p2p_client::utils;
use rand::distributions::Standard;
use rand::{thread_rng, Rng};
use router::Router;
use std::sync::mpsc;
use std::sync::{Arc, Mutex};
use std::thread;
use timer::Timer;

quick_main!(run);

#[derive(Clone)]
struct TestRunner {
    test_start: Arc<Mutex<Option<u64>>>,
    test_running: Arc<Mutex<bool>>,
    registered_times: Arc<Mutex<Vec<Measurement>>>,
    node: Arc<Mutex<P2PNode>>,
    nid: u16,
    packet_size: Arc<Mutex<Option<usize>>>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct Measurement {
    received_time: u64,
    node_id: String,
}

impl Measurement {
    pub fn new(received_time: u64, node_id: String) -> Self {
        Measurement { received_time: received_time,
                      node_id: node_id, }
    }
}

const DEFAULT_TEST_PACKET_SIZE: usize = 51_200;

impl TestRunner {
    pub fn new(node: P2PNode, nid: u16) -> Self {
        TestRunner { test_start: Arc::new(Mutex::new(None)),
                     test_running: Arc::new(Mutex::new(false)),
                     registered_times: Arc::new(Mutex::new(vec![])),
                     node: Arc::new(Mutex::new(node)),
                     nid: nid,
                     packet_size: Arc::new(Mutex::new(None)), }
    }

    fn index(&self) -> IronResult<Response> {
        let mut resp = Response::with((status::Ok, format!("<html><body><h1>Test runner service for {} v{}</h1>Operational!</p></body></html>", p2p_client::APPNAME, p2p_client::VERSION)));
        resp.headers.set(ContentType::html());
        Ok(resp)
    }

    fn register_receipt(&self, req: &mut Request) -> IronResult<Response> {
        match req.extensions.get::<Router>().unwrap().find("node_id") {
            Some(node_id) => {
                match req.extensions.get::<Router>().unwrap().find("packet_id") {
                    Some(pktid) => {
                        let time = common::get_current_stamp();
                        if let Ok(mut list) = self.registered_times.lock() {
                            list.push(Measurement::new(time, node_id.to_string()));
                            info!("Registered time for {}/{} @ {}", node_id, pktid, time);
                            Ok(Response::with((status::Ok, format!("REGISTERED packet {} FROM {} ON {}/{} @ {}", pktid, node_id, p2p_client::APPNAME, p2p_client::VERSION, time))))
                        } else {
                            error!("Couldn't register due to locking issues");
                            Ok(Response::with((status::InternalServerError,
                                               "Can't retrieve access to inner lock".to_string())))
                        }
                    }
                    _ => {
                        error!("Couldn't register due to missing params");
                        Ok(Response::with((status::NotFound,
                                           "Missing packet id in url".to_string())))
                    }
                }
            }
            _ => {
                error!("Couldn't register due to missing params");
                Ok(Response::with((status::NotFound, "Missing node id in url".to_string())))
            }
        }
    }

    fn start_test(&self, packet_size: usize) -> IronResult<Response> {
        match self.test_running.lock() {
            Ok(mut value) => {
                if !*value {
                    *value = true;
                    info!("Started test");
                    *self.test_start.lock().unwrap() = Some(common::get_current_stamp());
                    *self.packet_size.lock().unwrap() = Some(packet_size);
                    let random_pkt: Vec<u8> = thread_rng().sample_iter(&Standard)
                                                          .take(packet_size)
                                                          .collect();
                    self.node
                        .lock()
                        .unwrap()
                        .send_message(None, self.nid, None, &random_pkt, true)
                        .map_err(|e| error!("{}", e))
                        .ok();
                    Ok(Response::with((status::Ok,
                                       format!("TEST STARTED ON {}/{} @ {}",
                                               p2p_client::APPNAME,
                                               p2p_client::VERSION,
                                               common::get_current_stamp()))))
                } else {
                    error!("Couldn't start test as it's already running");
                    Ok(Response::with((status::Ok,
                                       "Test already running, can't start one!".to_string())))
                }
            }
            _ => {
                error!("Couldn't register due to locking issues");
                Ok(Response::with((status::InternalServerError,
                                   "Can't retrieve access to inner lock".to_string())))
            }
        }
    }

    fn reset_test(&self) -> IronResult<Response> {
        match self.test_running.lock() {
            Ok(mut value) => {
                if *value {
                    match self.test_start.lock() {
                        Ok(mut inner_value) => *inner_value = None,
                        _ => return Ok(Response::with((status::InternalServerError, "Can't retrieve access to inner lock".to_string()))),
                    }
                    match self.registered_times.lock() {
                        Ok(mut inner_value) => inner_value.clear(),
                        _ => return Ok(Response::with((status::InternalServerError, "Can't retrieve access to inner lock".to_string()))),
                    }
                    *value = false;
                    *self.test_start.lock().unwrap() = None;
                    *self.packet_size.lock().unwrap() = None;
                    info!("Testing reset on runner");
                    Ok(Response::with((status::Ok,
                                       format!("TEST RESET ON {}/{} @ {}",
                                               p2p_client::APPNAME,
                                               p2p_client::VERSION,
                                               common::get_current_stamp()))))
                } else {
                    error!("Test not running so can't reset right now");
                    Ok(Response::with((status::Ok,
                                       "Test not running, can't reset now!".to_string())))
                }
            }
            _ => {
                error!("Couldn't register due to locking issues");
                Ok(Response::with((status::InternalServerError,
                                   "Can't retrieve access to inner lock".to_string())))
            }
        }
    }

    fn get_results(&self) -> IronResult<Response> {
        match self.test_running.lock() {
            Ok(value) => {
                if *value {
                    match self.test_start.lock() {
                        Ok(test_start_time) => {
                            match self.registered_times.lock() {
                                Ok(inner_vals) => {
                                    let return_json = json!({
                                        "service_name": "TestRunner",
                                        "service_version": p2p_client::VERSION,
                                        "measurements": *inner_vals.clone(),
                                        "test_start_time": *test_start_time,
                                        "packet_size": *self.packet_size.lock().unwrap(),
                                    });
                                    let mut resp =
                                        Response::with((status::Ok, return_json.to_string()));
                                    resp.headers.set(ContentType::json());
                                    Ok(resp)
                                }
                                _ => {
                                    error!("Couldn't send results due to locking issues");
                                    Ok(Response::with((status::InternalServerError, "Can't retrieve access to inner lock".to_string())))
                                }
                            }
                        }
                        _ => {
                            error!("Couldn't send results due to locking issues");
                            Ok(Response::with((status::InternalServerError,
                                               "Can't retrieve access to inner lock".to_string())))
                        }
                    }
                } else {
                    Ok(Response::with((status::Ok,
                                       "Test not running, can't get results now!".to_string())))
                }
            }
            _ => {
                error!("Couldn't send results due to locking issues");
                Ok(Response::with((status::InternalServerError,
                                   "Can't retrieve access to inner lock".to_string())))
            }
        }
    }

    pub fn start_server(&mut self, listen_ip: &String, port: u16) -> thread::JoinHandle<()> {
        let mut router = Router::new();
        let _self_clone = Arc::new(self.clone());
        let _self_clone_2 = _self_clone.clone();
        let _self_clone_3 = _self_clone.clone();
        let _self_clone_4 = _self_clone.clone();
        let _self_clone_5 = _self_clone.clone();
        let _self_clone_6 = _self_clone.clone();
        router.get("/",
                   move |_: &mut Request| _self_clone.clone().index(),
                   "index");
        router.get("/register/:node_id/:packet_id",
                   move |req: &mut Request| _self_clone_2.clone().register_receipt(req),
                   "register");
        router.get("/start_test/:test_packet_size",
                   move |req: &mut Request| {
                       match req.extensions.get::<Router>().unwrap().find("test_packet_size") {
                            Some(size_str) => {
                                    match size_str.parse::<usize>() {
                                        Ok(size) => {
                                            _self_clone_3.clone().start_test(size)
                                        }
                                        _ => Ok(Response::with((status::BadRequest,
                                               "Invalid size for test packet given".to_string())))
                                    }
                                },
                            _ => Ok(Response::with((status::BadRequest,
                                               "Missing test packet size".to_string())))
                       }
                   },
                   "start_test_specific");
        router.get("/start_test",
                   move |_: &mut Request| {
                       _self_clone_4.clone().start_test(DEFAULT_TEST_PACKET_SIZE)
                   },
                   "start_test_generic");
        router.get("/reset_test",
                   move |_: &mut Request| _self_clone_5.clone().reset_test(),
                   "reset_test");
        router.get("/get_results",
                   move |_: &mut Request| _self_clone_6.clone().get_results(),
                   "get_results");
        let _listen = listen_ip.clone();
        thread::spawn(move || {
                          Iron::new(router).http(format!("{}:{}", _listen, port))
                                           .unwrap();
                      })
    }
}

fn run() -> ResultExtWrapper<()> {
    let conf = configuration::parse_testrunner_config();
    let mut app_prefs =
        configuration::AppPreferences::new(conf.config_dir.clone(), conf.data_dir.clone());

    info!("Starting up {}-TestRunner version {}!",
          p2p_client::APPNAME,
          p2p_client::VERSION);
    info!("Application data directory: {:?}",
          app_prefs.get_user_app_dir());
    info!("Application config directory: {:?}",
          app_prefs.get_user_config_dir());

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

    let mut db_path = app_prefs.get_user_app_dir().clone();
    db_path.push("p2p.db");

    let db = P2PDB::new(db_path.as_path());

    info!("Debugging enabled {}", conf.debug);

    let dns_resolvers = utils::get_resolvers(&conf.resolv_conf, &conf.dns_resolver);

    for resolver in &dns_resolvers {
        debug!("Using resolver: {}", resolver);
    }

    let bootstrap_nodes = utils::get_bootstrap_nodes(conf.bootstrap_server.clone(),
                                                     &dns_resolvers,
                                                     conf.no_dnssec,
                                                     conf.bootstrap_node.clone());

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
                     None,
                     conf.network_ids.clone(),
                     conf.min_peers_bucket)
    } else {
        P2PNode::new(node_id,
                     conf.listen_address,
                     conf.listen_port,
                     external_ip,
                     conf.external_port,
                     pkt_in,
                     None,
                     mode_type,
                     None,
                     conf.network_ids.clone(),
                     conf.min_peers_bucket)
    };

    let _node_th = node.spawn();

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

    let mut _node_self_clone = node.clone();

    let _no_trust_bans = conf.no_trust_bans;
    let _no_trust_broadcasts = conf.no_trust_broadcasts;
    let _desired_nodes_clone = conf.desired_nodes;
    let _guard_pkt = thread::spawn(move || {
                                       loop {
                                           if let Ok(full_msg) = pkt_out.recv() {
                                               match *full_msg.clone() {
                                               box NetworkMessage::NetworkPacket(NetworkPacket::DirectMessage(_, ref msgid, _, ref nid, ref msg), _, _) => {
                                                   info!("DirectMessage/{}/{} with size {} received", nid, msgid, msg.len());
                                               }
                                               box NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(_, ref msgid, ref nid, ref msg), _, _) => {
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
                                                   match _node_self_clone.get_nodes(&vec![]) {
                                                       Ok(x) => {
                                                           for peer_node in peers {
                                                               if _node_self_clone.connect(ConnectionType::Node, peer_node.ip(), peer_node.port(), Some(peer_node.id())).map_err(|e| error!("{}", e)).is_ok() {
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

    for connect_to in conf.connect_to {
        match utils::parse_host_port(&connect_to, &dns_resolvers, conf.no_dnssec) {
            Some((ip, port)) => {
                info!("Connecting to peer {}", &connect_to);
                node.connect(ConnectionType::Node, ip, port, None)
                    .map_err(|e| error!("{}", e))
                    .ok();
            }
            None => error!("Can't parse IP to connect to '{}'", &connect_to),
        }
    }

    if !conf.no_boostrap_dns {
        info!("Attempting to bootstrap");
        match bootstrap_nodes {
            Ok(nodes) => {
                for (ip, port) in nodes {
                    info!("Found bootstrap node IP: {} and port: {}", ip, port);
                    node.connect(ConnectionType::Bootstrapper, ip, port, None)
                        .map_err(|e| error!("{}", e))
                        .ok();
                }
            }
            Err(e) => error!("Couldn't retrieve bootstrap node list! {:?}", e),
        };
    }

    let mut testrunner = TestRunner::new(node.clone(), *conf.network_ids.first().unwrap());

    let timer = Timer::new();

    let _desired_nodes_count = conf.desired_nodes;
    let _bootstrappers_conf = conf.bootstrap_server;
    let _dnssec = conf.no_dnssec;
    let _dns_resolvers = dns_resolvers.clone();
    let _bootstrap_node = conf.bootstrap_node.clone();
    let _nids = conf.network_ids.clone();
    let _node_clone = node.clone();
    let _guard_timer =
        timer.schedule_repeating(chrono::Duration::seconds(30), move || {
                 match node.get_nodes(&vec![]) {
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
                         if _desired_nodes_count > x.len() as u8 {
                             if x.len() == 0 {
                                 info!("No nodes at all - retrying bootstrapping");
                                 match utils::get_bootstrap_nodes(_bootstrappers_conf.clone(),
                                                                  &_dns_resolvers,
                                                                  _dnssec,
                                                                  _bootstrap_node.clone())
                                 {
                                     Ok(nodes) => {
                                         for (ip, port) in nodes {
                                             info!("Found bootstrap node IP: {} and port: {}",
                                                   ip, port);
                                             node.connect(ConnectionType::Bootstrapper,
                                                          ip,
                                                          port,
                                                          None)
                                                 .map_err(|e| error!("{}", e))
                                                 .ok();
                                         }
                                     }
                                     _ => error!("Can't find any bootstrap nodes - check DNS!"),
                                 }
                             } else {
                                 info!("Not enough nodes, sending GetPeers requests");
                                 node.send_get_peers(_nids.clone())
                                     .map_err(|e| error!("{}", e))
                                     .ok();
                             }
                         }
                     }
                     Err(e) => error!("Couldn't get node list, {:?}", e),
                 };
             });

    let _th = testrunner.start_server(&conf.listen_http_address, conf.listen_http_port);

    _th.join().map_err(|e| error!("{:?}", e)).ok();

    Ok(())
}
