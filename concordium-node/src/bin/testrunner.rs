#![recursion_limit = "1024"]
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate serde_json;
#[macro_use]
extern crate log;
extern crate gotham;
#[macro_use]
extern crate gotham_derive;
extern crate hyper;
extern crate mime;
// Explicitly defining allocator to avoid future reintroduction of jemalloc
use std::alloc::System;
#[global_allocator]
static A: System = System;

use concordium_common::{
    lock_or_die, safe_lock, spawn_or_die, stats_export_service::StatsExportService,
    RelayOrStopEnvelope, RelayOrStopReceiver,
};
use failure::Fallible;
use gotham::{
    handler::IntoResponse,
    helpers::http::response::create_response,
    middleware::state::StateMiddleware,
    pipeline::{single::single_pipeline, single_middleware},
    router::{builder::*, Router},
    state::{FromState, State},
};
use hyper::{Body, Response, StatusCode};
use p2p_client::{
    common::{self, PeerType},
    configuration,
    network::{NetworkId, NetworkMessage, NetworkPacketType, NetworkRequest, NetworkResponse},
    p2p::*,
    utils::{self, get_config_and_logging_setup, load_bans},
};
use rand::{distributions::Standard, thread_rng, Rng};
use rkv::{Manager, Rkv};
use std::sync::{
    atomic::{AtomicBool, Ordering},
    mpsc, Arc, Mutex, RwLock,
};

#[derive(Clone, StateData)]
struct TestRunnerStateData {
    test_start:       Arc<Mutex<Option<u64>>>,
    test_running:     Arc<AtomicBool>,
    registered_times: Arc<Mutex<Vec<Measurement>>>,
    node:             Arc<Mutex<P2PNode>>,
    nid:              NetworkId,
    packet_size:      Arc<Mutex<Option<usize>>>,
}

impl TestRunnerStateData {
    fn new(node: Arc<Mutex<P2PNode>>, nid: NetworkId) -> Self {
        Self {
            test_start: Arc::new(Mutex::new(None)),
            test_running: Arc::new(AtomicBool::new(false)),
            registered_times: Arc::new(Mutex::new(vec![])),
            node: Arc::clone(&node),
            nid,
            packet_size: Arc::new(Mutex::new(None)),
        }
    }
}

#[derive(Clone)]
struct TestRunner {
    node: Arc<Mutex<P2PNode>>,
    nid:  NetworkId,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct Measurement {
    received_time: u64,
    node_id:       String,
}

impl Measurement {
    pub fn new(received_time: u64, node_id: &str) -> Self {
        Measurement {
            received_time,
            node_id: node_id.to_owned(),
        }
    }
}

#[derive(Deserialize, StateData, StaticResponseExtender)]
struct PathExtractor {
    node_id:          Option<String>,
    packet_id:        Option<String>,
    test_packet_size: Option<usize>,
}

struct HTMLStringResponse(pub String);

impl IntoResponse for HTMLStringResponse {
    fn into_response(self, state: &State) -> Response<Body> {
        create_response(state, StatusCode::OK, mime::TEXT_HTML, self.0)
    }
}

struct JsonStringResponse(pub String);

impl IntoResponse for JsonStringResponse {
    fn into_response(self, state: &State) -> Response<Body> {
        create_response(state, StatusCode::OK, mime::APPLICATION_JSON, self.0)
    }
}

impl TestRunner {
    pub fn new(node: P2PNode, nid: NetworkId) -> Self {
        TestRunner {
            node: Arc::new(Mutex::new(node)),
            nid,
        }
    }

    fn index(state: State) -> (State, HTMLStringResponse) {
        let message = HTMLStringResponse(format!(
            "<html><body><h1>Test runner service for {} v{}</h1>Operational!</p></body></html>",
            p2p_client::APPNAME,
            p2p_client::VERSION
        ));
        (state, message)
    }

    fn register_receipt(state: State) -> (State, HTMLStringResponse) {
        let state_data = TestRunnerStateData::borrow_from(&state);
        let path = PathExtractor::borrow_from(&state);
        let time = common::get_current_stamp();
        let node_id = path.node_id.clone().unwrap();
        let packet_id = path.packet_id.clone().unwrap();
        lock_or_die!(state_data.registered_times).push(Measurement::new(time, &node_id));
        info!("Registered time for {}/{} @ {}", &node_id, &packet_id, time);
        (
            state,
            HTMLStringResponse(format!(
                "REGISTERED packet {} FROM {} ON {}/{} @ {}",
                node_id,
                packet_id,
                p2p_client::APPNAME,
                p2p_client::VERSION,
                time
            )),
        )
    }

    fn start_test(state: State) -> (State, HTMLStringResponse) {
        let state_data = TestRunnerStateData::borrow_from(&state);
        let path = PathExtractor::borrow_from(&state);
        if !state_data.test_running.load(Ordering::Relaxed) {
            state_data.test_running.store(true, Ordering::Relaxed);
            info!("Started test");
            *lock_or_die!(state_data.test_start) = Some(common::get_current_stamp());
            *lock_or_die!(state_data.packet_size) = Some(path.test_packet_size.unwrap());
            let random_pkt: Vec<u8> = thread_rng()
                .sample_iter(&Standard)
                .take(path.test_packet_size.unwrap())
                .collect();
            lock_or_die!(state_data.node)
                .send_broadcast_message(None, state_data.nid, None, random_pkt)
                .map_err(|e| error!("{}", e))
                .ok();
            (
                state,
                HTMLStringResponse(format!(
                    "TEST STARTED ON {}/{} @ {}",
                    p2p_client::APPNAME,
                    p2p_client::VERSION,
                    common::get_current_stamp()
                )),
            )
        } else {
            error!("Couldn't start test as it's already running");
            (
                state,
                HTMLStringResponse("Test already running, can't start one!".to_string()),
            )
        }
    }

    fn reset_test(state: State) -> (State, HTMLStringResponse) {
        let state_data = TestRunnerStateData::borrow_from(&state);
        if state_data.test_running.load(Ordering::Relaxed) {
            *lock_or_die!(state_data.test_start) = None;
            lock_or_die!(state_data.registered_times).clear();
            state_data.test_running.store(false, Ordering::Relaxed);
            *lock_or_die!(state_data.test_start) = None;
            *lock_or_die!(state_data.packet_size) = None;
            info!("Testing reset on runner");
            (
                state,
                HTMLStringResponse(format!(
                    "TEST RESET ON {}/{} @ {}",
                    p2p_client::APPNAME,
                    p2p_client::VERSION,
                    common::get_current_stamp()
                )),
            )
        } else {
            (
                state,
                HTMLStringResponse("Test not running, can't reset now!".to_string()),
            )
        }
    }

    fn get_results(state: State) -> (State, Response<Body>) {
        let state_data = TestRunnerStateData::borrow_from(&state);
        let test_running = state_data.test_running.load(Ordering::Relaxed);
        if test_running {
            let test_start_time = lock_or_die!(state_data.test_start).clone().unwrap();
            let return_json = json!({
                "service_name": "TestRunner",
                "service_version": p2p_client::VERSION,
                "measurements": *lock_or_die!(state_data.registered_times),
                "test_start_time": test_start_time,
                "packet_size": *lock_or_die!(state_data.packet_size) ,
            });
            let resp = JsonStringResponse(return_json.to_string()).into_response(&state);
            (state, resp)
        } else {
            let resp = HTMLStringResponse("Test not running, can't get results now".to_string())
                .into_response(&state);
            (state, resp)
        }
    }

    fn router(&self) -> Router {
        let state_data = TestRunnerStateData::new(self.node.clone(), self.nid);
        let middleware = StateMiddleware::new(state_data);
        let pipeline = single_middleware(middleware);
        let (chain, pipelines) = single_pipeline(pipeline);
        build_router(chain, pipelines, |route| {
            route.get("/").to(Self::index);

            route
                .get("/start_test/:test_packet_size")
                .with_path_extractor::<PathExtractor>()
                .to(Self::start_test);

            route
                .get("/register/:node_id/:packet_id")
                .with_path_extractor::<PathExtractor>()
                .to(Self::register_receipt);

            route.get("/get_results").to(Self::get_results);

            route.get("/reset_test").to(Self::reset_test);
        })
    }

    pub fn start_server(&mut self, listen_ip: &str, port: u16) {
        let addr = format!("{}:{}", listen_ip, port);
        gotham::start(addr, self.router());
    }
}

fn instantiate_node(
    conf: &configuration::Config,
    app_prefs: &mut configuration::AppPreferences,
    stats_export_service: &Option<Arc<RwLock<StatsExportService>>>,
) -> (P2PNode, RelayOrStopReceiver<Arc<NetworkMessage>>) {
    let (pkt_in, pkt_out) = mpsc::channel::<RelayOrStopEnvelope<Arc<NetworkMessage>>>();

    let node_id = if conf.common.id.is_some() {
        conf.common.id.clone()
    } else {
        app_prefs.get_config(configuration::APP_PREFERENCES_PERSISTED_NODE_ID)
    };

    let arc_stats_export_service = if let Some(ref service) = stats_export_service {
        Some(Arc::clone(service))
    } else {
        None
    };

    let node_sender = if conf.common.debug {
        let (sender, receiver) = mpsc::channel();
        let _guard = spawn_or_die!("Log loop", move || loop {
            if let Ok(msg) = receiver.recv() {
                info!("{}", msg);
            }
        });
        Some(sender)
    } else {
        None
    };

    let node = P2PNode::new(
        node_id,
        &conf,
        pkt_in,
        node_sender,
        PeerType::Node,
        arc_stats_export_service,
    );

    (node, pkt_out)
}

fn setup_process_output(
    node: &P2PNode,
    conf: &configuration::Config,
    pkt_out: RelayOrStopReceiver<Arc<NetworkMessage>>,
    kvs_handle: Arc<RwLock<Rkv>>,
) {
    let mut _node_self_clone = node.clone();

    let _no_trust_bans = conf.common.no_trust_bans;
    let _no_trust_broadcasts = conf.connection.no_trust_broadcasts;
    let _desired_nodes_clone = conf.connection.desired_nodes;
    let _guard_pkt = spawn_or_die!("Node output processing", move || loop {
        while let Ok(RelayOrStopEnvelope::Relay(full_msg)) = pkt_out.recv() {
            match *full_msg {
                NetworkMessage::NetworkPacket(ref pac, ..) => match pac.packet_type {
                    NetworkPacketType::DirectMessage(..) => {
                        info!(
                            "DirectMessage/{}/{:?} with size {} received",
                            pac.network_id,
                            pac.message_id,
                            pac.message.len()
                        );
                    }
                    NetworkPacketType::BroadcastedMessage => {
                        if !_no_trust_broadcasts {
                            info!(
                                "BroadcastedMessage/{}/{:?} with size {} received",
                                pac.network_id,
                                pac.message_id,
                                pac.message.len()
                            );
                            _node_self_clone
                                .send_message_from_cursor(
                                    None,
                                    pac.network_id,
                                    Some(pac.message_id.to_owned()),
                                    pac.message.to_owned(),
                                    true,
                                )
                                .map_err(|e| error!("Error sending message {}", e))
                                .ok();
                        }
                    }
                },
                NetworkMessage::NetworkRequest(NetworkRequest::BanNode(ref peer, x), ..) => {
                    utils::ban_node(&mut _node_self_clone, peer, x, &kvs_handle, _no_trust_bans);
                }
                NetworkMessage::NetworkRequest(NetworkRequest::UnbanNode(ref peer, x), ..) => {
                    utils::unban_node(&mut _node_self_clone, peer, x, &kvs_handle, _no_trust_bans);
                }
                NetworkMessage::NetworkResponse(NetworkResponse::PeerList(_, ref peers), ..) => {
                    info!("Received PeerList response, attempting to satisfy desired peers");
                    let mut new_peers = 0;
                    let peer_count = _node_self_clone
                        .get_peer_stats(&[])
                        .iter()
                        .filter(|x| x.peer_type == PeerType::Node)
                        .count();
                    for peer_node in peers {
                        if _node_self_clone
                            .connect(PeerType::Node, peer_node.addr, Some(peer_node.id()))
                            .map_err(|e| error!("{}", e))
                            .is_ok()
                        {
                            new_peers += 1;
                        }
                        if new_peers + peer_count as u8 >= _desired_nodes_clone {
                            break;
                        }
                    }
                }
                _ => {}
            }
        }
    });
}

fn main() -> Fallible<()> {
    let (conf, mut app_prefs) = get_config_and_logging_setup()?;

    if conf.common.print_config {
        // Print out the configuration
        info!("{:?}", conf);
    }

    let data_dir_path = app_prefs.get_user_app_dir();

    info!("Debugging enabled: {}", conf.common.debug);

    let dns_resolvers =
        utils::get_resolvers(&conf.connection.resolv_conf, &conf.connection.dns_resolver);

    for resolver in &dns_resolvers {
        debug!("Using resolver: {}", resolver);
    }

    let bootstrap_nodes = utils::get_bootstrap_nodes(
        conf.connection.bootstrap_server.clone(),
        &dns_resolvers,
        conf.connection.dnssec_disabled,
        &conf.connection.bootstrap_node,
    );

    let (mut node, pkt_out) = instantiate_node(&conf, &mut app_prefs, &None);

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

    node.spawn();

    if !app_prefs.set_config(
        configuration::APP_PREFERENCES_PERSISTED_NODE_ID,
        Some(node.id().to_string()),
    ) {
        error!("Failed to persist own node id");
    }

    setup_process_output(&node, &conf, pkt_out, kvs_handle);

    for connect_to in conf.connection.connect_to {
        match utils::parse_host_port(&connect_to, &dns_resolvers, conf.connection.dnssec_disabled) {
            Ok(addrs) => {
                for addr in addrs {
                    info!("Connecting to peer {}", addr);
                    node.connect(PeerType::Node, addr, None)
                        .map_err(|e| error!("{}", e))
                        .ok();
                }
            }
            Err(err) => error!("{}", err),
        }
    }

    if !conf.connection.no_bootstrap_dns {
        info!("Attempting to bootstrap");
        match bootstrap_nodes {
            Ok(nodes) => {
                for addr in nodes {
                    info!("Found bootstrap node: {}", addr);
                    node.connect(PeerType::Bootstrapper, addr, None)
                        .map_err(|e| error!("{}", e))
                        .ok();
                }
            }
            Err(e) => error!("Couldn't retrieve bootstrap node list! {:?}", e),
        };
    }

    let mut testrunner = TestRunner::new(node.clone(), NetworkId::from(conf.common.network_ids[0]));

    testrunner.start_server(
        &conf.testrunner.listen_http_address,
        conf.testrunner.listen_http_port,
    );

    Ok(())
}
