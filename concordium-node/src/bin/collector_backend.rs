#![recursion_limit = "1024"]
#[macro_use]
extern crate gotham_derive;
use env_logger::Env;
use failure::Fallible;
use p2p_client::{
    common::{collector_utils::*, get_current_stamp},
    utils::setup_logger_env,
};
use structopt::StructOpt;
use twox_hash::XxHash64;
#[macro_use]
extern crate log;
use concordium_common::{read_or_die, spawn_or_die, write_or_die};
use futures::prelude::*;
use gotham::{
    handler::{HandlerFuture, IntoHandlerError, IntoResponse},
    helpers::http::response::{create_empty_response, create_response},
    middleware::state::StateMiddleware,
    pipeline::{single::single_pipeline, single_middleware},
    router::{builder::*, Router},
    state::{FromState, State},
};
use hyper::{body, Body, Response, StatusCode};
use rmp_serde;
use std::{
    collections::HashMap,
    hash::BuildHasherDefault,
    io::Cursor,
    pin::Pin,
    sync::{Arc, RwLock},
    thread,
    time::Duration,
};

// Force the system allocator on every platform
use std::alloc::System;
#[global_allocator]
static A: System = System;

#[derive(StructOpt, Debug)]
#[structopt(name = "Node Collector Backend")]
struct ConfigCli {
    #[structopt(long = "listen-address", help = "IP to listen on", default_value = "0.0.0.0")]
    pub host: String,
    #[structopt(long = "listen-port", help = "Port to listen on", default_value = "8080")]
    pub port: u16,
    #[structopt(
        long = "stale-time-allowed",
        help = "Time in ms nodes are allowed to not have reported updates in before being removed",
        default_value = "3600000"
    )]
    pub stale_time_allowed: u64,
    #[structopt(
        long = "cleanup-interval",
        help = "Time in ms to sleep between cleanups",
        default_value = "300000"
    )]
    pub cleanup_interval: u64,
    #[structopt(long = "print-config", help = "Print out config struct")]
    pub print_config: bool,
    #[structopt(long = "debug", short = "d", help = "Debug mode")]
    pub debug: bool,
    #[structopt(long = "trace", help = "Trace mode")]
    pub trace: bool,
    #[structopt(long = "info", help = "Info mode")]
    pub info: bool,
    #[structopt(long = "no-log-timestamp", help = "Do not output timestamp in log output")]
    pub no_log_timestamp: bool,
}

pub struct HTMLStringResponse(pub String);

impl IntoResponse for HTMLStringResponse {
    fn into_response(self, state: &State) -> Response<Body> {
        create_response(state, StatusCode::OK, mime::TEXT_HTML, self.0)
    }
}

pub struct JSONStringResponse(pub String);

impl IntoResponse for JSONStringResponse {
    fn into_response(self, state: &State) -> Response<Body> {
        let mut res = create_response(state, StatusCode::OK, mime::APPLICATION_JSON, self.0);
        res.headers_mut().insert("Access-Control-Allow-Origin", "*".parse().unwrap());
        res
    }
}

#[derive(Clone, StateData)]
struct CollectorStateData {
    pub nodes: Arc<RwLock<HashMap<String, NodeInfo, BuildHasherDefault<XxHash64>>>>,
}

impl CollectorStateData {
    fn new(nodes: Arc<RwLock<HashMap<String, NodeInfo, BuildHasherDefault<XxHash64>>>>) -> Self {
        Self {
            nodes,
        }
    }
}

pub fn main() -> Fallible<()> {
    let conf = ConfigCli::from_args();

    // Prepare the logger
    let env = if conf.trace {
        Env::default().filter_or("LOG_LEVEL", "trace")
    } else if conf.debug {
        Env::default().filter_or("LOG_LEVEL", "debug")
    } else if conf.info {
        Env::default().filter_or("LOG_LEVEL", "info")
    } else {
        Env::default().filter_or("LOG_LEVEL", "warn")
    };

    setup_logger_env(env, conf.no_log_timestamp);

    if conf.print_config {
        info!("{:?}", conf);
    }

    info!(
        "Starting up {}-node-collector-backend version {}!",
        p2p_client::APPNAME,
        p2p_client::VERSION
    );

    let node_info_map: Arc<RwLock<HashMap<String, NodeInfo, BuildHasherDefault<XxHash64>>>> =
        Arc::new(RwLock::new(HashMap::with_capacity_and_hasher(1500, Default::default())));

    let _allowed_stale_time = conf.stale_time_allowed;
    let _node_info_map_clone = Arc::clone(&node_info_map);
    let _cleanup_interval = conf.cleanup_interval;
    #[allow(unreachable_code)] // the loop never breaks on its own
    let _ = spawn_or_die!("collector backend cleanup", {
        loop {
            thread::sleep(Duration::from_millis(_cleanup_interval));
            info!("Running cleanup");
            let current_stamp = get_current_stamp();
            write_or_die!(_node_info_map_clone)
                .retain(|_, element| current_stamp < element.last_updated + _allowed_stale_time);
        }
    });

    let addr = format!("{}:{}", conf.host, conf.port);
    info!("Listening for requests at http://{}", addr);

    gotham::start(addr, router(node_info_map));
    Ok(())
}

fn index(state: State) -> (State, HTMLStringResponse) {
    trace!("Processing an index request");
    let message = HTMLStringResponse(format!(
        "<html><body><h1>Collector backend for {} v{}</h1>Operational!</p></body></html>",
        p2p_client::APPNAME,
        p2p_client::VERSION
    ));
    (state, message)
}

fn nodes_summary(state: State) -> (State, JSONStringResponse) {
    trace!("Processing an nodes summary request");
    let state_data = CollectorStateData::borrow_from(&state);
    let mut response = Vec::new();
    {
        let map_lock = &*read_or_die!(state_data.nodes);
        response.extend(b"[");
        for (i, node_info) in map_lock.values().enumerate() {
            if i != 0 {
                response.extend(b",");
            }
            serde_json::to_writer(&mut response, &NodeInfoDashboard::from(node_info)).unwrap()
        }
        response.extend(b"]");
    }
    (state, JSONStringResponse(String::from_utf8(response).unwrap()))
}

fn nodes_block_info(state: State) -> (State, JSONStringResponse) {
    trace!("Processing a nodes block info request");
    let state_data = CollectorStateData::borrow_from(&state);
    let mut response = Vec::new();
    {
        let map_lock = &*read_or_die!(state_data.nodes);
        response.extend(b"[");
        for (i, node_info) in map_lock.values().enumerate() {
            if i != 0 {
                response.extend(b",");
            }
            serde_json::to_writer(&mut response, &NodeInfoChainViz::from(node_info)).unwrap()
        }
        response.extend(b"]");
    }
    (state, JSONStringResponse(String::from_utf8(response).unwrap()))
}

fn nodes_staging_users_info(state: State) -> (State, JSONStringResponse) {
    trace!("Processing a nodes staging net users info request");
    let state_data = CollectorStateData::borrow_from(&state);
    let mut response = Vec::new();
    {
        let map_lock = &*read_or_die!(state_data.nodes);
        response.extend(b"[");
        for (i, node_info) in map_lock.values().enumerate() {
            if i != 0 {
                response.extend(b",");
            }
            serde_json::to_writer(&mut response, &NodeInfoStagingNetUsers::from(node_info)).unwrap()
        }
        response.extend(b"]");
    }
    (state, JSONStringResponse(String::from_utf8(response).unwrap()))
}

fn nodes_post_handler(mut state: State) -> Pin<Box<HandlerFuture>> {
    trace!("Processing a post from a node-collector");
    body::to_bytes(Body::take_from(&mut state))
        .then(|full_body| match full_body {
            Ok(body_content) => {
                let decoded: Result<NodeInfo, _> =
                    rmp_serde::decode::from_read(Cursor::new(&body_content));
                match decoded {
                    Ok(mut nodes_info) => {
                        if !nodes_info.nodeName.is_empty() && !nodes_info.nodeId.is_empty() {
                            let state_data = CollectorStateData::borrow_from(&state);
                            nodes_info.last_updated = get_current_stamp();
                            write_or_die!(state_data.nodes)
                                .insert(nodes_info.nodeId.clone(), nodes_info);
                        } else {
                            error!("Client submitted data without nodeName and nodeId");
                        }
                        let res = create_empty_response(&state, StatusCode::OK);
                        future::ok((state, res))
                    }
                    Err(e) => {
                        error!("Can't parse client data: {}", e);
                        future::err((state, e.into_handler_error()))
                    }
                }
            }
            Err(e) => future::err((state, e.into_handler_error())),
        })
        .boxed()
}

pub fn router(
    node_info_map: Arc<RwLock<HashMap<String, NodeInfo, BuildHasherDefault<XxHash64>>>>,
) -> Router {
    let state_data = CollectorStateData::new(node_info_map);
    let middleware = StateMiddleware::new(state_data);
    let pipeline = single_middleware(middleware);
    let (chain, pipelines) = single_pipeline(pipeline);
    build_router(chain, pipelines, |route| {
        route.get("/").to(index);
        route.get("/nodesSummary").to(nodes_summary);
        route.get("/data/nodesSummary").to(nodes_summary);
        route.get("/nodesBlocksInfo").to(nodes_block_info);
        route.get("/data/nodesBlocksInfo").to(nodes_block_info);
        route.get("/nodesStagingNetUsers").to(nodes_staging_users_info);
        route.get("/data/nodesStagingNetUsers").to(nodes_staging_users_info);
        route.post("/nodes/post").to(nodes_post_handler);
        route.post("/post/nodes").to(nodes_post_handler);
    })
}
