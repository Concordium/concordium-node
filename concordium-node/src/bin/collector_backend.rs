#![recursion_limit = "1024"]
#[cfg(not(target_os = "windows"))]
extern crate grpciounix as grpcio;
#[cfg(target_os = "windows")]
extern crate grpciowin as grpcio;
#[macro_use]
extern crate gotham_derive;
use env_logger::{Builder, Env};
use failure::Fallible;
use p2p_client::common::{collector_utils::NodeInfo, get_current_stamp};
use std::hash::BuildHasherDefault;
use structopt::StructOpt;
use twox_hash::XxHash64;
#[macro_use]
extern crate log;
use concordium_common::{read_or_die, safe_read, safe_write, spawn_or_die, write_or_die};
use futures::{future, Future, Stream};
use gotham::{
    handler::{HandlerFuture, IntoHandlerError, IntoResponse},
    helpers::http::response::{create_empty_response, create_response},
    middleware::state::StateMiddleware,
    pipeline::{single::single_pipeline, single_middleware},
    router::{builder::*, Router},
    state::{FromState, State},
};
use hyper::{Body, Response, StatusCode};
use serde_json::error::Error;
use std::{
    collections::HashMap,
    str,
    sync::{Arc, RwLock},
    thread,
    time::Duration,
};

// Explicitly defining allocator to avoid future reintroduction of jemalloc
use std::alloc::System;
#[global_allocator]
static A: System = System;

#[derive(StructOpt, Debug)]
#[structopt(name = "Node Collector Backend")]
struct ConfigCli {
    #[structopt(
        long = "listen-address",
        help = "IP to listen on",
        default_value = "0.0.0.0"
    )]
    pub host: String,
    #[structopt(
        long = "listen-port",
        help = "Port to listen on",
        default_value = "8080"
    )]
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
    #[structopt(
        long = "no-log-timestamp",
        help = "Do not output timestamp in log output"
    )]
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
        res.headers_mut()
            .insert("Access-Control-Allow-Origin", "*".parse().unwrap());
        res
    }
}

#[derive(Clone, StateData)]
struct CollectorStateData {
    pub nodes: Arc<RwLock<HashMap<String, NodeInfo, BuildHasherDefault<XxHash64>>>>,
}

impl CollectorStateData {
    fn new(nodes: Arc<RwLock<HashMap<String, NodeInfo, BuildHasherDefault<XxHash64>>>>) -> Self {
        Self { nodes }
    }
}

pub fn main() -> Fallible<()> {
    let conf = ConfigCli::from_args();

    // Prepare the logger
    let env = if conf.trace {
        Env::default().filter_or("LOG_LEVEL", "trace")
    } else if conf.debug {
        Env::default().filter_or("LOG_LEVEL", "debug")
    } else {
        Env::default().filter_or("LOG_LEVEL", "info")
    };

    let mut log_builder = Builder::from_env(env);
    if conf.no_log_timestamp {
        log_builder.default_format_timestamp(false);
    }
    log_builder.init();
    if conf.print_config {
        info!("{:?}", conf);
    }

    let node_info_map: Arc<RwLock<HashMap<String, NodeInfo, BuildHasherDefault<XxHash64>>>> =
        Arc::new(RwLock::new(HashMap::with_capacity_and_hasher(
            1500,
            Default::default(),
        )));

    let _allowed_stale_time = conf.stale_time_allowed;
    let _node_info_map_clone = Arc::clone(&node_info_map);
    let _cleanup_interval = conf.cleanup_interval;
    #[allow(unreachable_code)] // the loop never breaks on its own
    let _ = spawn_or_die!("Cleanup thread", {
        loop {
            thread::sleep(Duration::from_millis(_cleanup_interval));
            info!("Running cleanup");
            let current_stamp = get_current_stamp();
            write_or_die!(_node_info_map_clone)
                .retain(|_, element| current_stamp < element.last_updated + _allowed_stale_time);
        }
    });

    let addr = format!("{}:{}", conf.host, conf.port).to_string();
    println!("Listening for requests at http://{}", addr);

    gotham::start(addr, router(node_info_map));
    Ok(())
}

fn index(state: State) -> (State, HTMLStringResponse) {
    let message = HTMLStringResponse(format!(
        "<html><body><h1>Collector backend for {} v{}</h1>Operational!</p></body></html>",
        p2p_client::APPNAME,
        p2p_client::VERSION
    ));
    (state, message)
}

fn nodes_summary(state: State) -> (State, JSONStringResponse) {
    let state_data = CollectorStateData::borrow_from(&state);
    let elements = {
        let map_lock = &*read_or_die!(state_data.nodes);
        map_lock
            .iter()
            .map(|(_, value)| value.clone())
            .collect::<Vec<NodeInfo>>()
    };
    let message = JSONStringResponse(serde_json::to_string(&elements).unwrap());
    (state, message)
}

fn nodes_post_handler(mut state: State) -> Box<HandlerFuture> {
    let f = Body::take_from(&mut state)
        .concat2()
        .then(|full_body| match full_body {
            Ok(valid_body) => match str::from_utf8(&valid_body) {
                Ok(body_content) => {
                    let nodes_info_json: Result<NodeInfo, Error> =
                        serde_json::from_str(body_content);
                    match nodes_info_json {
                        Ok(mut nodes_info) => {
                            if !nodes_info.nodeName.is_empty() {
                                let state_data = CollectorStateData::borrow_from(&state);
                                nodes_info.last_updated = get_current_stamp();
                                write_or_die!(state_data.nodes)
                                    .insert(nodes_info.nodeName.clone(), nodes_info);
                            } else {
                                error!("Client submitted JSON without nodeName");
                            }
                            let res = create_empty_response(&state, StatusCode::OK);
                            future::ok((state, res))
                        }
                        Err(e) => {
                            error!("Can't parse JSON as valid due to {}", e);
                            future::err((state, e.into_handler_error()))
                        }
                    }
                }
                Err(e) => future::err((state, e.into_handler_error())),
            },
            Err(e) => future::err((state, e.into_handler_error())),
        });
    Box::new(f)
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
        route.post("/nodes/post").to(nodes_post_handler);
    })
}
