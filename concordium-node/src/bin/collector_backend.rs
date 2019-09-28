#![recursion_limit = "1024"]
#[cfg(not(target_os = "windows"))]
extern crate grpciounix as grpcio;
#[cfg(target_os = "windows")]
extern crate grpciowin as grpcio;
#[macro_use]
extern crate gotham_derive;
use env_logger::{Builder, Env};
use failure::Fallible;
use structopt::StructOpt;
use p2p_client::common::collector_utils::NodeInfo;
#[macro_use]
extern crate log;
use gotham::{
    handler::{HandlerFuture, IntoResponse, IntoHandlerError},
    helpers::http::response::create_response,
    middleware::state::StateMiddleware,
    pipeline::{single::single_pipeline, single_middleware},
    router::{builder::*, Router},
    state::{FromState, State},
};
use serde_json::error::Error;
use gotham::helpers::http::response::create_empty_response;
use hyper::{Body, Response, StatusCode};
use std::{sync::{Arc,RwLock}, collections::HashMap};
use futures::{future, Future, Stream};

// Explicitly defining allocator to avoid future reintroduction of jemalloc
use std::alloc::System;
#[global_allocator]
static A: System = System;

#[derive(StructOpt, Debug)]
#[structopt(name = "Node Collector Backend")]
struct ConfigCli {
    #[structopt(long = "listen-address", help = "IP to listen on", default_value = "0.0.0.0" )]
    pub host: String,
    #[structopt(long = "listen-port", help = "Port to listen on", default_value = "8080" )]
    pub port: u16,
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

#[derive(Clone, StateData)]
struct CollectorStateData {
    nodes: Arc<RwLock<HashMap<String,NodeInfo>>>,
}

impl CollectorStateData {
    fn new() -> Self {
        Self {
            nodes: Arc::new(RwLock::new(HashMap::new())),
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
    } else {
        Env::default().filter_or("LOG_LEVEL", "info")
    };

    let mut log_builder = Builder::from_env(env);
    if conf.no_log_timestamp {
        log_builder.default_format_timestamp(false);
    }
    log_builder.init();
    p2p_client::setup_panics();
    if conf.print_config {
        info!("{:?}", conf);
    }

    let addr = format!("{}:{}", conf.host, conf.port).to_string();
    println!("Listening for requests at http://{}", addr);
    gotham::start(addr, router() );
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

fn nodes_post_handler(mut state: State) -> Box<HandlerFuture> {
    let f = Body::take_from(&mut state)
        .concat2()
        .then(|full_body| match full_body {
            Ok(valid_body) => {
                match String::from_utf8(valid_body.to_vec()) {
                    Ok(body_content) => {
                        let nodes_info_json: Result<NodeInfo,Error> = serde_json::from_str(&body_content);
                        match nodes_info_json {
                            Ok(nodes_info) => {
                                println!("Body: {}", body_content);
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
                }
            }
            Err(e) => future::err((state, e.into_handler_error())),
        });
    Box::new(f)
}

pub fn router() -> Router {
    let state_data = CollectorStateData::new();
    let middleware = StateMiddleware::new(state_data);
    let pipeline = single_middleware(middleware);
    let (chain, pipelines) = single_pipeline(pipeline);
    build_router(chain, pipelines, |route| {
        route.get("/").to(index);
        route.post("/nodes/post").to(nodes_post_handler);
        //route.get("/metrics").to(Self::metrics);
    })
}