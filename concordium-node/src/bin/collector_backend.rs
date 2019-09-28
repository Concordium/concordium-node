#![recursion_limit = "1024"]
#[cfg(not(target_os = "windows"))]
extern crate grpciounix as grpcio;
#[cfg(target_os = "windows")]
extern crate grpciowin as grpcio;
use concordium_common::spawn_or_die;
use env_logger::{Builder, Env};
use failure::Fallible;
use grpcio::{ChannelBuilder, EnvBuilder};
use serde_json::Value;
use std::{thread, time::Duration};
use structopt::StructOpt;
use p2p_client::common::collector_utils::NodeInfo;
#[macro_use]
extern crate log;
use gotham::{
    handler::IntoResponse,
    helpers::http::response::create_response,
    middleware::state::StateMiddleware,
    pipeline::{single::single_pipeline, single_middleware},
    router::{builder::*, Router},
    state::{FromState, State},
};
use hyper::{Body, Response, StatusCode};
use std::{sync::{Arc,RwLock}, collections::HashMap};

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

struct HTMLStringResponse(pub String);

impl IntoResponse for HTMLStringResponse {
    fn into_response(self, state: &State) -> Response<Body> {
        create_response(state, StatusCode::OK, mime::TEXT_HTML, self.0)
    }
}

#[derive(Clone, StateData)]
struct CollectorStateData {
    nodes: Arc<RwLock<HashMap<String,NodeInfo>>,
}

impl CollectorStateData {
    fn new() -> Self {
        Self {
            nodes: Arc::new(RwLock::new(HashMap::new())),
        }
    }
}

const MAX_GRPC_FAILURES: usize = 20;

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
    gotham::start(addr, || router() );
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

fn router() -> Router {
    let state_data = CollectorStateData::new();
    let middleware = StateMiddleware::new(state_data);
    let pipeline = single_middleware(middleware);
    let (chain, pipelines) = single_pipeline(pipeline);
    build_router(chain, pipelines, |route| {
        //route.get("/").to(index());
        //route.get("/metrics").to(Self::metrics);
    })
}