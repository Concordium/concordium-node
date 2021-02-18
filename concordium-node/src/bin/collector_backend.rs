#![recursion_limit = "1024"]
#[macro_use]
extern crate gotham_derive;
use concordium_node::{
    common::{collector_utils::*, get_current_stamp},
    utils::setup_logger_env,
};
use env_logger::Env;
use failure::Fallible;
use structopt::StructOpt;
use twox_hash::XxHash64;
#[macro_use]
extern crate log;
use concordium_node::{read_or_die, spawn_or_die, write_or_die};
use gotham::{
    anyhow::*,
    handler::{HandlerError, IntoResponse},
    helpers::http::response::{create_empty_response, create_response},
    middleware::state::StateMiddleware,
    pipeline::{new_pipeline, single::single_pipeline},
    router::{builder::*, Router},
    state::{FromState, State},
};
use hyper::{body::HttpBody, Body, Response, StatusCode};
use std::{
    collections::HashMap,
    hash::BuildHasherDefault,
    io::Cursor,
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
    #[structopt(
        long = "banned-versions",
        help = "Versions that are banned from publishing to the collector backend"
    )]
    pub banned_versions: Vec<String>,
    #[structopt(flatten)]
    validation_config: ValidationConfig,
}

#[derive(Debug, Clone, StateData, StructOpt)]
pub struct ValidationConfig {
    #[structopt(
        long = "valid-content-length",
        env = "COLLECTOR_BACKEND_VALID_CONTENT_LENGTH",
        help = "Maximum number of bytes allowed for the message body",
        default_value = "100000"
    )]
    pub valid_content_length: u64,
    #[structopt(
        long = "valid-node-name-lenght",
        env = "COLLECTOR_BACKEND_VALID_NODE_NAME_LENGHT",
        help = "Maximum number of bytes allowed for the name of the node",
        default_value = "100"
    )]
    pub valid_node_name_lenght: usize,
    #[structopt(
        long = "valid-node-average-ping",
        env = "COLLECTOR_BACKEND_VALID_NODE_AVERAGE_PING",
        help = "Maximum average ping allowed in milliseconds",
        default_value = "30000"
    )]
    pub valid_node_average_ping: f64,
    #[structopt(
        long = "valid-node-peers-count",
        env = "COLLECTOR_BACKEND_VALID_NODE_PEERS_COUNT",
        help = "Maximum number for peers count",
        default_value = "50"
    )]
    pub valid_node_peers_count: u64,
    #[structopt(
        long = "validate-against-average-at",
        env = "COLLECTOR_BACKEND_VALIDATE_AGAINTS_AVERAGE_AT",
        help = "The minimum number of nodes needed to calculate averages for validation",
        default_value = "20"
    )]
    pub validate_against_average_at: u64,
    #[structopt(
        long = "valid-additional-best-block-height",
        env = "COLLECTOR_BACKEND_VALID_ADDITIONAL_BEST_BLOCK_HEIGHT",
        help = "Maximum additional height of the best block allowed compared to the average best \
                block height. See also --validate-against-average-at",
        default_value = "1000"
    )]
    pub valid_additional_best_block_height: u64,
    #[structopt(
        long = "valid-additional-finalized-block-height",
        env = "COLLECTOR_BACKEND_VALID_ADDITIONAL_FINALIZED_BLOCK_HEIGHT",
        help = "Maximum additional height of the finalized block allowed compared to the average \
                finalized block height. See also --validate-against-average-at",
        default_value = "1000"
    )]
    pub valid_additional_finalized_block_height: u64,
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

type NodeId = String;

/// Map from a NodeId to the most recent valid NodeInfo.
type NodeInfoMap = Arc<RwLock<HashMap<NodeId, NodeInfo, BuildHasherDefault<XxHash64>>>>;

#[derive(Clone, StateData)]
struct CollectorStateData {
    pub nodes:           NodeInfoMap,
    pub banned_versions: Vec<String>,
}

impl CollectorStateData {
    fn new(nodes: NodeInfoMap, banned_versions: Vec<String>) -> Self {
        Self {
            nodes,
            banned_versions,
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
        concordium_node::APPNAME,
        concordium_node::VERSION
    );

    let node_info_map: NodeInfoMap =
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

    gotham::start(addr, router(node_info_map, conf.banned_versions, conf.validation_config));
    Ok(())
}

fn index(state: State) -> (State, HTMLStringResponse) {
    trace!("Processing an index request");
    let message = HTMLStringResponse(format!(
        "<html><body><h1>Collector backend for {} v{}</h1>Operational!</p></body></html>",
        concordium_node::APPNAME,
        concordium_node::VERSION
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

async fn nodes_post_handler_wrapper(state: &mut State) -> Result<Response<Body>, HandlerError> {
    nodes_post_handler(state).await.map_err(|e| {
        info!("Bad request: {}", e);
        HandlerError::from(e).with_status(StatusCode::BAD_REQUEST)
    })
}

async fn nodes_post_handler(state: &mut State) -> gotham::anyhow::Result<Response<Body>> {
    trace!("Processing a post from a node-collector");
    let validation_conf = ValidationConfig::take_from(state);
    let mut body = Body::take_from(state);
    let content_length =
        body.size_hint().exact().ok_or_else(|| anyhow!("Header 'Content-Length' is required"))?;

    ensure!(
        content_length <= validation_conf.valid_content_length,
        "Content-Length is larger than the limit set by the collector backend"
    );

    // Fail if body is larger than content-length
    let body_content = {
        let mut content = Vec::with_capacity(content_length as usize);
        while let Some(buf) = body.data().await {
            let buffer = buf?;
            ensure!(content.len() + buffer.len() <= (content_length as usize), "Invalid body");
            content.append(&mut buffer.to_vec());
        }
        ensure!(content.len() == content_length as usize, "Invalid body");
        content
    };

    let mut nodes_info: NodeInfo = rmp_serde::decode::from_read(Cursor::new(&body_content))
        .context("Can't parse client data")?;

    ensure!(!nodes_info.nodeName.is_empty(), "nodeName cannot be empty");
    ensure!(!nodes_info.nodeId.is_empty(), "nodeId cannot be empty");
    ensure!(
        nodes_info.nodeName.len() <= validation_conf.valid_node_name_lenght,
        "nodeName is to long too be considered valid"
    );
    if let Some(avg_ping) = nodes_info.averagePing {
        ensure!(
            avg_ping <= validation_conf.valid_node_average_ping,
            "Average ping is too high to be considered valid"
        );
    }
    ensure!(
        nodes_info.peersCount <= validation_conf.valid_node_peers_count,
        "peersCount is too high to be considered valid"
    );
    let state_data = CollectorStateData::borrow_from(&state);
    ensure!(!state_data.banned_versions.contains(&nodes_info.client), "node version is banned");

    // Check the best block height and finalized block height against the average,
    // if state contains information from enough nodes.
    {
        let nodes = read_or_die!(state_data.nodes);
        let len = nodes.len() as u64;
        if len >= validation_conf.validate_against_average_at {
            let mut sum_best_block_height: u64 = 0;
            let mut sum_finalized_block_height: u64 = 0;
            for node in nodes.values() {
                sum_best_block_height += node.bestBlockHeight;
                sum_finalized_block_height += node.finalizedBlockHeight;
            }
            let avg_best_block_height = sum_best_block_height / len;
            let avg_finalized_block_height = sum_finalized_block_height / len;

            ensure!(
                nodes_info.bestBlockHeight
                    <= avg_best_block_height + validation_conf.valid_additional_best_block_height,
                "bestBlockHeight is too high above average to be considered valid",
            );

            ensure!(
                nodes_info.finalizedBlockHeight
                    <= avg_finalized_block_height
                        + validation_conf.valid_additional_finalized_block_height,
                "finalizedBlockHeight is too high above average to be considered valid",
            );
        }
    }

    nodes_info.last_updated = get_current_stamp();
    write_or_die!(state_data.nodes).insert(nodes_info.nodeId.clone(), nodes_info);

    Ok(create_empty_response(&state, StatusCode::OK))
}

pub fn router(
    node_info_map: NodeInfoMap,
    banned_versions: Vec<String>,
    validation_config: ValidationConfig,
) -> Router {
    let state_data = CollectorStateData::new(node_info_map, banned_versions);
    let validation_config_middleware = StateMiddleware::new(validation_config);
    let collector_state_middleware = StateMiddleware::new(state_data);
    let (chain, pipelines) = single_pipeline(
        new_pipeline().add(validation_config_middleware).add(collector_state_middleware).build(),
    );
    build_router(chain, pipelines, |route| {
        route.get("/").to(index);
        route.get("/nodesSummary").to(nodes_summary);
        route.get("/data/nodesSummary").to(nodes_summary);
        route.get("/nodesBlocksInfo").to(nodes_block_info);
        route.get("/data/nodesBlocksInfo").to(nodes_block_info);
        route.get("/nodesStagingNetUsers").to(nodes_staging_users_info);
        route.get("/data/nodesStagingNetUsers").to(nodes_staging_users_info);
        route.post("/nodes/post").to_async_borrowing(nodes_post_handler_wrapper);
        route.post("/post/nodes").to_async_borrowing(nodes_post_handler_wrapper);
    })
}
