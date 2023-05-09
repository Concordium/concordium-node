use anyhow::anyhow;
use collector_backend::{setup_logger, NodeInfo, NodeInfoChainViz, NodeInfoDashboard};
use gotham::{
    anyhow::*,
    handler::{HandlerError, IntoResponse},
    helpers::http::response::{create_empty_response, create_response},
    middleware::state::StateMiddleware,
    pipeline::{new_pipeline, single::single_pipeline},
    router::{builder::*, Router},
    state::{FromState, State},
};
use gotham_derive::*;
use hyper::{body::HttpBody, Body, Response, StatusCode};
use log::{info, trace, warn};
use std::{
    collections::HashMap,
    fs,
    hash::BuildHasherDefault,
    io::Cursor,
    path::PathBuf,
    sync::{Arc, RwLock},
    thread,
    time::Duration,
};
use structopt::StructOpt;
use twox_hash::XxHash64;

// Force the system allocator on every platform
use std::alloc::System;
#[global_allocator]
static A: System = System;

#[derive(StructOpt, Debug)]
#[structopt(name = "Node Collector Backend")]
struct ConfigCli {
    #[structopt(
        long = "listen-address",
        help = "IP to listen on",
        default_value = "0.0.0.0",
        env = "COLLECTOR_BACKEND_ADDRESS"
    )]
    pub host:                   String,
    #[structopt(
        long = "listen-port",
        help = "Port to listen on",
        default_value = "8080",
        env = "COLLECTOR_BACKEND_PORT"
    )]
    pub port:                   u16,
    #[structopt(
        long = "stale-time-allowed",
        help = "Time in ms nodes are allowed to not have reported updates in before being removed",
        default_value = "3600000",
        env = "COLLECTOR_BACKEND_STALE_TIME_ALLOWED"
    )]
    pub stale_time_allowed:     u64,
    #[structopt(
        long = "cleanup-interval",
        help = "Time in ms to sleep between cleanups",
        default_value = "300000",
        env = "COLLECTOR_BACKEND_CLEANUP_INTERVAL"
    )]
    pub cleanup_interval:       u64,
    #[structopt(
        long = "print-config",
        help = "Print out config struct",
        env = "COLLECTOR_BACKEND_PRINT_CONFIG"
    )]
    pub print_config:           bool,
    #[structopt(
        long = "debug",
        short = "d",
        help = "Debug mode",
        env = "COLLECTOR_BACKEND_LOG_LEVEL_DEBUG"
    )]
    pub debug:                  bool,
    #[structopt(long = "trace", help = "Trace mode", env = "COLLECTOR_BACKEND_LOG_LEVEL_TRACE")]
    pub trace:                  bool,
    #[structopt(long = "info", help = "Info mode", env = "COLLECTOR_BACKEND_LOG_LEVEL_INFO")]
    #[allow(dead_code)] // for backwards compatibility
    pub info: bool,
    #[structopt(
        long = "no-log-timestamp",
        help = "Do not output timestamp in log output",
        env = "COLLECTOR_BACKEND_NO_LOG_TIMESTAMP"
    )]
    pub no_log_timestamp:       bool,
    #[structopt(
        long = "banned-versions",
        help = "Versions that are banned from publishing to the collector backend",
        env = "COLLECTOR_BACKEND_BANNED_VERSIONS",
        use_delimiter = true
    )]
    pub banned_versions:        Vec<String>,
    #[structopt(
        long = "banned-node-names-file",
        env = "COLLECTOR_BACKEND_BANNED_NODE_NAMES_FILE",
        help = "Path to file containing node names that are banned from publishing to the \
                collector backend. Node names should be separated by line breaks.",
        env = "COLLECTOR_BACKEND_BANNED_NODE_NAMES_FILE"
    )]
    pub banned_node_names_file: Option<PathBuf>,
    #[structopt(flatten)]
    validation_config:          ValidationConfig,
}

#[derive(Debug, Clone, StateData, StructOpt)]
pub struct ValidationConfig {
    #[structopt(
        long = "valid-content-length",
        help = "Maximum number of bytes allowed for the message body",
        default_value = "100000",
        env = "COLLECTOR_BACKEND_VALIDATION_VALID_CONTENT_LENGTH"
    )]
    pub valid_content_length:                    u64,
    #[structopt(
        long = "valid-node-name-lenght",
        help = "Maximum number of bytes allowed for the name of the node",
        default_value = "100",
        env = "COLLECTOR_BACKEND_VALIDATION_VALID_NODE_NAME_LENGTH"
    )]
    pub valid_node_name_lenght:                  usize,
    #[structopt(
        long = "valid-node-average-ping",
        help = "Maximum average ping allowed in milliseconds",
        default_value = "150000",
        env = "COLLECTOR_BACKEND_VALIDATION_VALID_NODE_AVERAGE_PING"
    )]
    pub valid_node_average_ping:                 f64,
    #[structopt(
        long = "valid-node-peers-count",
        help = "Maximum number for peers count",
        default_value = "50",
        env = "COLLECTOR_BACKEND_VALIDATION_VALID_NODE_PEERS_COUNT"
    )]
    pub valid_node_peers_count:                  u64,
    #[structopt(
        long = "validate-against-average-at",
        help = "The minimum number of nodes needed to calculate averages for validation",
        default_value = "20",
        env = "COLLECTOR_BACKEND_VALIDATION_VALIDATE_AGAINTS_AVERAGE_AT"
    )]
    pub validate_against_average_at:             u64,
    #[structopt(
        long = "percentage-used-for-averages",
        help = "A whole number representing the percentage of node data to use when calculating \
                averages. The fraction leftout is taken from the highest and lower values, making \
                the average more resilient to outliers.",
        default_value = "60",
        env = "COLLECTOR_BACKEND_VALIDATION_PERCENTAGE_USED_FOR_AVERAGES"
    )]
    pub percentage_used_for_averages:            usize,
    #[structopt(
        long = "valid-additional-best-block-height",
        help = "Maximum additional height of the best block allowed compared to the average best \
                block height. See also --validate-against-average-at",
        default_value = "1000",
        env = "COLLECTOR_BACKEND_VALIDATION_VALID_ADDITIONAL_BEST_BLOCK_HEIGHT"
    )]
    pub valid_additional_best_block_height:      u64,
    #[structopt(
        long = "valid-additional-finalized-block-height",
        help = "Maximum additional height of the finalized block allowed compared to the average \
                finalized block height. See also --validate-against-average-at",
        default_value = "1000",
        env = "COLLECTOR_BACKEND_VALIDATION_VALID_ADDITIONAL_FINALIZED_BLOCK_HEIGHT"
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
    pub nodes:             NodeInfoMap,
    pub banned_versions:   Vec<String>,
    pub banned_node_names: Vec<String>,
}

impl CollectorStateData {
    fn new(
        nodes: NodeInfoMap,
        banned_versions: Vec<String>,
        banned_node_names: Vec<String>,
    ) -> Self {
        Self {
            nodes,
            banned_versions,
            banned_node_names,
        }
    }
}

pub fn main() -> anyhow::Result<()> {
    let conf = ConfigCli::from_args();

    setup_logger(conf.trace, conf.debug, conf.no_log_timestamp);

    if conf.print_config {
        info!("{:?}", conf);
    }

    info!("Starting up concordium-node-collector-backend version {}!", env!("CARGO_PKG_VERSION"));

    let banned_node_names: Vec<String> = if let Some(file) = conf.banned_node_names_file {
        fs::read_to_string(file)?.lines().map(|s| s.to_string()).collect()
    } else {
        vec![]
    };

    let node_info_map: NodeInfoMap =
        Arc::new(RwLock::new(HashMap::with_capacity_and_hasher(1500, Default::default())));

    let _allowed_stale_time = conf.stale_time_allowed;
    let _node_info_map_clone = Arc::clone(&node_info_map);
    let _cleanup_interval = conf.cleanup_interval;
    #[allow(unreachable_code)] // the loop never breaks on its own
    let _ = std::thread::Builder::new()
        .name("collector backend cleanup".into())
        .spawn(move || loop {
            thread::sleep(Duration::from_millis(_cleanup_interval));
            info!("Running cleanup");
            let current_stamp = chrono::Utc::now().timestamp_millis() as u64;
            _node_info_map_clone
                .write()
                .expect("RWLock poisoned")
                .retain(|_, element| current_stamp < element.last_updated + _allowed_stale_time);
        })
        .expect("The OS refused to create a new thread");

    let addr = format!("{}:{}", conf.host, conf.port);
    info!("Listening for requests at http://{}", addr);

    gotham::start(
        addr,
        router(node_info_map, conf.banned_versions, banned_node_names, conf.validation_config),
    );
    Ok(())
}

fn index(state: State) -> (State, HTMLStringResponse) {
    trace!("Processing an index request");
    let message = HTMLStringResponse(format!(
        "<html><body><h1>Collector backend for concordium \
         v{}</h1><p>Operational!</p></body></html>",
        env!("CARGO_PKG_VERSION")
    ));
    (state, message)
}

fn nodes_summary(state: State) -> (State, JSONStringResponse) {
    trace!("Processing an nodes summary request");
    let state_data = CollectorStateData::borrow_from(&state);
    let mut response = Vec::new();
    {
        let map_lock = &*state_data.nodes.read().expect("RWLock poisoned");
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
        let map_lock = &*state_data.nodes.read().expect("RWLock poisoned");
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

async fn nodes_post_handler_wrapper(state: &mut State) -> Result<Response<Body>, HandlerError> {
    nodes_post_handler(state).await.map_err(|e| {
        warn!("Bad request: {}", e);
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
    ensure!(
        nodes_info.nodeName.lines().count() == 1,
        "nodeName is not allowed to contain line breaks"
    );
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
    let state_data = CollectorStateData::borrow_from(state);
    ensure!(!state_data.banned_versions.contains(&nodes_info.client), "node version is banned");
    ensure!(
        !state_data.banned_node_names.contains(&nodes_info.nodeName.trim().to_string()),
        "node name is banned"
    );

    // Check the best block height and finalized block height against the average,
    // But only if the state contains information from enough nodes.
    {
        let nodes = state_data.nodes.read().expect("RWLock poisoned");
        let len = nodes.len() as u64;
        if len >= validation_conf.validate_against_average_at {
            let number_of_nodes_to_include =
                (nodes.len() * validation_conf.percentage_used_for_averages) / 100;

            let avg_best_block_height = average_without_outer_values(
                nodes.values().map(|n| n.bestBlockHeight).collect(),
                number_of_nodes_to_include,
            );

            ensure!(
                nodes_info.bestBlockHeight
                    <= avg_best_block_height + validation_conf.valid_additional_best_block_height,
                "bestBlockHeight is too high above average to be considered valid",
            );

            let avg_finalized_block_height = average_without_outer_values(
                nodes.values().map(|n| n.finalizedBlockHeight).collect(),
                number_of_nodes_to_include,
            );
            ensure!(
                nodes_info.finalizedBlockHeight
                    <= avg_finalized_block_height
                        + validation_conf.valid_additional_finalized_block_height,
                "finalizedBlockHeight is too high above average to be considered valid",
            );
        }
    }

    nodes_info.last_updated = chrono::Utc::now().timestamp_millis() as u64;
    state_data
        .nodes
        .write()
        .expect("RWLock poisoned")
        .insert(nodes_info.nodeId.clone(), nodes_info);

    Ok(create_empty_response(state, StatusCode::OK))
}

pub fn router(
    node_info_map: NodeInfoMap,
    banned_versions: Vec<String>,
    banned_node_names: Vec<String>,
    validation_config: ValidationConfig,
) -> Router {
    let state_data = CollectorStateData::new(node_info_map, banned_versions, banned_node_names);
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
        route.post("/nodes/post").to_async_borrowing(nodes_post_handler_wrapper);
        route.post("/post/nodes").to_async_borrowing(nodes_post_handler_wrapper);
    })
}

/// Calculates the average, but only from a part of the values. The omitted are
/// taken from the highest and lowest values.
fn average_without_outer_values(mut values: Vec<u64>, number_to_include: usize) -> u64 {
    // the unstable sort is faster and stability makes no difference for sorting
    // sequences of u64.
    values.sort_unstable();
    let omitting = values.len() - number_to_include;
    let start = omitting / 2;
    let end = start + number_to_include;

    values[start..end].iter().sum::<u64>() / number_to_include as u64
}
