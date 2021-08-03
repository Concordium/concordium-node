#![recursion_limit = "1024"]
use concordium_node::{
    common::{collector_utils::NodeInfo, grpc_api},
    req_with_auth,
    utils::setup_logger,
};
#[cfg(not(target_os = "macos"))]
use env_logger::Env;
use serde_json::Value;
use std::{
    borrow::ToOwned,
    default::Default,
    fmt,
    process::exit,
    str::FromStr,
    sync::atomic::{AtomicUsize, Ordering as AtomicOrdering},
    thread,
    time::Duration,
};
use structopt::StructOpt;
use tonic::{metadata::MetadataValue, transport::channel::Channel, Request};
#[macro_use]
extern crate log;

// Force the system allocator on every platform
use std::alloc::System;
#[global_allocator]
static A: System = System;

#[derive(Clone, Debug)]
struct NodeName(Vec<String>);

impl FromStr for NodeName {
    type Err = anyhow::Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Ok(Self(input.split_whitespace().map(ToOwned::to_owned).collect::<Vec<String>>()))
    }
}

impl fmt::Display for NodeName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.0[..].join(" "))
    }
}

#[derive(StructOpt, Debug)]
#[structopt(name = "Node Collector")]
struct ConfigCli {
    #[structopt(
        long = "grpc-authentication-token",
        help = "gRPC authentication token",
        default_value = "rpcadmin",
        env = "CONCORDIUM_NODE_COLLECTOR_GRPC_AUTHENTICATION_TOKEN",
        hide_env_values = true
    )]
    pub grpc_auth_token: String,
    #[structopt(
        long = "grpc-host",
        help = "gRPC host to collect from",
        default_value = "http://127.0.0.1:10000",
        env = "CONCORDIUM_NODE_COLLECTOR_GRPC_HOST",
        use_delimiter = true // default delimiter is a comma
    )]
    pub grpc_hosts: Vec<String>,
    #[structopt(
        long = "node-name",
        help = "Node name",
        env = "CONCORDIUM_NODE_COLLECTOR_NODE_NAME",
        use_delimiter = true // default delimiter is a comma
    )]
    pub node_names: Vec<NodeName>,
    #[structopt(
        long = "collector-url",
        help = "Alias submitted of the node collected from",
        default_value = "http://localhost:3000/post/nodes",
        env = "CONCORDIUM_NODE_COLLECTOR_URL"
    )]
    pub collector_url: String,
    #[structopt(
        long = "print-config",
        help = "Print out config struct",
        env = "CONCORDIUM_NODE_COLLECTOR_PRINT_CONFIG"
    )]
    pub print_config: bool,
    #[structopt(
        long = "debug",
        short = "d",
        help = "Debug mode",
        env = "CONCORDIUM_NODE_COLLECTOR_DEBUG"
    )]
    pub debug: bool,
    #[structopt(long = "trace", help = "Trace mode", env = "CONCORDIUM_NODE_COLLECTOR_TRACE")]
    pub trace: bool,
    #[structopt(long = "info", help = "Info mode", env = "CONCORDIUM_NODE_COLLECTOR_INFO")]
    pub info: bool,
    #[structopt(
        long = "no-log-timestamp",
        help = "Do not output timestamp in log output",
        env = "CONCORDIUM_NODE_COLLECTOR_NO_LOG_TIMESTAMP"
    )]
    pub no_log_timestamp: bool,
    #[structopt(
        long = "collect-interval",
        help = "Interval in miliseconds to sleep between runs of the collector",
        default_value = "5000",
        env = "CONCORDIUM_NODE_COLLECTOR_COLLECT_INTERVAL"
    )]
    pub collector_interval: u64,
    #[structopt(
        long = "artificial-start-delay",
        help = "Time (in ms) to delay when the first gRPC request is sent to the node",
        default_value = "3000",
        env = "CONCORDIUM_NODE_COLLECTOR_ARTIFICIAL_START_DELAY"
    )]
    pub artificial_start_delay: u64,
    #[structopt(
        long = "max-grpc-failures-allowed",
        help = "Maximum allowed times a gRPC call can fail before terminating the program",
        default_value = "50",
        env = "CONCORDIUM_NODE_COLLECTOR_MAX_GRPC_FAILURES_ALLOWED"
    )]
    pub max_grpc_failures_allowed: u64,
    #[cfg(target_os = "macos")]
    #[structopt(
        long = "net-name",
        help = "The net named used for logging on macOS, e.g. 'mainnet'. Messages will be logged \
                with the subsystem 'software.concordium.<net-name>.node', which can be searched \
                for in Console.app.",
        env = "CONCORDIUM_NODE_COLLECTOR_MACOS_NET_NAME"
    )]
    pub net_name: String,
}

#[tokio::main]
async fn main() {
    let conf = ConfigCli::from_args();

    #[cfg(target_os = "macos")]
    setup_logger(conf.trace, conf.debug, &conf.net_name);
    #[cfg(not(target_os = "macos"))]
    setup_logger(conf.trace, conf.debug, conf.no_log_timestamp);

    if conf.print_config {
        info!("{:?}", conf);
    }

    info!(
        "Starting up {}-node-collector version {}!",
        concordium_node::APPNAME,
        concordium_node::VERSION
    );

    if conf.node_names.len() != conf.grpc_hosts.len() {
        error!("The number of node-names and grpc-hosts must be equal!");
        exit(1);
    }

    if conf.artificial_start_delay > 0 {
        info!("Delaying first collection from the node for {} ms", conf.artificial_start_delay);
        thread::sleep(Duration::from_millis(conf.artificial_start_delay));
    }

    let atomic_counter: AtomicUsize = Default::default();
    #[allow(unreachable_code)]
    loop {
        let grpc_failure_count = atomic_counter.load(AtomicOrdering::Relaxed);
        trace!("Failure count is {}/{}", grpc_failure_count, conf.max_grpc_failures_allowed);
        for (node_name, grpc_host) in conf.node_names.iter().zip(conf.grpc_hosts.iter()) {
            trace!("Processing node {}/{}", node_name, grpc_host);
            match collect_data(node_name.clone(), grpc_host.to_owned(), &conf.grpc_auth_token).await
            {
                Ok(node_info) => {
                    trace!("Node data collected successfully from {}/{}", node_name, grpc_host);
                    match rmp_serde::encode::to_vec(&node_info) {
                        Ok(msgpack) => {
                            let client = reqwest::Client::new();
                            match client.post(&conf.collector_url).body(msgpack).send().await {
                                Ok(_) => trace!("Payload sent successfully to collector backend"),
                                Err(e) => error!(
                                    "Error sending payload to collector backend due to \"{}\"",
                                    e
                                ),
                            }
                        }
                        Err(e) => error!("Error serializing data for the backend due to \"{}\"", e),
                    }
                }
                Err(e) => {
                    let _ = atomic_counter.fetch_add(1, AtomicOrdering::SeqCst);
                    error!(
                        "gRPC failed with \"{}\" for {}, sleeping for {} ms",
                        e, &grpc_host, conf.collector_interval
                    );
                }
            }

            if grpc_failure_count + 1 >= conf.max_grpc_failures_allowed as usize {
                error!("Too many gRPC failures, exiting!");
                exit(1);
            }
        }
        trace!("Sleeping for {} ms", conf.collector_interval);
        thread::sleep(Duration::from_millis(conf.collector_interval));
    }
}

#[allow(clippy::cognitive_complexity)]
async fn collect_data<'a>(
    node_name: NodeName,
    grpc_host: String,
    grpc_auth_token: &str,
) -> anyhow::Result<NodeInfo> {
    info!(
        "Collecting node information via gRPC from {}/{}/{}",
        node_name, grpc_host, grpc_auth_token
    );

    let channel = Channel::from_shared(grpc_host).unwrap().connect().await?;
    let mut client = grpc_api::p2p_client::P2pClient::new(channel);

    let empty_req = || req_with_auth!(grpc_api::Empty {}, grpc_auth_token);

    trace!("Requesting basic node info via gRPC");
    let node_info_reply = client.node_info(empty_req()).await?;

    trace!("Requesting node uptime info via gRPC");
    let node_uptime_reply = client.peer_uptime(empty_req()).await?;

    trace!("Requesting node version info via gRPC");
    let node_version_reply = client.peer_version(empty_req()).await?;

    trace!("Requesting consensus status info via gRPC");
    let node_consensus_status_reply = client.get_consensus_status(empty_req()).await?;

    trace!("Requesting node peer stats info via gRPC");
    let node_peer_stats_reply = client
        .peer_stats(req_with_auth!(
            grpc_api::PeersRequest {
                include_bootstrappers: false,
            },
            grpc_auth_token
        ))
        .await?;

    trace!("Requesting node total sent message count info via gRPC");
    let node_total_sent_reply = client.peer_total_sent(empty_req()).await?;

    trace!("Requesting node total received message count via gRPC");
    let node_total_received_reply = client.peer_total_received(empty_req()).await?;

    let node_info_reply = node_info_reply.get_ref();
    let node_id = node_info_reply.node_id.to_owned().unwrap();
    let peer_type = node_info_reply.peer_type.to_owned();
    let baker_committee = node_info_reply.consensus_baker_committee();
    let finalization_committee = node_info_reply.consensus_finalizer_committee;
    let consensus_running = node_info_reply.consensus_running;
    let uptime = node_uptime_reply.get_ref().value as u64;
    let version = node_version_reply.get_ref().value.to_owned();
    let packets_sent = node_total_sent_reply.get_ref().value;
    let packets_received = node_total_received_reply.get_ref().value;
    let baker_id = node_info_reply.consensus_baker_id;
    let node_peer_stats_reply = node_peer_stats_reply.get_ref();
    let peer_stats = &node_peer_stats_reply.peerstats;
    let peers_summed_latency = peer_stats.iter().map(|element| element.latency).sum::<u64>() as f64;
    let peers_with_valid_latencies_count =
        peer_stats.iter().filter(|element| element.latency > 0).count();

    let avg_bps_in = node_peer_stats_reply.avg_bps_in;
    let avg_bps_out = node_peer_stats_reply.avg_bps_out;

    let average_ping = if peers_with_valid_latencies_count > 0 {
        Some(peers_summed_latency / peers_with_valid_latencies_count as f64)
    } else {
        None
    };
    let peers_count = peer_stats.len() as u64;
    let peers_list =
        peer_stats.iter().map(|element| element.node_id.clone()).collect::<Vec<String>>();

    trace!("Parsing consensus JSON status response");
    let json_consensus_value: Value =
        serde_json::from_str(&node_consensus_status_reply.get_ref().value)?;

    let best_block = json_consensus_value["bestBlock"].as_str().unwrap().to_owned();
    let best_block_height = json_consensus_value["bestBlockHeight"].as_u64().unwrap();
    let best_arrived_time = if json_consensus_value["blockLastArrivedTime"].is_string() {
        Some(json_consensus_value["blockLastArrivedTime"].as_str().unwrap().to_owned())
    } else {
        None
    };
    let block_arrive_period_ema = json_consensus_value["blockArrivePeriodEMA"].as_f64();
    let block_arrive_period_emsd = json_consensus_value["blockArrivePeriodEMSD"].as_f64();
    let block_arrive_latency_ema = json_consensus_value["blockArriveLatencyEMA"].as_f64();
    let block_arrive_latency_emsd = json_consensus_value["blockArriveLatencyEMSD"].as_f64();
    let block_receive_period_ema = json_consensus_value["blockReceivePeriodEMA"].as_f64();
    let block_receive_period_emsd = json_consensus_value["blockReceivePeriodEMSD"].as_f64();
    let block_receive_latency_ema = json_consensus_value["blockReceiveLatencyEMA"].as_f64();
    let block_receive_latency_emsd = json_consensus_value["blockReceiveLatencyEMSD"].as_f64();

    let blocks_verified_count = json_consensus_value["blocksVerifiedCount"].as_u64();
    let blocks_received_count = json_consensus_value["blocksReceivedCount"].as_u64();
    let finalization_count = json_consensus_value["finalizationCount"].as_u64();
    let genesis_block = json_consensus_value["genesisBlock"].as_str().unwrap().to_owned();

    let finalized_block = json_consensus_value["lastFinalizedBlock"].as_str().unwrap().to_owned();
    let finalized_block_height = json_consensus_value["lastFinalizedBlockHeight"].as_u64().unwrap();
    let finalized_time = if json_consensus_value["lastFinalizedTime"].is_string() {
        Some(json_consensus_value["lastFinalizedTime"].as_str().unwrap().to_owned())
    } else {
        None
    };
    let finalization_period_ema = json_consensus_value["finalizationPeriodEMA"].as_f64();
    let finalization_period_emsd = json_consensus_value["finalizationPeriodEMSD"].as_f64();
    let transactions_per_block_ema = json_consensus_value["transactionsPerBlockEMA"].as_f64();
    let transactions_per_block_emsd = json_consensus_value["transactionsPerBlockEMSD"].as_f64();

    let ancestors_since_best_block = if best_block_height > finalized_block_height {
        trace!("Requesting further consensus status via gRPC");
        let block_and_height_req = req_with_auth!(
            grpc_api::BlockHashAndAmount {
                block_hash: best_block.clone(),
                amount:     best_block_height - finalized_block_height,
            },
            grpc_auth_token
        );
        let node_ancestors_reply = client.get_ancestors(block_and_height_req).await?;
        let json_consensus_ancestors_value: Value =
            serde_json::from_str(&node_ancestors_reply.get_ref().value)?;
        if json_consensus_ancestors_value.is_array() {
            json_consensus_ancestors_value.as_array().map(|ancestors_arr| {
                ancestors_arr
                    .iter()
                    .map(|value| value.as_str().unwrap().to_owned())
                    .collect::<Vec<String>>()
            })
        } else {
            None
        }
    } else {
        None
    };

    let block_req = req_with_auth!(
        grpc_api::BlockHash {
            block_hash: best_block.clone(),
        },
        grpc_auth_token
    );

    let node_block_info_reply = client.get_block_info(block_req).await?;
    let json_block_info_value: Value =
        serde_json::from_str(&node_block_info_reply.get_ref().value)?;
    let best_block_total_encrypted_amount = json_block_info_value["totalEncryptedAmount"].as_u64();
    let best_block_transactions_size = json_block_info_value["transactionsSize"].as_u64();
    let best_block_total_amount = json_block_info_value["totalAmount"].as_u64();
    let best_block_transaction_count = json_block_info_value["transactionCount"].as_u64();
    let best_block_transaction_energy_cost =
        json_block_info_value["transactionEnergyCost"].as_u64();
    let best_block_execution_cost = json_block_info_value["executionCost"].as_u64();
    let best_block_central_bank_amount = json_block_info_value["centralBankAmount"].as_u64();
    let best_block_baker_id = json_block_info_value["blockBaker"].as_u64();

    let finalized_block_parent = if best_block_height == finalized_block_height {
        json_block_info_value["blockParent"].as_str().unwrap().to_owned()
    } else {
        let finalized_block_req = req_with_auth!(
            grpc_api::BlockHash {
                block_hash: finalized_block.clone(),
            },
            grpc_auth_token
        );

        let node_finalized_block_info_reply = client.get_block_info(finalized_block_req).await?;
        let json_finalized_block_info_value: Value =
            serde_json::from_str(&node_finalized_block_info_reply.get_ref().value)?;
        json_finalized_block_info_value["blockParent"].as_str().unwrap().to_owned()
    };

    Ok(NodeInfo {
        nodeName: node_name.to_string(),
        nodeId: node_id,
        peerType: peer_type,
        uptime,
        client: version,
        averagePing: average_ping,
        peersCount: peers_count,
        peersList: peers_list,
        bestBlock: best_block,
        bestBlockHeight: best_block_height,
        bestBlockBakerId: best_block_baker_id,
        bestArrivedTime: best_arrived_time,
        blockArrivePeriodEMA: block_arrive_period_ema,
        blockArrivePeriodEMSD: block_arrive_period_emsd,
        blockArriveLatencyEMA: block_arrive_latency_ema,
        blockArriveLatencyEMSD: block_arrive_latency_emsd,
        blockReceivePeriodEMA: block_receive_period_ema,
        blockReceivePeriodEMSD: block_receive_period_emsd,
        blockReceiveLatencyEMA: block_receive_latency_ema,
        blockReceiveLatencyEMSD: block_receive_latency_emsd,
        finalizedBlock: finalized_block,
        finalizedBlockHeight: finalized_block_height,
        finalizedTime: finalized_time,
        finalizationPeriodEMA: finalization_period_ema,
        finalizationPeriodEMSD: finalization_period_emsd,
        packetsSent: packets_sent,
        packetsReceived: packets_received,
        consensusRunning: consensus_running,
        bakingCommitteeMember: baker_committee,
        consensusBakerId: baker_id,
        finalizationCommitteeMember: finalization_committee,
        ancestorsSinceBestBlock: ancestors_since_best_block,
        stagingNetUsername: None,
        last_updated: 0,
        transactionsPerBlockEMA: transactions_per_block_ema,
        transactionsPerBlockEMSD: transactions_per_block_emsd,
        bestBlockTransactionsSize: best_block_transactions_size,
        bestBlockTotalEncryptedAmount: best_block_total_encrypted_amount,
        bestBlockTotalAmount: best_block_total_amount,
        bestBlockTransactionCount: best_block_transaction_count,
        bestBlockTransactionEnergyCost: best_block_transaction_energy_cost,
        bestBlockExecutionCost: best_block_execution_cost,
        bestBlockCentralBankAmount: best_block_central_bank_amount,
        blocksReceivedCount: blocks_received_count,
        blocksVerifiedCount: blocks_verified_count,
        finalizationCount: finalization_count,
        genesisBlock: genesis_block,
        averageBytesPerSecondIn: avg_bps_in,
        averageBytesPerSecondOut: avg_bps_out,
        finalizedBlockParent: finalized_block_parent,
    })
}
