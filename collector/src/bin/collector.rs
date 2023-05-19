use anyhow::{anyhow, bail};
use chrono::{SecondsFormat, TimeZone};
use collector_backend::{IsInBakingCommittee, NodeInfo};
use env_logger::{Builder, Env};
use futures::TryStreamExt;
use log::LevelFilter;
use std::{borrow::ToOwned, fmt, io::Write, process::exit, str::FromStr, time::Duration};
use structopt::StructOpt;
use tonic::transport::channel::Channel;

#[macro_use]
extern crate log;

#[allow(clippy::large_enum_variant, clippy::enum_variant_names)]
mod grpc {
    tonic::include_proto!("concordium.v2");
}
use grpc::{node_info::node::ConsensusStatus, tokenomics_info::Tokenomics};
type NodeDetails = grpc::node_info::Details;
type BakerStatus = grpc::node_info::baker_consensus_info::Status;

#[derive(StructOpt, Debug)]
#[structopt(name = "Node Collector")]
struct ConfigCli {
    #[structopt(
        long = "grpc-host",
        help = "gRPC host to collect from",
        default_value = "http://127.0.0.1:20000",
        env = "CONCORDIUM_NODE_COLLECTOR_GRPC_HOST",
        use_delimiter = true // default delimiter is a comma
    )]
    pub grpc_hosts:             Vec<String>,
    #[structopt(
        long = "node-name",
        help = "Node name",
        env = "CONCORDIUM_NODE_COLLECTOR_NODE_NAME",
        use_delimiter = true // default delimiter is a comma
    )]
    pub node_names:             Vec<NodeName>,
    #[structopt(
        long = "collector-url",
        help = "Alias submitted of the node collected from",
        default_value = "http://localhost:3000/post/nodes",
        env = "CONCORDIUM_NODE_COLLECTOR_URL"
    )]
    pub collector_url:          String,
    #[structopt(
        long = "print-config",
        help = "Print out config struct",
        env = "CONCORDIUM_NODE_COLLECTOR_PRINT_CONFIG"
    )]
    pub print_config:           bool,
    #[structopt(
        long = "debug",
        short = "d",
        help = "Debug mode",
        env = "CONCORDIUM_NODE_COLLECTOR_DEBUG"
    )]
    pub debug:                  bool,
    #[structopt(long = "trace", help = "Trace mode", env = "CONCORDIUM_NODE_COLLECTOR_TRACE")]
    pub trace:                  bool,
    #[structopt(long = "info", help = "Info mode", env = "CONCORDIUM_NODE_COLLECTOR_INFO")]
    #[allow(dead_code)] // allow for backwards compatibility.
    pub info: bool,
    #[structopt(
        long = "no-log-timestamp",
        help = "Do not output timestamp in log output",
        env = "CONCORDIUM_NODE_COLLECTOR_NO_LOG_TIMESTAMP"
    )]
    pub no_log_timestamp:       bool,
    #[structopt(
        long = "collect-interval",
        help = "Interval in miliseconds to sleep between runs of the collector",
        default_value = "5000",
        env = "CONCORDIUM_NODE_COLLECTOR_COLLECT_INTERVAL"
    )]
    pub collector_interval:     u64,
    #[structopt(
        long = "artificial-start-delay",
        help = "Time (in ms) to delay when the first gRPC request is sent to the node",
        default_value = "3000",
        env = "CONCORDIUM_NODE_COLLECTOR_ARTIFICIAL_START_DELAY"
    )]
    pub artificial_start_delay: u64,
    #[structopt(
        long = "grpc-timeout",
        help = "Time (in seconds) for gRPC request timeouts",
        default_value = "30",
        env = "CONCORDIUM_NODE_COLLECTOR_GRPC_TIMEOUT"
    )]
    pub grpc_timeout:           u64,
    #[cfg(target_os = "macos")]
    #[structopt(
        long = "use-mac-log",
        help = "Enable native logging on macOS by providing a subsystem name, e.g. \
                'software.concordium.mainnet.node'. This disables the normal logging system and \
                is incompatible with '--log-config'. Log messages can be found via Console.app or \
                the log commandline tool by searching for the subsystem.",
        env = "CONCORDIUM_NODE_COLLECTOR_USE_MAC_LOG",
        conflicts_with = "log-config"
    )]
    pub use_mac_log:            Option<String>,
}

#[tokio::main]
async fn main() {
    let conf = ConfigCli::from_args();

    #[cfg(target_os = "macos")]
    match conf.use_mac_log {
        Some(ref subsystem) => setup_macos_logger(conf.trace, conf.debug, subsystem),
        None => setup_logger(conf.trace, conf.debug, conf.no_log_timestamp),
    };
    #[cfg(not(target_os = "macos"))]
    setup_logger(conf.trace, conf.debug, conf.no_log_timestamp);

    if conf.print_config {
        info!("{:?}", conf);
    }

    info!("Starting up node-collector version {}!", env!("CARGO_PKG_VERSION"));

    if conf.node_names.len() != conf.grpc_hosts.len() {
        error!("{:?}, {:?}", conf.node_names, conf.grpc_hosts);
        error!("The number of node-names and grpc-hosts must be equal!");
        exit(1);
    }

    if conf.artificial_start_delay > 0 {
        info!("Delaying first collection from the node for {} ms", conf.artificial_start_delay);
        tokio::time::sleep(Duration::from_millis(conf.artificial_start_delay)).await;
    }

    let mut interval = tokio::time::interval(Duration::from_millis(conf.collector_interval));
    // If for some reason we cannot submit the statistics for a given period, we
    // skip it. This also handles cases such as when a computer goes to sleep. By
    // default the behaviour is [MissedTickBehaviour::Burst] which would,
    // in such a case, try to make up the missed ticks by submitting statistics as
    // quickly as possible.
    interval.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
    loop {
        interval.tick().await;
        for (node_name, grpc_host) in conf.node_names.iter().zip(conf.grpc_hosts.iter()) {
            trace!("Processing node {}/{}", node_name, grpc_host);
            match collect_data(node_name.clone(), grpc_host.to_owned(), &conf).await {
                Ok(node_info) => {
                    trace!("Node data collected successfully from {}/{}", node_name, grpc_host);
                    match rmp_serde::encode::to_vec(&node_info) {
                        Ok(msgpack) => {
                            let client_builder = reqwest::Client::builder()
                                .connect_timeout(Duration::from_millis(conf.collector_interval))
                                .timeout(Duration::from_millis(conf.collector_interval));
                            let client = match client_builder.build() {
                                Ok(client) => client,
                                Err(e) => {
                                    error!("Error constructing a network client: {}", e);
                                    continue;
                                }
                            };
                            match client.post(&conf.collector_url).body(msgpack).send().await {
                                Ok(v) if v.status().is_success() => {
                                    trace!("Payload sent successfully to collector backend.")
                                }
                                Ok(v) => error!(
                                    "Error sending payload to the collector backend. Status code: \
                                     {}",
                                    v.status()
                                ),
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
                    error!(
                        "gRPC failed with \"{}\" for {}, sleeping for {} ms",
                        e, &grpc_host, conf.collector_interval
                    );
                }
            }
        }
    }
}

#[allow(clippy::cognitive_complexity)]
async fn collect_data<'a>(
    node_name: NodeName,
    grpc_host: String,
    conf: &ConfigCli,
) -> anyhow::Result<NodeInfo> {
    info!("Collecting node information via gRPC from {}/{}", node_name, grpc_host);

    // Setting up Client
    let grpc_timeout = conf.grpc_timeout;
    let channel = Channel::from_shared(grpc_host)?.timeout(Duration::from_secs(grpc_timeout));
    let mut client = grpc::queries_client::QueriesClient::connect(channel.clone()).await?;

    // Blocks
    let best = grpc::BlockHashInput {
        block_hash_input: Some(grpc::block_hash_input::BlockHashInput::Best(grpc::Empty {})),
    };
    let last_final = grpc::BlockHashInput {
        block_hash_input: Some(grpc::block_hash_input::BlockHashInput::LastFinal(grpc::Empty {})),
    };

    // Client calls
    let node_info = client.get_node_info(grpc::Empty {}).await?.into_inner();
    let peers = client.get_peers_info(grpc::Empty {}).await?.into_inner().peers;
    let consensus = client.get_consensus_info(grpc::Empty {}).await?.into_inner();
    let best_block = client.get_block_info(best.clone()).await?.into_inner();
    let finalized_block = client.get_block_info(last_final).await?.into_inner();
    let tokenomics_info =
        client.get_tokenomics_info(get_block_hash_input(best_block.hash)?).await?.into_inner();

    // Helper variables
    let latencies = peers
        .iter()
        .map(|p| Ok(p.network_stats.req()?.latency))
        .collect::<anyhow::Result<Vec<u64>>>()?;
    let valid_latencies: Vec<u64> = latencies.clone().into_iter().filter(|x| *x != 0).collect();
    let latency_sum = valid_latencies.iter().sum::<u64>() as f64;

    let peer_type = match node_info.details.req()? {
        NodeDetails::Bootstrapper(_) => "Bootstrapper",
        NodeDetails::Node(_) => "Node",
    };
    let peer_list = peers
        .iter()
        .map(|p| Ok(p.peer_id.req()?.value.clone()))
        .collect::<anyhow::Result<Vec<_>>>()?;

    let (consensus_is_running, baker_id, committee_info, final_committee_info) =
        get_node_baker_info(&node_info)?;

    let (total_amount, total_encrypted_amount, foundation_amount) =
        match tokenomics_info.tokenomics.req()?.clone() {
            Tokenomics::V0(t) => (t.total_amount, t.total_encrypted_amount, None),
            Tokenomics::V1(t) => {
                (t.total_amount, t.total_encrypted_amount, t.foundation_transaction_rewards)
            }
        };

    let ancestors_since_best_block = {
        let best_block_height = consensus.best_block_height.req()?.value;
        let last_finalized_block_height = consensus.last_finalized_block_height.req()?.value;
        if best_block_height > last_finalized_block_height {
            let height_diff = best_block_height - last_finalized_block_height;

            let req = grpc::AncestorsRequest {
                block_hash: Some(best),
                amount:     height_diff,
            };
            let ancestors = client.get_ancestors(req).await?.into_inner();
            let hex_ancestors: Vec<String> =
                ancestors.map_ok(|x| hex::encode(x.value)).try_collect().await?;
            Some(hex_ancestors)
        } else {
            None
        }
    };

    Ok(NodeInfo {
        nodeName: node_name.to_string(),
        nodeId: node_info.network_info.req()?.node_id.req()?.value.clone(),
        peerType: peer_type.to_string(),
        uptime: node_info.peer_uptime.req()?.value,
        client: node_info.peer_version,
        averagePing: checked_div(latency_sum, valid_latencies.len() as f64),
        peersCount: peers.len() as u64,
        peersList: peer_list,
        bestBlock: hash_to_hex(consensus.best_block)?,
        bestBlockHeight: consensus.best_block_height.req()?.value,
        bestBlockBakerId: best_block.baker.map(|id| id.value),
        bestArrivedTime: Some(from_unix(best_block.arrive_time.req()?.clone())?),
        blockArrivePeriodEMA: consensus.block_arrive_period_ema,
        blockArrivePeriodEMSD: consensus.block_arrive_period_emsd,
        blockArriveLatencyEMA: Some(consensus.block_arrive_latency_ema),
        blockArriveLatencyEMSD: Some(consensus.block_arrive_latency_emsd),
        blockReceivePeriodEMA: consensus.block_receive_period_ema,
        blockReceivePeriodEMSD: consensus.block_receive_period_emsd,
        blockReceiveLatencyEMA: Some(consensus.block_receive_latency_ema),
        blockReceiveLatencyEMSD: Some(consensus.block_receive_latency_emsd),
        finalizedBlock: hash_to_hex(consensus.last_finalized_block)?,
        finalizedBlockHeight: consensus.last_finalized_block_height.req()?.value,
        finalizedTime: consensus.last_finalized_time.map(from_unix).transpose()?,
        finalizationPeriodEMA: consensus.finalization_period_ema,
        finalizationPeriodEMSD: consensus.finalization_period_emsd,
        packetsSent: node_info.network_info.req()?.peer_total_sent,
        packetsReceived: node_info.network_info.req()?.peer_total_received,
        consensusRunning: consensus_is_running,
        bakingCommitteeMember: committee_info,
        consensusBakerId: baker_id,
        finalizationCommitteeMember: final_committee_info,
        ancestorsSinceBestBlock: ancestors_since_best_block,
        stagingNetUsername: None,
        last_updated: 0,
        transactionsPerBlockEMA: Some(consensus.transactions_per_block_ema),
        transactionsPerBlockEMSD: Some(consensus.transactions_per_block_emsd),
        bestBlockTransactionsSize: Some(best_block.transactions_size.into()),
        bestBlockTotalEncryptedAmount: Some(total_encrypted_amount.req()?.value),
        bestBlockTotalAmount: Some(total_amount.req()?.value),
        bestBlockTransactionCount: Some(best_block.transaction_count.into()),
        bestBlockTransactionEnergyCost: Some(best_block.transactions_energy_cost.req()?.value),
        bestBlockExecutionCost: None,
        bestBlockCentralBankAmount: foundation_amount.map(|x| x.value),
        blocksReceivedCount: Some(consensus.blocks_received_count.into()),
        blocksVerifiedCount: Some(consensus.blocks_verified_count.into()),
        finalizationCount: Some(consensus.finalization_count.into()),
        genesisBlock: hash_to_hex(consensus.genesis_block)?,
        averageBytesPerSecondIn: node_info.network_info.req()?.avg_bps_in,
        averageBytesPerSecondOut: node_info.network_info.req()?.avg_bps_out,
        finalizedBlockParent: hash_to_hex(finalized_block.parent_block)?,
    })
}

// Helper functions and helper traits

/// Return whether the consensus is running, the baker id if running as a baker,
/// whether the node is in a baking committee, and whether the node is in the
/// finalization committee.
fn get_node_baker_info(
    node_info: &grpc::NodeInfo,
) -> anyhow::Result<(bool, Option<u64>, IsInBakingCommittee, bool)> {
    use IsInBakingCommittee::*;
    Ok(match &node_info.details.req()? {
        NodeDetails::Bootstrapper(_) => (false, None, NotInCommittee, false),
        NodeDetails::Node(node) => match &node.consensus_status.req()? {
            ConsensusStatus::NotRunning(_) => (false, None, NotInCommittee, false),
            ConsensusStatus::Passive(_) => (true, None, NotInCommittee, false),
            ConsensusStatus::Active(consensus_info) => {
                let baker_id = consensus_info.baker_id.req()?.value;
                let baker_status = consensus_info.status.req()?;
                let committee_status = match baker_status {
                    BakerStatus::PassiveCommitteeInfo(info) => match info {
                        0 => IsInBakingCommittee::NotInCommittee,
                        1 => IsInBakingCommittee::AddedButNotActiveInCommittee,
                        2 => IsInBakingCommittee::AddedButWrongKeys,
                        _ => bail!("Invalid PassiveCommitteeInfo received from Node"),
                    },
                    _ => IsInBakingCommittee::ActiveInCommittee,
                };
                let finalization_committee =
                    matches!(baker_status, BakerStatus::ActiveFinalizerCommitteeInfo(_));
                (true, Some(baker_id), committee_status, finalization_committee)
            }
        },
    })
}

// Helper functions and helper traits

/// Converts a GRPC timestamp to a string
fn from_unix(timestamp: grpc::Timestamp) -> anyhow::Result<String> {
    let unix_time: i64 = timestamp.value.try_into()?;
    match chrono::Utc.timestamp_millis_opt(unix_time).single() {
        Some(t) => Ok(t.to_rfc3339_opts(SecondsFormat::Nanos, true)),
        None => bail!("Invalid timestamp received from Node"),
    }
}

/// Gets a BlockHashInput from an optional blockhash option. Will panic if input
/// is None
fn get_block_hash_input(hash_opt: Option<grpc::BlockHash>) -> anyhow::Result<grpc::BlockHashInput> {
    let hash = hash_opt.ok_or_else(|| anyhow!("Invalid hash received from Node"))?;
    Ok(grpc::BlockHashInput {
        block_hash_input: Some(grpc::block_hash_input::BlockHashInput::Given(hash)),
    })
}

/// A checked division. Returns None on division by zero.
fn checked_div(a: f64, b: f64) -> Option<f64> {
    let div = a / b;
    if !div.is_nan() {
        Some(div)
    } else {
        None
    }
}

/// Turns a blockhash into a hex-encoded string
fn hash_to_hex(x: Option<grpc::BlockHash>) -> anyhow::Result<String> {
    Ok(hex::encode(x.req()?.value.clone()))
}

/// Defining a "Require trait". Helps make sure fields are present, otherwise
/// throws error
trait Require {
    type A;
    fn req(&self) -> Result<&Self::A, anyhow::Error>;
}

impl<A> Require for Option<A> {
    type A = A;

    fn req(&self) -> anyhow::Result<&Self::A> {
        match self {
            Some(v) => Ok(v),
            None => bail!("Missing field in response from Node"),
        }
    }
}

/// Parsing user NodeName input.
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

// Loggers

/// Sets up a logger that logs to stderr.
pub fn setup_logger(trace: bool, debug: bool, no_log_timestamp: bool) {
    let env = if trace {
        Env::default().filter_or("LOG_LEVEL", "trace")
    } else if debug {
        Env::default().filter_or("LOG_LEVEL", "debug")
    } else {
        Env::default().filter_or("LOG_LEVEL", "info")
    };

    let mut log_builder = Builder::from_env(env);
    if no_log_timestamp {
        log_builder.format_timestamp(None);
    } else {
        log_builder.format(|buf, record| {
            writeln!(buf, "{}: {}: {}", buf.timestamp_nanos(), record.level(), record.args())
        });
    }
    log_builder.filter(Some("tokio_reactor"), LevelFilter::Error);
    log_builder.filter(Some("hyper"), LevelFilter::Error);
    log_builder.filter(Some("reqwest"), LevelFilter::Error);
    log_builder.filter(Some("gotham"), LevelFilter::Error);
    log_builder.filter(Some("h2"), LevelFilter::Error);
    log_builder.init();
}

/// Sets up a logger for the macOS syslog which logs with the provided
/// subsystem name.
#[cfg(target_os = "macos")]
pub fn setup_macos_logger(trace: bool, debug: bool, subsystem: &str) {
    // NB: Timestamps and levels are included automatically. No need to encode them
    // in the message.
    let level_filter = if trace {
        LevelFilter::Trace
    } else if debug {
        LevelFilter::Debug
    } else {
        LevelFilter::Info
    };

    macos_logger_wrapper::MacOsLogger::new(subsystem)
        .level_filter(level_filter)
        .category_level_filter("tokio_reactor", LevelFilter::Error)
        .category_level_filter("hyper", LevelFilter::Error)
        .category_level_filter("reqwest", LevelFilter::Error)
        .category_level_filter("h2", LevelFilter::Error)
        .init()
        .expect("Failed to initialise MacOsLogger");
}
