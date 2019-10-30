#![recursion_limit = "1024"]
#[cfg(not(target_os = "windows"))]
extern crate grpciounix as grpcio;
#[cfg(target_os = "windows")]
extern crate grpciowin as grpcio;
use concordium_common::spawn_or_die;
use env_logger::Env;
use failure::Fallible;
use grpcio::{ChannelBuilder, EnvBuilder};
use p2p_client::{
    common::collector_utils::NodeInfo, proto::concordium_p2p_rpc_grpc::P2PClient,
    utils::setup_logger_env,
};
use serde_json::Value;
use std::{
    borrow::ToOwned,
    default::Default,
    fmt,
    process::exit,
    str::FromStr,
    sync::{
        atomic::{AtomicUsize, Ordering as AtomicOrdering},
        Arc,
    },
    thread,
    time::Duration,
};
use structopt::StructOpt;
#[macro_use]
extern crate log;

// Explicitly defining allocator to avoid future reintroduction of jemalloc
use std::alloc::System;
#[global_allocator]
static A: System = System;

#[derive(Debug)]
struct NodeName(Vec<String>);

impl FromStr for NodeName {
    type Err = failure::Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Ok(Self(
            input
                .split_whitespace()
                .map(ToOwned::to_owned)
                .collect::<Vec<String>>(),
        ))
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
        default_value = "rpcadmin"
    )]
    pub grpc_auth_token: String,
    #[structopt(
        long = "grpc-host",
        help = "gRPC host to collect from",
        default_value = "127.0.0.1:10000"
    )]
    pub grpc_hosts: Vec<String>,
    #[structopt(long = "node-name", help = "Node name")]
    pub node_names: Vec<NodeName>,
    #[structopt(
        long = "collector-url",
        help = "Alias submitted of the node collected from",
        default_value = "http://localhost:3000/post/nodes"
    )]
    pub collector_url: String,
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
    #[structopt(
        long = "collect-interval",
        help = "Interval in miliseconds to sleep between runs of the collector",
        default_value = "5000"
    )]
    pub collector_interval: u64,
    #[structopt(
        long = "artificial-start-delay",
        help = "Time (in ms) to delay when the first gRPC request is sent to the node",
        default_value = "3000"
    )]
    pub artificial_start_delay: u64,
    #[structopt(
        long = "max-grpc-failures-allowed",
        help = "Maximum allowed times a gRPC call can fail before terminating the program",
        default_value = "50"
    )]
    pub max_grpc_failures_allowed: u64,
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

    setup_logger_env(env, conf.no_log_timestamp);

    if conf.print_config {
        info!("{:?}", conf);
    }

    info!(
        "Starting up {}-node-collector version {}!",
        p2p_client::APPNAME,
        p2p_client::VERSION
    );

    if conf.node_names.len() != conf.grpc_hosts.len() {
        error!("Amount of node-names and grpc-hosts must be equal!");
        exit(1);
    }

    #[allow(unreachable_code)]
    let main_thread = spawn_or_die!("Main loop", {
        let atomic_counter: AtomicUsize = Default::default();
        loop {
            let grpc_failure_count = atomic_counter.load(AtomicOrdering::Relaxed);
            trace!(
                "Failure count is {}/{}",
                grpc_failure_count,
                conf.max_grpc_failures_allowed
            );
            conf.node_names.iter().zip(conf.grpc_hosts.iter()).for_each(
                |(node_name, grpc_host)| {
                    trace!("Processing node {}/{}", node_name, grpc_host);
                    if let Ok(node_info) =
                        collect_data(&node_name, &grpc_host, &conf.grpc_auth_token)
                    {
                        trace!(
                            "Node data collected successfully from {}/{}",
                            node_name,
                            grpc_host
                        );
                        if let Ok(json_string) = serde_json::to_string(&node_info) {
                            trace!("Posting JSON {}", json_string);
                            let client = reqwest::Client::new();
                            if let Err(e) = client.post(&conf.collector_url).json(&node_info).send()
                            {
                                error!("Could not post to dashboard server due to error {}", e);
                            }
                        }
                    } else {
                        let _ = atomic_counter.fetch_add(1, AtomicOrdering::SeqCst);
                        error!(
                            "gRPC failed for {}, sleeping for {} ms",
                            &grpc_host, conf.collector_interval
                        );
                    }

                    if grpc_failure_count + 1 >= conf.max_grpc_failures_allowed as usize {
                        error!("Too many gRPC failures, exiting!");
                        exit(1);
                    }
                },
            );
            trace!("Sleeping for {} ms", conf.collector_interval);
            thread::sleep(Duration::from_millis(conf.collector_interval));
        }
    });
    main_thread.join().expect("Main thread panicked");
    Ok(())
}

fn collect_data(
    node_name: &NodeName,
    grpc_host: &str,
    grpc_auth_token: &str,
) -> Fallible<NodeInfo> {
    trace!(
        "Collecting node information via gRPC from {}/{}/{}",
        node_name,
        grpc_host,
        grpc_auth_token
    );
    let env = Arc::new(EnvBuilder::new().build());
    let ch = ChannelBuilder::new(env).connect(grpc_host);
    let client = P2PClient::new(ch);

    let mut req_meta_builder = ::grpcio::MetadataBuilder::new();
    req_meta_builder
        .add_str("Authentication", grpc_auth_token)
        .unwrap();
    let meta_data = req_meta_builder.build();
    let call_options = ::grpcio::CallOption::default().headers(meta_data);

    trace!("Requesting basic node info via gRPC");
    let node_info_reply =
        client.node_info_opt(&p2p_client::proto::Empty::new(), call_options.clone())?;

    trace!("Requesting node uptime info via gRPC");
    let node_uptime_reply =
        client.peer_uptime_opt(&p2p_client::proto::Empty::new(), call_options.clone())?;

    trace!("Requesting node version info via gRPC");
    let node_version_reply =
        client.peer_version_opt(&p2p_client::proto::Empty::new(), call_options.clone())?;

    trace!("Requesting consensus statuc info via gRPC");
    let node_consensus_status_reply =
        client.get_consensus_status_opt(&p2p_client::proto::Empty::new(), call_options.clone())?;

    trace!("Requesting node peer stats info via gRPC");
    let node_peer_stats_reply = client.peer_stats_opt(
        &p2p_client::proto::PeersRequest::new(),
        call_options.clone(),
    )?;

    trace!("Requesting node total sent message count info via gRPC");
    let node_total_sent_reply =
        client.peer_total_sent_opt(&p2p_client::proto::Empty::new(), call_options.clone())?;

    trace!(" Requesting node total received message count via gRPC");
    let node_total_received_reply =
        client.peer_total_received_opt(&p2p_client::proto::Empty::new(), call_options.clone())?;

    let node_id = node_info_reply.get_node_id().get_value().to_owned();
    let beta_username = if node_info_reply.has_beta_username() {
        Some(node_info_reply.get_beta_username().get_value().to_owned())
    } else {
        None
    };
    let peer_type = node_info_reply.get_peer_type().to_owned();
    let baker_committee = node_info_reply.get_consensus_baker_committee();
    let finalization_committee = node_info_reply.get_consensus_finalizer_committee();
    let consensus_running = node_info_reply.get_consensus_running();
    let uptime = node_uptime_reply.get_value() as f64;
    let version = node_version_reply.get_value().to_owned();
    let packets_sent = node_total_sent_reply.get_value() as f64;
    let packets_received = node_total_received_reply.get_value() as f64;

    let peer_stats = node_peer_stats_reply.get_peerstats();
    let peers_summed_latency = peer_stats
        .iter()
        .map(|element| element.get_measured_latency())
        .sum::<u64>() as f64;
    let peers_with_valid_latencies_count = peer_stats
        .iter()
        .filter(|element| element.get_valid_latency())
        .count();

    let average_ping = if peers_with_valid_latencies_count > 0 {
        Some(peers_summed_latency / peers_with_valid_latencies_count as f64)
    } else {
        None
    };
    let peers_count = peer_stats.len() as f64;
    let peers_list = peer_stats
        .iter()
        .map(|element| element.node_id.clone())
        .collect::<Vec<String>>();

    trace!("Parsing consensus JSON status response");
    let json_consensus_value: Value =
        serde_json::from_str(&node_consensus_status_reply.json_value)?;

    let best_block = json_consensus_value["bestBlock"]
        .as_str()
        .unwrap()
        .to_owned();
    let best_block_height = json_consensus_value["bestBlockHeight"].as_f64().unwrap();
    let best_arrived_time = if json_consensus_value["blockLastArrivedTime"].is_string() {
        Some(
            json_consensus_value["blockLastArrivedTime"]
                .as_str()
                .unwrap()
                .to_owned(),
        )
    } else {
        None
    };
    let block_arrive_period_ema = json_consensus_value["blockArrivePeriodEMA"].as_f64();
    let block_arrive_period_emsd = json_consensus_value["blockArrivePeriodEMSD"].as_f64();
    let finalized_block = json_consensus_value["lastFinalizedBlock"]
        .as_str()
        .unwrap()
        .to_owned();
    let finalized_block_height = json_consensus_value["lastFinalizedBlockHeight"]
        .as_f64()
        .unwrap();
    let finalized_time = if json_consensus_value["lastFinalizedTime"].is_string() {
        Some(
            json_consensus_value["lastFinalizedTime"]
                .as_str()
                .unwrap()
                .to_owned(),
        )
    } else {
        None
    };
    let finalization_period_ema = json_consensus_value["finalizationPeriodEMA"].as_f64();
    let finalization_period_emsd = json_consensus_value["finalizationPeriodEMSD"].as_f64();

    let ancestors_since_best_block = if best_block_height > finalized_block_height {
        trace!("Requesting further consensus status via gRPC");
        let block_and_height_req = &mut p2p_client::proto::BlockHashAndAmount::new();
        block_and_height_req.set_block_hash(best_block.clone());
        block_and_height_req.set_amount(best_block_height as u64 - finalized_block_height as u64);
        let node_ancestors_reply =
            client.get_ancestors_opt(block_and_height_req, call_options.clone())?;
        let json_consensus_ancestors_value: Value =
            serde_json::from_str(&node_ancestors_reply.json_value)?;
        if json_consensus_ancestors_value.is_array() {
            if let Some(ancestors_arr) = json_consensus_ancestors_value.as_array() {
                Some(
                    ancestors_arr
                        .iter()
                        .map(|value| value.as_str().unwrap().to_owned())
                        .collect::<Vec<String>>(),
                )
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
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
        bestArrivedTime: best_arrived_time,
        blockArrivePeriodEMA: block_arrive_period_ema,
        blockArrivePeriodEMSD: block_arrive_period_emsd,
        finalizedBlock: finalized_block,
        finalizedBlockHeight: finalized_block_height,
        finalizedTime: finalized_time,
        finalizationPeriodEMA: finalization_period_ema,
        finalizationPeriodEMSD: finalization_period_emsd,
        packetsSent: packets_sent,
        packetsReceived: packets_received,
        consensusRunning: consensus_running,
        bakingCommitteeMember: baker_committee,
        finalizationCommitteeMember: finalization_committee,
        ancestorsSinceBestBlock: ancestors_since_best_block,
        betaUsername: beta_username,
        last_updated: 0,
    })
}
