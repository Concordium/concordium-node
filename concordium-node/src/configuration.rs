//! The client's parameters and constants used by other modules.

use crate::{
    connection::DeduplicationHashAlgorithm,
    network::{WireProtocolVersion, WIRE_PROTOCOL_VERSION},
};
use app_dirs2::*;
use failure::Fallible;
use preferences::{Preferences, PreferencesMap};
use std::{
    fs::{File, OpenOptions},
    io::{BufReader, BufWriter, Write},
    path::{Path, PathBuf},
};
use structopt::{clap::AppSettings, StructOpt};

/// Client's details for local directory setup purposes.
pub const APP_INFO: AppInfo = AppInfo {
    name:   "concordium",
    author: "concordium",
};

/// Check that the 'other' version is compatible with our version. This is by
/// default permissive, and future versions should be mindful to disallow older
/// incompatible ones.
/// When we reach version 1 we should stick to major versions being for breaking
/// changes.
pub(crate) fn is_compatible_version(other: &semver::Version) -> bool {
    other.major == 0 && other.minor == 5
}

/// Check that the other wire version is compatible with ours. See
/// `network::WIRE_PROTOCOL_VERSION`. For now it only checks if there is a
/// matching version because we only have version 0. In the future, a node
/// should support several versions and it should find the highest matching
/// version that can be used.
pub(crate) fn is_compatible_wire_version(
    other: &[WireProtocolVersion],
) -> Option<WireProtocolVersion> {
    other.iter().find(|x| **x == WIRE_PROTOCOL_VERSION).copied()
}

/// The maximum size of objects accepted from the network.
pub const PROTOCOL_MAX_MESSAGE_SIZE: u32 = 20_971_520; // 20 MIB

const APP_PREFERENCES_MAIN: &str = "main.config";
const APP_PREFERENCES_KEY_VERSION: &str = "VERSION";
/// Used for a persistent node id setup.
pub const APP_PREFERENCES_PERSISTED_NODE_ID: &str = "PERSISTED_NODE_ID";

/// Maximum time allowed for a peer to catch up with, in milliseconds.
pub const MAX_CATCH_UP_TIME: u64 = 300_000;

// dump queue depths
#[cfg(feature = "network_dump")]
pub const DUMP_QUEUE_DEPTH: usize = 100;
#[cfg(feature = "network_dump")]
pub const DUMP_SWITCH_QUEUE_DEPTH: usize = 0;

// connection-related consts
/// Maximum time (in ms) a node's connection can remain unreachable.
pub const UNREACHABLE_EXPIRATION_SECS: u64 = 86_400;
/// Maximum time (in ms) a bootstrapper can hold a connection to a node.
pub const MAX_BOOTSTRAPPER_KEEP_ALIVE: u64 = 20_000;
/// Maximum time (in ms) a node can hold an inactive connection to a peer.
pub const MAX_NORMAL_KEEP_ALIVE: u64 = 1_200_000;
/// Maximum time (in ms) a connection can be kept without concluding a
/// handshake.
pub const MAX_PREHANDSHAKE_KEEP_ALIVE: u64 = 10_000;
/// Maximum time (in s) a soft ban is in force.
pub const SOFT_BAN_DURATION_SECS: u64 = 300;
/// Maximum number of networks a peer can share
pub const MAX_PEER_NETWORKS: usize = 20;
/// Database subdirectory name
pub const DATABASE_SUB_DIRECTORY_NAME: &str = "database-v4";

#[cfg(feature = "database_emitter")]
#[derive(StructOpt, Debug)]
/// Parameters related to the database emitter.
pub struct DatabaseEmitterConfig {
    #[structopt(long = "import-file", help = "File to import from")]
    pub import_file: String,

    #[structopt(
        long = "batches-delay",
        help = "Delay between batches in miliseconds",
        default_value = "2000"
    )]
    pub delay_between_batches: u64,

    #[structopt(long = "batch-size", help = "Size of each batch to emit", default_value = "40")]
    pub batch_sizes: u64,

    #[structopt(
        long = "skip-first",
        help = "Amount of the initial blocks to skip",
        default_value = "0"
    )]
    pub skip_first: u64,
}

#[cfg(feature = "instrumentation")]
#[derive(StructOpt, Debug)]
/// Parameters related to Prometheus.
pub struct PrometheusConfig {
    #[structopt(
        long = "prometheus-listen-addr",
        help = "IP to listen for prometheus requests on",
        default_value = "127.0.0.1"
    )]
    pub prometheus_listen_addr: String,
    #[structopt(
        long = "prometheus-listen-port",
        help = "Port for prometheus to listen on",
        default_value = "9090"
    )]
    pub prometheus_listen_port: u16,
    #[structopt(long = "prometheus-server", help = "Enable prometheus server for metrics")]
    pub prometheus_server: bool,
    #[structopt(long = "prometheus-push-gateway", help = "Enable prometheus via push gateway")]
    pub prometheus_push_gateway: Option<String>,
    #[structopt(
        long = "prometheus-job-name",
        help = "Job name to send to push gateway",
        default_value = "p2p_node_push"
    )]
    pub prometheus_job_name: String,
    #[structopt(long = "prometheus-instance-name", help = "If not present node_id will be used")]
    pub prometheus_instance_name: Option<String>,
    #[structopt(
        long = "prometheus-push-gateway-username",
        help = "Username to use for push gateway, if either username or password is omitted \
                authentication isn't used"
    )]
    pub prometheus_push_username: Option<String>,
    #[structopt(
        long = "prometheus-push-gateway-password",
        help = "Password to use for push gateway, if either username or password is omitted \
                authentication isn't used"
    )]
    pub prometheus_push_password: Option<String>,
    #[structopt(
        long = "prometheus-push-gateway-interval",
        help = "Interval in seconds between pushes",
        default_value = "2"
    )]
    pub prometheus_push_interval: u64,
}

#[derive(StructOpt, Debug)]
/// Parameters related to Baking (only used in cli).
pub struct BakerConfig {
    #[cfg(feature = "profiling")]
    #[structopt(
        long = "heap-profiling",
        help = "Profile the heap [(`cost`,-hc), (`type`, -hy), (`module`, -hm), (`description`, \
                -hd)] in the Haskell subsystem",
        default_value = "none"
    )]
    pub heap_profiling: String,
    #[cfg(feature = "profiling")]
    #[structopt(long = "time-profiling", help = "Profile the time in the Haskell subsystem")]
    pub time_profiling: bool,
    #[cfg(feature = "profiling")]
    #[structopt(
        long = "backtraces",
        help = "Show bactraces generated by exceptions in the Haskell subsystem"
    )]
    pub backtraces_profiling: bool,
    #[cfg(feature = "profiling")]
    #[structopt(
        long = "stack-profiling",
        help = "Include memory occupied by threads in the heap profile. Only has effect if \
                `heap-profiling` is enabled."
    )]
    pub stack_profiling: bool,
    #[cfg(feature = "profiling")]
    #[structopt(
        long = "profiling-sampling-interval",
        help = "Profile sampling interval in seconds",
        default_value = "0.1"
    )]
    pub profiling_sampling_interval: String,
    #[structopt(long = "haskell-gc-logging", help = "Enable Haskell garbage collection logging")]
    pub gc_logging: Option<String>,
    #[structopt(
        long = "haskell-rts-flags",
        help = "Haskell RTS flags to pass to consensus.",
        default_value = ""
    )]
    pub rts_flags: Vec<String>,
    #[structopt(
        long = "maximum-block-size",
        help = "Maximum block size in bytes",
        default_value = "12582912"
    )]
    pub maximum_block_size: u32,
    #[structopt(
        long = "transaction-insertions-before-purge",
        help = "Number of transaction insertions between purges on the transaction table",
        default_value = "1000"
    )]
    pub transaction_insertions_before_purge: u32,
    #[structopt(
        long = "transaction-keep-alive",
        help = "Time during which a transaction can not be purged in seconds",
        default_value = "600"
    )]
    pub transaction_keep_alive: u32,
    #[structopt(
        long = "transactions-purging-delay",
        help = "Time between automatic transaction table purging runs in seconds",
        default_value = "300"
    )]
    pub transactions_purging_delay: u32,
    #[structopt(
        long = "scheduler-outcome-logging",
        help = "Enable outcome of finalized baked blocks from the scheduler"
    )]
    pub scheduler_outcome_logging: bool,
    #[structopt(
        long = "import-blocks-from",
        help = "Path to a file exported by the database exporter"
    )]
    pub import_path: Option<String>,
    #[structopt(long = "baker-credentials-file", help = "Path to the baker credentials file")]
    pub baker_credentials_file: Option<PathBuf>,
    #[structopt(
        long = "decrypt-baker-credentials",
        help = "Indicate that the baker credentials are provided encrypted and thus need to be \
                decrypted."
    )]
    pub decrypt_baker_credentials: bool,
}

#[derive(StructOpt, Debug)]
/// Parameters related to the RPC (only used in cli).
pub struct RpcCliConfig {
    #[structopt(long = "no-rpc-server", help = "Disable the built-in RPC server")]
    pub no_rpc_server: bool,
    #[structopt(long = "rpc-server-port", help = "RPC server port", default_value = "10000")]
    pub rpc_server_port: u16,
    #[structopt(
        long = "rpc-server-addr",
        help = "RPC server listen address",
        default_value = "127.0.0.1"
    )]
    pub rpc_server_addr: String,
    #[structopt(
        long = "rpc-server-token",
        help = "RPC server access token",
        default_value = "rpcadmin"
    )]
    pub rpc_server_token: String,
}

#[derive(StructOpt, Debug)]
/// Parameters related to connections.
pub struct ConnectionConfig {
    #[structopt(
        long = "desired-nodes",
        help = "Desired nodes to always have",
        default_value = "7"
    )]
    pub desired_nodes: u16,
    #[structopt(long = "max-allowed-nodes", help = "Maximum nodes to allow a connection to")]
    pub max_allowed_nodes: Option<u16>,
    #[structopt(
        long = "max-allowed-nodes-percentage",
        help = "Maximum nodes to allow a connection to is set as a percentage of desired-nodes \
                (minimum 100, to set it to desired-nodes",
        default_value = "150"
    )]
    pub max_allowed_nodes_percentage: u16,
    #[structopt(long = "no-bootstrap", help = "Do not bootstrap via DNS")]
    pub no_bootstrap_dns: bool,
    #[structopt(
        long = "relay-broadcast-percentage",
        help = "The percentage of peers to relay broadcasted messages to",
        default_value = "1.0"
    )]
    pub relay_broadcast_percentage: f64,
    #[structopt(
        long = "bootstrap-server",
        help = "DNS name to resolve bootstrap nodes from",
        default_value = "bootstrap.p2p.concordium.com"
    )]
    pub bootstrap_server: String,
    #[structopt(
        long = "connect-to",
        short = "c",
        help = "Peer to connect to upon startup (host/ip:port)"
    )]
    pub connect_to: Vec<String>,
    #[structopt(
        long = "no-dnssec",
        help = "Do not perform DNSsec tests for lookups. If flag is set, then no DNSSEC \
                validation will be performed"
    )]
    pub dnssec_disabled: bool,
    #[structopt(long = "dns-resolver", help = "DNS resolver to use")]
    pub dns_resolver: Vec<String>,
    #[structopt(
        long = "bootstrap-node",
        help = "Bootstrap nodes to use upon startup host/ip:port (this disables DNS bootstrapping)"
    )]
    pub bootstrap_nodes: Vec<String>,
    #[structopt(
        long = "resolv-conf",
        help = "Location of resolv.conf",
        default_value = "/etc/resolv.conf"
    )]
    pub resolv_conf: String,
    #[structopt(
        long = "housekeeping-interval",
        help = "The connection housekeeping interval in seconds",
        default_value = "30"
    )]
    pub housekeeping_interval: u64,
    #[structopt(
        long = "bootstrapping-interval",
        help = "The bootstrapping interval in seconds",
        default_value = "7200"
    )]
    pub bootstrapping_interval: u64,
    #[structopt(long = "max-latency", help = "The maximum allowed connection latency in ms")]
    pub max_latency: Option<u64>,
    #[structopt(
        long = "hard-connection-limit",
        help = "Maximum connections to keep open at any time",
        default_value = "50"
    )]
    pub hard_connection_limit: u16,
    #[structopt(
        long = "catch-up-batch-limit",
        help = "The maximum batch size for a catch-up round.",
        default_value = "50"
    )]
    pub catch_up_batch_limit: i64,
    #[structopt(
        long = "thread-pool-size",
        help = "The size of the threadpool processing connection events in parallel",
        default_value = "4"
    )]
    pub thread_pool_size: usize,
    #[structopt(
        long = "dedup-size-long",
        help = "The size of the long deduplication queues",
        default_value = "65536"
    )]
    pub dedup_size_long: usize,
    #[structopt(
        long = "dedup-size-short",
        help = "The size of the short deduplication queues",
        default_value = "4096"
    )]
    pub dedup_size_short: usize,
    #[structopt(
        long = "socket-write-size",
        help = "The desired size of single socket writes; must be no bigger than socket_read_size",
        default_value = "16384"
    )]
    pub socket_write_size: usize,
    #[structopt(
        long = "socket-read-size",
        help = "The desired size of single socket reads; must be >= 65535 (max noise message size)",
        default_value = "131072"
    )]
    pub socket_read_size: usize,
    #[structopt(long = "linger-time", help = "Max seconds a socket may linger")]
    pub socket_so_linger: Option<u16>,
    #[structopt(
        long = "events-queue-size",
        help = "Events queue size per poll iteration",
        default_value = "10"
    )]
    pub events_queue_size: usize,
    #[structopt(
        long = "deduplication-hashing-algorithm",
        help = "Hash algorithm used for deduplication [xxhash64|sha256]",
        default_value = "xxhash64"
    )]
    pub deduplication_hashing_algorithm: DeduplicationHashAlgorithm,
}

#[derive(StructOpt, Debug)]
/// Parameters pertaining to basic setup.
pub struct CommonConfig {
    #[structopt(long = "external-port", help = "Own external port")]
    pub external_port: Option<u16>,
    #[structopt(
        long = "id",
        short = "i",
        help = "Set forced node id (64 bit unsigned integer in zero padded HEX. Must be 16 \
                characters long)"
    )]
    pub id: Option<String>,
    #[structopt(
        long = "listen-port",
        short = "p",
        help = "Port to listen on",
        default_value = "8888"
    )]
    pub listen_port: u16,
    #[structopt(long = "listen-address", short = "l", help = "Address to listen on")]
    pub listen_address: Option<String>,
    #[structopt(long = "debug", short = "d", help = "DEBUG-level logging mode")]
    pub debug: bool,
    #[structopt(long = "trace", help = "TRACE-level logging mode")]
    pub trace: bool,
    #[structopt(long = "info", help = "INFO-level logging mode")]
    pub info: bool,
    #[structopt(long = "no-consensus-logs", help = "Disables consensus logs except for ERRORs")]
    pub no_consensus_logs: bool,
    #[structopt(
        long = "network-id",
        short = "n",
        help = "Enable network id",
        default_value = "1000"
    )]
    pub network_ids: Vec<u16>,
    #[structopt(
        long = "config-dir",
        help = "Location of configuration files.",
        env = "CONCORDIUM_NODE_CONFIG_DIR"
    )]
    pub(crate) config_dir: PathBuf,
    #[structopt(
        long = "data-dir",
        help = "Location of data files.",
        env = "CONCORDIUM_NODE_DATA_DIR"
    )]
    pub(crate) data_dir: PathBuf,
    #[structopt(long = "no-log-timestamp", help = "Do not output timestamp in log output")]
    pub no_log_timestamp: bool,
    #[structopt(
        long = "minimum-peers-bucket",
        help = "Minimum peers to keep in each bucket always",
        default_value = "100"
    )]
    pub min_peers_bucket: usize,
    #[structopt(long = "print-config", help = "Print out config struct")]
    pub print_config: bool,
    #[structopt(
        long = "bucket-cleanup-interval",
        help = "Try to timeout entries in the buckets every set interval (in ms)",
        default_value = "600000"
    )]
    pub bucket_cleanup_interval: u64,
}

/// Client's parameters.
#[derive(StructOpt, Debug)]
pub struct CliConfig {
    #[structopt(long = "no-network", help = "Disable network")]
    pub no_network: bool,
    #[structopt(
        long = "poll-interval",
        help = "The polling interval in milliseconds",
        default_value = "100"
    )]
    pub poll_interval: u64,
    #[structopt(flatten)]
    pub baker: BakerConfig,
    #[structopt(flatten)]
    pub rpc: RpcCliConfig,
    #[cfg(feature = "staging_net")]
    #[structopt(long = "staging-net-token", help = "Staging network client token")]
    pub staging_net_token: String,
    #[structopt(
        long = "timeout-bucket-entry-period",
        help = "Timeout an entry in the buckets after a given period (in ms), 0 means never",
        default_value = "0"
    )]
    pub timeout_bucket_entry_period: u64,
    #[structopt(
        long = "no-rebroadcast-consensus-validation",
        help = "Disable consensus controlling whether to rebroadcast or not"
    )]
    pub no_rebroadcast_consensus_validation: bool,
    #[structopt(
        long = "drop-rebroadcast-probability",
        help = "Drop a message from being rebroadcasted by a certain probability"
    )]
    pub drop_rebroadcast_probability: Option<f64>,

    #[cfg(feature = "malicious_testing")]
    #[structopt(
        long = "breakage-type",
        help = "Break for test purposes; spam - send duplicate messages / fuzz - mangle messages \
                [fuzz|spam]"
    )]
    pub breakage_type: Option<String>,

    #[cfg(feature = "malicious_testing")]
    #[structopt(
        long = "breakage-target",
        help = "Used together with breakage-type; 0/1/2/3/4/99 - blocks/txs/fin msgs/fin \
                recs/catch-up msgs/everything [0|1|2|3|4|99]"
    )]
    pub breakage_target: Option<u8>,

    #[cfg(feature = "malicious_testing")]
    #[structopt(
        long = "breakage-level",
        help = "Used together with breakage-type; either the number of spammed duplicates or \
                mangled bytes"
    )]
    pub breakage_level: Option<usize>,

    #[structopt(long = "transaction-outcome-logging", help = "Enable transaction outcome logging")]
    pub transaction_outcome_logging: bool,
    #[structopt(
        long = "transaction-outcome-logging-database-name",
        help = "Transaction outcome logging database name",
        default_value = "concordium"
    )]
    pub transaction_outcome_logging_database_name: String,
    #[structopt(
        long = "transaction-outcome-logging-database-host",
        help = "Transaction outcome logging database host",
        default_value = "127.0.0.1"
    )]
    pub transaction_outcome_logging_database_host: String,
    #[structopt(
        long = "transaction-outcome-logging-database-username",
        help = "Transaction outcome logging database username",
        default_value = "concordium"
    )]
    pub transaction_outcome_logging_database_username: String,
    #[structopt(
        long = "transaction-outcome-logging-database-password",
        help = "Transaction outcome logging database password",
        default_value = "concordium"
    )]
    pub transaction_outcome_logging_database_password: String,
    #[structopt(
        long = "transaction-outcome-logging-database-port",
        help = "Transaction outcome logging database port",
        default_value = "5432"
    )]
    pub transaction_outcome_logging_database_port: u16,
}

/// Parameters applicable to a bootstrapper.
#[derive(StructOpt, Debug)]
#[structopt(name = "BootstrapperNode")]
pub struct BootstrapperConfig {
    #[structopt(
        long = "max-nodes",
        help = "Max nodes allowed to connect",
        default_value = "10000"
    )]
    pub max_nodes: u16,
    #[structopt(
        long = "wait-until-minimum-nodes",
        help = "Wait until a minumum number of nodes have been obtained before sending out peer \
                lists to peers",
        default_value = "0"
    )]
    pub wait_until_minimum_nodes: u16,
    #[structopt(
        long = "bootstrapper-timeout-bucket-entry-period",
        help = "Timeout an entry in the buckets after a given period (in ms), 0 means never",
        default_value = "7200000"
    )]
    pub bootstrapper_timeout_bucket_entry_period: u64,

    #[cfg(feature = "malicious_testing")]
    #[structopt(
        long = "partition-network-for-time",
        help = "Partition the network for a set amount of time since startup (in ms)"
    )]
    pub partition_network_for_time: Option<usize>,

    #[structopt(
        long = "peer-list-size",
        help = "The number of random peers shared by a bootstrapper in a PeerList",
        default_value = "10"
    )]
    pub peer_list_size: usize,

    #[structopt(
        long = "regenesis-block-hashes-file",
        help = "Path to a file that contains a json array of regenesis hashes."
    )]
    pub regenesis_block_hashes: Option<PathBuf>,
}

/// The main configuration object.
#[derive(StructOpt, Debug)]
pub struct Config {
    #[structopt(flatten)]
    pub common: CommonConfig,
    #[cfg(feature = "instrumentation")]
    #[structopt(flatten)]
    pub prometheus: PrometheusConfig,
    #[structopt(flatten)]
    pub connection: ConnectionConfig,
    #[structopt(flatten)]
    pub cli: CliConfig,
    #[structopt(flatten)]
    pub bootstrapper: BootstrapperConfig,
    #[cfg(feature = "database_emitter")]
    #[structopt(flatten)]
    pub database_emitter: DatabaseEmitterConfig,
}

impl Config {
    pub fn add_options(
        mut self,
        listen_address: Option<String>,
        listen_port: u16,
        network_ids: Vec<u16>,
        min_peers_bucket: usize,
    ) -> Self {
        self.common.listen_address = listen_address;
        self.common.listen_port = listen_port;
        self.common.network_ids = network_ids;
        self.common.min_peers_bucket = min_peers_bucket;
        self
    }
}

/// Verifies the validity of the configuration.
pub fn parse_config() -> Fallible<Config> {
    let conf = {
        let app = Config::clap()
            .setting(AppSettings::ArgRequiredElseHelp)
            .global_setting(AppSettings::ColoredHelp);
        Config::from_clap(&app.get_matches())
    };

    ensure!(
        conf.connection.max_allowed_nodes_percentage >= 100,
        "Can't provide a lower percentage than 100, as that would limit the maximum amount of \
         nodes to less than the desired nodes is set to"
    );

    if let Some(max_allowed_nodes) = conf.connection.max_allowed_nodes {
        ensure!(
            max_allowed_nodes >= conf.connection.desired_nodes,
            "Desired nodes set to {}, but max allowed nodes is set to {}. Max allowed nodes must \
             be greater or equal to desired amount of nodes",
            conf.connection.desired_nodes,
            max_allowed_nodes
        );
    }

    ensure!(
        conf.connection.hard_connection_limit >= conf.connection.desired_nodes,
        "Hard connection limit can't be less than what desired nodes is set to"
    );

    ensure!(
        conf.connection.relay_broadcast_percentage >= 0.0
            && conf.connection.relay_broadcast_percentage <= 1.0,
        "Percentage of peers to relay broadcasted packets to, must be between 0.0 and 1.0"
    );

    ensure!(
        conf.cli.baker.maximum_block_size <= 4_000_000_000
            && ((f64::from(conf.cli.baker.maximum_block_size) * 0.9).ceil()) as u32
                <= PROTOCOL_MAX_MESSAGE_SIZE,
        "Maximum block size set higher than 90% of network protocol max size ({})",
        PROTOCOL_MAX_MESSAGE_SIZE
    );

    ensure!(
        conf.connection.socket_read_size >= 65535,
        "Socket read size must be set to at least 65535"
    );

    ensure!(
        conf.connection.socket_read_size >= conf.connection.socket_write_size,
        "Socket read size must be greater or equal to the write size"
    );

    #[cfg(feature = "malicious_testing")]
    ensure!(
        conf.cli.breakage_type.is_some()
            && conf.cli.breakage_target.is_some()
            && conf.cli.breakage_level.is_some()
            || conf.cli.breakage_type.is_none()
                && conf.cli.breakage_target.is_none()
                && conf.cli.breakage_level.is_none(),
        "The 3 breakage options (breakage-type, breakage-target, breakage-level) must be enabled \
         or disabled together"
    );

    ensure!(
        conf.bootstrapper.wait_until_minimum_nodes as usize <= conf.bootstrapper.peer_list_size,
        "wait-until-minimum-nodes must be lower than or equal to peer-list-size"
    );

    // TODO: Remove surrounding block expr once cargo fmt has been updated in
    // pipeline.
    #[cfg(feature = "malicious_testing")]
    {
        if let Some(ref breakage_type) = conf.cli.breakage_type {
            ensure!(
                ["spam", "fuzz"].contains(&breakage_type.as_str()),
                "Unsupported breakage-type"
            );
            if let Some(breakage_target) = conf.cli.breakage_target {
                ensure!(
                    [0, 1, 2, 3, 4, 99].contains(&breakage_target),
                    "Unsupported breakage-target"
                );
            }
        }
    }

    #[cfg(feature = "instrumentation")]
    {
        ensure!(
            conf.prometheus.prometheus_server || conf.prometheus.prometheus_push_gateway.is_some(),
            "The instrumentation feature requires either prometheus-server or \
             prometheus-push-gateway argument to be set"
        );
    }

    Ok(conf)
}

/// Handles the configuration data.
#[derive(Debug)]
pub struct AppPreferences {
    preferences_map:     PreferencesMap<String>,
    override_data_dir:   PathBuf,
    override_config_dir: PathBuf,
}

impl AppPreferences {
    /// Creates an `AppPreferences` object.
    pub fn new(override_conf: PathBuf, override_data: PathBuf) -> Self {
        let file_path = Self::calculate_config_file_path(&override_conf, APP_PREFERENCES_MAIN);
        let mut new_prefs = match OpenOptions::new().read(true).write(true).open(&file_path) {
            Ok(file) => {
                let mut reader = BufReader::new(&file);
                let load_result = PreferencesMap::<String>::load_from(&mut reader);
                let prefs = load_result.unwrap_or_else(|_| PreferencesMap::<String>::new());

                AppPreferences {
                    preferences_map:     prefs,
                    override_data_dir:   override_data,
                    override_config_dir: override_conf,
                }
            }
            _ => match File::create(&file_path) {
                Ok(_) => {
                    let prefs = PreferencesMap::<String>::new();
                    AppPreferences {
                        preferences_map:     prefs,
                        override_data_dir:   override_data,
                        override_config_dir: override_conf,
                    }
                }
                _ => panic!("Can't write to config file!"),
            },
        };
        new_prefs.set_config(APP_PREFERENCES_KEY_VERSION, Some(super::VERSION.to_string()));
        new_prefs
    }

    fn calculate_config_file_path(config_path: &PathBuf, key: &str) -> PathBuf {
        let mut new_path = config_path.clone();
        new_path.push(&format!("{}.json", key));
        new_path
    }

    /// Add a piece of config to the config map.
    pub fn set_config(&mut self, key: &str, value: Option<String>) -> bool {
        match value {
            Some(val) => self.preferences_map.insert(key.to_string(), val),
            _ => self.preferences_map.remove(&key.to_string()),
        };
        let file_path =
            Self::calculate_config_file_path(&self.override_config_dir, APP_PREFERENCES_MAIN);
        match OpenOptions::new().read(true).write(true).open(&file_path) {
            Ok(ref mut file) => {
                let mut writer = BufWriter::new(file);
                if self.preferences_map.save_to(&mut writer).is_err() {
                    error!("Couldn't save config file changes");
                    return false;
                }
                writer.flush().ok();
                true
            }
            _ => {
                error!("Couldn't save config file changes");
                false
            }
        }
    }

    /// Get a piece of config from the config map.
    pub fn get_config(&self, key: &str) -> Option<String> { self.preferences_map.get(key).cloned() }

    /// Returns the path to the application directory.
    pub fn get_user_app_dir(&self) -> &Path { &self.override_data_dir }

    /// Returns the path to the config directory.
    pub fn get_user_config_dir(&self) -> &Path { &self.override_config_dir }
}
