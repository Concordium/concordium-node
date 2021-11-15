//! The client's parameters and constants used by other modules.

use crate::{
    common::P2PNodeId,
    connection::DeduplicationHashAlgorithm,
    network::{WireProtocolVersion, WIRE_PROTOCOL_VERSIONS},
};
use anyhow::{ensure, Context};
use app_dirs2::*;
use preferences::{Preferences, PreferencesMap};
use std::{
    fs::{File, OpenOptions},
    io::{BufReader, BufWriter, Write},
    path::{Path, PathBuf},
    str::FromStr,
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
pub(crate) fn is_compatible_version(other: &semver::Version) -> bool { other.major >= 1 }

/// Check that the other wire version is compatible with ours. This returns
/// the highest wire protocol version that is supported by both nodes (since
/// `network::WIRE_PROTOCOL_VERSIONS` is in descending order).
pub(crate) fn is_compatible_wire_version(
    other: &[WireProtocolVersion],
) -> Option<WireProtocolVersion> {
    WIRE_PROTOCOL_VERSIONS.iter().find(|&&ours| other.iter().any(|&theirs| theirs == ours)).copied()
}

/// The maximum size of objects accepted from the network.
pub const PROTOCOL_MAX_MESSAGE_SIZE: u32 = 20_971_520; // 20 MIB

/// Upper bound on the transaction object size, in bytes.
pub const PROTOCOL_MAX_TRANSACTION_SIZE: usize = 100 * 1024; // 100 kB.

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
/// Maximum time (in s) a node's connection can remain unreachable.
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
// Parameters related to the database emitter.
pub struct DatabaseEmitterConfig {
    #[structopt(
        long = "import-file",
        help = "File to import from",
        env = "CONCORDIUM_NODE_DB_EMITTER_IMPORT_FILE"
    )]
    pub import_file: String,

    #[structopt(
        long = "batches-delay",
        help = "Delay between batches in miliseconds",
        default_value = "2000",
        env = "CONCORDIUM_NODE_DB_EMITTER_BATCHES_DELAY"
    )]
    pub delay_between_batches: u64,

    #[structopt(
        long = "batch-size",
        help = "Size of each batch to emit",
        default_value = "40",
        env = "CONCORDIUM_NODE_DB_EMITTER_BATCH_SIZES"
    )]
    pub batch_sizes: u64,

    #[structopt(
        long = "skip-first",
        help = "Amount of the initial blocks to skip",
        default_value = "0",
        env = "CONCORDIUM_NODE_DB_EMITTER_SKIP_FIRST"
    )]
    pub skip_first: u64,
}

#[cfg(feature = "instrumentation")]
#[derive(StructOpt, Debug)]
// Parameters related to Prometheus.
pub struct PrometheusConfig {
    #[structopt(
        long = "prometheus-listen-addr",
        help = "IP to listen for prometheus requests on",
        default_value = "127.0.0.1",
        env = "CONCORDIUM_NODE_PROMETHEUS_LISTEN_ADDRESSS"
    )]
    pub prometheus_listen_addr:   String,
    #[structopt(
        long = "prometheus-listen-port",
        help = "Port for prometheus to listen on",
        default_value = "9090",
        env = "CONCORDIUM_NODE_PROMETHEUS_LISTEN_PORT"
    )]
    pub prometheus_listen_port:   u16,
    #[structopt(
        long = "prometheus-server",
        help = "Enable prometheus server for metrics",
        env = "CONCORDIUM_NODE_PROMETHEUS_SERVER"
    )]
    pub prometheus_server:        bool,
    #[structopt(
        long = "prometheus-push-gateway",
        help = "Enable prometheus via push gateway",
        env = "CONCORDIUM_NODE_PROMETHEUS_PUSH_GATEWAY"
    )]
    pub prometheus_push_gateway:  Option<String>,
    #[structopt(
        long = "prometheus-job-name",
        help = "Job name to send to push gateway",
        default_value = "p2p_node_push",
        env = "CONCORDIUM_NODE_PROMETHEUS_JOB_NAME"
    )]
    pub prometheus_job_name:      String,
    #[structopt(
        long = "prometheus-instance-name",
        help = "If not present node_id will be used",
        env = "CONCORDIUM_NODE_PROMETHEUS_INSTANCE_NAME"
    )]
    pub prometheus_instance_name: Option<String>,
    #[structopt(
        long = "prometheus-push-gateway-username",
        help = "Username to use for push gateway, if either username or password is omitted \
                authentication isn't used",
        env = "CONCORDIUM_NODE_PROMETHEUS_PUSH_GATEWAY_USERNAME"
    )]
    pub prometheus_push_username: Option<String>,
    #[structopt(
        long = "prometheus-push-gateway-password",
        help = "Password to use for push gateway, if either username or password is omitted \
                authentication isn't used",
        env = "CONCORDIUM_NODE_PROMETHEUS_PUSH_GATEWAY_PASSWORD",
        hide_env_values = true
    )]
    pub prometheus_push_password: Option<String>,
    #[structopt(
        long = "prometheus-push-gateway-interval",
        help = "Interval in seconds between pushes",
        default_value = "2",
        env = "CONCORDIUM_NODE_PROMETHEUS_PUSH_GATEWAY_INTERVAL"
    )]
    pub prometheus_push_interval: u64,
}

#[derive(StructOpt, Debug)]
// Parameters related to Baking (only used in cli).
pub struct BakerConfig {
    #[cfg(feature = "profiling")]
    #[structopt(
        long = "heap-profiling",
        help = "Profile the heap [(`cost`,-hc), (`type`, -hy), (`module`, -hm), (`description`, \
                -hd)] in the Haskell subsystem",
        default_value = "none",
        env = "CONCORDIUM_NODE_RUNTIME_HEAP_PROFILING"
    )]
    pub heap_profiling: String,
    #[cfg(feature = "profiling")]
    #[structopt(
        long = "time-profiling",
        help = "Profile the time in the Haskell subsystem",
        env = "CONCORDIUM_NODE_RUNTIME_TIME_PROFILING"
    )]
    pub time_profiling: bool,
    #[cfg(feature = "profiling")]
    #[structopt(
        long = "backtraces",
        help = "Show bactraces generated by exceptions in the Haskell subsystem",
        env = "CONCORDIUM_NODE_RUNTIME_SHOW_BACKTRACES"
    )]
    pub backtraces_profiling: bool,
    #[cfg(feature = "profiling")]
    #[structopt(
        long = "stack-profiling",
        help = "Include memory occupied by threads in the heap profile. Only has effect if \
                `heap-profiling` is enabled.",
        env = "CONCORDIUM_NODE_RUNTIME_STACK_PROFILING"
    )]
    pub stack_profiling: bool,
    #[cfg(feature = "profiling")]
    #[structopt(
        long = "profiling-sampling-interval",
        help = "Profile sampling interval in seconds",
        default_value = "0.1",
        env = "CONCORDIUM_NODE_PROFILING_SAMPLING_INTERVAL"
    )]
    pub profiling_sampling_interval: String,
    #[structopt(
        long = "haskell-gc-logging",
        help = "Enable Haskell garbage collection logging",
        env = "CONCORDIUM_NODE_RUNTIME_HASKELL_GC_LOGGING"
    )]
    pub gc_logging: Option<String>,
    #[structopt(
        long = "haskell-rts-flags",
        help = "Haskell RTS flags to pass to consensus.",
        default_value = "",
        env = "CONCORDIUM_NODE_RUNTIME_HASKELL_RTS_FLAGS",
        // parse the argument as a comma separated list of values.
        // Especially useful when supplied via the environment variable.
        use_delimiter = true
    )]
    pub rts_flags: Vec<String>,
    #[structopt(
        long = "maximum-block-size",
        help = "Maximum block size in bytes",
        default_value = "4194304",
        env = "CONCORDIUM_NODE_BAKER_MAXIMUM_BLOCK_SIZE"
    )]
    pub maximum_block_size: u32,
    #[structopt(
        long = "block-construction-timeout",
        help = "Block construction timeout in milliseconds",
        default_value = "3000",
        env = "CONCORDIUM_NODE_BAKER_BLOCK_CONSTRUCTION_TIMEOUT"
    )]
    pub block_construction_timeout: u32,
    #[structopt(
        long = "transaction-insertions-before-purge",
        help = "Number of transaction insertions between purges on the transaction table",
        default_value = "1000",
        env = "CONCORDIUM_NODE_CONSENSUS_TRANSACTION_INSERTIONS_BEFORE_PURGE"
    )]
    pub transaction_insertions_before_purge: u32,
    #[structopt(
        long = "transaction-keep-alive",
        help = "Time during which a transaction can not be purged in seconds",
        default_value = "600",
        env = "CONCORDIUM_NODE_CONSENSUS_TRANSACTION_KEEP_ALIVE"
    )]
    pub transaction_keep_alive: u32,
    #[structopt(
        long = "transactions-purging-delay",
        help = "Time between automatic transaction table purging runs in seconds",
        default_value = "300",
        env = "CONCORDIUM_NODE_CONSENSUS_TRANSACTIONS_PURGING_DELAY"
    )]
    pub transactions_purging_delay: u32,
    #[structopt(
        long = "import-blocks-from",
        help = "Path to a file exported by the database exporter",
        env = "CONCORDIUM_NODE_CONSENSUS_IMPORT_BLOCKS_FROM"
    )]
    pub import_path: Option<String>,
    #[structopt(
        long = "max-expiry-duration",
        help = "Maximum allowed time difference between now and a transaction's expiry time in \
                seconds",
        default_value = "7200",
        env = "CONCORDIUM_NODE_CONSENSUS_MAX_EXPIRY_DURATION"
    )]
    pub max_time_to_expiry: u64,
    #[structopt(
        long = "genesis-data-file",
        help = "Path to the data that constitutes the genesis block. If the path is relative it \
                is interpreted relative to the supplied data directory.",
        default_value = "genesis.dat",
        env = "CONCORDIUM_NODE_CONSENSUS_GENESIS_DATA_FILE"
    )]
    pub genesis_data_file: PathBuf,
    #[structopt(
        long = "baker-credentials-file",
        help = "Path to the baker credentials file. If the path is relative it is interpreted \
                relative to the node process' working directory.",
        env = "CONCORDIUM_NODE_BAKER_CREDENTIALS_FILE"
    )]
    pub baker_credentials_file: Option<PathBuf>,
    #[structopt(
        long = "decrypt-baker-credentials",
        help = "Indicate that the baker credentials are provided encrypted and thus need to be \
                decrypted.",
        env = "CONCORDIUM_NODE_BAKER_DECRYPT_CREDENTIALS"
    )]
    pub decrypt_baker_credentials: bool,
}

#[derive(StructOpt, Debug)]
// Parameters related to the RPC (only used in cli).
pub struct RpcCliConfig {
    #[structopt(
        long = "no-rpc-server",
        help = "Disable the built-in RPC server",
        env = "CONCORDIUM_NODE_DISABLE_RPC_SERVER"
    )]
    pub no_rpc_server:    bool,
    #[structopt(
        long = "rpc-server-port",
        help = "RPC server port",
        default_value = "10000",
        env = "CONCORDIUM_NODE_RPC_SERVER_PORT"
    )]
    pub rpc_server_port:  u16,
    #[structopt(
        long = "rpc-server-addr",
        help = "RPC server listen address",
        default_value = "127.0.0.1",
        env = "CONCORDIUM_NODE_RPC_SERVER_ADDR"
    )]
    pub rpc_server_addr:  String,
    #[structopt(
        long = "rpc-server-token",
        help = "RPC server access token",
        default_value = "rpcadmin",
        env = "CONCORDIUM_NODE_RPC_SERVER_TOKEN",
        hide_env_values = true
    )]
    pub rpc_server_token: String,
}

#[derive(StructOpt, Debug)]
// Parameters related to connections.
pub struct ConnectionConfig {
    #[structopt(
        long = "desired-nodes",
        help = "Desired nodes to always have",
        default_value = "7",
        env = "CONCORDIUM_NODE_CONNECTION_DESIRED_NODES"
    )]
    pub desired_nodes: u16,
    #[structopt(
        long = "max-allowed-nodes",
        help = "Maximum nodes to allow a connection to",
        env = "CONCORDIUM_NODE_CONNECTION_MAX_ALLOWED_NODES"
    )]
    pub max_allowed_nodes: Option<u16>,
    #[structopt(
        long = "max-allowed-nodes-percentage",
        help = "Maximum nodes to allow a connection to is set as a percentage of desired-nodes \
                (minimum 100, to set it to desired-nodes",
        default_value = "150",
        env = "CONCORDIUM_NODE_CONNECTION_MAX_ALLOWED_NODES_PERCENTAGE"
    )]
    pub max_allowed_nodes_percentage: u16,
    #[structopt(
        long = "no-bootstrap",
        help = "Do not bootstrap via DNS",
        env = "CONCORDIUM_NODE_CONNECTION_NO_BOOTSTRAP_DNS"
    )]
    pub no_bootstrap_dns: bool,
    #[structopt(
        long = "no-clear-bans",
        help = "Do not clear the ban database on start.",
        env = "CONCORDIUM_NODE_CONNECTION_NO_CLEAR_BANS"
    )]
    pub no_clear_bans: bool,
    #[structopt(
        long = "relay-broadcast-percentage",
        help = "The percentage of peers to relay broadcasted messages to",
        default_value = "1.0",
        env = "CONCORDIUM_NODE_CONNECTION_RELAY_BROADCAST_PERCENTAGE"
    )]
    pub relay_broadcast_percentage: f64,
    #[structopt(
        long = "connect-to",
        short = "c",
        help = "Peer to connect to upon startup (host/ip:port)",
        use_delimiter = true, // allow a single argument with a comma separated list of values.
        env = "CONCORDIUM_NODE_CONNECTION_CONNECT_TO"
    )]
    pub connect_to: Vec<String>,
    #[structopt(
        long = "disallow-multiple-peers-on-ip",
        help = "Disallow multiple peers on the same IP address.",
        env = "CONCORDIUM_NODE_CONNECTION_DISALLOW_MULTIPLE_PEERS_ON_SAME_IP"
    )]
    pub disallow_multiple_peers_on_ip: bool,
    #[structopt(
        long = "dns-resolver",
        help = "DNS resolver to use",
        env = "CONCORDIUM_NODE_CONNECTION_DNS_RESOLVER",
        use_delimiter = true
    )]
    pub dns_resolver: Vec<String>,
    #[structopt(
        name = "bootstrap-node",
        long = "bootstrap-node",
        help = "Bootstrap nodes to use upon startup host/ip:port (this disables DNS bootstrapping)",
        env = "CONCORDIUM_NODE_CONNECTION_BOOTSTRAP_NODES",
        use_delimiter = true
    )]
    pub bootstrap_nodes: Vec<String>,
    #[structopt(
        long = "housekeeping-interval",
        help = "The connection housekeeping interval in seconds",
        default_value = "30",
        env = "CONCORDIUM_NODE_CONNECTION_HOUSEKEEPING_INTERVAL"
    )]
    pub housekeeping_interval: u64,
    #[structopt(
        long = "bootstrapping-interval",
        help = "The bootstrapping interval in seconds",
        default_value = "7200",
        env = "CONCORDIUM_NODE_CONNECTION_BOOTSTRAPPING_INTERVAL"
    )]
    pub bootstrapping_interval: u64,
    #[structopt(
        long = "max-latency",
        help = "The maximum allowed connection latency in ms",
        env = "CONCORDIUM_NODE_CONNECTION_MAX_LATENCY"
    )]
    pub max_latency: Option<u64>,
    #[structopt(
        long = "hard-connection-limit",
        help = "Maximum connections to keep open at any time",
        default_value = "50",
        env = "CONCORDIUM_NODE_CONNECTION_HARD_CONNECTION_LIMIT"
    )]
    pub hard_connection_limit: u16,
    #[structopt(
        long = "connection-requests-batch-limit",
        help = "Maximum number of incoming connection requests to attempt to process per \
                iteration.",
        default_value = "9",
        env = "CONCORDIUM_NODE_CONNECTION_REQUESTS_BATCH_LIMIT"
    )]
    pub conn_requests_batch_limit: u16,
    #[structopt(
        long = "catch-up-batch-limit",
        help = "The maximum batch size for a catch-up round.",
        default_value = "50",
        env = "CONCORDIUM_NODE_CONNECTION_CATCH_UP_BATCH_LIMIT"
    )]
    pub catch_up_batch_limit: i64,
    #[structopt(
        long = "thread-pool-size",
        help = "The size of the threadpool processing connection events in parallel",
        default_value = "4",
        env = "CONCORDIUM_NODE_CONNECTION_THREAD_POOL_SIZE"
    )]
    pub thread_pool_size: usize,
    #[structopt(
        long = "dedup-size-long",
        help = "The size of the long deduplication queues",
        default_value = "65536",
        env = "CONCORDIUM_NODE_CONNECTION_DEDUP_SIZE_LONG"
    )]
    pub dedup_size_long: usize,
    #[structopt(
        long = "dedup-size-short",
        help = "The size of the short deduplication queues",
        default_value = "4096",
        env = "CONCORDIUM_NODE_CONNECTION_DEDUP_SIZE_SHORT"
    )]
    pub dedup_size_short: usize,
    #[structopt(
        long = "socket-write-size",
        help = "The desired size of single socket writes; must be no bigger than socket_read_size",
        default_value = "16384",
        env = "CONCORDIUM_NODE_CONNECTION_SOCKET_WRITE_SIZE"
    )]
    pub socket_write_size: usize,
    #[structopt(
        long = "socket-read-size",
        help = "The desired size of single socket reads; must be >= 65535 (max noise message size)",
        default_value = "131072",
        env = "CONCORDIUM_NODE_CONNECTION_SOCKET_READ_SIZE"
    )]
    pub socket_read_size: usize,
    #[structopt(
        long = "linger-time",
        help = "Max seconds a socket may linger",
        env = "CONCORDIUM_NODE_CONNECTION_SOCKET_SO_LINGER"
    )]
    pub socket_so_linger: Option<u16>,
    #[structopt(
        long = "events-queue-size",
        help = "Events queue size per poll iteration",
        default_value = "10",
        env = "CONCORDIUM_NODE_CONNECTION_EVENTS_QUEUE_SIZE"
    )]
    pub events_queue_size: usize,
    #[structopt(
        long = "deduplication-hashing-algorithm",
        help = "Hash algorithm used for deduplication [xxhash64|sha256]",
        default_value = "xxhash64",
        env = "CONCORDIUM_NODE_CONNECTION_DEDUPLICATION_HASHING_ALGORITHM"
    )]
    pub deduplication_hashing_algorithm: DeduplicationHashAlgorithm,
}

#[derive(StructOpt, Debug)]
// Parameters pertaining to basic setup.
pub struct CommonConfig {
    #[structopt(
        long = "external-port",
        help = "Own external port",
        env = "CONCORDIUM_NODE_EXTERNAL_PORT"
    )]
    pub external_port: Option<u16>,
    #[structopt(
        long = "id",
        short = "i",
        help = "Set forced node id (64 bit unsigned integer in zero padded HEX. Must be 16 \
                characters long)",
        env = "CONCORDIUM_NODE_ID"
    )]
    pub id: Option<P2PNodeId>,
    #[structopt(
        long = "listen-port",
        short = "p",
        help = "Port to listen on",
        default_value = "8888",
        env = "CONCORDIUM_NODE_LISTEN_PORT"
    )]
    pub listen_port: u16,
    #[structopt(
        long = "listen-address",
        short = "l",
        help = "Address to listen on",
        env = "CONCORDIUM_NODE_LISTEN_ADDRESS"
    )]
    pub listen_address: Option<String>,
    #[structopt(
        long = "debug",
        short = "d",
        help = "DEBUG-level logging mode",
        env = "CONCORDIUM_NODE_LOG_LEVEL_DEBUG"
    )]
    pub debug: bool,
    #[structopt(
        long = "trace",
        help = "TRACE-level logging mode",
        env = "CONCORDIUM_NODE_LOG_LEVEL_TRACE"
    )]
    pub trace: bool,
    #[structopt(
        long = "info",
        help = "INFO-level logging mode",
        env = "CONCORDIUM_NODE_LOG_LEVEL_INFO"
    )]
    pub info: bool,
    #[structopt(
        long = "no-consensus-logs",
        help = "Disables consensus logs except for ERRORs",
        env = "CONCORDIUM_NODE_NO_CONSENSUS_LOG"
    )]
    pub no_consensus_logs: bool,
    #[structopt(
        long = "network-id",
        short = "n",
        help = "Enable network id",
        default_value = "1000",
        env = "CONCORDIUM_NODE_NETWORK_ID",
        use_delimiter = true
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
    #[structopt(
        long = "no-log-timestamp",
        help = "Do not output timestamp in log output",
        env = "CONCORDIUM_NODE_NO_LOG_TIMESTAMP"
    )]
    pub no_log_timestamp: bool,
    #[structopt(
        long = "log-config",
        help = "Configure logging with a log4rs configuration file. Overrides the default logging.",
        env = "CONCORDIUM_NODE_LOG_CONFIG"
    )]
    pub log_config: Option<PathBuf>,
    #[structopt(
        long = "minimum-peers-bucket",
        help = "Minimum peers to keep in each bucket always",
        default_value = "100",
        env = "CONCORDIUM_NODE_MINIMUM_PEERS_BUCKET"
    )]
    pub min_peers_bucket: usize,
    #[structopt(
        long = "print-config",
        help = "Print out config struct",
        env = "CONCORDIUM_NODE_PRINT_CONFIG"
    )]
    pub print_config: bool,
    #[structopt(
        long = "bucket-cleanup-interval",
        help = "Try to timeout entries in the buckets every set interval (in ms)",
        default_value = "600000",
        env = "CONCORDIUM_NODE_BUCKET_CLEANUP_INTERVAL"
    )]
    pub bucket_cleanup_interval: u64,
}

// Client's parameters.
#[derive(StructOpt, Debug)]
pub struct CliConfig {
    #[structopt(long = "no-network", help = "Disable network", env = "CONCORDIUM_NODE_NO_NETWORK")]
    pub no_network: bool,
    #[structopt(
        long = "poll-interval",
        help = "The polling interval in milliseconds",
        default_value = "100",
        env = "CONCORDIUM_NODE_POLL_INTERVAL"
    )]
    pub poll_interval: u64,
    #[structopt(flatten)]
    pub baker: BakerConfig,
    #[structopt(flatten)]
    pub rpc: RpcCliConfig,
    #[structopt(
        long = "timeout-bucket-entry-period",
        help = "Timeout an entry in the buckets after a given period (in ms), 0 means never",
        default_value = "0",
        env = "CONCORDIUM_NODE_TIMEOUT_BUCKET_ENTRY_PERIOD"
    )]
    pub timeout_bucket_entry_period: u64,
    #[structopt(
        long = "no-rebroadcast-consensus-validation",
        help = "Disable consensus controlling whether to rebroadcast or not",
        env = "CONCORDIUM_NODE_NO_REBROADCAST_CONSENSUS_VALIDATION"
    )]
    pub no_rebroadcast_consensus_validation: bool,
    #[structopt(
        long = "drop-rebroadcast-probability",
        help = "Drop a message from being rebroadcasted by a certain probability",
        env = "CONCORDIUM_NODE_DROP_REBROADCSAT_PROBABILITY"
    )]
    pub drop_rebroadcast_probability: Option<f64>,
    #[structopt(
        long = "transaction-outcome-logging",
        help = "Enable transaction outcome logging",
        env = "CONCORDIUM_NODE_TRANSACTION_OUTCOME_LOGGING"
    )]
    pub transaction_outcome_logging: bool,
    #[structopt(
        long = "transaction-outcome-logging-database-name",
        help = "Transaction outcome logging database name",
        default_value = "concordium",
        env = "CONCORDIUM_NODE_TRANSACTION_OUTCOME_LOGGING_NAME"
    )]
    pub transaction_outcome_logging_database_name: String,
    #[structopt(
        long = "transaction-outcome-logging-database-host",
        help = "Transaction outcome logging database host",
        default_value = "127.0.0.1",
        env = "CONCORDIUM_NODE_TRANSACTION_OUTCOME_LOGGING_HOST"
    )]
    pub transaction_outcome_logging_database_host: String,
    #[structopt(
        long = "transaction-outcome-logging-database-username",
        help = "Transaction outcome logging database username",
        default_value = "concordium",
        env = "CONCORDIUM_NODE_TRANSACTION_OUTCOME_LOGGING_USERNAME"
    )]
    pub transaction_outcome_logging_database_username: String,
    #[structopt(
        long = "transaction-outcome-logging-database-password",
        help = "Transaction outcome logging database password",
        default_value = "concordium",
        env = "CONCORDIUM_NODE_TRANSACTION_OUTCOME_LOGGING_PASSWORD",
        hide_env_values = true
    )]
    pub transaction_outcome_logging_database_password: String,
    #[structopt(
        long = "transaction-outcome-logging-database-port",
        help = "Transaction outcome logging database port",
        default_value = "5432",
        env = "CONCORDIUM_NODE_TRANSACTION_OUTCOME_LOGGING_PORT"
    )]
    pub transaction_outcome_logging_database_port: u16,
}

#[derive(StructOpt, Debug)]
// Parameters applicable to a bootstrapper.
pub struct BootstrapperConfig {
    #[structopt(
        long = "max-nodes",
        help = "Max nodes allowed to connect",
        default_value = "10000",
        env = "CONCORDIUM_NODE_BOOTSTRAPPER_MAX_NODES"
    )]
    pub max_nodes: u16,
    #[structopt(
        long = "wait-until-minimum-nodes",
        help = "Wait until a minumum number of nodes have been obtained before sending out peer \
                lists to peers",
        default_value = "0",
        env = "CONCORDIUM_NODE_BOOTSTRAPPER_WAIT_UNTIL_MINIMUM_NODES"
    )]
    pub wait_until_minimum_nodes: u16,
    #[structopt(
        long = "bootstrapper-timeout-bucket-entry-period",
        help = "Timeout an entry in the buckets after a given period (in ms), 0 means never",
        default_value = "7200000",
        env = "CONCORDIUM_NODE_BOOTSTRAPPER_TIMEOUT_BUCKET_ENTRY_PERIOD"
    )]
    pub bootstrapper_timeout_bucket_entry_period: u64,
    #[structopt(
        long = "peer-list-size",
        help = "The number of random peers shared by a bootstrapper in a PeerList",
        default_value = "10",
        env = "CONCORDIUM_NODE_BOOTSTRAPPER_PEER_LIST_SIZE"
    )]
    pub peer_list_size: usize,
    #[structopt(
        long = "regenesis-block-hashes-file",
        help = "Path to a file that contains a json array of regenesis hashes.",
        env = "CONCORDIUM_NODE_BOOTSTRAPPER_REGENESIS_BLOCK_HASHES_FILE"
    )]
    pub regenesis_block_hashes: Option<PathBuf>,
}

#[cfg(target_os = "macos")]
#[derive(StructOpt, Debug)]
// Parameters applicable to macOS.
pub struct MacOsConfig {
    #[structopt(
        long = "use-mac-log",
        help = "Enable native logging on macOS by providing a subsystem name, e.g. \
                'software.concordium.mainnet.node'. This disables the normal logging system and \
                is incompatible with '--log-config'. Log messages can be found via Console.app or \
                the log commandline tool by searching for the subsystem.",
        env = "CONCORDIUM_NODE_MACOS_USE_MAC_LOG",
        conflicts_with = "log-config"
    )]
    pub use_mac_log: Option<String>,
}

// The main configuration object.
#[derive(StructOpt, Debug)]
#[structopt(about = "Concordium P2P node.")]
pub struct Config {
    #[structopt(flatten)]
    pub common:           CommonConfig,
    #[cfg(feature = "instrumentation")]
    #[structopt(flatten)]
    pub prometheus:       PrometheusConfig,
    #[structopt(flatten)]
    pub connection:       ConnectionConfig,
    #[structopt(flatten)]
    pub cli:              CliConfig,
    #[structopt(flatten)]
    pub bootstrapper:     BootstrapperConfig,
    #[cfg(feature = "database_emitter")]
    #[structopt(flatten)]
    pub database_emitter: DatabaseEmitterConfig,
    #[cfg(target_os = "macos")]
    #[structopt(flatten)]
    pub macos:            MacOsConfig,
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
pub fn parse_config() -> anyhow::Result<Config> {
    let conf = {
        let app = Config::clap()
            .setting(AppSettings::ArgRequiredElseHelp)
            .setting(AppSettings::NextLineHelp)
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

    ensure!(
        conf.bootstrapper.wait_until_minimum_nodes as usize <= conf.bootstrapper.peer_list_size,
        "wait-until-minimum-nodes must be lower than or equal to peer-list-size"
    );

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
    pub fn new(override_conf: PathBuf, override_data: PathBuf) -> anyhow::Result<Self> {
        let file_path = Self::calculate_config_file_path(&override_conf, APP_PREFERENCES_MAIN);
        let mut new_prefs =
            if let Ok(file) = OpenOptions::new().open(&file_path) {
                let mut reader = BufReader::new(&file);
                let load_result = PreferencesMap::<String>::load_from(&mut reader);
                let prefs = load_result.unwrap_or_else(|_| PreferencesMap::<String>::new());

                AppPreferences {
                    preferences_map:     prefs,
                    override_data_dir:   override_data,
                    override_config_dir: override_conf,
                }
            } else {
                info!("Node configuration file not found. Creating a new one.");

                let _ = File::create(&file_path).with_context(|| {
                    format!("Could not create configuration file: '{}'", file_path.as_path().display())
                })?;
                let prefs = PreferencesMap::<String>::new();

                AppPreferences {
                    preferences_map:     prefs,
                    override_data_dir:   override_data,
                    override_config_dir: override_conf,
                }
            };
        new_prefs.set_config(APP_PREFERENCES_KEY_VERSION, Some(super::VERSION));
        Ok(new_prefs)
    }

    fn calculate_config_file_path(config_path: &Path, key: &str) -> PathBuf {
        let mut new_path = config_path.to_path_buf();
        new_path.push(&format!("{}.json", key));
        new_path
    }

    /// Add a piece of config to the config map.
    pub fn set_config<X: ToString>(&mut self, key: &str, value: Option<X>) -> bool {
        match value {
            Some(val) => self.preferences_map.insert(key.to_string(), val.to_string()),
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
    /// If the value is not present return Ok(None), if it is present, but
    /// cannot be parsed as the requested type return an error.
    pub fn get_config<X: FromStr<Err = anyhow::Error>>(
        &self,
        key: &str,
    ) -> anyhow::Result<Option<X>> {
        match self.preferences_map.get(key) {
            Some(x_str) => {
                let x = X::from_str(x_str).context(
                    "Cannot parse value from the persistent configuration as the requried type.",
                )?;
                Ok(Some(x))
            }
            None => Ok(None),
        }
    }

    /// Returns the path to the application directory.
    pub fn get_data_dir(&self) -> &Path { &self.override_data_dir }

    /// Returns the path to the config directory.
    pub fn get_config_dir(&self) -> &Path { &self.override_config_dir }
}
