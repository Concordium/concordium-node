use app_dirs2::*;
use preferences::{Preferences, PreferencesMap};
use semver::Version;
use std::{
    fs::{File, OpenOptions},
    io::{BufReader, BufWriter, Write},
    path::PathBuf,
    sync::{Arc, RwLock},
};
use structopt::StructOpt;

const APP_INFO: AppInfo = AppInfo {
    name:   "ConcordiumP2P",
    author: "Concordium",
};
const APP_PREFERENCES_MAIN: &str = "main.config";
pub const APP_PREFERENCES_KEY_VERSION: &str = "VERSION";
pub const APP_PREFERENCES_PERSISTED_NODE_ID: &str = "PERSISTED_NODE_ID";

#[derive(StructOpt, Debug)]
pub struct CliConfig {
    #[structopt(long = "external-ip", help = "Own external IP")]
    pub external_ip: Option<String>,
    #[structopt(long = "external-port", help = "Own external port")]
    pub external_port: Option<u16>,
    #[structopt(long = "no-network", help = "Disable network")]
    pub no_network: bool,
    #[structopt(
        long = "connect-to",
        short = "c",
        help = "Peer to connect to upon startup (host/ip:port)"
    )]
    pub connect_to: Vec<String>,
    #[structopt(
        long = "bootstrap-node",
        help = "Bootstrap node to use upon startup host/ip:port (this disables DNS bootstrapping)"
    )]
    pub bootstrap_node: Vec<String>,
    #[structopt(
        long = "listen-port",
        short = "p",
        help = "Port to listen on",
        default_value = "8888"
    )]
    pub listen_port: u16,
    #[structopt(long = "listen-address", short = "l", help = "Address to listen on")]
    pub listen_address: Option<String>,
    #[structopt(long = "id", short = "i", help = "Set forced node id")]
    pub id: Option<String>,
    #[structopt(long = "debug", short = "d", help = "Debug mode")]
    pub debug: bool,
    #[structopt(long = "trace", help = "Trace mode")]
    pub trace: bool,
    #[structopt(long = "no-rpc-server", help = "Disable the built-in RPC server")]
    pub no_rpc_server: bool,
    #[structopt(
        long = "rpc-server-port",
        help = "RPC server port",
        default_value = "10000"
    )]
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
    #[structopt(
        long = "desired-nodes",
        help = "Desired nodes to always have",
        default_value = "50"
    )]
    pub desired_nodes: u8,
    #[structopt(long = "no-trust-broadcasts", help = "Don't blindly relay broadcasts")]
    pub no_trust_broadcasts: bool,
    #[structopt(
        long = "no-trust-bans",
        help = "Don't blindly trust ban/unban requests"
    )]
    pub no_trust_bans: bool,
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
    #[structopt(
        long = "prometheus-server",
        help = "Enable prometheus server for metrics"
    )]
    pub prometheus_server: bool,
    #[structopt(
        long = "prometheus-push-gateway",
        help = "Enable prometheus via push gateway"
    )]
    pub prometheus_push_gateway: Option<String>,
    #[structopt(
        long = "prometheus-job-name",
        help = "Job name to send to push gateway",
        default_value = "p2p_node_push"
    )]
    pub prometheus_job_name: String,
    #[structopt(
        long = "prometheus-instance-name",
        help = "If not present node_id will be used"
    )]
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
    #[structopt(
        long = "bootstrap-server",
        help = "DNS name to resolve bootstrap nodes from",
        default_value = "bootstrap.p2p.concordium.com"
    )]
    pub bootstrap_server: String,
    #[structopt(long = "no-bootstrap", help = "Do not bootstrap via DNS")]
    pub no_boostrap_dns: bool,
    #[structopt(
        long = "network-id",
        short = "n",
        help = "Enable network id",
        default_value = "1000"
    )]
    pub network_ids: Vec<u16>,
    #[structopt(
        long = "override-config-dir",
        help = "Override location of configuration files"
    )]
    pub config_dir: Option<String>,
    #[structopt(long = "override-data-dir", help = "Override location of data files")]
    pub data_dir: Option<String>,
    #[structopt(
        long = "no-log-timestamp",
        help = "Do not output timestamp in log output"
    )]
    pub no_log_timestamp: bool,
    #[structopt(
        long = "testrunner-url",
        help = "URL for the test runner to submit data to"
    )]
    pub test_runner_url: Option<String>,
    #[structopt(long = "no-dnssec", help = "Do not perform DNSsec tests for lookups")]
    pub no_dnssec: bool,
    #[structopt(long = "dns-resolver", help = "DNS resolver to use")]
    pub dns_resolver: Vec<String>,
    #[structopt(
        long = "resolv-conf",
        help = "Location of resolv.conf",
        default_value = "/etc/resolv.conf"
    )]
    pub resolv_conf: String,
    #[structopt(long = "baker-id", help = "Baker ID")]
    pub baker_id: Option<u64>,
    #[structopt(
        long = "num-bakers",
        help = "Amount of bakers in the network",
        default_value = "60"
    )]
    pub baker_num_bakers: u64,
    #[structopt(
        long = "baker-genesis",
        help = "Genesis time to build with",
        default_value = "0"
    )]
    pub baker_genesis: u64,
    #[structopt(
        long = "minimum-peers-bucket",
        help = "Minimum peers to keep in each bucket always",
        default_value = "100"
    )]
    pub min_peers_bucket: usize,
    #[structopt(long = "enable-tps-test-recv", help = "Enable TPS test recv")]
    pub enable_tps_test: bool,
    #[structopt(long = "tps-test-recv-id", help = "Receiver of TPS test")]
    pub tps_test_recv_id: Option<String>,
    #[structopt(
        long = "tps-test-data-dir",
        help = "Directory containing files to perform TPS test independent of other layers"
    )]
    pub tps_test_data_dir: Option<String>,
    #[structopt(
        long = "tps-stats-save-amount",
        help = "Amount of stats to save for TPS statistics",
        default_value = "10000"
    )]
    pub tps_stats_save_amount: u64,
    #[structopt(
        long = "tps-message-count",
        help = "Amount of messages to be sent and received",
        default_value = "1000"
    )]
    pub tps_message_count: u64,
}

pub fn parse_cli_config() -> CliConfig { CliConfig::from_args() }

#[derive(StructOpt, Debug)]
#[structopt(name = "BootstrapperNode")]
pub struct BootstrapperConfig {
    #[structopt(
        long = "max-nodes",
        help = "Max nodes allowed to connect",
        default_value = "10000"
    )]
    pub max_nodes: u16,
    #[structopt(long = "external-ip", help = "Own external IP")]
    pub external_ip: Option<String>,
    #[structopt(long = "external-port", help = "Own external port")]
    pub external_port: Option<u16>,
    #[structopt(long = "debug", short = "d", help = "Debug mode")]
    pub debug: bool,
    #[structopt(long = "trace", help = "Trace mode")]
    pub trace: bool,
    #[structopt(long = "id", short = "i", help = "Own node id")]
    pub id: String,
    #[structopt(
        long = "listen-port",
        short = "p",
        help = "Port to listen on",
        default_value = "8888"
    )]
    pub listen_port: u16,
    #[structopt(long = "listen-address", short = "l", help = "Address to listen on")]
    pub listen_address: Option<String>,
    #[structopt(
        long = "no-trust-bans",
        help = "Don't blindly trust ban/unban requests"
    )]
    pub no_trust_bans: bool,
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
    #[structopt(
        long = "prometheus-server",
        help = "Enable prometheus server for metrics"
    )]
    pub prometheus_server: bool,
    #[structopt(
        long = "prometheus-push-gateway",
        help = "Enable prometheus via push gateway"
    )]
    pub prometheus_push_gateway: Option<String>,
    #[structopt(
        long = "prometheus-job-name",
        help = "Job name to send to push gateway",
        default_value = "p2p_node_push"
    )]
    pub prometheus_job_name: String,
    #[structopt(
        long = "prometheus-instance-name",
        help = "If not present node_id will be used"
    )]
    pub prometheus_instance_name: Option<String>,
    #[structopt(
        long = "prometheus-push-gateway-interval",
        help = "Interval in seconds between pushes",
        default_value = "2"
    )]
    pub prometheus_push_interval: u64,
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
        long = "network-id",
        short = "n",
        help = "Enable network id",
        default_value = "1000"
    )]
    pub network_ids: Vec<u16>,
    #[structopt(
        long = "override-config-dir",
        help = "Override location of configuration files"
    )]
    pub config_dir: Option<String>,
    #[structopt(long = "override-data-dir", help = "Override location of data files")]
    pub data_dir: Option<String>,
    #[structopt(
        long = "no-log-timestamp",
        help = "Do not output timestamp in log output"
    )]
    pub no_log_timestamp: bool,
    #[structopt(
        long = "minimum-peers-bucket",
        help = "Minimum peers to keep in each bucket always",
        default_value = "100"
    )]
    pub min_peers_bucket: usize,
}

pub fn parse_bootstrapper_config() -> BootstrapperConfig { BootstrapperConfig::from_args() }

#[derive(StructOpt, Debug)]
#[structopt(name = "Test Runner Service")]
pub struct TestRunnerConfig {
    #[structopt(long = "external-ip", help = "Own external IP")]
    pub external_ip: Option<String>,
    #[structopt(long = "external-port", help = "Own external port")]
    pub external_port: Option<u16>,
    #[structopt(long = "id", short = "i", help = "Own node id")]
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
    #[structopt(
        long = "listen-http-port",
        help = "Port to listen for http on",
        default_value = "8950"
    )]
    pub listen_http_port: u16,
    #[structopt(
        long = "listen-http-address",
        help = "Address to listen for http on",
        default_value = "0.0.0.0"
    )]
    pub listen_http_address: String,
    #[structopt(long = "debug", short = "d", help = "Debug mode")]
    pub debug: bool,
    #[structopt(long = "trace", help = "Trace mode")]
    pub trace: bool,
    #[structopt(
        long = "override-config-dir",
        help = "Override location of configuration files"
    )]
    pub config_dir: Option<String>,
    #[structopt(long = "override-data-dir", help = "Override location of data files")]
    pub data_dir: Option<String>,
    #[structopt(
        long = "no-log-timestamp",
        help = "Do not output timestamp in log output"
    )]
    pub no_log_timestamp: bool,
    #[structopt(
        long = "network-id",
        short = "n",
        help = "Enable network id",
        default_value = "1000"
    )]
    pub network_ids: Vec<u16>,
    #[structopt(
        long = "desired-nodes",
        help = "Desired nodes to always have",
        default_value = "50"
    )]
    pub desired_nodes: u8,
    #[structopt(long = "no-bootstrap", help = "Do not bootstrap via DNS")]
    pub no_boostrap_dns: bool,
    #[structopt(
        long = "bootstrap-server",
        help = "DNS name to resolve bootstrap nodes from",
        default_value = "bootstrap.p2p.concordium.com"
    )]
    pub bootstrap_server: String,
    #[structopt(long = "no-trust-broadcasts", help = "Don't blindly relay broadcasts")]
    pub no_trust_broadcasts: bool,
    #[structopt(
        long = "no-trust-bans",
        help = "Don't blindly trust ban/unban requests"
    )]
    pub no_trust_bans: bool,
    #[structopt(
        long = "connect-to",
        short = "c",
        help = "Peer to connect to upon startup (host/ip:port)"
    )]
    pub connect_to: Vec<String>,
    #[structopt(long = "no-dnssec", help = "Do not perform DNSsec tests for lookups")]
    pub no_dnssec: bool,
    #[structopt(long = "dns-resolver", help = "DNS resolver to use")]
    pub dns_resolver: Vec<String>,
    #[structopt(
        long = "bootstrap-node",
        help = "Bootstrap node to use upon startup host/ip:port (this disables DNS bootstrapping)"
    )]
    pub bootstrap_node: Vec<String>,
    #[structopt(
        long = "resolv-conf",
        help = "Location of resolv.conf",
        default_value = "/etc/resolv.conf"
    )]
    pub resolv_conf: String,
    #[structopt(
        long = "minimum-peers-bucket",
        help = "Minimum peers to keep in each bucket always",
        default_value = "100"
    )]
    pub min_peers_bucket: usize,
}

pub fn parse_testrunner_config() -> TestRunnerConfig { TestRunnerConfig::from_args() }

#[derive(Clone, Debug)]
pub struct AppPreferences {
    preferences_map:     Arc<RwLock<PreferencesMap<String>>>,
    override_data_dir:   Option<String>,
    override_config_dir: Option<String>,
}

impl AppPreferences {
    pub fn new(override_conf: Option<String>, override_data: Option<String>) -> Self {
        let file_path = Self::calculate_config_file_path(&override_conf, APP_PREFERENCES_MAIN);
        match OpenOptions::new().read(true).write(true).open(&file_path) {
            Ok(file) => {
                let mut reader = BufReader::new(&file);
                let load_result = PreferencesMap::<String>::load_from(&mut reader);
                if load_result.is_ok() {
                    let mut prefs = load_result.unwrap();
                    if !prefs.contains_key(&APP_PREFERENCES_KEY_VERSION.to_string()) {
                        prefs.insert(
                            APP_PREFERENCES_KEY_VERSION.to_string(),
                            super::VERSION.to_string(),
                        );
                        let mut writer = BufWriter::new(&file);
                        if !prefs.save_to(&mut writer).is_ok() {
                            panic!("Can't write to config file!");
                        }
                    } else if *prefs.get(&APP_PREFERENCES_KEY_VERSION.to_string()).unwrap()
                        != super::VERSION.to_string()
                    {
                        if let Some(ref vers_str) =
                            prefs.get(&APP_PREFERENCES_KEY_VERSION.to_string())
                        {
                            match Version::parse(vers_str) {
                                Ok(vers) => {
                                    let int_vers = Version::parse(super::VERSION).unwrap();
                                    if int_vers.major != vers.major {
                                        panic!(
                                            "Major versions do not match {} != {}",
                                            int_vers.major, vers.major
                                        );
                                    } else if vers.minor > int_vers.minor {
                                        panic!(
                                            "Version file too new {} > {}",
                                            vers.to_string(),
                                            int_vers.to_string()
                                        );
                                    }
                                }
                                Err(e) => panic!("Can't parse version in config file '{}'", e),
                            }
                        };
                    }
                    AppPreferences {
                        preferences_map:     Arc::new(RwLock::new(prefs)),
                        override_data_dir:   override_data,
                        override_config_dir: override_conf,
                    }
                } else {
                    let mut prefs = PreferencesMap::<String>::new();
                    prefs.insert(
                        APP_PREFERENCES_KEY_VERSION.to_string(),
                        super::VERSION.to_string(),
                    );
                    let mut writer = BufWriter::new(&file);
                    if !prefs.save_to(&mut writer).is_ok() {
                        panic!("Can't write to config file!");
                    }
                    writer.flush().ok();
                    AppPreferences {
                        preferences_map:     Arc::new(RwLock::new(prefs)),
                        override_data_dir:   override_data,
                        override_config_dir: override_conf,
                    }
                }
            }
            _ => match File::create(&file_path) {
                Ok(file) => {
                    let mut prefs = PreferencesMap::<String>::new();
                    prefs.insert(
                        APP_PREFERENCES_KEY_VERSION.to_string(),
                        super::VERSION.to_string(),
                    );
                    let mut writer = BufWriter::new(&file);
                    if !prefs.save_to(&mut writer).is_ok() {
                        panic!("Can't write to config file!");
                    }
                    writer.flush().ok();
                    AppPreferences {
                        preferences_map:     Arc::new(RwLock::new(prefs)),
                        override_data_dir:   override_data,
                        override_config_dir: override_conf,
                    }
                }
                _ => panic!("Can't write to config file!"),
            },
        }
    }

    fn calculate_config_path(override_path: &Option<String>) -> PathBuf {
        match override_path {
            Some(ref path) => PathBuf::from(path),
            None => app_root(AppDataType::UserConfig, &APP_INFO).unwrap(),
        }
    }

    fn calculate_data_path(override_path: &Option<String>) -> PathBuf {
        match override_path {
            Some(ref path) => PathBuf::from(path),
            None => app_root(AppDataType::UserData, &APP_INFO).unwrap(),
        }
    }

    fn calculate_config_file_path(override_config_path: &Option<String>, key: &str) -> PathBuf {
        match override_config_path {
            Some(ref path) => {
                let mut new_path = PathBuf::from(path);
                new_path.push(&format!("{}.json", key));
                new_path
            }
            None => {
                let mut path = Self::calculate_config_path(&None);
                path.push(&format!("{}.json", key));
                path
            }
        }
    }

    pub fn set_config(&mut self, key: &str, value: Option<String>) -> bool {
        if let Ok(ref mut store) = safe_write!(self.preferences_map) {
            match value {
                Some(val) => {
                    store.insert(key.to_string(), val);
                }
                _ => {
                    store.remove(&key.to_string());
                }
            }
            let file_path =
                Self::calculate_config_file_path(&self.override_config_dir, APP_PREFERENCES_MAIN);
            match OpenOptions::new().read(true).write(true).open(&file_path) {
                Ok(ref mut file) => {
                    let mut writer = BufWriter::new(file);
                    if !store.save_to(&mut writer).is_ok() {
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
        } else {
            false
        }
    }

    pub fn get_config(&self, key: &str) -> Option<String> {
        match safe_read!(self.preferences_map) {
            Ok(pm) => match pm.get(key) {
                Some(res) => Some(res.to_owned()),
                _ => None,
            },
            Err(_) => None,
        }
    }

    pub fn get_user_app_dir(&self) -> PathBuf { Self::calculate_data_path(&self.override_data_dir) }

    pub fn get_user_config_dir(&self) -> PathBuf {
        Self::calculate_config_path(&self.override_config_dir)
    }
}
