use app_dirs::*;
use preferences::{AppInfo, Preferences, PreferencesMap};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use structopt::StructOpt;

const APP_INFO: AppInfo = AppInfo { name: "ConcordiumP2P",
                                    author: "Concordium", };
const APP_PREFERENCES_MAIN: &str = "main/config";
const APP_PREFERENCES_KEY_VERSION: &str = "VERSION";

#[derive(StructOpt, Debug)]
pub struct CliConfig {
    #[structopt(long = "external-ip", help = "Own external IP")]
    pub external_ip: Option<String>,
    #[structopt(long = "external-port", help = "Own external port")]
    pub external_port: Option<u16>,
    #[structopt(long = "ip-discovery-service",
                help = "Use external IP discovery service")]
    pub ip_discovery_service: bool,
    #[structopt(long = "ip-discovery-service-host",
                help = "IP discovery service host",
                default_value = "ipdiscovery.p2p.concordium.com")]
    pub ip_discovery_service_host: String,
    #[structopt(long = "no-network",
                short = "nonet",
                help = "Disable network")]
    pub no_network: bool,
    #[structopt(long = "connect-to",
                short = "c",
                help = "Peer to connect to upon startup")]
    pub connect_to: Vec<String>,
    #[structopt(long = "listen-port",
                short = "p",
                help = "Port to listen on",
                default_value = "8888")]
    pub listen_port: u16,
    #[structopt(long = "listen-address",
                short = "l",
                help = "Address to listen on")]
    pub listen_address: Option<String>,
    #[structopt(long = "id", short = "i", help = "Set forced node id")]
    pub id: Option<String>,
    #[structopt(long = "debug", short = "d", help = "Debug mode")]
    pub debug: bool,
    #[structopt(long = "trace", help = "Trace mode")]
    pub trace: bool,
    #[structopt(long = "no-rpc-server",
                help = "Disable the built-in RPC server")]
    pub no_rpc_server: bool,
    #[structopt(long = "rpc-server-port",
                help = "RPC server port",
                default_value = "10000")]
    pub rpc_server_port: u16,
    #[structopt(long = "rpc-server-addr",
                help = "RPC server listen address",
                default_value = "127.0.0.1")]
    pub rpc_server_addr: String,
    #[structopt(long = "rpc-server-token",
                help = "RPC server access token",
                default_value = "rpcadmin")]
    pub rpc_server_token: String,
    #[structopt(long = "desired-nodes",
                help = "Desired nodes to always have",
                default_value = "50")]
    pub desired_nodes: u8,
    #[structopt(long = "no-trust-broadcasts",
                help = "Don't blindly relay broadcasts")]
    pub no_trust_broadcasts: bool,
    #[structopt(long = "no-trust-bans",
                help = "Don't blindly trust ban/unban requests")]
    pub no_trust_bans: bool,
    #[structopt(long = "prometheus-listen-addr",
                help = "IP to listen for prometheus requests on",
                default_value = "127.0.0.1")]
    pub prometheus_listen_addr: String,
    #[structopt(long = "prometheus-listen-port",
                help = "Port for prometheus to listen on",
                default_value = "9090")]
    pub prometheus_listen_port: u16,
    #[structopt(long = "prometheus-server",
                help = "Enable prometheus server for metrics")]
    pub prometheus_server: bool,
    #[structopt(long = "prometheus-push-gateway",
                help = "Enable prometheus via push gateway")]
    pub prometheus_push_gateway: Option<String>,
    #[structopt(long = "prometheus-job-name",
                help = "Job name to send to push gateway",
                default_value = "p2p_node_push")]
    pub prometheus_job_name: String,
    #[structopt(long = "prometheus-instance-name",
                help = "If not present node_id will be used")]
    pub prometheus_instance_name: Option<String>,
    #[structopt(long = "prometheus-push-gateway-username",
                help = "Username to use for push gateway, if either username or password is omitted authentication isn't used")]
    pub prometheus_push_username: Option<String>,
    #[structopt(long = "prometheus-push-gateway-password",
                help = "Password to use for push gateway, if either username or password is omitted authentication isn't used")]
    pub prometheus_push_password: Option<String>,
    #[structopt(long = "prometheus-push-gateway-interval",
                help = "Interval in seconds between pushes",
                default_value = "2")]
    pub prometheus_push_interval: u64,
    #[structopt(long = "bootstrap-server",
                help = "DNS name to resolve bootstrap nodes from",
                default_value = "bootstrap.p2p.concordium.com")]
    pub bootstrap_server: String,
    #[structopt(long = "no-bootstrap", help = "Do not bootstrap via DNS")]
    pub no_boostrap_dns: bool,
    #[structopt(long = "private-node", help = "Allow RFC1918 peers")]
    pub private_node: bool,
    #[structopt(long = "network-id", short = "n", help = "Enable network id", default_value  = "1000")]
    pub network_ids: Vec<u8>,
}

pub fn parse_cli_config() -> CliConfig {
    CliConfig::from_args()
}

#[derive(StructOpt, Debug)]
#[structopt(name = "BootstrapperNode")]
pub struct BootstrapperConfig {
    #[structopt(long = "max-nodes",
                help = "Max nodes allowed to connect",
                default_value = "10000")]
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
    #[structopt(long = "listen-port",
                short = "p",
                help = "Port to listen on",
                default_value = "8888")]
    pub listen_port: u16,
    #[structopt(long = "listen-address",
                short = "l",
                help = "Address to listen on")]
    pub listen_address: Option<String>,
    #[structopt(long = "no-trust-bans",
                help = "Don't blindly trust ban/unban requests")]
    pub no_trust_bans: bool,
    #[structopt(long = "prometheus-listen-addr",
                help = "IP to listen for prometheus requests on",
                default_value = "127.0.0.1")]
    pub prometheus_listen_addr: String,
    #[structopt(long = "prometheus-listen-port",
                help = "Port for prometheus to listen on",
                default_value = "9090")]
    pub prometheus_listen_port: u16,
    #[structopt(long = "prometheus-server",
                help = "Enable prometheus server for metrics")]
    pub prometheus_server: bool,
    #[structopt(long = "prometheus-push-gateway",
                help = "Enable prometheus via push gateway")]
    pub prometheus_push_gateway: Option<String>,
    #[structopt(long = "prometheus-job-name",
                help = "Job name to send to push gateway",
                default_value = "p2p_node_push")]
    pub prometheus_job_name: String,
    #[structopt(long = "prometheus-instance-name",
                help = "If not present node_id will be used")]
    pub prometheus_instance_name: Option<String>,
    #[structopt(long = "prometheus-push-gateway-interval",
                help = "Interval in seconds between pushes",
                default_value = "2")]
    pub prometheus_push_interval: u64,
    #[structopt(long = "prometheus-push-gateway-username",
                help = "Username to use for push gateway, if either username or password is omitted authentication isn't used")]
    pub prometheus_push_username: Option<String>,
    #[structopt(long = "prometheus-push-gateway-password",
                help = "Password to use for push gateway, if either username or password is omitted authentication isn't used")]
    pub prometheus_push_password: Option<String>,
    #[structopt(long = "private-node", help = "Allow RFC1918 peers")]
    pub private_node: bool,
    #[structopt(long = "network-id", short = "n", help = "Enable network id", default_value  = "1000")]
    pub network_ids: Vec<u8>,
}

pub fn parse_bootstrapper_config() -> BootstrapperConfig {
    BootstrapperConfig::from_args()
}

#[derive(StructOpt, Debug)]
#[structopt(name = "IP Discovery Service")]
pub struct IpDiscoveryConfig {
    #[structopt(long = "listen-port",
                short = "p",
                help = "Port to listen on",
                default_value = "8900")]
    pub listen_port: u16,
    #[structopt(long = "listen-address",
                short = "l",
                help = "Address to listen on",
                default_value = "0.0.0.0")]
    pub listen_address: String,
    #[structopt(long = "prometheus-listen-addr",
                help = "IP to listen for prometheus requests on",
                default_value = "127.0.0.1")]
    pub prometheus_listen_addr: String,
    #[structopt(long = "prometheus-listen-port",
                help = "Port for prometheus to listen on",
                default_value = "9090")]
    pub prometheus_listen_port: u16,
    #[structopt(long = "prometheus-server",
                help = "Enable prometheus server for metrics")]
    pub prometheus_server: bool,
    #[structopt(long = "prometheus-push-gateway",
                help = "Enable prometheus via push gateway")]
    pub prometheus_push_gateway: Option<String>,
    #[structopt(long = "prometheus-job-name",
                help = "Job name to send to push gateway",
                default_value = "p2p_ipdiscovery_push")]
    pub prometheus_job_name: String,
    #[structopt(long = "prometheus-instance-name",
                help = "Instance id to present to prometheus")]
    pub prometheus_instance_name: Option<String>,
    #[structopt(long = "prometheus-push-gateway-interval",
                help = "Interval in seconds between pushes",
                default_value = "2")]
    pub prometheus_push_interval: u64,
    #[structopt(long = "prometheus-push-gateway-username",
                help = "Username to use for push gateway, if either username or password is omitted authentication isn't used")]
    pub prometheus_push_username: Option<String>,
    #[structopt(long = "prometheus-push-gateway-password",
                help = "Password to use for push gateway, if either username or password is omitted authentication isn't used")]
    pub prometheus_push_password: Option<String>,
    #[structopt(long = "debug", short = "d", help = "Debug mode")]
    pub debug: bool,
    #[structopt(long = "trace", help = "Trace mode")]
    pub trace: bool,
    #[structopt(long = "header-name",
                help = "Name of header to fetch remote address from if behind load balancer",
                default_value = "X-Forwarded-For")]
    pub header_name: String,
}

pub fn parse_ipdiscovery_config() -> IpDiscoveryConfig {
    IpDiscoveryConfig::from_args()
}

pub struct AppPreferences {
    preferences_map: Arc<Mutex<PreferencesMap<String>>>,
}

impl AppPreferences {
    pub fn new() -> Self {
        let load_result = PreferencesMap::<String>::load(&APP_INFO, APP_PREFERENCES_MAIN);
        if load_result.is_ok() {
            let mut prefs = load_result.unwrap();
            if !prefs.contains_key(&APP_PREFERENCES_KEY_VERSION.to_string()) {
                prefs.insert(APP_PREFERENCES_KEY_VERSION.to_string(),
                             super::VERSION.to_string());
                if !prefs.save(&APP_INFO, APP_PREFERENCES_MAIN).is_ok() {
                    panic!("Can't write to config file!");
                }
            } else if *prefs.get(&APP_PREFERENCES_KEY_VERSION.to_string()).unwrap()
                      != super::VERSION.to_string()
            {
                panic!("Incorrect version of config file!");
            }
            AppPreferences { preferences_map: Arc::new(Mutex::new(prefs)), }
        } else {
            let mut prefs = PreferencesMap::<String>::new();
            prefs.insert(APP_PREFERENCES_KEY_VERSION.to_string(),
                         super::VERSION.to_string());
            if !prefs.save(&APP_INFO, APP_PREFERENCES_MAIN).is_ok() {
                panic!("Can't write to config file!");
            }
            AppPreferences { preferences_map: Arc::new(Mutex::new(prefs)), }
        }
    }

    pub fn set_config(&mut self, key: String, value: Option<String>) -> bool {
        if let Ok(ref mut store) = self.preferences_map.try_lock() {
            match value {
                Some(val) => {
                    store.insert(key, val);
                }
                _ => {
                    store.remove(&key);
                }
            }
            store.save(&APP_INFO, APP_PREFERENCES_MAIN).is_ok()
        } else {
            false
        }
    }

    pub fn get_config(&self, key: String) -> Option<String> {
        match self.preferences_map.lock().unwrap().get(&key) {
            Some(res) => Some(res.clone()),
            _ => None,
        }
    }

    pub fn get_user_app_dir(&self) -> PathBuf {
        app_root(AppDataType::UserData, &APP_INFO).unwrap()
    }

    pub fn get_user_config_dir(&self) -> PathBuf {
        app_root(AppDataType::UserConfig, &APP_INFO).unwrap()
    }
}
