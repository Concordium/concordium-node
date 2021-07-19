use crate::subprocess::send_child_ctrl_break;
use log::*;
use std::collections::HashMap;
use std::fs::File;
use std::io::{Read, Seek, SeekFrom, Write};
use std::iter::FromIterator;
use std::net::{IpAddr, Ipv4Addr, Ipv6Addr};
use std::os::windows::process::CommandExt;
use std::path::PathBuf;
use std::process::{Child, Command, ExitStatus, Stdio};
use tempfile::{NamedTempFile, TempPath};
use toml::Value;
use winapi::um::winbase::CREATE_NEW_PROCESS_GROUP;

/// Default RPC port
const DEFAULT_RPC_PORT: u16 = 10000;

pub struct LogFileConfig {
    pub path: PathBuf,
    pub roll_size: Option<String>,
    pub roll_count: Option<u16>,
    pub roll_pattern: Option<String>,
}

pub enum LoggerConfig {
    NoLog,
    ConfigLog(PathBuf),
    FileLog(LogFileConfig),
}

/// Runner for a node
pub struct NodeConfig {
    // Name of the node for logging purposes
    pub name: String,
    // Bootstrap node string
    pub bootstrap_nodes: String,
    // Configuration directory for the node
    pub config_dir: PathBuf,
    // Data directory for the node
    pub data_dir: PathBuf,
    // Credentials file (optional)
    pub baker_credentials: Option<PathBuf>,
    // Address to accept GRPC requests on
    pub rpc_address: Option<IpAddr>,
    // Port for GRPC requests
    pub rpc_port: Option<u16>,
    // Whether GRPC is enabled on the node
    pub rpc_enabled: Option<bool>,
    // GRPC authentication token
    pub rpc_token: Option<String>,
    // Address to listen for peer-to-peer connections on
    pub listen_address: Option<IpAddr>,
    // Port for peer-to-peer connections
    pub listen_port: Option<u16>,
    // Log configuration file; if specified, other log optsion
    pub log_config: LoggerConfig,
    // log level
    pub log_level: Option<Level>,
    // Node binary
    pub node_bin: Option<PathBuf>,
    // Node environment variables
    pub node_env: HashMap<String, String>,
    // Node arguments
    pub node_args: Vec<String>,
    // Collector enabled
    pub collector_enabled: Option<bool>,
    // Collector URL
    pub collector_url: Option<String>,
    // Collector node name
    pub collector_node_name: Option<String>,
    // Collector interval
    pub collector_interval: Option<u64>,
    // Collector logging file
    pub collector_log_file: Option<PathBuf>,
    // Collector binary
    pub collector_bin: Option<PathBuf>,
    // Collector environment variables
    pub collector_env: HashMap<String, String>,
    // Collector arguments
    pub collector_args: Vec<String>,
}

/// A started node.
pub struct Node {
    /// The child process for the node.
    node_process: Child,
    /// The configuration settings used to start the node.
    pub node_config: NodeConfig,
    /// A temporary file used to redirect the node's stderr.
    stderr_path: TempPath,
    /// The child process for the collector.
    collector_process: Option<Child>,
    /// A temporary file used for configuring the node's logging.
    /// (When this is dropped, the file will be deleted.)
    #[allow(dead_code)]
    log_config_file: Option<TempPath>,
}

impl NodeConfig {
    /// Start a collector for this node configuration.
    fn start_collector(&self) -> anyhow::Result<Option<Child>> {
        let run_collector = self.collector_enabled.unwrap_or(true)
            && self.collector_url.is_some()
            && self.rpc_enabled != Some(false);
        if self.collector_enabled == Some(true) && !run_collector {
            let reason = if self.collector_url.is_none() {
                "collector.url was not specified"
            } else if self.rpc_enabled == Some(false) {
                "rpc is disabled"
            } else {
                "was not configured correctly"
            };
            anyhow::bail!("The collector was enabled, but {}.", reason);
        }
        let collector_process = if run_collector {
            // If a collector binary is specified, use that. Default to "node-collector.exe" in the
            // same directory as the service binary.
            let collector_bin_path = if let Some(path) = &self.collector_bin {
                path.clone()
            } else {
                let mut path = std::env::current_exe()?;
                path.pop();
                path.push("node-collector.exe");
                path
            };
            let mut collector_cmd = Command::new(collector_bin_path);

            collector_cmd.envs(&self.collector_env);

            // We have already checked that the URL is specified, so unwrap won't fail
            collector_cmd.env(
                "CONCORDIUM_NODE_COLLECTOR_URL",
                self.collector_url.as_ref().unwrap(),
            );

            // If the rpc address is not given, or is unspecified, default to local host
            let mut collector_rpc_ip = self.rpc_address.unwrap_or(IpAddr::V4(Ipv4Addr::LOCALHOST));
            if collector_rpc_ip.is_unspecified() {
                collector_rpc_ip = if collector_rpc_ip.is_ipv4() {
                    IpAddr::V4(Ipv4Addr::LOCALHOST)
                } else {
                    IpAddr::V6(Ipv6Addr::LOCALHOST)
                };
            }
            collector_cmd.env(
                "CONCORDIUM_NODE_COLLECTOR_GRPC_HOST",
                if collector_rpc_ip.is_ipv4() {
                    format!(
                        "http://{}:{}",
                        collector_rpc_ip,
                        self.rpc_port.unwrap_or(DEFAULT_RPC_PORT)
                    )
                } else {
                    format!(
                        "http://[{}]:{}",
                        collector_rpc_ip,
                        self.rpc_port.unwrap_or(DEFAULT_RPC_PORT)
                    )
                },
            );

            collector_cmd.env(
                "CONCORDIUM_NODE_COLLECTOR_NODE_NAME",
                self.collector_node_name
                    .clone()
                    .unwrap_or_else(|| self.name.clone()),
            );

            if let Some(interval) = self.collector_interval {
                collector_cmd.env(
                    "CONCORDIUM_NODE_COLLECTOR_COLLECT_INTERVAL",
                    interval.to_string(),
                );
            }

            if let Some(auth_token) = &self.rpc_token {
                collector_cmd.env(
                    "CONCORDIUM_NODE_COLLECTOR_GRPC_AUTHENTICATION_TOKEN",
                    &auth_token,
                );
            }

            collector_cmd.args(&self.collector_args);

            collector_cmd.stdin(Stdio::null());
            collector_cmd.stdout(Stdio::null());
            if let Some(lf) = &self.collector_log_file {
                let log_file = std::fs::File::create(lf)?;
                collector_cmd.stderr(log_file);
            } else {
                collector_cmd.stderr(Stdio::null());
            }

            Some(collector_cmd.spawn()?)
        } else {
            None
        };
        Ok(collector_process)
    }

    /// Start a node and its collector with this configuration.
    pub fn start(self) -> anyhow::Result<Node> {
        let bin_path = if let Some(path) = &self.node_bin {
            path.clone()
        } else {
            let mut path = std::env::current_exe()?;
            path.pop();
            path.push("concordium-node.exe");
            path
        };
        let mut cmd = Command::new(bin_path);
        cmd.envs(&self.node_env);
        cmd.env("CONCORDIUM_NODE_CONFIG_DIR", self.config_dir.clone());
        cmd.env("CONCORDIUM_NODE_DATA_DIR", self.data_dir.clone());
        cmd.env(
            "CONCORDIUM_NODE_CONNECTION_BOOTSTRAP_NODES",
            self.bootstrap_nodes.clone(),
        );
        self.baker_credentials
            .as_ref()
            .map(|bcred| cmd.env("CONCORDIUM_NODE_BAKER_CREDENTIALS_FILE", bcred.clone()));
        self.rpc_address
            .as_ref()
            .map(|rpcaddr| cmd.env("CONCORDIUM_NODE_RPC_SERVER_ADDR", rpcaddr.to_string()));
        self.rpc_port
            .as_ref()
            .map(|rpcport| cmd.env("CONCORDIUM_NODE_RPC_SERVER_PORT", rpcport.to_string()));
        self.rpc_token
            .as_ref()
            .map(|rpctoken| cmd.env("CONCORDIUM_NODE_RPC_SERVER_TOKEN", &rpctoken));
        if Some(false) == self.rpc_enabled {
            cmd.env("CONCORDIUM_NODE_DISABLE_RPC_SERVER", "true");
        }
        self.listen_address
            .as_ref()
            .map(|listenaddr| cmd.env("CONCORDIUM_NODE_LISTEN_ADDRESS", listenaddr.to_string()));
        self.listen_port
            .as_ref()
            .map(|listenport| cmd.env("CONCORDIUM_NODE_LISTEN_PORT", listenport.to_string()));
        match &self.log_level {
            Some(Level::Error) => {
                cmd.env("CONCORDIUM_NODE_NO_CONSENSUS_LOG", "true");
            }
            Some(Level::Info) => {
                cmd.env("CONCORDIUM_NODE_LOG_LEVEL_INFO", "true");
            }
            Some(Level::Debug) => {
                cmd.env("CONCORDIUM_NODE_LOG_LEVEL_DEBUG", "true");
            }
            Some(Level::Trace) => {
                cmd.env("CONCORDIUM_NODE_LOG_LEVEL_TRACE", "true");
            }
            _ => {}
        }
        let log_config_file = make_log_config_file(&self.log_config, &self.log_level, &mut cmd)?;
        cmd.args(&self.node_args);
        cmd.creation_flags(CREATE_NEW_PROCESS_GROUP);
        cmd.stdin(Stdio::null());
        cmd.stdout(Stdio::null());
        let (stderr_file, stderr_path) = NamedTempFile::new()?.into_parts();
        cmd.stderr(Stdio::from(stderr_file));

        let node_process = cmd.spawn()?;

        let collector_process = self.start_collector().unwrap_or_else(|e| {
            error!(
                "Failed to start collector for node '{}': {:#}",
                self.name, e
            );
            None
        });

        Ok(Node {
            node_process,
            node_config: self,
            stderr_path,
            collector_process,
            log_config_file,
        })
    }
}

/// Where necessary, generate a temporary log config file. This also sets the relevant environment
/// variable for the node to find the log config file.
fn make_log_config_file(
    logger_config: &LoggerConfig,
    log_level: &Option<Level>,
    cmd: &mut Command,
) -> anyhow::Result<Option<TempPath>> {
    match &logger_config {
        LoggerConfig::NoLog => {
            let temp_file = tempfile::Builder::new()
                .prefix("node-log-conf")
                .suffix(".toml")
                .tempfile()?;
            let (mut lc_file, lc_path) = temp_file.into_parts();
            lc_file.write_all(b"[root]\nlevel = 'error'\nappenders = []")?;
            lc_file.sync_data()?;
            drop(lc_file);
            cmd.env("CONCORDIUM_NODE_LOG_CONFIG", &lc_path);
            Ok(Some(lc_path))
        }
        LoggerConfig::ConfigLog(log_config_file) => {
            cmd.env("CONCORDIUM_NODE_LOG_CONFIG", &log_config_file);
            Ok(None)
        }
        LoggerConfig::FileLog(log_config) => {
            use toml::map::Map;

            let mut appenders_file_map = Map::with_capacity(3);
            appenders_file_map.insert(
                "path".to_string(),
                Value::String(log_config.path.to_string_lossy().to_string()),
            );
            if let Some(roll_size) = &log_config.roll_size {
                appenders_file_map.insert(
                    "kind".to_string(),
                    Value::String("rolling_file".to_string()),
                );
                let roll_count = log_config.roll_count.unwrap_or(0);
                let roller_map = if roll_count == 0 {
                    Map::from_iter([("kind".to_string(), Value::String("delete".to_string()))])
                } else {
                    let pattern = log_config.roll_pattern.clone().unwrap_or_else(|| {
                        let new_extension = if let Some(old_extension) = log_config.path.extension()
                        {
                            format!("{}.{}", "{}", old_extension.to_str().unwrap_or_default())
                        } else {
                            "{}".to_string()
                        };
                        log_config
                            .path
                            .with_extension(new_extension)
                            .to_string_lossy()
                            .to_string()
                    });
                    Map::from_iter([
                        (
                            "kind".to_string(),
                            Value::String("fixed_window".to_string()),
                        ),
                        ("count".to_string(), Value::Integer(roll_count.into())),
                        ("pattern".to_string(), Value::String(pattern)),
                    ])
                };
                appenders_file_map.insert(
                    "policy".to_string(),
                    Value::Table(Map::from_iter([
                        ("kind".to_string(), Value::String("compound".to_string())),
                        (
                            "trigger".to_string(),
                            Value::Table(Map::from_iter([
                                ("kind".to_string(), Value::String("size".to_string())),
                                ("limit".to_string(), Value::String(roll_size.clone())),
                            ])),
                        ),
                        ("roller".to_string(), Value::Table(roller_map)),
                    ])),
                );
            } else {
                appenders_file_map.insert("kind".to_string(), Value::String("file".to_string()));
            }

            let log_toml = Value::Table(Map::from_iter([
                (
                    "root".to_string(),
                    Value::Table(Map::from_iter([
                        (
                            "level".to_string(),
                            // Default to 'warn' log level.
                            Value::String(log_level.unwrap_or(Level::Warn).to_string()),
                        ),
                        (
                            "appenders".to_string(),
                            Value::Array(vec![Value::String("file".to_string())]),
                        ),
                    ])),
                ),
                (
                    "appenders".to_string(),
                    Value::Table(Map::from_iter([(
                        "file".to_string(),
                        Value::Table(appenders_file_map),
                    )])),
                ),
            ]));
            let temp_file = tempfile::Builder::new()
                .prefix("node-log-conf")
                .suffix(".toml")
                .tempfile()?;
            let (mut lc_file, lc_path) = temp_file.into_parts();
            lc_file.write_all(toml::to_string(&log_toml)?.as_bytes())?;
            lc_file.sync_data()?;
            drop(lc_file);
            cmd.env("CONCORDIUM_NODE_LOG_CONFIG", &lc_path);
            Ok(Some(lc_path))
        }
    }
}

/// The maximum length of a string to obtain from the end of the error output.
const ERROR_MAX_LENGTH: u64 = 1000;

impl Node {
    /// Check whether the node has exited.
    /// If there is a collector process, check if that is alive, and kill it if the node has
    /// exited.
    /// The return value is the exit status of the node process.
    pub fn check_exit(&mut self) -> anyhow::Result<Option<ExitStatus>> {
        let exit_status = self.node_process.try_wait()?;
        if let Some(collector) = &mut self.collector_process {
            if exit_status.is_some() {
                if collector.try_wait().map(|x| x.is_none()).unwrap_or(false) {
                    collector
                        .kill()
                        .unwrap_or_else(|e| error!("Failed to kill collector process: {:#}", e));
                }
            } else {
                match collector.try_wait() {
                    Ok(Some(e)) => {
                        error!(
                            "The collector for node '{}' exited unexpectedly. ({})",
                            self.node_config.name, e
                        );
                        self.collector_process = None;
                    }
                    Err(e) => {
                        error!(
                            "Could not get the status of the collector for node '{}': {:#}",
                            self.node_config.name, e
                        );
                        self.collector_process = None;
                    }
                    _ => {}
                }
            }
        }
        Ok(exit_status)
    }
    /// Signal the node to shutdown. This does not affect the collector.
    pub fn shutdown(&mut self) -> anyhow::Result<()> {
        send_child_ctrl_break(&self.node_process)?;
        Ok(())
    }
    /// Kill the node and any collector process.
    pub fn force_shutdown(&mut self) -> anyhow::Result<()> {
        let r1 = self.node_process.kill();
        if let Some(cp) = &mut self.collector_process {
            let r2 = cp.kill();
            r1?;
            r2?
        } else {
            r1?
        }
        Ok(())
    }
    /// Get the end of the error output.
    pub fn get_error_output(&mut self) -> anyhow::Result<String> {
        let mut stderr_file = File::open(&self.stderr_path)?;
        let length = stderr_file.metadata()?.len();
        if length == 0 {
            Ok(String::default())
        } else if length <= ERROR_MAX_LENGTH {
            let mut buf = String::with_capacity(length as usize);
            stderr_file.read_to_string(&mut buf)?;
            Ok(buf)
        } else {
            stderr_file.seek(SeekFrom::End(-(ERROR_MAX_LENGTH as i64)))?;
            let mut buf = String::with_capacity(ERROR_MAX_LENGTH as usize);
            stderr_file.read_to_string(&mut buf)?;
            if let Some((_, rest)) = buf.split_once('\n') {
                Ok(String::from(rest))
            } else {
                Ok(buf)
            }
        }
    }
}
