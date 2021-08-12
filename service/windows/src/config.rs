use crate::node::{LogFileConfig, LoggerConfig, NodeConfig};
use anyhow::{anyhow, Context};
use log::Level;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fs;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use toml::Value;
use winreg::{enums::HKEY_LOCAL_MACHINE, RegKey};

/// Look up the config file path in the registry.
pub fn get_config_file_path() -> anyhow::Result<PathBuf> {
    let hklm = RegKey::predef(HKEY_LOCAL_MACHINE);
    let key_name = r"SOFTWARE\Concordium\Node Runner";
    let node_runner_key = hklm.open_subkey(key_name).with_context(|| {
        format!(
            "Could not resolve registry key 'HKEY_LOCAL_MACHINE\\{}'",
            key_name
        )
    })?;
    let value_name = "Config";
    let conf: String = node_runner_key.get_value(value_name).with_context(|| {
        format!(
            "Could not get registry value 'HKEY_LOCAL_MACHINE\\{}\\{}'",
            key_name, value_name
        )
    })?;
    Ok(PathBuf::from(conf))
}

/// Configuration of nodes to run.
pub struct Config {
    pub nodes: Vec<NodeConfig>,
}

/// Convenience macro for looking up a value in a TOML table.
/// This can be used to look up a path and try to treat it as a particular type, as in:
/// ```
/// toml_get_as(as_integer, v, "node", "exe")
/// ```
/// This can be applied to a `Table` or a `Value`.  The first argument should be a function on
/// `Value` that tries to convert it to another type.
macro_rules! toml_get_as {
    ( $as:ident, $v:expr, $( $x:expr),* ) => {
        {(|| {
            let temp = $v;
            $(
                let temp = temp.get($x)?;
            )*
            temp.$as()
        }) ()}
    }
}

/// Given an (optional) TOML table, add all of its entries to a HashMap of environment variables.
/// If there are non-string entries in the table, this will fail, and the supplied name of the
/// table is used in the error message.
fn get_env(
    env_table_opt: Option<&toml::value::Table>,
    table_name: &str,
    env: &mut HashMap<String, String>,
) -> anyhow::Result<()> {
    if let Some(env_table) = env_table_opt {
        for (k, v) in env_table {
            let value = v.as_str().ok_or_else(|| {
                anyhow!(
                    "Environment variables must be strings, but {}.{} is not",
                    table_name,
                    k
                )
            })?;
            env.insert(k.to_string(), value.to_string());
        }
    }
    Ok(())
}

/// Given an (optional) TOML array, produce an (optional) vector of command-line arguments.
/// If any entry is not a string, this fails with an error message that includes the supplied
/// table name.
fn get_args(
    args_array_opt: Option<&toml::value::Array>,
    array_name: &str,
) -> anyhow::Result<Option<Vec<String>>> {
    if let Some(vs) = args_array_opt {
        let mut args = Vec::with_capacity(vs.len());
        for arg in vs {
            let arg_val = arg
                .as_str()
                .ok_or_else(|| anyhow!("{} contains a non-string argument", array_name))?;
            args.push(arg_val.to_string());
        }
        Ok(Some(args))
    } else {
        Ok(None)
    }
}

/// Load a configuration file given the contents of the file and the file's parent folder.
fn load_config_file(conf_str: &str, conf_root: &Path) -> anyhow::Result<Config> {
    // Function to treat a path as though it is relative to conf_root.
    let make_relative_path = |pb: &str| {
        let mut rel_pb = conf_root.to_path_buf();
        rel_pb.push(PathBuf::from(pb));
        rel_pb
    };

    // Parse the file as TOML.
    let conf_val: Value = toml::from_str(conf_str)?;

    // The [common] table.
    let common = toml_get_as!(as_table, &conf_val, "common")
        .cloned()
        .unwrap_or_default();

    // Common node environment variables
    let mut common_node_env = HashMap::new();
    get_env(
        toml_get_as!(as_table, &common, "node", "env"),
        "common.node.env",
        &mut common_node_env,
    )?;
    let common_node_env = common_node_env; // Should not be mutated further

    // Common node arguments
    let common_node_args = get_args(
        toml_get_as!(as_array, &common, "node", "args"),
        "common.node.args",
    )?;

    // Common collector environment variables
    let mut common_collector_env = HashMap::new();
    get_env(
        toml_get_as!(as_table, &common, "collector", "env"),
        "common.collector.env",
        &mut common_collector_env,
    )?;
    let common_collector_env = common_collector_env; // Should not be mutated further

    // Common collector arguments
    let common_collector_args = get_args(
        toml_get_as!(as_array, &common, "collector", "args"),
        "common.collector.args",
    )?;

    let mut nodes = Vec::new();
    if let Some(node_table) = toml_get_as!(as_table, &conf_val, "node") {
        for (nname, node) in node_table {
            // Skip this node if "enabled" is false.
            if !toml_get_as!(as_bool, &node, "enabled").unwrap_or(true) {
                continue;
            }

            let name = toml_get_as!(as_str, &node, "name")
                .unwrap_or_else(|| nname)
                .to_string();
            let bootstrap_nodes = toml_get_as!(as_str, &node, "bootstrap_nodes")
                .or_else(|| toml_get_as!(as_str, &common, "bootstrap_nodes"))
                .ok_or_else(|| anyhow!("Missing string entry node.{}.bootstrap_nodes", nname))?
                .into();
            let config_dir = make_relative_path(
                toml_get_as!(as_str, &node, "config_dir")
                    .ok_or_else(|| anyhow!("Missing string entry node.{}.config_dir", nname))?,
            );
            let data_dir = make_relative_path(
                toml_get_as!(as_str, &node, "data_dir")
                    .ok_or_else(|| anyhow!("Missing string entry node.{}.data_dir", nname))?,
            );
            let baker_credentials =
                toml_get_as!(as_str, &node, "baker_credentials").map(make_relative_path);
            let log_config = if let Some(config) = toml_get_as!(as_str, &node, "log", "config") {
                LoggerConfig::Config(make_relative_path(config))
            } else if let Some(log_path) = toml_get_as!(as_str, &node, "log", "path") {
                let roll_size =
                    toml_get_as!(as_str, &node, "log", "roll", "size").map(String::from);
                let roll_count = toml_get_as!(as_integer, &node, "log", "roll", "count")
                    .and_then(|c| u16::try_from(c).ok());
                let roll_pattern = toml_get_as!(as_str, &node, "log", "roll", "pattern")
                    .map(|pattern| make_relative_path(pattern).to_string_lossy().to_string());
                LoggerConfig::File(LogFileConfig {
                    path: make_relative_path(log_path),
                    roll_size,
                    roll_count,
                    roll_pattern,
                })
            } else {
                LoggerConfig::None
            };
            let log_level = toml_get_as!(as_str, &node, "log", "level")
                .or_else(|| toml_get_as!(as_str, &common, "log", "level"))
                .and_then(|level| Level::from_str(level).ok());
            let rpc_address = if let Some(ip_str) = toml_get_as!(as_str, &node, "rpc", "ip") {
                Some(ip_str.parse()?)
            } else {
                None
            };
            let rpc_port = if let Some(port) = toml_get_as!(as_integer, &node, "rpc", "port") {
                Some(u16::try_from(port)?)
            } else {
                None
            };
            let rpc_enabled = toml_get_as!(as_bool, &node, "rpc", "enabled")
                .or_else(|| toml_get_as!(as_bool, &common, "rpc", "enabled"));
            let rpc_token = toml_get_as!(as_str, &node, "rpc", "token")
                .or_else(|| toml_get_as!(as_str, &common, "rpc", "token"))
                .map(String::from);
            let listen_address = if let Some(ip_str) = toml_get_as!(as_str, &node, "listen", "ip") {
                Some(ip_str.parse()?)
            } else {
                None
            };
            let listen_port = if let Some(port) = toml_get_as!(as_integer, &node, "listen", "port")
            {
                Some(u16::try_from(port)?)
            } else {
                None
            };
            let node_bin = toml_get_as!(as_str, &node, "node", "exe")
                .or_else(|| toml_get_as!(as_str, &common, "node", "exe"))
                .map(make_relative_path);
            // Collector
            let collector_enabled = toml_get_as!(as_bool, &node, "collector", "enabled")
                .or_else(|| toml_get_as!(as_bool, &common, "collector", "enabled"));
            let collector_url = toml_get_as!(as_str, &node, "collector", "url")
                .or_else(|| toml_get_as!(as_str, &common, "collector", "url"))
                .map(String::from);
            let collector_node_name = toml_get_as!(as_str, &node, "collector", "node_name")
                .or_else(|| toml_get_as!(as_str, &common, "collector", "node_name"))
                .map(String::from);
            let collector_interval = if let Some(interval) =
                toml_get_as!(as_integer, &node, "collector", "interval")
                    .or_else(|| toml_get_as!(as_integer, &common, "collector", "interval"))
            {
                Some(u64::try_from(interval)?)
            } else {
                None
            };
            let collector_log_file =
                toml_get_as!(as_str, &node, "collector", "log_file").map(make_relative_path);
            let collector_bin = toml_get_as!(as_str, &node, "collector", "exe")
                .or_else(|| toml_get_as!(as_str, &common, "collector", "exe"))
                .map(make_relative_path);

            let mut node_env = common_node_env.clone();
            get_env(
                toml_get_as!(as_table, &node, "node", "env"),
                &format!("node.{}.node.env", nname),
                &mut node_env,
            )?;

            let node_args = get_args(
                toml_get_as!(as_array, &node, "node", "args"),
                &format!("node.{}.node.args", nname),
            )?
            .or_else(|| common_node_args.clone())
            .unwrap_or_default();

            let mut collector_env = common_collector_env.clone();
            get_env(
                toml_get_as!(as_table, &node, "collector", "env"),
                &format!("node.{}.collector.env", nname),
                &mut collector_env,
            )?;

            let collector_args = get_args(
                toml_get_as!(as_array, &node, "collector", "args"),
                &format!("node.{}.collector.args", nname),
            )?
            .or_else(|| common_collector_args.clone())
            .unwrap_or_default();

            nodes.push(NodeConfig {
                name,
                bootstrap_nodes,
                config_dir,
                data_dir,
                baker_credentials,
                rpc_address,
                rpc_port,
                rpc_enabled,
                rpc_token,
                listen_address,
                listen_port,
                log_config,
                log_level,
                node_bin,
                node_env,
                node_args,
                collector_enabled,
                collector_url,
                collector_node_name,
                collector_interval,
                collector_log_file,
                collector_bin,
                collector_env,
                collector_args,
            });
        }
    }
    Ok(Config { nodes })
}

/// Load the configuration by looking up the configuration file in the registry and then parsing
/// the file.
pub fn load_config() -> anyhow::Result<Config> {
    let conf_path = get_config_file_path()?;
    let conf_str = fs::read_to_string(&conf_path).with_context(|| {
        format!(
            "Could not read configuration file '{}'",
            conf_path.as_path().display(),
        )
    })?;
    let conf_root = conf_path.parent().ok_or_else(|| {
        anyhow!(
            "Could not get parent path of configuration file '{}'",
            conf_path.as_path().display()
        )
    })?;

    load_config_file(conf_str.as_ref(), conf_root).with_context(|| {
        format!(
            "Failed to parse configuration file '{}'",
            conf_path.as_path().display()
        )
    })
}
