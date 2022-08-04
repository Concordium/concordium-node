//! Miscellaneous utilities.

use crate::configuration as config;
use env_logger::{Builder, Env};
use log::LevelFilter;
use std::{
    io::Write,
    net::{SocketAddr, ToSocketAddrs},
    path::Path,
};

pub fn to_hex_string(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{:02x}", b)).collect()
}

/// Setup a log4rs logger based on the given configuration file.
pub fn setup_logger_config(config_file: &Path) {
    log4rs::init_file(config_file, Default::default()).unwrap();
}

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

    crate::macos_log::MacOsLogger::new(subsystem)
        .level_filter(level_filter)
        .category_level_filter("tokio_reactor", LevelFilter::Error)
        .category_level_filter("hyper", LevelFilter::Error)
        .category_level_filter("reqwest", LevelFilter::Error)
        .category_level_filter("gotham", LevelFilter::Error)
        .category_level_filter("h2", LevelFilter::Error)
        .init()
        .expect("Failed to initialise MacOsLogger");
}

pub fn get_bootstrap_nodes(bootstrap_nodes: &[String]) -> Result<Vec<SocketAddr>, String> {
    if !bootstrap_nodes.is_empty() {
        debug!("Resolving bootstrapper nodes.");
        let bootstrap_nodes = bootstrap_nodes
            .iter()
            .filter_map(|ip_port| {
                ToSocketAddrs::to_socket_addrs(ip_port)
                    .map_err(|err| error!("Invalid bootstrapper node received: {}", err))
                    .ok()
            })
            .flatten()
            .collect::<Vec<_>>();
        Ok(bootstrap_nodes)
    } else {
        Err("No bootstrap nodes specified.".to_string())
    }
}

pub fn get_config_and_logging_setup() -> anyhow::Result<(config::Config, config::AppPreferences)> {
    // Get config and app preferences
    let conf = config::parse_config()?;
    let app_prefs = config::AppPreferences::new(
        conf.common.config_dir.to_owned(),
        conf.common.data_dir.to_owned(),
    )?;

    if conf.common.print_config {
        info!("Config:{:?}\n", conf);
    }

    let log_lvl = if conf.common.trace {
        "trace"
    } else if conf.common.debug {
        "debug"
    } else {
        "info"
    };

    #[cfg(target_os = "macos")]
    match conf.macos.use_mac_log {
        Some(ref subsystem) => setup_macos_logger(conf.common.trace, conf.common.debug, subsystem),
        None => setup_logger(conf.common.trace, conf.common.debug, conf.common.no_log_timestamp),
    };

    #[cfg(not(target_os = "macos"))]
    if let Some(ref log_config) = conf.common.log_config {
        setup_logger_config(log_config);
    } else {
        setup_logger(conf.common.trace, conf.common.debug, conf.common.no_log_timestamp);
    }

    info!("Starting up {} version {}!", crate::APPNAME, crate::VERSION);
    info!("Application data directory: {}", app_prefs.get_data_dir().display());
    info!("Application config directory: {}", app_prefs.get_config_dir().display());
    info!(
        "Network: {}",
        if conf.cli.no_network {
            "disabled"
        } else {
            "enabled"
        }
    );

    // FIXME: The semantics of the log level changes when the --log-config flag is
    // used. With regular and macOS logging, the log level printed here is the
    // actual logging level used troughout the node. But with --log-config, you
    // provide a file which can have more fine-grained settings for logging. And
    // in that case, the --info, --debug, --trace flags are only used to set the
    // logging level for consensus.
    info!("Log level: {}", log_lvl);

    Ok((conf, app_prefs))
}
