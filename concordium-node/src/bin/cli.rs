#![recursion_limit = "1024"]
#[macro_use]
extern crate log;

// Force the system allocator on every platform
use std::{alloc::System, sync::RwLock};
#[global_allocator]
static A: System = System;

use failure::{Fallible, ResultExt};
use mio::Poll;
use parking_lot::Mutex as ParkingMutex;

use concordium_node::{
    common::PeerType,
    configuration as config,
    consensus_ffi::{
        blockchain_types::BlockHash,
        consensus::{
            ConsensusContainer, ConsensusLogLevel, CALLBACK_QUEUE, CONSENSUS_QUEUE_DEPTH_IN_HI,
            CONSENSUS_QUEUE_DEPTH_OUT_HI,
        },
        ffi,
        helpers::QueueMsg,
        messaging::ConsensusMessage,
    },
    p2p::{
        connectivity::connect,
        maintenance::{attempt_bootstrap, spawn},
        *,
    },
    plugins::{self, consensus::*},
    rpc::RpcServerImpl,
    spawn_or_die,
    stats_export_service::{instantiate_stats_export_engine, StatsExportService},
    utils::{self, get_config_and_logging_setup},
};
use rand::Rng;
use std::{
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
    thread::JoinHandle,
};

#[cfg(feature = "instrumentation")]
use concordium_node::stats_export_service::start_push_gateway;
#[cfg(feature = "instrumentation")]
use std::net::{IpAddr, SocketAddr};

#[tokio::main]
async fn main() -> Fallible<()> {
    let (conf, mut app_prefs) = get_config_and_logging_setup()?;
    let shutdown_handler_state = Arc::new(AtomicBool::new(false));

    #[cfg(feature = "staging_net")]
    {
        concordium_node::plugins::staging_net::authenticate(&conf.cli.staging_net_token)
            .await
            .context("Staging network client authentication failed")?;
    }

    let stats_export_service = instantiate_stats_export_engine(&conf)?;
    let regenesis_arc = Arc::new(RwLock::new(vec![]));

    // The P2PNode thread
    let (node, poll) =
        instantiate_node(&conf, &mut app_prefs, stats_export_service, regenesis_arc.clone())
            .context("Failed to create the node.")?;

    // Signal handling closure. so we shut down cleanly
    let signal_closure = |signal_handler_node: &Arc<P2PNode>,
                          shutdown_handler_state: &Arc<AtomicBool>| {
        if !shutdown_handler_state.compare_and_swap(false, true, Ordering::SeqCst) {
            info!("Signal received attempting to shutdown node cleanly");
            if !signal_handler_node.close() {
                error!("Can't shutdown node properly!");
                std::process::exit(1);
            }
        } else {
            info!(
                "Signal received to shutdown node cleanly, but an attempt to do so is already in \
                 progress."
            );
        }
    };

    // Register a SIGTERM handler for a POSIX.1-2001 system
    #[cfg(not(windows))]
    {
        let sigterm_shutdown_handler_state = shutdown_handler_state.clone();
        let signal_hook_node = node.clone();
        unsafe {
            signal_hook::register(signal_hook::SIGTERM, move || {
                signal_closure(&signal_hook_node, &sigterm_shutdown_handler_state)
            })
        }?;
    }

    // Register a safe handler for SIGINT / ^C
    let ctrlc_node = node.clone();
    ctrlc::set_handler(move || signal_closure(&ctrlc_node, &shutdown_handler_state))?;

    #[cfg(feature = "instrumentation")]
    {
        let stats = node.stats.clone();
        let pla = conf
            .prometheus
            .prometheus_listen_addr
            .parse::<IpAddr>()
            .context("Invalid Prometheus address")?;
        let plp = conf.prometheus.prometheus_listen_port;
        tokio::spawn(async move { stats.start_server(SocketAddr::new(pla, plp)).await });
    }

    for resolver in &node.config.dns_resolvers {
        debug!("Using resolver: {}", resolver);
    }

    #[cfg(feature = "instrumentation")]
    // The push gateway to Prometheus thread
    start_push_gateway(&conf.prometheus, &node.stats, node.id());

    let (gen_data, priv_data) = get_baker_data(&app_prefs, &conf.cli.baker)
        .context("Can't get genesis data or private data. Aborting")?;

    let consensus_database_url = if conf.cli.transaction_outcome_logging {
        format!(
            "host={} port={} user={} dbname={} password={}",
            conf.cli.transaction_outcome_logging_database_host,
            conf.cli.transaction_outcome_logging_database_port,
            conf.cli.transaction_outcome_logging_database_username,
            conf.cli.transaction_outcome_logging_database_name,
            conf.cli.transaction_outcome_logging_database_password
        )
    } else {
        String::new()
    };

    let data_dir_path = app_prefs.get_user_app_dir();
    let mut database_directory = data_dir_path.to_path_buf();
    database_directory.push(concordium_node::configuration::DATABASE_SUB_DIRECTORY_NAME);
    if !database_directory.exists() {
        std::fs::create_dir_all(&database_directory)?;
    }

    info!("Starting consensus layer");
    let consensus = plugins::consensus::start_consensus_layer(
        &conf.cli.baker,
        gen_data,
        priv_data,
        if conf.common.no_consensus_logs {
            ConsensusLogLevel::Error
        } else if conf.common.trace {
            ConsensusLogLevel::Trace
        } else if conf.common.debug {
            ConsensusLogLevel::Debug
        } else {
            ConsensusLogLevel::Info
        },
        &database_directory,
        &consensus_database_url,
        regenesis_arc,
    )?;
    info!("Consensus layer started");

    // Start the RPC server
    if !conf.cli.rpc.no_rpc_server {
        let mut serv = RpcServerImpl::new(node.clone(), Some(consensus.clone()), &conf.cli.rpc)
            .context("Cannot create RPC server.")?;
        tokio::spawn(async move {
            serv.start_server().await.expect("Can't start the RPC server");
        });
        info!("RPC server started");
    };

    if let Some(ref import_path) = conf.cli.baker.import_path {
        info!("Starting out of band catch-up");
        consensus.import_blocks(import_path.as_bytes());
        info!("Completed out of band catch-up");
    }

    // Consensus queue threads
    let consensus_queue_threads = start_consensus_message_threads(&node, consensus.clone());

    // The P2P node event loop thread
    spawn(&node, poll, Some(consensus.clone()));

    // Connect to nodes (args and bootstrap)
    if !conf.cli.no_network {
        establish_connections(&conf, &node);
    }

    // start baking
    consensus.start_baker();

    // Wait for the P2PNode to close
    node.join().context("The node thread panicked!")?;

    // Wait for the consensus queue threads to stop
    for consensus_queue_thread in consensus_queue_threads {
        consensus_queue_thread.join().expect("A consensus queue thread panicked");
    }

    // Shut down the consensus layer
    consensus.stop();
    // And finally stop the haskell runtime. It is important that this is the last
    // action after the consensus queues have been stopped and __no__ calls will
    // be made to any Haskell functions. Otherwise this will likely lead to
    // undefined behaviour and/or panics.
    // This will stop any outstanding Haskell threads.
    // It will wait for all blocking FFI calls to terminate, but any interruptible
    // Haskell code, including interruptible FFI calls, will be forcibly stopped.
    ffi::stop_haskell();

    info!("P2PNode gracefully closed.");

    Ok(())
}

fn instantiate_node(
    conf: &config::Config,
    app_prefs: &mut config::AppPreferences,
    stats_export_service: Arc<StatsExportService>,
    regenesis_arc: Arc<RwLock<Vec<BlockHash>>>,
) -> Fallible<(Arc<P2PNode>, Poll)> {
    // If the node id is supplied on the command line (in the conf argument) use it.
    // Otherwise try to look it up from the persistent config.
    let node_id = match conf.common.id {
        None => {
            let maybe_id =
                app_prefs.get_config(config::APP_PREFERENCES_PERSISTED_NODE_ID).context(
                    "Could not read ID from persistent config.\nFix or delete the \
                     `main.config.json` file in the configuration directory.",
                )?;
            // we generate a fresh id here if it is not already present so that it can be
            // stored in the persistent config, even though this is duplicating
            // the logic from the node.
            maybe_id.unwrap_or_else(|| rand::thread_rng().gen())
        }
        Some(id) => id,
    };

    // Failing to persist the node id does not stop the node starting.
    // This failure is unlikely.
    if !app_prefs.set_config(config::APP_PREFERENCES_PERSISTED_NODE_ID, Some(node_id)) {
        error!("Failed to persist own node id.");
    };

    P2PNode::new(Some(node_id), &conf, PeerType::Node, stats_export_service, regenesis_arc)
}

fn establish_connections(conf: &config::Config, node: &Arc<P2PNode>) {
    info!("Starting the P2P layer");
    connect_to_config_nodes(&conf.connection, node);
    if !conf.connection.no_bootstrap_dns {
        attempt_bootstrap(node);
    }
}

fn connect_to_config_nodes(conf: &config::ConnectionConfig, node: &Arc<P2PNode>) {
    for connect_to in &conf.connect_to {
        match utils::parse_host_port(&connect_to, &node.config.dns_resolvers, conf.dnssec_disabled)
        {
            Ok(addrs) => {
                for addr in addrs {
                    let _ = connect(node, PeerType::Node, addr, None).map_err(|e| error!("{}", e));
                }
            }
            Err(err) => error!("Can't parse configured addresses to connect to: {}", err),
        }
    }
}

fn start_consensus_message_threads(
    node: &Arc<P2PNode>,
    consensus: ConsensusContainer,
) -> Vec<JoinHandle<()>> {
    let mut threads: Vec<JoinHandle<()>> = Default::default();

    let node_ref = Arc::clone(node);
    threads.push(spawn_or_die!("inbound consensus requests", {
        let consensus_receiver_high_priority =
            CALLBACK_QUEUE.inbound.receiver_high_priority.lock().unwrap();
        let consensus_receiver_low_priority =
            CALLBACK_QUEUE.inbound.receiver_low_priority.lock().unwrap();
        let cvar = &CALLBACK_QUEUE.inbound.signaler;
        let lock = ParkingMutex::new(false);
        let mut lock_guard = lock.lock();
        let mut exhausted: bool;

        'outer_loop: loop {
            exhausted = false;
            // Update size of queues
            node_ref.stats.set_inbound_low_priority_consensus_size(
                consensus_receiver_low_priority.len() as i64,
            );
            node_ref.stats.set_inbound_high_priority_consensus_size(
                consensus_receiver_high_priority.len() as i64,
            );
            // instead of using `try_iter()` we specifically only loop over the max numbers
            // possible to ever be in the queue
            for _ in 0..CONSENSUS_QUEUE_DEPTH_IN_HI {
                if let Ok(message) = consensus_receiver_high_priority.try_recv() {
                    let stop_loop = !handle_queue_stop(message, "inbound", |msg| {
                        handle_consensus_inbound_msg(&node_ref, &consensus, msg)
                    });
                    if stop_loop {
                        break 'outer_loop;
                    }
                } else {
                    exhausted = true;
                    break;
                }
            }

            if let Ok(message) = consensus_receiver_low_priority.try_recv() {
                exhausted = false;
                let stop_loop = !handle_queue_stop(message, "inbound", |msg| {
                    handle_consensus_inbound_msg(&node_ref, &consensus, msg)
                });
                if stop_loop {
                    break 'outer_loop;
                }
            }

            if exhausted {
                cvar.wait(&mut lock_guard);
            }
        }
    }));

    let node_ref = Arc::clone(node);
    threads.push(spawn_or_die!("outbound consensus requests", {
        let consensus_receiver_high_priority =
            CALLBACK_QUEUE.outbound.receiver_high_priority.lock().unwrap();
        let consensus_receiver_low_priority =
            CALLBACK_QUEUE.outbound.receiver_low_priority.lock().unwrap();
        let cvar = &CALLBACK_QUEUE.outbound.signaler;
        let lock = ParkingMutex::new(false);
        let mut lock_guard = lock.lock();
        let mut exhausted: bool;

        'outer_loop: loop {
            exhausted = false;
            // Update size of queues
            node_ref.stats.set_outbound_low_priority_consensus_size(
                consensus_receiver_low_priority.len() as i64,
            );
            node_ref.stats.set_outbound_high_priority_consensus_size(
                consensus_receiver_high_priority.len() as i64,
            );
            // instead of using `try_iter()` we specifically only loop over the max numbers
            // possible to ever be in the queue
            for _ in 0..CONSENSUS_QUEUE_DEPTH_OUT_HI {
                if let Ok(message) = consensus_receiver_high_priority.try_recv() {
                    let stop_loop = !handle_queue_stop(message, "outbound", |msg| {
                        handle_consensus_outbound_msg(&node_ref, msg)
                    });
                    if stop_loop {
                        break 'outer_loop;
                    }
                } else {
                    exhausted = true;
                    break;
                }
            }

            if let Ok(message) = consensus_receiver_low_priority.try_recv() {
                exhausted = false;
                let stop_loop = !handle_queue_stop(message, "outbound", |msg| {
                    handle_consensus_outbound_msg(&node_ref, msg)
                });
                if stop_loop {
                    break 'outer_loop;
                }
            }

            if exhausted {
                cvar.wait(&mut lock_guard);
            }
        }
    }));

    threads
}

fn handle_queue_stop<F>(msg: QueueMsg<ConsensusMessage>, dir: &'static str, f: F) -> bool
where
    F: FnOnce(ConsensusMessage) -> Fallible<()>, {
    match msg {
        QueueMsg::Relay(msg) => {
            if let Err(e) = f(msg) {
                error!("There's an issue with an {} consensus request: {}", dir, e);
            }
        }
        QueueMsg::Stop => {
            debug!("Closing the {} consensus channel", dir);
            return false;
        }
    }
    true
}

#[cfg(feature = "elastic_logging")]
fn setup_transfer_log_thread(conf: &config::CliConfig) -> JoinHandle<()> {
    use concordium_node::plugins::elasticlogging;

    let (enabled, url) = (conf.elastic_logging_enabled, conf.elastic_logging_url.clone());
    if enabled {
        if let Err(e) = elasticlogging::create_transfer_index(&url) {
            error!("{}", e);
        }
    }
    spawn_or_die!("transfer log", {
        let receiver = consensus_ffi::transferlog::TRANSACTION_LOG_QUEUE.receiver.lock().unwrap();
        loop {
            match receiver.recv() {
                Ok(QueueMsg::Relay(msg)) => {
                    if enabled {
                        if let Err(e) = elasticlogging::log_transfer_event(&url, msg) {
                            error!("{}", e);
                        }
                    } else {
                        info!("{}", msg);
                    }
                }
                Ok(QueueMsg::Stop) => {
                    debug!("Shutting down transfer log queues");
                    break;
                }
                Err(_) => error!("Error receiving a transfer log message from the consensus layer"),
            }
        }
    })
}
