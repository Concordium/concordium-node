#![recursion_limit = "1024"]
#[macro_use]
extern crate log;

// Force the system allocator on every platform
use futures::FutureExt;
use std::alloc::System;
#[global_allocator]
static A: System = System;

use anyhow::Context;
use concordium_node::{
    common::PeerType,
    configuration as config,
    consensus_ffi::{
        consensus::{
            ConsensusContainer, ConsensusLogLevel, Regenesis, CALLBACK_QUEUE,
            CONSENSUS_QUEUE_DEPTH_IN_HI, CONSENSUS_QUEUE_DEPTH_OUT_HI,
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
    read_or_die,
    rpc::RpcServerImpl,
    spawn_or_die,
    stats_export_service::{instantiate_stats_export_engine, StatsExportService},
    utils::get_config_and_logging_setup,
};
use mio::{net::TcpListener, Poll};
use parking_lot::Mutex as ParkingMutex;
use rand::Rng;
use std::{sync::Arc, thread::JoinHandle};
#[cfg(unix)]
use tokio::signal::unix as unix_signal;
#[cfg(windows)]
use tokio::signal::windows as windows_signal;
use tokio::sync::oneshot;

#[cfg(feature = "instrumentation")]
use concordium_node::stats_export_service::start_push_gateway;
#[cfg(feature = "instrumentation")]
use std::net::{IpAddr, SocketAddr};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let (conf, mut app_prefs) = get_config_and_logging_setup()?;

    let stats_export_service = instantiate_stats_export_engine(&conf)?;
    let regenesis_arc: Arc<Regenesis> = Arc::new(Default::default());

    // Setup task with signal handling.
    let shutdown_signal = {
        let (tx, rx) = oneshot::channel();
        tokio::spawn(async move {
            get_shutdown_signal().await.unwrap();
            info!("Signal received attempting to shutdown node cleanly");
            tx.send(()).unwrap();
            loop {
                get_shutdown_signal().await.unwrap();
                info!(
                    "Signal received to shutdown node cleanly, but an attempt to do so is already \
                     in progress."
                );
            }
        });
        rx
    };

    // The P2PNode thread
    let (node, server, poll) =
        instantiate_node(&conf, &mut app_prefs, stats_export_service, regenesis_arc.clone())
            .context("Failed to create the node.")?;

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

    let data_dir_path = app_prefs.get_data_dir();
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

    // Start the RPC server with a channel for shutting it down.
    let (shutdown_rpc_sender, shutdown_rpc_signal) = oneshot::channel();
    let rpc_server_task = if !conf.cli.rpc.no_rpc_server {
        info!("Starting RPC server");
        let mut serv = RpcServerImpl::new(node.clone(), Some(consensus.clone()), &conf.cli.rpc)
            .context("Cannot create RPC server.")?;

        let task = tokio::spawn(async move {
            serv.start_server(shutdown_rpc_signal.map(|_| ()))
                .await
                .expect("Can't start the RPC server");
        });
        Some(task)
    } else {
        None
    };

    if let Some(ref import_path) = conf.cli.baker.import_path {
        info!("Starting out of band catch-up");
        consensus.import_blocks(import_path.as_bytes());
        info!("Completed out of band catch-up");
    }

    // Consensus queue threads
    let consensus_queue_threads = start_consensus_message_threads(&node, consensus.clone());

    // The P2P node event loop thread
    spawn(&node, server, poll, Some(consensus.clone()));

    // Connect to nodes (args and bootstrap)
    if !conf.cli.no_network {
        establish_connections(&conf, &node)?;
    }

    // Start baking
    consensus.start_baker();

    // Everything is running, so we wait for a signal to shutdown.
    if shutdown_signal.await.is_err() {
        error!("Shutdown signal handler was dropped unexpectedly. Shutting down.");
    }

    // Message rpc to shutdown first
    if let Some(task) = rpc_server_task {
        if shutdown_rpc_sender.send(()).is_err() {
            error!("Could not stop the RPC server correctly. Forcing shutdown.");
            task.abort();
        }
        // Force the rpc server to shut down in at most 10 seconds.
        let timeout_duration = std::time::Duration::from_secs(10);
        match tokio::time::timeout(timeout_duration, task).await {
            Ok(res) => {
                if let Err(err) = res {
                    if err.is_cancelled() {
                        info!("RPC server was successfully shutdown by force.");
                    } else if err.is_panic() {
                        error!("RPC server panicked: {}", err);
                    }
                }
            }
            Err(timed_out) => {
                warn!("RPC server was forcefully shut down due to: {}", timed_out);
            }
        }
    }

    // Shutdown node
    if !node.close() {
        error!("Can't shutdown node properly!");
        std::process::exit(1);
    }

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

/// Construct a future for shutdown signals (for unix: SIGINT and SIGTERM) (for
/// windows: ctrl c and ctrl break). The signal handler is set when the future
/// is polled and until then the default signal handler.
async fn get_shutdown_signal() -> anyhow::Result<()> {
    #[cfg(unix)]
    {
        let mut terminate_stream = unix_signal::signal(unix_signal::SignalKind::terminate())?;
        let mut interrupt_stream = unix_signal::signal(unix_signal::SignalKind::interrupt())?;
        let terminate = Box::pin(terminate_stream.recv());
        let interrupt = Box::pin(interrupt_stream.recv());
        futures::future::select(terminate, interrupt).await;
    }
    #[cfg(windows)]
    {
        let mut ctrl_break_stream = windows_signal::ctrl_break()?;
        let mut ctrl_c_stream = windows_signal::ctrl_c()?;
        let ctrl_break = Box::pin(ctrl_break_stream.recv());
        let ctrl_c = Box::pin(ctrl_c_stream.recv());
        futures::future::select(ctrl_break, ctrl_c).await;
    }
    Ok(())
}

fn instantiate_node(
    conf: &config::Config,
    app_prefs: &mut config::AppPreferences,
    stats_export_service: Arc<StatsExportService>,
    regenesis_arc: Arc<Regenesis>,
) -> anyhow::Result<(Arc<P2PNode>, TcpListener, Poll)> {
    // todo: decide how to manage the private key
    let csprng = &mut rand::rngs::OsRng;
    let secret_key = csprng.gen::<[u8; noiseexplorer_xx::consts::DHLEN]>();
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

    P2PNode::new(secret_key, &conf, PeerType::Node, stats_export_service, regenesis_arc)
}

fn establish_connections(conf: &config::Config, node: &Arc<P2PNode>) -> anyhow::Result<()> {
    info!("Starting the P2P layer");
    connect_to_config_nodes(node);
    if !conf.connection.no_bootstrap_dns {
        attempt_bootstrap(node);
    }
    Ok(())
}

fn connect_to_config_nodes(node: &Arc<P2PNode>) {
    // clone the addresses to release the lock before the relatively expensive
    // connect calls.
    let conns = read_or_die!(node.config.given_addresses).clone();
    // We try to connect to all the given addresses, only warning if we fail.
    // This logic is consistent with subsequent retries in connection
    // housekeeping and means that it is a bit easier to set up a fully
    // connected network of given addresses. Warnings should suffice to detect
    // configuration mistakes.
    for &given_addr in conns.iter() {
        if let Err(e) = connect(node, PeerType::Node, given_addr, None, false) {
            warn!("Could not connect to a given address {}: {}", given_addr, e);
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
    F: FnOnce(ConsensusMessage) -> anyhow::Result<()>, {
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
