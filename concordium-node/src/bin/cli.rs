#![recursion_limit = "1024"]
#[macro_use]
extern crate log;

// Force the system allocator on every platform
use futures::{stream::StreamExt, FutureExt, TryStreamExt};
use std::{alloc::System, io::Write, sync::atomic};
use tempfile::TempPath;
use tokio::io::AsyncBufReadExt;
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
        helpers::{HashBytes, QueueMsg},
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
use std::{path::Path, sync::Arc, thread::JoinHandle};
#[cfg(unix)]
use tokio::signal::unix as unix_signal;
#[cfg(windows)]
use tokio::signal::windows as windows_signal;
use tokio::sync::{broadcast, oneshot};

#[cfg(feature = "instrumentation")]
use concordium_node::stats_export_service::start_push_gateway;
#[cfg(feature = "instrumentation")]
use std::net::{IpAddr, SocketAddr};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let (conf, mut app_prefs) = get_config_and_logging_setup()?;

    let stats_export_service = instantiate_stats_export_engine(&conf)?;
    let regenesis_arc: Arc<Regenesis> = Arc::new(Default::default());

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

    // Setup task with signal handling before doing any irreversible operations
    // to avoid being interrupted in the middle of sensitive operations, e.g.,
    // creating the database.
    let (mut shutdown_signal_1, mut shutdown_signal_2) = setup_shutdown_signal_handling();

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
        regenesis_arc.clone(),
    )?;
    info!("Consensus layer started");

    // A flag to record that the import was stopped by a signal handler.
    let import_stopped = Arc::new(atomic::AtomicBool::new(false));
    {
        // set up the handler for terminating block state import.
        let consensus = consensus.clone();
        let import_stopped = Arc::clone(&import_stopped);
        tokio::spawn(async move {
            if shutdown_signal_2.recv().await.is_err() {
                error!("Signal handler dropped. This should not happen.");
            }
            import_stopped.store(true, atomic::Ordering::Release);
            consensus.stop_importing_blocks();
        });
    }

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

    maybe_do_out_of_band_catchup(
        &consensus,
        regenesis_arc,
        import_stopped,
        conf.cli.baker.import_blocks_from.as_deref(),
        conf.cli.baker.download_blocks_from.as_ref(),
        data_dir_path,
    )
    .await;

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
    if shutdown_signal_1.recv().await.is_err() {
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

/// Set up shutdown signal handling.
/// Return two receivers that will be notified when the handled signals are
/// triggered. See documentation of [get_shutdown_signal] for details on which
/// signals are handled.
fn setup_shutdown_signal_handling() -> (broadcast::Receiver<()>, broadcast::Receiver<()>) {
    let (sender, receiver1) = broadcast::channel(1);
    let receiver2 = sender.subscribe();
    tokio::spawn(async move {
        get_shutdown_signal().await.unwrap();
        info!("Signal received attempting to shutdown node cleanly");
        sender.send(()).unwrap();
        loop {
            get_shutdown_signal().await.unwrap();
            info!(
                "Signal received to shutdown node cleanly, but an attempt to do so is already in \
                 progress."
            );
        }
    });
    (receiver1, receiver2)
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

    P2PNode::new(Some(node_id), conf, PeerType::Node, stats_export_service, regenesis_arc)
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

/// If either the local import path, or the URL are specified do out of band
/// catchup with them.
/// If the local path is specified that is used, otherwise we try the URL if it
/// is specified.
async fn maybe_do_out_of_band_catchup(
    consensus: &ConsensusContainer,
    regenesis_arc: Arc<Regenesis>,
    import_stopped: Arc<atomic::AtomicBool>,
    import_blocks_from: Option<&Path>,
    download_blocks_from: Option<&reqwest::Url>,
    data_dir_path: &Path,
) {
    // Out-of-band catch-up
    if let Some(import_blocks_from) = import_blocks_from {
        info!("Starting out of band catch-up");
        if let Err(e) = consensus.import_blocks(import_blocks_from) {
            if import_stopped.load(atomic::Ordering::Acquire) {
                info!("Out of band catchup stopped.");
            } else {
                info!(
                    "Could not complete out of band catch-up from {} due to: {:#}.",
                    import_blocks_from.display(),
                    e
                );
            }
        } else {
            info!("Completed out of band catch-up from {}.", import_blocks_from.display());
        }
    } else if let Some(download_url) = download_blocks_from.as_ref().cloned() {
        info!("Starting out of band catch-up");
        let genesis_block_hashes = regenesis_arc.blocks.read().unwrap().clone();
        if let Err(e) =
            import_missing_blocks(consensus, &genesis_block_hashes, download_url, data_dir_path)
                .await
        {
            if import_stopped.load(atomic::Ordering::Acquire) {
                info!("Out of band catchup stopped.");
            } else {
                info!("Could not complete out of band catch-up due to: {:#}.", e);
            }
        } else {
            info!("Completed out of band catch-up from {}.", download_url)
        }
    }
}

// An index entry for a chunk of blocks. Its format must correspond to one
// produced by `database-exporter`.
#[derive(serde::Deserialize)]
struct BlockChunkData {
    // exported chunk of blocks' filename
    filename:           String,
    // genesis block index from which relative heights of blocks in the chunk are counted
    genesis_index:      usize,
    // relative height of the oldest block stored in the chunk
    first_block_height: u64,
    // relative height of the newest block stored in the chunk
    last_block_height:  u64,
}

async fn import_missing_blocks(
    consensus: &ConsensusContainer,
    genesis_block_hashes: &[HashBytes],
    index_url: &url::Url,
    data_dir_path: &Path,
) -> anyhow::Result<()> {
    let current_genesis_index = genesis_block_hashes.len() - 1;
    let last_finalized_block_height = consensus.get_last_finalized_block_height();

    info!("Current genesis index: {}", current_genesis_index);
    info!("Local last finalized block height: {}", last_finalized_block_height);

    let comments_stream = reqwest::get(index_url.clone())
        .await?
        .bytes_stream()
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e));
    let mut comments_lines = tokio_util::io::StreamReader::new(comments_stream).lines();
    while let Some(line) = comments_lines.next_line().await? {
        if line.starts_with("# genesis hash ") {
            let index_genesis_block_hash = line.strip_prefix("# genesis hash ").unwrap();
            let genesis_hash = genesis_block_hashes[0].to_string();
            if index_genesis_block_hash != genesis_hash {
                return Err(anyhow::anyhow!(
                    "The genesis block hash in the catchup index file {} does not match the \
                     genesis block hash {} in the local tree state. Please verify that you chose \
                     the catchup service for the correct chain.",
                    index_genesis_block_hash,
                    genesis_hash
                ));
            } else {
                break;
            }
        }
    }

    let index_reader = reqwest::get(index_url.clone())
        .await?
        .bytes_stream()
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))
        .into_async_read();
    let mut chunk_records = csv_async::AsyncReaderBuilder::new()
        .comment(Some(b'#'))
        .create_deserializer(index_reader)
        .into_deserialize();
    while let Some(result) = chunk_records.next().await {
        let block_chunk_data: BlockChunkData = result?;
        // no need to reimport blocks that are present in the database
        if block_chunk_data.genesis_index < current_genesis_index
            || block_chunk_data.last_block_height <= last_finalized_block_height
        {
            info!(
                "Skipping chunk {}: no blocks above last finalized block height",
                block_chunk_data.filename
            );
            continue;
        }
        let block_chunk_url = index_url.join(&block_chunk_data.filename)?;
        let path = download_chunk(block_chunk_url, data_dir_path).await?;
        consensus.import_blocks(&path)?;
        // attempt to properly clean up the downloaded file.
        if let Err(e) = path.close() {
            error!("Could not delete the downloaded file: {}", e);
        }
    }
    Ok(())
}

async fn download_chunk(download_url: url::Url, data_dir_path: &Path) -> anyhow::Result<TempPath> {
    // Create a temporary file inside the data directory.
    // The data directory is the most reliable place where the node should have
    // write access, that is why it is used.
    let mut temp_file =
        tempfile::NamedTempFile::new_in(data_dir_path).context("Cannot create output file.")?;
    info!("Downloading the catch-up file from {} to {}", download_url, temp_file.path().display());
    {
        let file = temp_file.as_file_mut();
        let mut buffer = std::io::BufWriter::new(file);
        let mut stream = reqwest::get(download_url).await?.bytes_stream();
        while let Some(Ok(bytes)) = stream.next().await {
            buffer.write_all(&bytes)?;
        }
        buffer.flush()?;
    }
    Ok(temp_file.into_temp_path())
}
