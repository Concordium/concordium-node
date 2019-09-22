#![recursion_limit = "1024"]
#[cfg(not(target_os = "windows"))]
extern crate grpciounix as grpcio;
#[cfg(target_os = "windows")]
extern crate grpciowin as grpcio;
#[macro_use]
extern crate log;

// Explicitly defining allocator to avoid future reintroduction of jemalloc
use std::alloc::System;
#[global_allocator]
static A: System = System;

use concordium_common::{
    cache::Cache,
    spawn_or_die,
    stats_export_service::{StatsExportService, StatsServiceMode},
    PacketType, RelayOrStopEnvelope, RelayOrStopReceiver,
};
use concordium_consensus::{
    consensus::{ConsensusContainer, CALLBACK_QUEUE},
    ffi,
};
use concordium_global_state::tree::{
    messaging::{DistributionMode, GlobalStateMessage},
    GlobalState,
};
use p2p_client::{
    client::{
        plugins::{self, consensus::*},
        utils as client_utils,
    },
    common::PeerType,
    configuration as config,
    connection::message_handlers::{handle_incoming_packet, handle_retransmit_req},
    network::{NetworkId, NetworkMessage, NetworkRequest, NetworkResponse},
    p2p::*,
    rpc::RpcServerImpl,
    stats_engine::StatsEngine,
    utils::{self, get_config_and_logging_setup, GlobalStateReceivers, GlobalStateSenders},
};

use failure::Fallible;
use rkv::{Manager, Rkv};

use std::{
    sync::{mpsc, Arc},
    thread,
    time::Duration,
};

fn main() -> Fallible<()> {
    let (conf, mut app_prefs) = get_config_and_logging_setup()?;
    if conf.common.print_config {
        // Print out the configuration
        info!("{:?}", conf);
    }

    // Instantiate stats export engine
    let stats_export_service =
        client_utils::instantiate_stats_export_engine(&conf, StatsServiceMode::NodeMode)
            .unwrap_or_else(|e| {
                error!(
                    "I was not able to instantiate the stats export service: {}",
                    e
                );
                None
            });

    info!("Debugging enabled: {}", conf.common.debug);

    let (subscription_queue_in, subscription_queue_out) =
        mpsc::sync_channel(config::RPC_QUEUE_DEPTH);

    // Thread #1: instantiate the P2PNode
    let ((node, receivers), pkt_out) = instantiate_node(
        &conf,
        &mut app_prefs,
        stats_export_service.clone(),
        subscription_queue_in.clone(),
    );

    for resolver in &node.config.dns_resolvers {
        debug!("Using resolver: {}", resolver);
    }

    #[cfg(feature = "instrumentation")]
    // Thread #2 (optional): the push gateway to Prometheus
    client_utils::start_push_gateway(&conf.prometheus, &stats_export_service, node.id())?;

    // Start the P2PNode
    //
    // Thread #2 (#3): P2P event loop
    node.spawn(receivers);

    let is_baker = conf.cli.baker.baker_id.is_some();

    let mut consensus = plugins::consensus::start_consensus_layer(&conf.cli.baker, &app_prefs);

    // Start the RPC server
    let mut rpc_serv = if !conf.cli.rpc.no_rpc_server {
        let mut serv = RpcServerImpl::new(
            node.clone(),
            consensus.clone(),
            &conf.cli.rpc,
            subscription_queue_out,
        );
        serv.start_server()?;
        Some(serv)
    } else {
        None
    };

    // Start the transaction logging thread
    setup_transfer_log_thread(&conf.cli);

    let (global_state_senders, global_state_receivers) = utils::create_global_state_queues();

    // Connect outgoing messages to be forwarded into the baker and RPC streams.
    //
    // Thread #3 (#4): read P2PNode output
    let higer_process_threads = if let Some(ref mut consensus) = consensus {
        start_consensus_threads(
            &node,
            (&conf, &app_prefs),
            consensus.clone(),
            pkt_out,
            global_state_senders.clone(),
            global_state_receivers,
        )
    } else {
        vec![]
    };

    // Create a listener on baker output to forward to the P2PNode
    //
    // Thread #4 (#5): the Baker thread
    let baker_thread = if is_baker {
        Some(start_baker_thread(global_state_senders.clone()))
    } else {
        None
    };

    // Connect to nodes (args and bootstrap)
    if !conf.cli.no_network {
        establish_connections(&conf, &node);
    }

    // Wait for the P2PNode to close
    node.join().expect("The node thread panicked!");

    // Shut down the consensus layer
    if let Some(ref mut consensus) = consensus {
        consensus.stop();
        ffi::stop_haskell();
    }

    // Wait for the higher process threads to stop
    for th in higer_process_threads {
        th.join().expect("Higher process thread panicked")
    }

    // Close the baker thread
    if let Some(th) = baker_thread {
        th.join().expect("Baker sub-thread panicked")
    }

    // Close the RPC server if present
    if let Some(ref mut serv) = rpc_serv {
        serv.stop_server()?;
    }

    // Close the stats server if present
    client_utils::stop_stats_export_engine(&conf, &stats_export_service);

    Ok(())
}

fn instantiate_node(
    conf: &config::Config,
    app_prefs: &mut config::AppPreferences,
    stats_export_service: Option<StatsExportService>,
    subscription_queue_in: mpsc::SyncSender<NetworkMessage>,
) -> (
    (Arc<P2PNode>, Receivers),
    mpsc::Receiver<RelayOrStopEnvelope<NetworkMessage>>,
) {
    let (pkt_in, pkt_out) = mpsc::sync_channel(config::CLI_PACKET_QUEUE_DEPTH);
    let node_id = conf.common.id.clone().map_or(
        app_prefs.get_config(config::APP_PREFERENCES_PERSISTED_NODE_ID),
        |id| {
            if !app_prefs.set_config(config::APP_PREFERENCES_PERSISTED_NODE_ID, Some(id.clone())) {
                error!("Failed to persist own node id");
            };
            Some(id)
        },
    );
    let data_dir_path = app_prefs.get_user_app_dir();

    // Start the thread reading P2PEvents from P2PNode
    let (node, receivers) = if conf.common.debug {
        let (sender, receiver) = mpsc::sync_channel(config::EVENT_LOG_QUEUE_DEPTH);
        let _guard = spawn_or_die!("Log loop", move || loop {
            if let Ok(msg) = receiver.recv() {
                info!("{}", msg);
            }
        });
        P2PNode::new(
            node_id,
            &conf,
            pkt_in,
            Some(sender),
            PeerType::Node,
            stats_export_service,
            subscription_queue_in,
            Some(data_dir_path),
        )
    } else {
        P2PNode::new(
            node_id,
            &conf,
            pkt_in,
            None,
            PeerType::Node,
            stats_export_service,
            subscription_queue_in,
            Some(data_dir_path),
        )
    };

    ((node, receivers), pkt_out)
}

fn establish_connections(conf: &config::Config, node: &P2PNode) {
    info!("Starting the P2P layer");
    connect_to_config_nodes(&conf.connection, node);
    if !conf.connection.no_bootstrap_dns {
        node.attempt_bootstrap();
    }
}

fn connect_to_config_nodes(conf: &config::ConnectionConfig, node: &P2PNode) {
    for connect_to in &conf.connect_to {
        match utils::parse_host_port(
            &connect_to,
            &node.config.dns_resolvers,
            conf.dnssec_disabled,
        ) {
            Ok(addrs) => {
                for addr in addrs {
                    info!("Connecting to peer {}", &connect_to);
                    node.connect(PeerType::Node, addr, None)
                        .unwrap_or_else(|e| debug!("{}", e));
                }
            }
            Err(err) => error!("Can't parse data for node to connect to {}", err),
        }
    }
}

fn start_consensus_threads(
    node: &Arc<P2PNode>,
    (conf, app_prefs): (&config::Config, &config::AppPreferences),
    consensus: ConsensusContainer,
    pkt_out: RelayOrStopReceiver<NetworkMessage>,
    global_state_senders: GlobalStateSenders,
    global_state_receivers: GlobalStateReceivers,
) -> Vec<std::thread::JoinHandle<()>> {
    let is_global_state_persistent = conf.cli.baker.persist_global_state;
    let data_dir_path = app_prefs.get_user_app_dir();

    let node_ref = Arc::clone(node);
    let mut consensus_clone = consensus.clone();
    let network_id = NetworkId::from(conf.common.network_ids[0]); // defaulted so there's always first()
    let global_state_thread = spawn_or_die!("Process global state requests", {
        // Open the GlobalState-exclusive k-v store environment
        let gs_kvs_handle = Manager::singleton()
            .write()
            .expect("Can't write to the kvs manager for GlobalState purposes!")
            .get_or_create(data_dir_path.as_ref(), Rkv::new)
            .expect("Can't load the GlobalState kvs environment!");

        let gs_kvs_env = gs_kvs_handle
            .read()
            .expect("Can't unlock the kvs env for GlobalState!");

        let mut global_state = GlobalState::new(
            &consensus_clone.genesis,
            &gs_kvs_env,
            is_global_state_persistent,
        );

        // consensus_clone.send_global_state_ptr(&global_state);

        let mut loop_interval: u64;
        'outer_loop: loop {
            loop_interval = 100;

            for request in global_state_receivers
                .high_prio
                .try_iter()
                .chain(global_state_receivers.low_prio.try_iter())
            {
                if let GlobalStateMessage::Shutdown = request {
                    warn!("Shutting the global state queues down");
                    break 'outer_loop;
                } else if let Err(e) = handle_global_state_request(
                    &node_ref,
                    network_id,
                    &mut consensus_clone,
                    request,
                    &mut global_state,
                ) {
                    error!("There's an issue with a global state request: {}", e);
                }

                loop_interval = loop_interval.saturating_sub(1);
            }

            thread::sleep(Duration::from_millis(loop_interval));
        }
    });

    let node_ref = Arc::clone(node);
    let gs_senders = global_state_senders.clone();
    let mut _stats_engine = StatsEngine::new(&conf.cli);
    let (_tps_test_enabled, _tps_message_count) = tps_setup_process_output(&conf.cli);
    let guard_pkt = spawn_or_die!("Higher queue processing", {
        use p2p_client::client::plugins::consensus::DeduplicationQueues;
        const DEDUP_QUEUE_SIZE: usize = 32 * 1024;

        let mut _msg_count = 0;
        let mut transactions_cache = Cache::default();
        let mut deduplication_queues = DeduplicationQueues::new(DEDUP_QUEUE_SIZE);

        while let Ok(RelayOrStopEnvelope::Relay(msg)) = pkt_out.recv() {
            match msg {
                NetworkMessage::NetworkPacket(ref pac, ..) => handle_incoming_packet(
                    pac,
                    &gs_senders,
                    &mut transactions_cache,
                    &mut deduplication_queues,
                    &mut _stats_engine,
                    &mut _msg_count,
                    _tps_test_enabled,
                    _tps_message_count,
                ),
                NetworkMessage::NetworkRequest(
                    NetworkRequest::Retransmit(requester, element_type, since, nid),
                    ..
                ) => {
                    handle_retransmit_req(
                        &node_ref,
                        requester,
                        element_type,
                        since,
                        nid,
                        &mut transactions_cache,
                    );
                }
                // TODO: handle the initial catch-up delivery more elegantly
                NetworkMessage::NetworkResponse(
                    NetworkResponse::Handshake(src, ref nets, _),
                    ..
                ) => {
                    if src.peer_type() == PeerType::Node {
                        if let Some(network_id) = nets.iter().next() {
                            // send a catch-up status
                            if send_consensus_msg_to_net(
                                &node_ref,
                                vec![],
                                Some(src.id()),
                                *network_id,
                                PacketType::CatchUpStatus,
                                None,
                                &consensus.get_catch_up_status(),
                            )
                            .is_err()
                            {
                                error!("Can't send the initial catch-up messages!")
                            }
                        } else {
                            error!("A handshaking peer doesn't seem to have any networks!")
                        }
                    }
                }
                _ => {}
            }
        }
    });

    let node_ref = Arc::clone(node);
    let gs_senders = global_state_senders.clone();
    #[allow(unreachable_code)] // the loop never breaks on its own
    let ticker_thread = spawn_or_die!("Ticker", {
        // an initial delay before we begin catching up and baking
        thread::sleep(Duration::from_secs(10));

        loop {
            thread::sleep(Duration::from_secs(u64::from(config::TICKER_INTERVAL_SECS)));

            let current_peers = node_ref.get_node_peer_ids();

            // don't provide the global state with the peer information until their
            // number is within the desired range
            if current_peers.len() <= node_ref.config.max_allowed_nodes as usize {
                let msg = GlobalStateMessage::PeerListUpdate(current_peers);
                if let Err(e) = gs_senders.send_with_priority(msg) {
                    error!("Error updating the global state peer list: {}", e)
                }
            }
        }
    });

    info!(
        "Concordium P2P layer. Network disabled: {}",
        conf.cli.no_network
    );

    vec![global_state_thread, guard_pkt, ticker_thread]
}

fn start_baker_thread(global_state_senders: GlobalStateSenders) -> std::thread::JoinHandle<()> {
    spawn_or_die!("Process consensus messages", {
        let receiver = CALLBACK_QUEUE.receiver.lock().unwrap();
        loop {
            if let Ok(msg) = receiver.recv() {
                match msg {
                    RelayOrStopEnvelope::Relay(msg) => {
                        let is_direct = msg.distribution_mode() == DistributionMode::Direct;
                        let msg = GlobalStateMessage::ConsensusMessage(msg);
                        if if is_direct {
                            global_state_senders.send_with_priority(msg)
                        } else {
                            global_state_senders.send(msg)
                        }
                        .is_err()
                        {
                            error!("Can't pass a consensus message to the global state queue");
                        }
                    }
                    RelayOrStopEnvelope::Stop => {
                        debug!("Shutting down consensus queues");
                        break;
                    }
                }
            } else {
                error!("Error receiving a message from the consensus layer");
            }
        }
    })
}

#[cfg(feature = "benchmark")]
fn tps_setup_process_output(cli: &config::CliConfig) -> (bool, u64) {
    (cli.tps.enable_tps_test, cli.tps.tps_message_count)
}

#[cfg(not(feature = "benchmark"))]
fn tps_setup_process_output(_: &config::CliConfig) -> (bool, u64) { (false, 0) }

#[cfg(feature = "elastic_logging")]
fn setup_transfer_log_thread(conf: &config::CliConfig) -> std::thread::JoinHandle<()> {
    let (enabled, url) = (
        conf.elastic_logging_enabled,
        conf.elastic_logging_url.clone(),
    );
    if enabled {
        if let Err(e) = p2p_client::client::plugins::elasticlogging::create_transfer_index(&url) {
            error!("{}", e);
        }
    }
    spawn_or_die!("Process transfer log messages", {
        let receiver = concordium_consensus::transferlog::TRANSACTION_LOG_QUEUE
            .receiver
            .lock()
            .unwrap();
        loop {
            if let Ok(msg) = receiver.recv() {
                match msg {
                    RelayOrStopEnvelope::Relay(msg) => {
                        if enabled {
                            if let Err(e) =
                                p2p_client::client::plugins::elasticlogging::log_transfer_event(
                                    &url, msg,
                                )
                            {
                                error!("{}", e);
                            }
                        } else {
                            info!("{}", msg);
                        }
                    }
                    RelayOrStopEnvelope::Stop => {
                        debug!("Shutting down transfer log queues");
                        break;
                    }
                }
            } else {
                error!("Error receiving a transfer log message from the consensus layer");
            }
        }
    })
}

#[cfg(not(feature = "elastic_logging"))]
fn setup_transfer_log_thread(_: &config::CliConfig) -> std::thread::JoinHandle<()> {
    spawn_or_die!("Process transfer log messages", {
        let receiver = concordium_consensus::transferlog::TRANSACTION_LOG_QUEUE
            .receiver
            .lock()
            .unwrap();
        loop {
            if let Ok(msg) = receiver.recv() {
                match msg {
                    RelayOrStopEnvelope::Relay(msg) => {
                        info!("{}", msg);
                    }
                    RelayOrStopEnvelope::Stop => {
                        debug!("Shutting down transfer log queues");
                        break;
                    }
                }
            } else {
                error!("Error receiving a transfer log message from the consensus layer");
            }
        }
    })
}
