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
    spawn_or_die,
    QueueMsg::{self, Relay},
};
use consensus_rust::{
    consensus::{
        ConsensusContainer, ConsensusLogLevel, CALLBACK_QUEUE, CONSENSUS_QUEUE_DEPTH_IN_HI,
        CONSENSUS_QUEUE_DEPTH_OUT_HI,
    },
    ffi,
};
use globalstate_rust::tree::{messaging::ConsensusMessage, GlobalState};
use p2p_client::{
    client::{
        plugins::{self, consensus::*},
        utils as client_utils,
    },
    common::{get_current_stamp, P2PNodeId, PeerType},
    configuration as config,
    network::{NetworkId, NetworkMessage},
    p2p::*,
    rpc::RpcServerImpl,
    stats_export_service::{StatsExportService, StatsServiceMode},
    utils::{self, get_config_and_logging_setup},
};
use parking_lot::Mutex as ParkingMutex;

use failure::Fallible;
use rkv::{Manager, Rkv};

use std::{
    sync::{mpsc, Arc},
    thread::{self, JoinHandle},
    time::Duration,
};

fn main() -> Fallible<()> {
    let (conf, mut app_prefs) = get_config_and_logging_setup()?;
    if conf.common.print_config {
        // Print out the configuration
        info!("{:?}", conf);
    }

    #[cfg(feature = "beta")]
    {
        use failure::bail;
        if !p2p_client::client::plugins::beta::authenticate(
            &conf.cli.beta_username,
            &conf.cli.beta_token,
        ) {
            bail!("Beta client authentication failed");
        }
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
    let node = instantiate_node(
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
    client_utils::start_push_gateway(&conf.prometheus, &stats_export_service, node.id());

    // Start the P2PNode
    //
    // Thread #2 (#3): P2P event loop
    node.spawn();

    let is_baker = conf.cli.baker.baker_id.is_some();

    let data_dir_path = app_prefs.get_user_app_dir();
    let (gen_data, prov_data) = get_baker_data(&app_prefs, &conf.cli.baker, is_baker)
        .expect("Can't get genesis data or private data. Aborting");
    let gs_kvs_handle = Manager::singleton()
        .write()
        .expect("Can't write to the kvs manager for GlobalState purposes!")
        .get_or_create(data_dir_path.as_ref(), Rkv::new)
        .expect("Can't load the GlobalState kvs environment!");

    if let Err(e) = gs_kvs_handle
        .write()
        .unwrap()
        .set_map_size(1024 * 1024 * 256)
    {
        error!("Can't set up the desired RKV map size: {}", e);
    }

    let global_state = GlobalState::new(
        &gen_data,
        gs_kvs_handle,
        conf.cli.baker.persist_global_state,
    );

    let consensus = plugins::consensus::start_consensus_layer(
        &conf.cli.baker,
        global_state,
        gen_data,
        prov_data,
        if conf.common.trace {
            ConsensusLogLevel::Trace
        } else if conf.common.debug {
            ConsensusLogLevel::Debug
        } else if conf.common.info {
            ConsensusLogLevel::Info
        } else {
            ConsensusLogLevel::Warning
        },
    );

    // Start the RPC server
    let mut rpc_serv = if !conf.cli.rpc.no_rpc_server {
        let mut serv = RpcServerImpl::new(
            node.clone(),
            Some(consensus.clone()),
            &conf.cli.rpc,
            subscription_queue_out,
            get_baker_private_data_json_file(&app_prefs, &conf.cli.baker),
        );
        serv.start_server()?;
        Some(serv)
    } else {
        None
    };

    // Start the transaction logging thread
    setup_transfer_log_thread(&conf.cli);

    // Connect outgoing messages to be forwarded into the baker and RPC streams.
    //
    // Thread #3 (#4): read P2PNode output
    let consensus_queue_threads = start_consensus_message_threads(&node, &conf, consensus.clone());

    info!(
        "Concordium P2P layer. Network disabled: {}",
        conf.cli.no_network
    );

    // Connect to nodes (args and bootstrap)
    if !conf.cli.no_network {
        establish_connections(&conf, &node);
    }

    // Wait for the P2PNode to close
    node.join().expect("The node thread panicked!");

    // Shut down the consensus layer

    consensus.stop();
    ffi::stop_haskell();

    // Wait for the consensus queue threads to stop

    for consensus_queue_thread in consensus_queue_threads {
        consensus_queue_thread
            .join()
            .expect("A consensus queue thread panicked");
    }

    // Close the RPC server if present
    if let Some(ref mut serv) = rpc_serv {
        serv.stop_server()?;
    }

    // Close the stats server if present
    client_utils::stop_stats_export_engine(&conf, &stats_export_service);

    info!("P2PNode gracefully closed.");

    Ok(())
}

fn instantiate_node(
    conf: &config::Config,
    app_prefs: &mut config::AppPreferences,
    stats_export_service: Option<StatsExportService>,
    subscription_queue_in: mpsc::SyncSender<NetworkMessage>,
) -> Arc<P2PNode> {
    let node_id = match conf.common.id.clone() {
        None => match app_prefs.get_config(config::APP_PREFERENCES_PERSISTED_NODE_ID) {
            None => {
                let new_id: P2PNodeId = Default::default();
                Some(new_id.to_string())
            }
            Some(id) => Some(id),
        },
        Some(id) => Some(id),
    };

    if !app_prefs.set_config(config::APP_PREFERENCES_PERSISTED_NODE_ID, node_id.clone()) {
        error!("Failed to persist own node id");
    };

    match conf.common.id.clone().map_or(
        app_prefs.get_config(config::APP_PREFERENCES_PERSISTED_NODE_ID),
        |id| {
            if !app_prefs.set_config(config::APP_PREFERENCES_PERSISTED_NODE_ID, Some(id.clone())) {
                error!("Failed to persist own node id");
            };
            Some(id)
        },
    ) {
        Some(id) => Some(id),
        None => {
            let new_id: P2PNodeId = Default::default();
            if !app_prefs.set_config(
                config::APP_PREFERENCES_PERSISTED_NODE_ID,
                Some(new_id.to_string()),
            ) {
                error!("Failed to persist own node id");
            };
            Some(new_id.to_string())
        }
    };

    let data_dir_path = app_prefs.get_user_app_dir();

    // Start the thread reading P2PEvents from P2PNode
    if conf.common.debug {
        let (sender, receiver) = mpsc::sync_channel(config::EVENT_LOG_QUEUE_DEPTH);
        let _guard = spawn_or_die!("Log loop", move || loop {
            if let Ok(Relay(msg)) = receiver.recv() {
                info!("{}", msg);
            }
        });
        P2PNode::new(
            node_id,
            &conf,
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
            None,
            PeerType::Node,
            stats_export_service,
            subscription_queue_in,
            Some(data_dir_path),
        )
    }
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

fn start_consensus_message_threads(
    node: &Arc<P2PNode>,
    conf: &config::Config,
    consensus: ConsensusContainer,
) -> (Vec<JoinHandle<()>>) {
    let mut threads: Vec<JoinHandle<()>> = Default::default();
    let nid = NetworkId::from(conf.common.network_ids[0]); // defaulted so there's always first()

    let peers = Default::default();
    let node_peers_ref = Arc::clone(node);
    let peers_thread_ref = Arc::clone(&peers);
    let mut consensus_peers_ref = consensus.clone();
    threads.push(spawn_or_die!(
        "Peers status notifier thread for consensus",
        {
            // don't do anything until the peer number is within the desired range
            while node_peers_ref.get_node_peer_ids().len()
                > node_peers_ref.config.max_allowed_nodes as usize
            {
                thread::sleep(Duration::from_secs(1));
            }

            let peer_stats_notifier_control_queue_receiver =
                CALLBACK_QUEUE.receiver_peer_notifier.lock().unwrap();
            let mut last_peer_list_update = 0;
            loop {
                if node_peers_ref.last_peer_update() > last_peer_list_update {
                    update_peer_list(&node_peers_ref, Arc::clone(&peers_thread_ref));
                    last_peer_list_update = get_current_stamp();
                }

                if let Err(e) = check_peer_states(
                    &node_peers_ref,
                    nid,
                    &mut consensus_peers_ref,
                    Arc::clone(&peers_thread_ref),
                ) {
                    error!("Couldn't update the catch-up peer list: {}", e);
                }

                if let Ok(msg) = peer_stats_notifier_control_queue_receiver.try_recv() {
                    if let QueueMsg::Stop = msg {
                        break;
                    }
                }

                thread::sleep(Duration::from_millis(200));
            }
        }
    ));

    let node_in_ref = Arc::clone(node);
    let peers_in_ref = Arc::clone(&peers);
    let mut consensus_in_ref = consensus.clone();
    threads.push(spawn_or_die!("Process inbound consensus requests", {
        // don't do anything until the peer number is within the desired range
        while node_in_ref.get_node_peer_ids().len() > node_in_ref.config.max_allowed_nodes as usize
        {
            thread::sleep(Duration::from_secs(1));
        }

        let consensus_receiver_high_priority = CALLBACK_QUEUE
            .inbound
            .receiver_high_priority
            .lock()
            .unwrap();
        let consensus_receiver_low_priority =
            CALLBACK_QUEUE.inbound.receiver_low_priority.lock().unwrap();
        let cvar = &*CALLBACK_QUEUE.inbound.signaler;
        let lock = ParkingMutex::new(false);
        let mut lock_guard = lock.lock();
        let mut exhausted: bool;

        'outer_loop: loop {
            exhausted = false;
            // instead of using `try_iter()` we specifically only loop over the max amounts
            // possible to ever be in the queue
            for _ in 0..CONSENSUS_QUEUE_DEPTH_IN_HI {
                if let Ok(message) = consensus_receiver_high_priority.try_recv() {
                    let stop_loop = !handle_inbound_message(message, |msg| {
                        handle_consensus_inbound_message(
                            &node_in_ref,
                            nid,
                            &mut consensus_in_ref,
                            msg,
                            Arc::clone(&peers_in_ref),
                        )
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
                let stop_loop = !handle_inbound_message(message, |msg| {
                    handle_consensus_inbound_message(
                        &node_in_ref,
                        nid,
                        &mut consensus_in_ref,
                        msg,
                        Arc::clone(&peers_in_ref),
                    )
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

    let node_out_ref = Arc::clone(node);
    threads.push(spawn_or_die!("Process outbound consensus requests", {
        // don't do anything until the peer number is within the desired range
        while node_out_ref.get_node_peer_ids().len()
            > node_out_ref.config.max_allowed_nodes as usize
        {
            thread::sleep(Duration::from_secs(1));
        }

        let consensus_receiver_high_priority = CALLBACK_QUEUE
            .outbound
            .receiver_high_priority
            .lock()
            .unwrap();
        let consensus_receiver_low_priority = CALLBACK_QUEUE
            .outbound
            .receiver_low_priority
            .lock()
            .unwrap();
        let cvar = &*CALLBACK_QUEUE.outbound.signaler;
        let lock = ParkingMutex::new(false);
        let mut lock_guard = lock.lock();
        let mut exhausted: bool;

        'outer_loop: loop {
            exhausted = false;
            // instead of using `try_iter()` we specifically only loop over the max amounts
            // possible to ever be in the queue
            for _ in 0..CONSENSUS_QUEUE_DEPTH_OUT_HI {
                if let Ok(message) = consensus_receiver_high_priority.try_recv() {
                    let stop_loop = !handle_outbound_message(message, |msg| {
                        handle_consensus_outbound_message(&node_out_ref, nid, msg)
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
                let stop_loop = !handle_outbound_message(message, |msg| {
                    handle_consensus_outbound_message(&node_out_ref, nid, msg)
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

fn handle_inbound_message<F>(message: QueueMsg<ConsensusMessage>, f: F) -> bool
where
    F: FnOnce(ConsensusMessage) -> Fallible<()>, {
    match message {
        QueueMsg::Relay(msg) => {
            if let Err(e) = f(msg) {
                error!("There's an issue with an inbound consensus request: {}", e);
            }
        }
        QueueMsg::Stop => {
            warn!("Closing the inbound consensus channel");
            return false;
        }
    }
    true
}

fn handle_outbound_message<F>(message: QueueMsg<ConsensusMessage>, f: F) -> bool
where
    F: FnOnce(ConsensusMessage) -> Fallible<()>, {
    match message {
        QueueMsg::Relay(msg) => {
            if let Err(e) = f(msg) {
                error!("There's an issue with an outbound consensus request: {}", e);
            }
        }
        QueueMsg::Stop => {
            warn!("Closing the outbound consensus channel");
            return false;
        }
    }
    true
}

#[cfg(feature = "elastic_logging")]
fn setup_transfer_log_thread(conf: &config::CliConfig) -> JoinHandle<()> {
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
        let receiver = consensus_rust::transferlog::TRANSACTION_LOG_QUEUE
            .receiver
            .lock()
            .unwrap();
        loop {
            if let Ok(msg) = receiver.recv() {
                match msg {
                    QueueMsg::Relay(msg) => {
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
                    QueueMsg::Stop => {
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
fn setup_transfer_log_thread(_: &config::CliConfig) -> JoinHandle<()> {
    spawn_or_die!("Process transfer log messages", {
        let receiver = consensus_rust::transferlog::TRANSACTION_LOG_QUEUE
            .receiver
            .lock()
            .unwrap();
        loop {
            if let Ok(msg) = receiver.recv() {
                match msg {
                    QueueMsg::Relay(msg) => {
                        info!("{}", msg);
                    }
                    QueueMsg::Stop => {
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
