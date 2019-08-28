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

use byteorder::{NetworkEndian, WriteBytesExt};

use concordium_common::{
    cache::Cache,
    spawn_or_die,
    stats_export_service::{StatsExportService, StatsServiceMode},
    PacketType, RelayOrStopEnvelope, RelayOrStopReceiver, RelayOrStopSender,
    RelayOrStopSenderHelper,
};
use concordium_consensus::{consensus, ffi};
use concordium_global_state::tree::{messaging::GlobalStateMessage, GlobalState};
use failure::Fallible;
use p2p_client::{
    client::{
        plugins::{self, consensus::*},
        utils as client_utils,
    },
    common::{P2PNodeId, PeerType},
    configuration,
    network::{
        packet::MessageId, request::RequestedElementType, NetworkId, NetworkMessage,
        NetworkPacketType, NetworkRequest, NetworkResponse,
    },
    p2p::{p2p_node::send_direct_message, *},
    rpc::RpcServerImpl,
    stats_engine::StatsEngine,
    utils::{self, get_config_and_logging_setup, load_bans},
};

use rkv::{Manager, Rkv};

use std::{
    net::SocketAddr,
    str,
    sync::{mpsc, Arc, RwLock},
    thread,
    time::Duration,
};

fn main() -> Fallible<()> {
    let (conf, mut app_prefs) = get_config_and_logging_setup()?;
    if conf.common.print_config {
        // Print out the configuration
        info!("{:?}", conf);
    }
    let data_dir_path = app_prefs.get_user_app_dir();

    // Retrieving bootstrap nodes
    let dns_resolvers =
        utils::get_resolvers(&conf.connection.resolv_conf, &conf.connection.dns_resolver);
    for resolver in &dns_resolvers {
        debug!("Using resolver: {}", resolver);
    }

    let bootstrap_nodes = utils::get_bootstrap_nodes(
        conf.connection.bootstrap_server.clone(),
        &dns_resolvers,
        conf.connection.dnssec_disabled,
        &conf.connection.bootstrap_node,
    );

    // Instantiate stats export engine
    let stats_export_service =
        client_utils::instantiate_stats_export_engine(&conf, StatsServiceMode::NodeMode)
            .unwrap_or_else(|e| {
                error!(
                    "I was not able to instantiate an stats export service: {}",
                    e
                );
                None
            });

    info!("Debugging enabled: {}", conf.common.debug);

    let (subscription_queue_in, subscription_queue_out) = mpsc::sync_channel(10000);

    // Thread #1: instantiate the P2PNode
    let (mut node, pkt_out) = instantiate_node(
        &conf,
        &mut app_prefs,
        stats_export_service.clone(),
        subscription_queue_in.clone(),
    );

    // Create the cli key-value store environment
    let cli_kvs_handle = Manager::singleton()
        .write()
        .unwrap()
        .get_or_create(data_dir_path.as_path(), Rkv::new)
        .unwrap();

    // Load and apply existing bans
    if let Err(e) = load_bans(&mut node, &cli_kvs_handle) {
        error!("{}", e);
    };

    #[cfg(feature = "instrumentation")]
    // Thread #2 (optional): the push gateway to Prometheus
    client_utils::start_push_gateway(&conf.prometheus, &stats_export_service, node.id())?;

    // Start the P2PNode
    //
    // Thread #2 (#3): P2P event loop
    node.spawn();

    let is_baker = conf.cli.baker.baker_id.is_some();

    let mut consensus = plugins::consensus::start_consensus_layer(&conf.cli.baker, &app_prefs);

    // Start the RPC server
    let mut rpc_serv = if !conf.cli.rpc.no_rpc_server {
        let mut serv = RpcServerImpl::new(
            node.clone(),
            Arc::clone(&cli_kvs_handle),
            consensus.clone(),
            &conf.cli.rpc,
            subscription_queue_out,
        );
        serv.start_server()?;
        Some(serv)
    } else {
        None
    };

    let (gs_sender, gs_receiver) = mpsc::channel::<RelayOrStopEnvelope<GlobalStateMessage>>();

    // Connect outgoing messages to be forwarded into the baker and RPC streams.
    //
    // Thread #3 (#4): read P2PNode output
    let higer_process_threads = if let Some(ref mut consensus) = consensus {
        start_consensus_threads(
            &node,
            cli_kvs_handle,
            (&conf, &app_prefs),
            consensus.clone(),
            pkt_out,
            (gs_receiver, gs_sender.clone()),
        )
    } else {
        vec![]
    };

    // Create a listener on baker output to forward to the P2PNode
    //
    // Thread #4 (#5): the Baker thread
    let baker_thread = if is_baker {
        Some(start_baker_thread(gs_sender.clone()))
    } else {
        None
    };

    // Connect to nodes (args and bootstrap)
    if !conf.cli.no_network {
        info!("Starting the P2P layer");
        create_connections_from_config(&conf.connection, &dns_resolvers, &mut node);
        if !conf.connection.no_bootstrap_dns {
            info!("Attempting to bootstrap");
            bootstrap(&bootstrap_nodes, &mut node);
        }
    }

    // Wait for the P2PNode to close
    node.join().expect("The node thread panicked!");

    // Stop the GlobalState thread
    gs_sender.send_stop()?;

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
    conf: &configuration::Config,
    app_prefs: &mut configuration::AppPreferences,
    stats_export_service: Option<StatsExportService>,
    subscription_queue_in: mpsc::SyncSender<NetworkMessage>,
) -> (P2PNode, mpsc::Receiver<RelayOrStopEnvelope<NetworkMessage>>) {
    let (pkt_in, pkt_out) = mpsc::sync_channel(10000);
    let node_id = conf.common.id.clone().map_or(
        app_prefs.get_config(configuration::APP_PREFERENCES_PERSISTED_NODE_ID),
        |id| {
            if !app_prefs.set_config(
                configuration::APP_PREFERENCES_PERSISTED_NODE_ID,
                Some(id.clone()),
            ) {
                error!("Failed to persist own node id");
            };
            Some(id)
        },
    );

    // Start the thread reading P2PEvents from P2PNode
    let node = if conf.common.debug {
        let (sender, receiver) = mpsc::sync_channel(10000);
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
        )
    };
    (node, pkt_out)
}

fn create_connections_from_config(
    conf: &configuration::ConnectionConfig,
    dns_resolvers: &[String],
    node: &mut P2PNode,
) {
    for connect_to in &conf.connect_to {
        match utils::parse_host_port(&connect_to, &dns_resolvers, conf.dnssec_disabled) {
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

fn bootstrap(bootstrap_nodes: &Result<Vec<SocketAddr>, &'static str>, node: &mut P2PNode) {
    match bootstrap_nodes {
        Ok(nodes) => {
            for &addr in nodes {
                info!("Found bootstrap node: {}", addr);
                node.connect(PeerType::Bootstrapper, addr, None)
                    .unwrap_or_else(|e| error!("{}", e));
            }
        }
        Err(e) => error!("Couldn't retrieve bootstrap node list! {:?}", e),
    };
}

fn start_consensus_threads(
    node: &P2PNode,
    kvs_handle: Arc<RwLock<Rkv>>,
    (conf, app_prefs): (&configuration::Config, &configuration::AppPreferences),
    consensus: consensus::ConsensusContainer,
    pkt_out: RelayOrStopReceiver<NetworkMessage>,
    (gs_receiver, gs_sender): (
        RelayOrStopReceiver<GlobalStateMessage>,
        RelayOrStopSender<GlobalStateMessage>,
    ),
) -> Vec<std::thread::JoinHandle<()>> {
    let _no_trust_bans = conf.common.no_trust_bans;
    let _desired_nodes_clone = conf.connection.desired_nodes;
    let mut _stats_engine = StatsEngine::new(&conf.cli);
    let mut _msg_count = 0;
    let (_tps_test_enabled, _tps_message_count) = tps_setup_process_output(&conf.cli);
    let mut transactions_cache = Cache::default();
    let _network_id = NetworkId::from(conf.common.network_ids[0]); // defaulted so there's always first()
    let data_dir_path = app_prefs.get_user_app_dir();
    let is_global_state_persistent = conf.cli.baker.persist_global_state;

    let node_ref = node.clone();
    let mut consensus_clone = consensus.clone();
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

        loop {
            match gs_receiver.recv() {
                Ok(RelayOrStopEnvelope::Relay(request)) => {
                    if let Err(e) = handle_global_state_request(
                        &node_ref,
                        _network_id,
                        &mut consensus_clone,
                        request,
                        &mut global_state,
                    ) {
                        error!("There's an issue with a global state request: {}", e);
                    }
                }
                Ok(RelayOrStopEnvelope::Stop) => break,
                _ => panic!("Can't receive global state requests! Another thread must have died."),
            }
        }
    });

    let mut node_ref = node.clone();
    let gs_sender_ref = gs_sender.clone();
    let guard_pkt = spawn_or_die!("Higher queue processing", {
        while let Ok(RelayOrStopEnvelope::Relay(full_msg)) = pkt_out.recv() {
            match full_msg {
                NetworkMessage::NetworkRequest(
                    NetworkRequest::BanNode(ref peer, peer_to_ban),
                    ..
                ) => {
                    utils::ban_node(
                        &mut node_ref,
                        peer,
                        peer_to_ban,
                        &kvs_handle,
                        _no_trust_bans,
                    );
                }
                NetworkMessage::NetworkRequest(
                    NetworkRequest::UnbanNode(ref peer, peer_to_ban),
                    ..
                ) => {
                    utils::unban_node(
                        &mut node_ref,
                        peer,
                        peer_to_ban,
                        &kvs_handle,
                        _no_trust_bans,
                    );
                }
                NetworkMessage::NetworkResponse(
                    NetworkResponse::PeerList(ref peer, ref peers),
                    ..
                ) => {
                    trace!("Received PeerList response, attempting to satisfy desired peers");
                    let mut new_peers = 0;
                    let peer_count = node_ref
                        .get_peer_stats()
                        .iter()
                        .filter(|x| x.peer_type == PeerType::Node)
                        .count();
                    for peer_node in peers {
                        trace!(
                            "Peer {}/{}/{} sent us peer info for {}/{}/{}",
                            peer.id(),
                            peer.ip(),
                            peer.port(),
                            peer_node.id(),
                            peer_node.ip(),
                            peer_node.port()
                        );
                        if node_ref
                            .connect(PeerType::Node, peer_node.addr, Some(peer_node.id()))
                            .map_err(|e| trace!("{}", e))
                            .is_ok()
                        {
                            new_peers += 1;
                        }
                        if new_peers + peer_count as u8 >= _desired_nodes_clone {
                            break;
                        }
                    }
                }
                NetworkMessage::NetworkPacket(ref pac, ..) => {
                    if let NetworkPacketType::DirectMessage(..) = pac.packet_type {
                        if _tps_test_enabled {
                            if let Ok(len) = pac.message.len() {
                                _stats_engine.add_stat(len);
                                _msg_count += 1;

                                if _msg_count == _tps_message_count {
                                    info!(
                                        "TPS over {} messages is {}",
                                        _tps_message_count,
                                        _stats_engine.calculate_total_tps_average()
                                    );
                                    _msg_count = 0;
                                    _stats_engine.clear();
                                }
                            }
                        }
                    }

                    let is_broadcast = match pac.packet_type {
                        NetworkPacketType::BroadcastedMessage(..) => true,
                        _ => false,
                    };

                    let dont_relay_to =
                        if let NetworkPacketType::BroadcastedMessage(ref peers) = pac.packet_type {
                            let mut list = peers.clone().to_owned();
                            list.push(pac.peer.id());
                            list
                        } else {
                            vec![]
                        };

                    if let Err(e) = handle_pkt_out(
                        dont_relay_to,
                        pac.peer.id(),
                        pac.message.clone(),
                        &gs_sender_ref,
                        &mut transactions_cache,
                        is_broadcast,
                    ) {
                        error!("There's an issue with an outbound packet: {}", e);
                    }
                }
                NetworkMessage::NetworkRequest(ref pac @ NetworkRequest::Retransmit(..), ..) => {
                    if let NetworkRequest::Retransmit(requester, element_type, since, nid) = pac {
                        match element_type {
                            RequestedElementType::Transaction => {
                                let transactions = transactions_cache.get_since(*since);
                                transactions.iter().for_each(|transaction| {
                                    send_consensus_msg_to_net(
                                        &node_ref,
                                        vec![],
                                        Some(requester.id()),
                                        *nid,
                                        PacketType::Transaction,
                                        Some(format!("{:?}", transaction)),
                                        &transaction,
                                    );
                                })
                            }
                            _ => error!(
                                "Received request for unknown element type in a Retransmit request"
                            ),
                        }
                    }
                }
                // FIXME: should possibly be triggered by the Handshake reply instead
                NetworkMessage::NetworkRequest(
                    NetworkRequest::Handshake(remote_peer, nets, _),
                    ..
                ) => {
                    if let Some(network_id) = nets.iter().next() {
                        // catch up to the finalization point
                        send_consensus_msg_to_net(
                            &node_ref,
                            vec![],
                            Some(remote_peer.id()),
                            *network_id,
                            PacketType::CatchUpFinalizationMessagesByPoint,
                            None,
                            &consensus.get_finalization_point(),
                        );

                        // send a catch-up status
                        send_consensus_msg_to_net(
                            &node_ref,
                            vec![],
                            Some(remote_peer.id()),
                            *network_id,
                            PacketType::CatchUpStatus,
                            None,
                            &consensus.get_catch_up_status(),
                        );
                    } else {
                        error!("A handshaking peer doesn't seem to have any networks!");
                    }
                }
                _ => {}
            }
        }
    });

    let node_ref = node.clone();
    let gs_sender_ref = gs_sender.clone();
    #[allow(unreachable_code)] // the loop never breaks on its own
    let ticker_thread = spawn_or_die!("Ticker", {
        // an initial delay before we begin catching up and baking
        thread::sleep(Duration::from_secs(10));

        loop {
            thread::sleep(Duration::from_secs(1));

            let current_peers = node_ref.get_node_peer_ids();

            // don't provide the global state with the peer information until their
            // number is within the desired range
            if current_peers.len() <= node_ref.max_nodes.unwrap_or(u16::max_value()) as usize {
                let msg = GlobalStateMessage::PeerListUpdate(current_peers);
                if let Err(e) = gs_sender_ref.send(RelayOrStopEnvelope::Relay(msg)) {
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

fn start_baker_thread(
    gs_sender: RelayOrStopSender<GlobalStateMessage>,
) -> std::thread::JoinHandle<()> {
    spawn_or_die!("Process consensus messages", {
        loop {
            match consensus::CALLBACK_QUEUE.recv_message() {
                Ok(RelayOrStopEnvelope::Relay(msg)) => {
                    let msg = GlobalStateMessage::ConsensusMessage(msg);
                    if let Err(e) = gs_sender.send(RelayOrStopEnvelope::Relay(msg)) {
                        error!("Error passing a message from the consensus layer: {}", e)
                    }
                }
                Ok(RelayOrStopEnvelope::Stop) => break,
                Err(e) => error!("Error receiving a message from the consensus layer: {}", e),
            }
        }
    })
}

#[cfg(feature = "benchmark")]
fn tps_setup_process_output(cli: &configuration::CliConfig) -> (bool, u64) {
    (cli.tps.enable_tps_test, cli.tps.tps_message_count)
}

#[cfg(not(feature = "benchmark"))]
fn tps_setup_process_output(_: &configuration::CliConfig) -> (bool, u64) { (false, 0) }

fn _send_retransmit_packet(
    node: &P2PNode,
    receiver: P2PNodeId,
    network_id: NetworkId,
    message_id: &MessageId,
    payload_type: u16,
    data: &[u8],
) {
    let mut out_bytes = Vec::with_capacity(2 + data.len());
    match out_bytes.write_u16::<NetworkEndian>(payload_type as u16) {
        Ok(_) => {
            out_bytes.extend(data);
            match send_direct_message(
                node,
                Some(receiver),
                network_id,
                Some(message_id.to_owned()),
                out_bytes,
            ) {
                Ok(_) => debug!("Retransmitted packet of type {}", payload_type),
                Err(_) => error!("Couldn't retransmit packet of type {}!", payload_type),
            }
        }
        Err(_) => error!("Can't write payload type, so failing retransmit of packet"),
    }
}
