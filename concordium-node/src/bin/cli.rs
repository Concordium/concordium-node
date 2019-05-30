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
    functor::{FilterFunctor, Functorable},
    make_atomic_callback, safe_write, spawn_or_die, write_or_die, RelayOrStopEnvelope,
    RelayOrStopReceiver,
};
use concordium_consensus::{
    consensus,
    ffi::{self, PacketType::*},
};
use concordium_global_state::{
    common::{sha256, SerializeToBytes},
    finalization::{FinalizationMessage, FinalizationRecord},
    tree::{SkovData, SKOV_QUEUE},
};
use env_logger::{Builder, Env};
use failure::Fallible;
use p2p_client::{
    client::{
        plugins::{self, consensus::*},
        utils as client_utils,
    },
    common::{P2PNodeId, PeerType},
    configuration,
    connection::network_handler::message_handler::MessageManager,
    db::P2PDB,
    network::{
        packet::MessageId, NetworkId, NetworkMessage, NetworkPacket, NetworkPacketType,
        NetworkRequest, NetworkResponse,
    },
    p2p::*,
    rpc::RpcServerImpl,
    stats_engine::StatsEngine,
    stats_export_service::{StatsExportService, StatsServiceMode},
    utils,
};

use std::{
    net::SocketAddr,
    str,
    sync::{mpsc, Arc, RwLock},
};

fn get_config_and_logging_setup() -> Fallible<(configuration::Config, configuration::AppPreferences)>
{
    // Get config and app preferences
    let conf = configuration::parse_config()?;
    let app_prefs = configuration::AppPreferences::new(
        conf.common.config_dir.to_owned(),
        conf.common.data_dir.to_owned(),
    );

    // Prepare the logger
    let env = if conf.common.trace {
        Env::default().filter_or("MY_LOG_LEVEL", "trace")
    } else if conf.common.debug {
        Env::default().filter_or("MY_LOG_LEVEL", "debug")
    } else {
        Env::default().filter_or("MY_LOG_LEVEL", "info")
    };

    let mut log_builder = Builder::from_env(env);
    if conf.common.no_log_timestamp {
        log_builder.default_format_timestamp(false);
    }
    log_builder.init();

    p2p_client::setup_panics();

    info!(
        "Starting up {} version {}!",
        p2p_client::APPNAME,
        p2p_client::VERSION
    );
    info!(
        "Application data directory: {:?}",
        app_prefs.get_user_app_dir()
    );
    info!(
        "Application config directory: {:?}",
        app_prefs.get_user_config_dir()
    );

    Ok((conf, app_prefs))
}

fn setup_baker_guards(
    baker: &mut Option<consensus::ConsensusContainer>,
    node: &P2PNode,
    conf: &configuration::Config,
) -> Vec<std::thread::JoinHandle<()>> {
    if let Some(ref mut baker) = baker {
        let network_id = NetworkId::from(conf.common.network_ids[0].to_owned()); // defaulted so there's always first()

        let baker_clone = baker.to_owned();
        let mut node_ref = node.clone();
        let catch_up_thread = spawn_or_die!("Process consensus catch-up requests", {
            use concordium_consensus::consensus::CatchupRequest::*;
            use concordium_global_state::common::DELTA_LENGTH;
            loop {
                match baker_clone.out_queue().recv_catchup() {
                    Ok(RelayOrStopEnvelope::Relay(msg)) => {
                        let (receiver_id, serialized_bytes) = match msg {
                            BlockByHash(receiver_id, ref hash, delta) => {
                                let mut inner_out_bytes = Vec::with_capacity(
                                    PAYLOAD_TYPE_LENGTH as usize
                                        + hash.len()
                                        + DELTA_LENGTH as usize,
                                );
                                inner_out_bytes
                                    .write_u16::<NetworkEndian>(CatchupBlockByHash as u16)
                                    .expect("Can't write to buffer");
                                inner_out_bytes.extend(hash.iter());
                                inner_out_bytes
                                    .write_u64::<NetworkEndian>(delta)
                                    .expect("Can't write to buffer");

                                (P2PNodeId(receiver_id), inner_out_bytes)
                            }
                            FinalizationRecordByHash(receiver_id, ref hash) => {
                                let mut inner_out_bytes =
                                    Vec::with_capacity(PAYLOAD_TYPE_LENGTH as usize + hash.len());
                                inner_out_bytes
                                    .write_u16::<NetworkEndian>(
                                        CatchupFinalizationRecordByHash as u16,
                                    )
                                    .expect("Can't write to buffer");
                                inner_out_bytes.extend(hash.iter());

                                (P2PNodeId(receiver_id), inner_out_bytes)
                            }
                            FinalizationRecordByIndex(receiver_id, index) => {
                                let mut inner_out_bytes =
                                    Vec::with_capacity(PAYLOAD_TYPE_LENGTH as usize + 8);
                                inner_out_bytes
                                    .write_u16::<NetworkEndian>(
                                        CatchupFinalizationRecordByIndex as u16,
                                    )
                                    .expect("Can't write to buffer");
                                inner_out_bytes
                                    .write_u64::<NetworkEndian>(index)
                                    .expect("Can't write to buffer");

                                (P2PNodeId(receiver_id), inner_out_bytes)
                            }
                            FinalizationMessagesByPoint(receiver_id, ref msg) => {
                                let msg_bytes = msg.serialize();
                                let mut inner_out_bytes = Vec::with_capacity(
                                    PAYLOAD_TYPE_LENGTH as usize + msg_bytes.len(),
                                );
                                inner_out_bytes
                                    .write_u16::<NetworkEndian>(FinalizationMessage as u16)
                                    .expect("Can't write to buffer");
                                inner_out_bytes.extend(&*msg_bytes);

                                (P2PNodeId(receiver_id), inner_out_bytes)
                            }
                        };
                        match &node_ref.send_message(
                            Some(receiver_id),
                            network_id,
                            None,
                            serialized_bytes,
                            false,
                        ) {
                            Ok(_) => info!(
                                "Peer {} sent a {} to peer {}",
                                node_ref.id(),
                                msg,
                                receiver_id,
                            ),
                            Err(_) => error!(
                                "Peer {} couldn't send a {} catch-up request to peer {}",
                                node_ref.id(),
                                msg,
                                receiver_id,
                            ),
                        }
                    }
                    Ok(RelayOrStopEnvelope::Stop) => break,
                    Err(_) => error!("Can't read from queue"),
                }
            }
        });

        let baker_clone = baker.to_owned();
        let mut node_ref = node.clone();
        let consensus_block_thread = spawn_or_die!("Process consensus block output", {
            loop {
                match baker_clone.out_queue().recv_block() {
                    Ok(RelayOrStopEnvelope::Relay(block_bytes)) => {
                        let mut out_bytes =
                            Vec::with_capacity(PAYLOAD_TYPE_LENGTH as usize + block_bytes.len());
                        match out_bytes.write_u16::<NetworkEndian>(ffi::PacketType::Block as u16) {
                            Ok(_) => {
                                out_bytes.extend(&*block_bytes);
                                match &node_ref
                                    .send_message(None, network_id, None, out_bytes, true)
                                {
                                    Ok(_) => info!(
                                        "Peer {} broadcasted block ({:?})",
                                        node_ref.id(),
                                        sha256(&block_bytes),
                                    ),
                                    Err(_) => error!(
                                        "Peer {} couldn't broadcast block ({:?})!",
                                        node_ref.id(),
                                        sha256(&block_bytes),
                                    ),
                                }
                            }
                            Err(_) => error!("Can't write type to packet"),
                        }
                    }
                    Ok(RelayOrStopEnvelope::Stop) => break,
                    _ => error!("Error receiving block from the consensus layer"),
                }
            }
        });

        let baker_clone = baker.to_owned();
        let mut node_ref = node.clone();
        let consensus_fin_msg_thread =
            spawn_or_die!("Process consensus finalization message output", {
                loop {
                    match baker_clone.out_queue().recv_finalization() {
                        Ok(RelayOrStopEnvelope::Relay((peer_id_opt, bytes))) => {
                            let mut out_bytes =
                                Vec::with_capacity(PAYLOAD_TYPE_LENGTH as usize + bytes.len());
                            match out_bytes.write_u16::<NetworkEndian>(
                                ffi::PacketType::FinalizationMessage as u16,
                            ) {
                                Ok(_) => {
                                    out_bytes.extend(&*bytes);
                                    let res = if let Some(peer_id) = peer_id_opt {
                                        node_ref.send_message(
                                            Some(P2PNodeId(peer_id)),
                                            network_id,
                                            None,
                                            out_bytes,
                                            false,
                                        )
                                    } else {
                                        node_ref
                                            .send_message(None, network_id, None, out_bytes, true)
                                    };

                                    match res {
                                        Ok(_) => info!(
                                            "Peer {} broadcasted a {}",
                                            node_ref.id(),
                                            FinalizationMessage::deserialize(&bytes)
                                                .map(|msg| msg.to_string())
                                                .unwrap_or_else(|_| String::from(
                                                    "corrupt finalization message"
                                                ))
                                        ),
                                        Err(_) => {
                                            error!("Couldn't broadcast a finalization packet!")
                                        }
                                    }
                                }
                                Err(_) => error!("Can't write type to packet"),
                            }
                        }
                        Ok(RelayOrStopEnvelope::Stop) => break,
                        _ => error!("Error receiving finalization packet from the consensus layer"),
                    }
                }
            });

        let baker_clone = baker.to_owned();
        let mut node_ref = node.clone();
        let consensus_fin_rec_thread =
            spawn_or_die!("Process consensus finalization records output", {
                loop {
                    match baker_clone.out_queue().recv_finalization_record() {
                        Ok(RelayOrStopEnvelope::Relay(bytes)) => {
                            let mut out_bytes =
                                Vec::with_capacity(PAYLOAD_TYPE_LENGTH as usize + bytes.len());
                            match out_bytes.write_u16::<NetworkEndian>(
                                ffi::PacketType::FinalizationRecord as u16,
                            ) {
                                Ok(_) => {
                                    out_bytes.extend(&*bytes);
                                    match &node_ref
                                        .send_message(None, network_id, None, out_bytes, true)
                                    {
                                        Ok(_) => info!(
                                            "Peer {} broadcasted a {}",
                                            node_ref.id(),
                                            FinalizationRecord::deserialize(&bytes)
                                                .map(|msg| msg.to_string())
                                                .unwrap_or_else(|_| String::from(
                                                    "corrupt finalization record"
                                                )),
                                        ),
                                        Err(_) => {
                                            error!("Couldn't broadcast a finalization record!")
                                        }
                                    }
                                }
                                Err(_) => error!("Can't write type to packet"),
                            }
                        }
                        Ok(RelayOrStopEnvelope::Stop) => break,
                        _ => error!("Error receiving finalization record from the consensus layer"),
                    }
                }
            });

        vec![
            catch_up_thread,
            consensus_block_thread,
            consensus_fin_msg_thread,
            consensus_fin_rec_thread,
        ]
    } else {
        vec![]
    }
}

fn instantiate_node(
    conf: &configuration::Config,
    app_prefs: &mut configuration::AppPreferences,
    stats_export_service: &Option<Arc<RwLock<StatsExportService>>>,
) -> (
    P2PNode,
    mpsc::Receiver<RelayOrStopEnvelope<Arc<NetworkMessage>>>,
) {
    let (pkt_in, pkt_out) = mpsc::channel::<RelayOrStopEnvelope<Arc<NetworkMessage>>>();
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
    let arc_stats_export_service = if let Some(ref service) = stats_export_service {
        Some(Arc::clone(service))
    } else {
        None
    };

    let broadcasting_checks = Arc::new(FilterFunctor::new("Broadcasting_checks"));

    // Thread #1: Read P2PEvents from P2PNode
    let node = if conf.common.debug {
        let (sender, receiver) = mpsc::channel();
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
            arc_stats_export_service,
            Arc::clone(&broadcasting_checks),
        )
    } else {
        P2PNode::new(
            node_id,
            &conf,
            pkt_in,
            None,
            PeerType::Node,
            arc_stats_export_service,
            Arc::clone(&broadcasting_checks),
        )
    };
    (node, pkt_out)
}

#[cfg(feature = "benchmark")]
fn tps_setup_process_output(cli: &configuration::CliConfig) -> (bool, u64) {
    (cli.tps.enable_tps_test, cli.tps.tps_message_count)
}

#[cfg(not(feature = "benchmark"))]
fn tps_setup_process_output(_: &configuration::CliConfig) -> (bool, u64) { (false, 0) }

fn setup_process_output(
    node: &P2PNode,
    db: &P2PDB,
    conf: &configuration::Config,
    rpc_serv: &Option<RpcServerImpl>,
    baker: &mut Option<consensus::ConsensusContainer>,
    pkt_out: RelayOrStopReceiver<Arc<NetworkMessage>>,
) -> Vec<std::thread::JoinHandle<()>> {
    let mut _db = db.clone();
    let _no_trust_bans = conf.common.no_trust_bans;
    let _no_trust_broadcasts = conf.connection.no_trust_broadcasts;
    let mut _rpc_clone = rpc_serv.clone();
    let _desired_nodes_clone = conf.connection.desired_nodes;
    let _test_runner_url = conf.cli.test_runner_url.clone();
    let mut _stats_engine = StatsEngine::new(&conf.cli);
    let mut _msg_count = 0;
    let (_tps_test_enabled, _tps_message_count) = tps_setup_process_output(&conf.cli);

    let _network_id = NetworkId::from(conf.common.network_ids[0].to_owned()); // defaulted so there's always first()

    let mut baker_clone = baker.clone();
    let mut node_ref = node.clone();
    let global_state_thread = spawn_or_die!("Process global state requests", {
        let mut skov_data = SkovData::default();
        // add the genesis block to the Skov
        skov_data.add_genesis(
            &baker_clone
                .clone()
                .map(|baker| baker.get_genesis_data())
                .unwrap()
                .unwrap(),
        );

        loop {
            match SKOV_QUEUE.recv_request() {
                Ok(RelayOrStopEnvelope::Relay(request)) => {
                    let source = request.source.unwrap_or_else(|| node_ref.id().0);

                    if let Err(e) = handle_global_state_request(
                        &mut node_ref,
                        &mut baker_clone,
                        P2PNodeId(source),
                        _network_id,
                        request,
                        &mut skov_data,
                    ) {
                        error!("There's an issue with a global state request: {}", e);
                    }
                }
                Ok(RelayOrStopEnvelope::Stop) => break,
                _ => error!("Can't receive a global state request!"),
            }
        }
    });

    let mut baker_clone = baker.clone();
    let mut node_ref = node.clone();
    let guard_pkt = spawn_or_die!("Higher queue processing", {
        while let Ok(RelayOrStopEnvelope::Relay(full_msg)) = pkt_out.recv() {
            match *full_msg {
                NetworkMessage::NetworkRequest(
                    NetworkRequest::BanNode(ref peer, peer_to_ban),
                    ..
                ) => {
                    utils::ban_node(&mut node_ref, peer, peer_to_ban, &_db, _no_trust_bans);
                }
                NetworkMessage::NetworkRequest(
                    NetworkRequest::UnbanNode(ref peer, peer_to_ban),
                    ..
                ) => {
                    utils::unban_node(&mut node_ref, peer, peer_to_ban, &_db, _no_trust_bans);
                }
                NetworkMessage::NetworkResponse(
                    NetworkResponse::PeerList(ref peer, ref peers),
                    ..
                ) => {
                    debug!("Received PeerList response, attempting to satisfy desired peers");
                    let mut new_peers = 0;
                    let peer_count = node_ref
                        .get_peer_stats(&[])
                        .iter()
                        .filter(|x| x.peer_type == PeerType::Node)
                        .count();
                    for peer_node in peers {
                        debug!(
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
                            .map_err(|e| debug!("{}", e))
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
                    match pac.packet_type {
                        NetworkPacketType::DirectMessage(..) => {
                            if _tps_test_enabled {
                                _stats_engine.add_stat(pac.message.len() as u64);
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
                            };
                        }
                        NetworkPacketType::BroadcastedMessage(..) => {
                            if let Some(ref testrunner_url) = _test_runner_url {
                                send_packet_to_testrunner(&node_ref, testrunner_url, &pac);
                            };
                        }
                    };

                    if let Err(e) = handle_pkt_out(
                        &mut node_ref,
                        &mut baker_clone,
                        pac.peer.id(),
                        _network_id,
                        pac.message.clone(),
                    ) {
                        error!("There's an issue with an outbound packet: {}", e);
                    }
                }
                NetworkMessage::NetworkRequest(NetworkRequest::Retransmit(..), ..) => {
                    panic!("Not implemented yet");
                }
                _ => {}
            }
        }
    });

    info!(
        "Concordium P2P layer. Network disabled: {}",
        conf.cli.no_network
    );

    vec![global_state_thread, guard_pkt]
}

fn main() -> Fallible<()> {
    let (conf, mut app_prefs) = get_config_and_logging_setup()?;
    if conf.common.print_config {
        // Print out the configuration
        info!("{:?}", conf);
    }

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

    // Create the database
    let mut db_path = app_prefs.get_user_app_dir();
    db_path.push("p2p.db");
    let db = P2PDB::new(db_path.as_path());

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

    // Instantiate the p2p node
    let (mut node, pkt_out) = instantiate_node(&conf, &mut app_prefs, &stats_export_service);

    // Banning nodes in database
    match db.get_banlist() {
        Some(nodes) => {
            info!("Found existing banlist, loading up!");
            for n in nodes {
                node.ban_node(n);
            }
        }
        None => {
            warn!("Couldn't find existing banlist. Creating new!");
            db.create_banlist();
        }
    };

    #[cfg(feature = "instrumentation")]
    // Start push gateway to prometheus
    client_utils::start_push_gateway(&conf.prometheus, &stats_export_service, node.id())?;

    // Start the P2PNode
    //
    // Thread #3: P2P event loop
    node.spawn();

    // Connect to nodes (args and bootstrap)
    if !conf.cli.no_network {
        info!("Concordium P2P layer, Network disabled");
        create_connections_from_config(&conf.connection, &dns_resolvers, &mut node);
        if !conf.connection.no_bootstrap_dns {
            info!("Attempting to bootstrap");
            bootstrap(&bootstrap_nodes, &mut node);
        }
    }

    let mut baker = if conf.cli.baker.baker_id.is_some() {
        // Starting baker
        plugins::consensus::start_baker(&conf.cli.baker, &app_prefs)
    } else {
        None
    };

    if let Some(ref baker) = baker {
        // Register handler for sending out a consensus catch-up request after handshake
        // by finalization point.
        let cloned_handshake_response_node = Arc::new(RwLock::new(node.clone()));
        let baker_clone = baker.clone();
        let message_handshake_response_handler = &node.message_handler();
        safe_write!(message_handshake_response_handler)?.add_response_callback(
            make_atomic_callback!(move |msg: &NetworkResponse| {
                if let NetworkResponse::Handshake(ref remote_peer, ref nets, _) = msg {
                    if remote_peer.peer_type() == PeerType::Node {
                        if let Some(net) = nets.iter().next() {
                            let mut locked_cloned_node =
                                write_or_die!(cloned_handshake_response_node);
                            if let Ok(bytes) = baker_clone.get_finalization_point() {
                                let mut out_bytes =
                                    Vec::with_capacity(PAYLOAD_TYPE_LENGTH as usize + bytes.len());
                                match out_bytes.write_u16::<NetworkEndian>(
                                    ffi::PacketType::CatchupFinalizationMessagesByPoint as u16,
                                ) {
                                    Ok(_) => {
                                        out_bytes.extend(&bytes);
                                        match locked_cloned_node.send_message(
                                            Some(remote_peer.id()),
                                            *net,
                                            None,
                                            out_bytes,
                                            false,
                                        ) {
                                            Ok(_) => info!(
                                                "Peer {} requested finalization messages by point \
                                                 from peer {}",
                                                locked_cloned_node.id(),
                                                remote_peer.id()
                                            ),
                                            Err(_) => error!(
                                                "Peer {} couldn't send a catch-up request for \
                                                 finalization messages by point!",
                                                locked_cloned_node.id(),
                                            ),
                                        }
                                    }
                                    Err(_) => error!(
                                        "Can't write type to packet {}",
                                        ffi::PacketType::CatchupFinalizationMessagesByPoint
                                    ),
                                }
                            }
                        } else {
                            error!(
                                "Handshake without network, so can't ask for finalization messages"
                            );
                        }
                    }
                }
                Ok(())
            }),
        );
    }

    // Starting rpc server
    let mut rpc_serv = if !conf.cli.rpc.no_rpc_server {
        let mut serv = RpcServerImpl::new(node.clone(), db.clone(), baker.clone(), &conf.cli.rpc);
        serv.start_server()?;
        Some(serv)
    } else {
        None
    };

    // Connect outgoing messages to be forwarded into the baker and RPC streams.
    //
    // Thread #4: Read P2PNode output
    let higer_process_threads =
        setup_process_output(&node, &db, &conf, &rpc_serv, &mut baker, pkt_out);

    // Create listeners on baker output to forward to P2PNode
    //
    // Threads #5, #6, #7, #8, #9
    let ths = setup_baker_guards(&mut baker, &node, &conf);

    // Wait for node closing
    node.join().expect("Node thread panicked!");

    // Close baker if present
    if let Some(ref mut baker_ref) = baker {
        if let Some(baker_id) = conf.cli.baker.baker_id {
            baker_ref.stop_baker(baker_id)
        };
        ffi::stop_haskell();
    }

    // Wait for the threads to stop
    for th in higer_process_threads {
        th.join().expect("Higher process thread panicked")
    }

    for th in ths {
        th.join().expect("Baker sub-thread panicked");
    }

    // Close rpc server if present
    if let Some(ref mut serv) = rpc_serv {
        serv.stop_server()?;
    }

    // Close stats server export if present
    client_utils::stop_stats_export_engine(&conf, &stats_export_service);

    Ok(())
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

#[cfg(feature = "instrumentation")]
fn send_packet_to_testrunner(node: &P2PNode, test_runner_url: &str, pac: &NetworkPacket) {
    debug!("Sending information to test runner");
    match reqwest::get(&format!(
        "{}/register/{}/{:?}",
        test_runner_url,
        node.id(),
        pac.message_id
    )) {
        Ok(ref mut res) if res.status().is_success() => {
            info!("Registered packet received with test runner")
        }
        _ => error!("Couldn't register packet received with test runner"),
    }
}

#[cfg(not(feature = "instrumentation"))]
fn send_packet_to_testrunner(_: &P2PNode, _: &str, _: &NetworkPacket) {}

fn _send_retransmit_packet(
    node: &mut P2PNode,
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
            match node.send_message(
                Some(receiver),
                network_id,
                Some(message_id.to_owned()),
                out_bytes,
                false,
            ) {
                Ok(_) => debug!("Retransmitted packet of type {}", payload_type),
                Err(_) => error!("Couldn't retransmit packet of type {}!", payload_type),
            }
        }
        Err(_) => error!("Can't write payload type, so failing retransmit of packet"),
    }
}
