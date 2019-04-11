#![recursion_limit = "1024"]
#[cfg(not(target_os = "windows"))]
extern crate grpciounix as grpcio;
#[cfg(target_os = "windows")]
extern crate grpciowin as grpcio;
#[macro_use]
extern crate log;
#[macro_use]
extern crate failure;

// Explicitly defining allocator to avoid future reintroduction of jemalloc
use std::alloc::System;
#[global_allocator]
static A: System = System;

use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};
use consensus_sys::consensus;
use env_logger::{Builder, Env};
use failure::Fallible;
use p2p_client::{
    common::{ConnectionType, P2PNodeId, UCursor},
    configuration,
    connection::{P2PEvent, P2PNodeMode},
    db::P2PDB,
    network::{NetworkMessage, NetworkPacketType, NetworkRequest, NetworkResponse},
    p2p::*,
    prometheus_exporter::{PrometheusMode, PrometheusServer},
    rpc::RpcServerImpl,
    safe_read,
    stats_engine::StatsEngine,
    utils,
};
use std::{
    collections::HashMap,
    fs::OpenOptions,
    io::{Read, Write},
    net::IpAddr,
    rc::Rc,
    str,
    sync::{mpsc, Arc, RwLock},
    thread,
};

fn main() -> Fallible<()> {
    // Get config and app preferences
    let conf = configuration::parse_config();
    let mut app_prefs = configuration::AppPreferences::new(
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

    // Retrieving bootstrap nodes
    let dns_resolvers =
        utils::get_resolvers(&conf.connection.resolv_conf, &conf.connection.dns_resolver);
    for resolver in &dns_resolvers {
        debug!("Using resolver: {}", resolver);
    }
    let bootstrap_nodes = utils::get_bootstrap_nodes(
        conf.connection.bootstrap_server.clone(),
        &dns_resolvers,
        conf.connection.no_dnssec,
        &conf.connection.bootstrap_node,
    );

    // Create the database
    let mut db_path = app_prefs.get_user_app_dir();
    db_path.push("p2p.db");
    let db = P2PDB::new(db_path.as_path());

    // Instantiate prometheus
    let prometheus = if conf.prometheus.prometheus_server {
        info!("Enabling prometheus server");
        let mut srv = PrometheusServer::new(PrometheusMode::NodeMode);
        srv.start_server(
            &conf.prometheus.prometheus_listen_addr,
            conf.prometheus.prometheus_listen_port,
        )
        .unwrap_or_else(|e| error!("{}", e));

        Some(Arc::new(RwLock::new(srv)))
    } else if let Some(ref push_gateway) = conf.prometheus.prometheus_push_gateway {
        info!("Enabling prometheus push gateway at {}", push_gateway);
        let srv = PrometheusServer::new(PrometheusMode::NodeMode);
        Some(Arc::new(RwLock::new(srv)))
    } else {
        None
    };

    info!("Debugging enabled: {}", conf.common.debug);

    // Instantiate the p2p node
    let (pkt_in, pkt_out) = mpsc::channel::<Arc<NetworkMessage>>();
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
    let arc_prometheus = if let Some(ref p) = prometheus {
        Some(Arc::clone(p))
    } else {
        None
    };

    // Thread #1: Read P2PEvents from P2PNode
    let mut node = if conf.common.debug {
        let (sender, receiver) = mpsc::channel();
        let _guard = thread::spawn(move || loop {
            if let Ok(msg) = receiver.recv() {
                match msg {
                    P2PEvent::ConnectEvent(ip, port) => {
                        info!("Received connection from {}:{}", ip, port)
                    }
                    P2PEvent::DisconnectEvent(msg) => info!("Received disconnect for {}", msg),
                    P2PEvent::ReceivedMessageEvent(node_id) => {
                        info!("Received message from {:?}", node_id)
                    }
                    P2PEvent::SentMessageEvent(node_id) => info!("Sent message to {:?}", node_id),
                    P2PEvent::InitiatingConnection(ip, port) => {
                        info!("Initiating connection to {}:{}", ip, port)
                    }
                    P2PEvent::JoinedNetwork(peer, network_id) => {
                        info!(
                            "Peer {} joined network {}",
                            peer.id(),
                            network_id
                        );
                    }
                    P2PEvent::LeftNetwork(peer, network_id) => {
                        info!("Peer {} left network {}", peer.id(), network_id);
                    }
                }
            }
        });
        P2PNode::new(
            node_id,
            &conf,
            pkt_in,
            Some(sender),
            P2PNodeMode::NormalMode,
            arc_prometheus,
        )
    } else {
        P2PNode::new(
            node_id,
            &conf,
            pkt_in,
            None,
            P2PNodeMode::NormalMode,
            arc_prometheus,
        )
    };

    // Banning nodes in database
    match db.get_banlist() {
        Some(nodes) => {
            info!("Found existing banlist, loading up!");
            for n in nodes {
                node.ban_node(n.to_peer())?;
            }
        }
        None => {
            info!("Couldn't find existing banlist. Creating new!");
            db.create_banlist();
        }
    };

    // Starting baker
    let mut baker = start_baker(&conf.cli.baker, &app_prefs);

    // Starting rpc server
    let mut rpc_serv = if !conf.cli.rpc.no_rpc_server {
        let mut serv = RpcServerImpl::new(node.clone(), db.clone(), baker.clone(), &conf.cli.rpc);
        serv.start_server()?;
        Some(serv)
    } else {
        None
    };

    // Connect outgoing messages to be forwarded into the baker
    // or editing the db, or triggering new connections
    //
    // Thread #2: Read P2PNode output
    {
        let mut _node_self_clone = node.clone();

        let _no_trust_bans = conf.common.no_trust_bans;
        let _no_trust_broadcasts = conf.connection.no_trust_broadcasts;
        let mut _rpc_clone = rpc_serv.clone();
        let _desired_nodes_clone = conf.connection.desired_nodes;
        let _test_runner_url = conf.cli.test_runner_url.clone();
        let mut _baker_pkt_clone = baker.clone();
        let _tps_test_enabled = conf.cli.tps.enable_tps_test;
        let mut _stats_engine = StatsEngine::new(conf.cli.tps.tps_stats_save_amount);
        let mut _msg_count = 0;
        let _tps_message_count = conf.cli.tps.tps_message_count;
        let _guard_pkt = thread::spawn(move || {
            fn send_msg_to_baker(
                baker_ins: &mut Option<consensus::ConsensusContainer>,
                mut msg: UCursor,
            ) -> Fallible<()> {
                if let Some(ref mut baker) = baker_ins {
                    ensure!(
                        msg.len() >= msg.position() + 2,
                        "Message needs at least 2 bytes"
                    );

                    let consensus_type = msg.read_u16::<BigEndian>()?;
                    let view = msg.read_all_into_view()?;
                    let content = view.as_slice();

                    match consensus_type {
                        0 => match consensus::Block::deserialize(content) {
                            Some(block) => {
                                baker.send_block(&block);
                                info!("Sent block from network to baker");
                            }
                            _ => error!(
                                "Couldn't deserialize block, can't move forward with the message"
                            ),
                        },
                        1 => {
                            baker.send_transaction(content);
                            info!("Sent transaction to baker");
                        }
                        2 => {
                            baker.send_finalization(content);
                            info!("Sent finalization package to consensus layer");
                        }
                        3 => {
                            baker.send_finalization_record(content);
                            info!("Sent finalization record to consensus layer");
                        }
                        _ => {
                            error!("Couldn't read bytes properly for type");
                        }
                    }
                }
                Ok(())
            }

            loop {
                if let Ok(full_msg) = pkt_out.recv() {
                    match *full_msg {
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
                                    }
                                    if let Some(ref mut rpc) = _rpc_clone {
                                        rpc.queue_message(&full_msg)
                                            .unwrap_or_else(|e| error!("Couldn't queue message {}", e));
                                    }
                                    info!(
                                        "DirectMessage/{}/{} with size {} received",
                                        pac.network_id,
                                        pac.message_id,
                                        pac.message.len()
                                    );
                                }
                                NetworkPacketType::BroadcastedMessage => {
                                    if let Some(ref mut rpc) = _rpc_clone {
                                        rpc.queue_message(&full_msg)
                                            .unwrap_or_else(|e| error!("Couldn't queue message {}", e));
                                    }
                                    if let Some(ref testrunner_url) = _test_runner_url {
                                        info!("Sending information to test runner");
                                        match reqwest::get(&format!(
                                            "{}/register/{}/{}",
                                            testrunner_url,
                                            _node_self_clone.id(),
                                            pac.message_id
                                        )) {
                                            Ok(ref mut res) if res.status().is_success() => {
                                                info!("Registered packet received with test runner")
                                            }
                                            _ => error!(
                                                "Couldn't register packet received with test \
                                                 runner"
                                            ),
                                        }
                                    };
                                }
                            };
                            if let Err(e) =
                                send_msg_to_baker(&mut _baker_pkt_clone, pac.message.clone())
                            {
                                error!("Send network message to baker has failed: {:?}", e);
                            }
                        }

                        NetworkMessage::NetworkRequest(
                            NetworkRequest::BanNode(ref peer, ref x),
                            ..
                        ) => {
                            info!("Ban node request for {:?}", x);
                            let ban = _node_self_clone
                                .ban_node(x.clone())
                                .map_err(|e| error!("{}", e));
                            if ban.is_ok() {
                                db.insert_ban(
                                    &peer.id().to_string(),
                                    &peer.ip().to_string(),
                                    peer.port(),
                                );
                                if !_no_trust_bans {
                                    _node_self_clone
                                        .send_ban(x.clone())
                                        .unwrap_or_else(|e| error!("{}", e));
                                }
                            }
                        }
                        NetworkMessage::NetworkRequest(
                            NetworkRequest::UnbanNode(ref peer, ref x),
                            ..
                        ) => {
                            info!("Unban node requets for {:?}", x);
                            let req = _node_self_clone
                                .unban_node(x.to_owned())
                                .map_err(|e| error!("{}", e));
                            if req.is_ok() {
                                db.delete_ban(
                                    peer.id().to_string(),
                                    peer.ip().to_string(),
                                    peer.port(),
                                );
                                if !_no_trust_bans {
                                    _node_self_clone
                                        .send_unban(x.to_owned())
                                        .unwrap_or_else(|e| error!("{}", e));
                                }
                            }
                        }
                        NetworkMessage::NetworkResponse(
                            NetworkResponse::PeerList(ref peer, ref peers),
                            ..
                        ) => {
                            info!(
                                "Received PeerList response, attempting to satisfy desired peers"
                            );
                            let mut new_peers = 0;
                            match _node_self_clone.get_peer_stats(&[]) {
                                Ok(x) => {
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
                                        if _node_self_clone
                                            .connect(
                                                ConnectionType::Node,
                                                peer_node.ip(),
                                                peer_node.port(),
                                                Some(peer_node.id()),
                                            )
                                            .map_err(|e| info!("{}", e))
                                            .is_ok()
                                        {
                                            new_peers += 1;
                                        }
                                        if new_peers + x.len() as u8 >= _desired_nodes_clone {
                                            break;
                                        }
                                    }
                                }
                                _ => {
                                    error!(
                                        "Can't get nodes - so not trying to connect to new peers!"
                                    );
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
        });

        info!(
            "Concordium P2P layer. Network disabled: {}",
            conf.cli.no_network
        );
    }

    // Start push gateway to prometheus
    start_push_gateway(&conf.prometheus, &prometheus, node.id())?;

    // Start the P2PNode
    //
    // Thread #3: P2P event loop
    node.spawn();
    let _node_th = Rc::try_unwrap(node.process_th_sc().unwrap())
        .ok()
        .unwrap()
        .into_inner();

    // Connect to nodes (args and bootstrap)
    if !conf.cli.no_network {
        info!("Concordium P2P layer, Network disabled");
        create_connections_from_config(&conf.connection, &dns_resolvers, &mut node);
        if !conf.connection.no_bootstrap_dns {
            info!("Attempting to bootstrap");
            bootstrap(&bootstrap_nodes, &mut node);
        }
    }

    // Create listeners on baker output to forward to P2PNode
    //
    // Threads #4, #5, #6
    if let Some(ref mut baker) = baker {
        let mut _baker_clone = baker.to_owned();
        let mut _node_ref = node.clone();
        let _network_id = conf.common.network_ids.first().unwrap().to_owned(); // defaulted so there's always first()
        thread::spawn(move || loop {
            match _baker_clone.out_queue().recv_block() {
                Ok(x) => match x.serialize() {
                    Ok(bytes) => {
                        let mut out_bytes = vec![];
                        match out_bytes.write_u16::<BigEndian>(0 as u16) {
                            Ok(_) => {
                                out_bytes.extend(bytes);
                                match &_node_ref.send_message(
                                    None,
                                    _network_id,
                                    None,
                                    out_bytes,
                                    true,
                                ) {
                                    Ok(_) => {
                                        info!("Broadcasted block {}/{}", x.slot_id(), x.baker_id())
                                    }
                                    Err(_) => error!("Couldn't broadcast block!"),
                                }
                            }
                            Err(_) => error!("Can't write type to packet"),
                        }
                    }
                    Err(_) => error!("Couldn't serialize block {:?}", x),
                },
                _ => error!("Error receiving block from baker"),
            }
        });
        let _baker_clone_2 = baker.to_owned();
        let mut _node_ref_2 = node.clone();
        thread::spawn(move || loop {
            match _baker_clone_2.out_queue().recv_finalization() {
                Ok(x) => {
                    let mut out_bytes = vec![];
                    match out_bytes.write_u16::<BigEndian>(2 as u16) {
                        Ok(_) => {
                            out_bytes.extend(x);
                            match &_node_ref_2.send_message(
                                None,
                                _network_id,
                                None,
                                out_bytes,
                                true,
                            ) {
                                Ok(_) => info!("Broadcasted finalization packet"),
                                Err(_) => error!("Couldn't broadcast finalization packet!"),
                            }
                        }
                        Err(_) => error!("Can't write type to packet"),
                    }
                }
                _ => error!("Error receiving finalization packet from baker"),
            }
        });
        let _baker_clone_3 = baker.to_owned();
        let mut _node_ref_3 = node.clone();
        thread::spawn(move || loop {
            match _baker_clone_3.out_queue().recv_finalization_record() {
                Ok(x) => {
                    let mut out_bytes = vec![];
                    match out_bytes.write_u16::<BigEndian>(3 as u16) {
                        Ok(_) => {
                            out_bytes.extend(x);
                            match &_node_ref_3.send_message(
                                None,
                                _network_id,
                                None,
                                out_bytes,
                                true,
                            ) {
                                Ok(_) => info!("Broadcasted finalization record"),
                                Err(_) => error!("Couldn't broadcast finalization record!"),
                            }
                        }
                        Err(_) => error!("Can't write type to packet"),
                    }
                }
                _ => error!("Error receiving finalization record from baker"),
            }
        });
    }

    // TPS test

    if let Some(ref tps_test_recv_id) = conf.cli.tps.tps_test_recv_id {
        let mut _id_clone = tps_test_recv_id.to_owned();
        let mut _dir_clone = conf.cli.tps.tps_test_data_dir.to_owned();
        let mut _node_ref = node.clone();
        let _network_id = conf.common.network_ids.first().unwrap().to_owned();
        thread::spawn(move || {
            let mut done = false;
            while !done {
                // Test if we have any peers yet. Otherwise keep trying until we do
                if let Ok(node_list) = _node_ref.get_peer_stats(&vec![_network_id]) {
                    if node_list.len() > 0 {
                        let test_messages = utils::get_tps_test_messages(_dir_clone.clone());
                        for message in test_messages {
                            let mut out_bytes = vec![];
                            out_bytes.extend(message);
                            let out_bytes_len = out_bytes.len();
                            let to_send = P2PNodeId::from_str(&_id_clone).ok();
                            match _node_ref.send_message(
                                to_send,
                                _network_id,
                                None,
                                out_bytes,
                                false,
                            ) {
                                Ok(_) => {
                                    info!("Sent TPS test bytes of len {}", out_bytes_len);
                                }
                                Err(_) => error!("Couldn't send TPS test message!"),
                            }
                        }

                        done = true;
                    }
                }
            }
        });
    }

    // Wait for node closing
    _node_th.join().expect("Node thread panicked!");

    // Close rpc server if present
    if let Some(ref mut serv) = rpc_serv {
        serv.stop_server()?;
    }

    // Close baker if present
    if let Some(ref mut baker_ref) = baker {
        baker_ref.stop_baker(conf.cli.baker.baker_id.unwrap()); // only reached if not None, so it's safe
        consensus::ConsensusContainer::stop_haskell();
    }

    Ok(())
}

fn start_baker(
    conf: &configuration::BakerConfig,
    app_prefs: &configuration::AppPreferences,
) -> Option<consensus::ConsensusContainer> {
    conf.baker_id.and_then(|_| {
        info!("Starting up baker thread");
        consensus::ConsensusContainer::start_haskell();
        match get_baker_data(app_prefs, conf) {
            Ok((genesis, private_data)) => {
                let mut consensus_runner = consensus::ConsensusContainer::new(genesis);
                &consensus_runner.start_baker(conf.baker_id.unwrap(), private_data);
                Some(consensus_runner)
            }
            Err(_) => {
                error!("Can't read needed data...");
                None
            }
        }
    })
}

fn bootstrap(bootstrap_nodes: &Result<Vec<(IpAddr, u16)>, &'static str>, node: &mut P2PNode) {
    match bootstrap_nodes {
        Ok(nodes) => {
            for (ip, port) in nodes {
                info!("Found bootstrap node IP: {} and port: {}", ip, port);
                node.connect(ConnectionType::Bootstrapper, *ip, *port, None)
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
        match utils::parse_host_port(&connect_to, &dns_resolvers, conf.no_dnssec) {
            Some((ip, port)) => {
                info!("Connecting to peer {}", &connect_to);
                node.connect(ConnectionType::Node, ip, port, None)
                    .unwrap_or_else(|e| error!("{}", e));
            }
            None => error!("Can't parse IP to connect to '{}'", &connect_to),
        }
    }
}

fn start_push_gateway(
    conf: &configuration::PrometheusConfig,
    prometheus: &Option<Arc<RwLock<PrometheusServer>>>,
    id: P2PNodeId,
) -> Fallible<()> {
    if let Some(ref prom) = prometheus {
        if let Some(ref prom_push_addy) = conf.prometheus_push_gateway {
            let instance_name = if let Some(ref instance_id) = conf.prometheus_instance_name {
                instance_id.clone()
            } else {
                id.to_string()
            };
            safe_read!(prom)?.start_push_to_gateway(
                prom_push_addy.clone(),
                conf.prometheus_push_interval,
                conf.prometheus_job_name.clone(),
                instance_name,
                conf.prometheus_push_username.clone(),
                conf.prometheus_push_password.clone(),
            )?
        }
    }
    Ok(())
}

fn get_baker_data(
    app_prefs: &configuration::AppPreferences,
    conf: &configuration::BakerConfig,
) -> Result<(Vec<u8>, Vec<u8>), &'static str> {
    let mut genesis_loc = app_prefs.get_user_app_dir();
    genesis_loc.push("genesis.dat");
    let mut private_loc = app_prefs.get_user_app_dir();
    private_loc.push(format!("baker_private_{}.dat", conf.baker_id.unwrap())); // only reached if not None
    let (generated_genesis, generated_private_data) = if !genesis_loc.exists()
        || !private_loc.exists()
    {
        match consensus::ConsensusContainer::generate_data(
            conf.baker_genesis,
            conf.baker_num_bakers,
        ) {
            Ok((genesis, private_data)) => (genesis, private_data),
            Err(_) => return Err("Error generating genesis and/or private baker data via haskell!"),
        }
    } else {
        (vec![], HashMap::new())
    };
    let given_genesis = if !genesis_loc.exists() {
        match OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(&genesis_loc)
        {
            Ok(mut file) => match file.write_all(&generated_genesis) {
                Ok(_) => generated_genesis,
                Err(_) => return Err("Couldn't write out genesis data"),
            },
            Err(_) => return Err("Couldn't open up genesis file for writing"),
        }
    } else {
        match OpenOptions::new().read(true).open(&genesis_loc) {
            Ok(mut file) => {
                let mut read_data = vec![];
                match file.read_to_end(&mut read_data) {
                    Ok(_) => read_data.clone(),
                    Err(_) => return Err("Couldn't read genesis file properly"),
                }
            }
            _ => return Err("Unexpected"),
        }
    };
    let given_private_data = if !private_loc.exists() {
        match OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(&private_loc)
        {
            Ok(mut file) => {
                match file.write_all(generated_private_data.get(&(conf.baker_id.unwrap() as i64))
                                                           .unwrap()) // safe to assume it is set
                {
                    Ok(_) => {
                        generated_private_data.get(&(conf.baker_id.unwrap() as i64))
                                              .unwrap()
                                              .to_owned()
                    }
                    Err(_) => return Err("Couldn't write out private baker data"),
                }
            }
            Err(_) => return Err("Couldn't open up private baker file for writing"),
        }
    } else {
        match OpenOptions::new().read(true).open(&private_loc) {
            Ok(mut file) => {
                let mut read_data = vec![];
                match file.read_to_end(&mut read_data) {
                    Ok(_) => read_data,
                    Err(_) => return Err("Couldn't open up private baker file for reading"),
                }
            }
            _ => return Err("Unexpected"),
        }
    };
    Ok((given_genesis, given_private_data))
}
