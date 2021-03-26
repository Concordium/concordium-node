//! Test utilities.

use failure::Fallible;
use rand::{distributions::Alphanumeric, thread_rng, Rng};
use structopt::StructOpt;

use crate::{
    common::{get_current_stamp, P2PNodeId, PeerType},
    configuration::Config,
    connection::ConnChange,
    consensus_ffi::{
        blockchain_types::BlockHash,
        helpers::{PacketType, SHA256},
    },
    netmsg,
    network::{NetworkId, NetworkMessage, NetworkPacket, PacketDestination},
    p2p::{maintenance::spawn, P2PNode},
    read_or_die,
    stats_export_service::StatsExportService,
};
use crypto_common::Serial;

use std::{
    io::Write,
    net::TcpListener,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc, RwLock,
    },
    thread,
    time::Duration,
};

static PORT_OFFSET: AtomicUsize = AtomicUsize::new(0);
static PORT_START_NODE: u16 = 8888;

/// Returns the next available port.
pub fn next_available_port() -> u16 {
    let mut available_port = None;

    while available_port.is_none() {
        let port = PORT_OFFSET.fetch_add(1, Ordering::SeqCst) as u16 + PORT_START_NODE;
        available_port = TcpListener::bind(("127.0.0.1", port)).map(|_| port).ok();
        assert!(port < std::u16::MAX);
    }

    available_port.unwrap()
}

/// Produces a config object for test purposes.
/// This has data and config dir set to some temporary directory.
/// It is the responsibility of the test to delete those temporary directories.
pub fn get_test_config(port: u16, networks: Vec<u16>) -> Config {
    let td = tempfile::tempdir().expect("Cannot create temporary test directory.");
    let test_config =
        ["concordium_node".to_string(), "--config-dir=.".to_string(), "--data-dir=.".to_string()];
    let mut config = Config::from_iter(test_config.iter()).add_options(
        Some("127.0.0.1".to_owned()),
        port,
        networks,
        100,
    );
    config.connection.no_bootstrap_dns = true;
    config.connection.dnssec_disabled = true;
    config.cli.no_network = true;
    let dir = td.into_path();
    config.common.data_dir = dir.clone();
    config.common.config_dir = dir;
    config
}

pub fn dummy_regenesis_blocks() -> Vec<BlockHash> { vec![BlockHash::from([0u8; SHA256 as usize])] }

/// Creates a `P2PNode` for test purposes
pub fn make_node_and_sync(
    port: u16,
    networks: Vec<u16>,
    node_type: PeerType,
    regenesis_blocks: Vec<BlockHash>,
) -> Fallible<Arc<P2PNode>> {
    // locally-run tests and benches can be polled with a much greater frequency
    let mut config = get_test_config(port, networks);
    config.cli.no_network = true;
    config.cli.poll_interval = 1;
    config.connection.housekeeping_interval = 10;
    let regenesis_arc = Arc::new(RwLock::new(regenesis_blocks));

    let stats = Arc::new(StatsExportService::new().unwrap());
    let (node, poll) = P2PNode::new(None, &config, node_type, stats, regenesis_arc);

    spawn(&node, poll, None);
    Ok(node)
}

/// Connects `source` and `target` nodes
pub fn connect(source: &Arc<P2PNode>, target: &P2PNode) {
    source.register_conn_change(ConnChange::NewPeers(vec![target.self_peer]));
}

/// Waits until all handshakes with other nodes have concluded.
pub fn await_handshakes(node: &P2PNode) {
    loop {
        if node.connection_handler.conn_changes.changes.is_empty()
            && !read_or_die!(node.connections()).is_empty()
        {
            return;
        }

        thread::sleep(Duration::from_millis(10));
    }
}

/// Creates a vector of given size containing random bytes.
pub fn generate_random_data(size: usize) -> Vec<u8> {
    thread_rng().sample_iter(&Alphanumeric).take(size).map(|c| c as u32 as u8).collect()
}

fn generate_fake_block(size: usize) -> Fallible<Vec<u8>> {
    let mut buffer = Vec::with_capacity(1 + size);
    (PacketType::Block as u8).serial(&mut buffer);
    buffer.write_all(&generate_random_data(size))?;
    Ok(buffer)
}

/// Produces a network message containing a packet that simulates a block of
/// given size.
pub fn create_random_packet(size: usize) -> NetworkMessage {
    netmsg!(NetworkPacket, NetworkPacket {
        destination: PacketDestination::Direct(P2PNodeId::default()),
        network_id:  NetworkId::from(thread_rng().gen::<u16>()),
        message:     generate_fake_block(size).unwrap(),
    })
}
