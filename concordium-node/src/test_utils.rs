//! Test utilities.

use chrono::{offset::Utc, DateTime};
use failure::Fallible;
use rand::{distributions::Alphanumeric, thread_rng, Rng};
use structopt::StructOpt;

use crate::{
    common::{get_current_stamp, P2PNodeId, PeerType},
    configuration::Config,
    connection::ConnChange,
    consensus_ffi::helpers::PacketType,
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
        Arc, Once,
    },
    thread,
    time::Duration,
};

static INIT: Once = Once::new();
static PORT_OFFSET: AtomicUsize = AtomicUsize::new(0);
static PORT_START_NODE: u16 = 8888;

const TESTCONFIG: &[&str] = &[];

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
pub fn get_test_config(port: u16, networks: Vec<u16>) -> Config {
    let mut config = Config::from_iter(TESTCONFIG.iter()).add_options(
        Some("127.0.0.1".to_owned()),
        port,
        networks,
        100,
    );
    config.connection.no_bootstrap_dns = true;
    config.connection.dnssec_disabled = true;
    config.cli.no_network = true;
    config
}

/// Initializes the global logger with an `env_logger` - just once.
pub fn setup_logger() {
    // @note It adds thread ID to each message.
    INIT.call_once(|| {
        let mut builder = env_logger::Builder::from_default_env();
        builder
            .format(|buf, record| {
                let curr_thread = std::thread::current();
                let now: DateTime<Utc> = std::time::SystemTime::now().into();
                writeln!(
                    buf,
                    "[{} {} {} {:?}] {}",
                    now.format("%c"),
                    record.level(),
                    record.target(),
                    curr_thread.id(),
                    record.args()
                )
            })
            .init();
    });
}

/// Creates a `P2PNode` for test purposes
pub fn make_node_and_sync(
    port: u16,
    networks: Vec<u16>,
    node_type: PeerType,
) -> Fallible<Arc<P2PNode>> {
    // locally-run tests and benches can be polled with a much greater frequency
    let mut config = get_test_config(port, networks);
    config.cli.no_network = true;
    config.cli.poll_interval = 1;
    config.connection.housekeeping_interval = 10;

    let stats = Arc::new(StatsExportService::new().unwrap());
    let (node, poll) = P2PNode::new(None, &config, node_type, stats, None);

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
