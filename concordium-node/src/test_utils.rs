use chrono::{offset::Utc, DateTime};
use failure::Fallible;
use rand::{distributions::Alphanumeric, thread_rng, Rng};
use structopt::StructOpt;

use crate::{
    common::{get_current_stamp, P2PNodeId, PeerType},
    configuration::Config,
    netmsg,
    network::{NetworkId, NetworkMessage, NetworkMessagePayload, NetworkPacket, NetworkPacketType},
    p2p::{connectivity, maintenance::spawn, P2PNode},
    stats_export_service::{StatsExportService, StatsServiceMode},
};
use concordium_common::PacketType;
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

/// It returns the next available port
pub fn next_available_port() -> u16 {
    let mut available_port = None;

    while available_port.is_none() {
        let port = PORT_OFFSET.fetch_add(1, Ordering::SeqCst) as u16 + PORT_START_NODE;
        available_port = TcpListener::bind(("127.0.0.1", port)).map(|_| port).ok();
        assert!(port < std::u16::MAX);
    }

    available_port.unwrap()
}

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

/// It initializes the global logger with an `env_logger`, but just once.
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

    let stats = Arc::new(StatsExportService::new(StatsServiceMode::NodeMode).unwrap());
    let node = P2PNode::new(None, &config, node_type, stats, None);

    spawn(&node);
    Ok(node)
}

/// Connects `source` and `target` nodes
pub fn connect(source: &Arc<P2PNode>, target: &P2PNode) -> Fallible<()> {
    connectivity::connect(source, target.self_peer.peer_type, target.internal_addr(), None)
}

pub fn await_handshake(node1: &P2PNode, node2: &P2PNode) -> Fallible<()> {
    loop {
        if let Some(conn) = read_or_die!(node1.connections()).values().next() {
            if let Some(id) = conn.remote_id() {
                if id == node2.id() {
                    break;
                }
            }
        }

        thread::sleep(Duration::from_millis(100));
    }

    Ok(())
}

pub fn generate_random_data(size: usize) -> Vec<u8> {
    thread_rng().sample_iter(&Alphanumeric).take(size).map(|c| c as u32 as u8).collect()
}

fn generate_fake_block(size: usize) -> Fallible<Vec<u8>> {
    let mut buffer = Vec::with_capacity(1 + size);
    (PacketType::Block as u8).serial(&mut buffer);
    buffer.write_all(&generate_random_data(size))?;
    Ok(buffer)
}

pub fn create_random_packet(size: usize) -> NetworkMessage {
    netmsg!(NetworkPacket, NetworkPacket {
        packet_type: NetworkPacketType::DirectMessage(P2PNodeId::default()),
        network_id:  NetworkId::from(thread_rng().gen::<u16>()),
        message:     generate_fake_block(size).unwrap(),
    })
}
