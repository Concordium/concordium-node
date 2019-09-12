use crate::{
    common::PeerType,
    configuration::Config,
    network::{NetworkMessage, NetworkPacketType, NetworkRequest, NetworkResponse},
    p2p::p2p_node::P2PNode,
};
use concordium_common::{
    hybrid_buf::HybridBuf,
    stats_export_service::{StatsExportService, StatsServiceMode},
    RelayOrStopEnvelope,
};
use failure::Fallible;
use std::{
    cell::RefCell,
    net::TcpListener,
    path::PathBuf,
    sync::{
        atomic::{AtomicUsize, Ordering},
        mpsc::Receiver,
        Once, ONCE_INIT,
    },
    time,
};
use structopt::StructOpt;

static INIT: Once = ONCE_INIT;
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

use chrono::{offset::Utc, DateTime};
use std::io::Write;

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

#[cfg(debug_assertions)]
pub fn max_recv_timeout() -> std::time::Duration {
    time::Duration::from_secs(5 * 60) // 5 minutes
}

#[cfg(not(debug_assertions))]
pub fn max_recv_timeout() -> std::time::Duration {
    time::Duration::from_secs(60) // 1 minutes
}

/// It makes a list of nodes using `make_node_and_sync`.
///
/// # Arguments
/// * `port` - Initial port. Each node will use the port `port` + `i` where `i`
///   is `[0,
/// count)`.
/// * `count` - Number of nodes to be generated.
/// * `networks` - Networks added to new nodes.
///
/// # Return
/// As `make_node_and_sync`, this returns a tuple but it contains list
/// of objects instead of just one.
#[allow(clippy::type_complexity)]
pub fn make_nodes_from_port(
    count: usize,
    networks: Vec<u16>,
) -> Fallible<
    Vec<(
        RefCell<P2PNode>,
        Receiver<RelayOrStopEnvelope<NetworkMessage>>,
    )>,
> {
    let mut nodes_and_receivers = Vec::with_capacity(count);

    for _ in 0..count {
        let (node, receiver) =
            make_node_and_sync(next_available_port(), networks.clone(), PeerType::Node)?;

        nodes_and_receivers.push((RefCell::new(node), receiver));
    }

    Ok(nodes_and_receivers)
}

/// It creates a pair of `P2PNode` and a `Receiver` which can be used to
/// wait for specific messages.
/// Using this approach protocol tests will be easier and cleaner.
pub fn make_node_and_sync(
    port: u16,
    networks: Vec<u16>,
    node_type: PeerType,
) -> Fallible<(P2PNode, Receiver<RelayOrStopEnvelope<NetworkMessage>>)> {
    let (net_tx, net_rx) = std::sync::mpsc::sync_channel(64);
    let (rpc_tx, _rpc_rx) = std::sync::mpsc::sync_channel(64);

    let export_service = StatsExportService::new(StatsServiceMode::NodeMode).unwrap();
    let (mut node, receivers) = P2PNode::new(
        None,
        &get_test_config(port, networks),
        net_tx,
        None,
        node_type,
        Some(export_service),
        rpc_tx,
        None,
    );

    // locally-run tests and benches can be polled with a much greater frequency
    node.config.poll_interval = 1;
    node.config.housekeeping_interval = 10;
    node.config.no_net = true;

    node.spawn(receivers);
    Ok((node, net_rx))
}

pub fn make_node_and_sync_with_rpc(
    port: u16,
    networks: Vec<u16>,
    node_type: PeerType,
    data_dir_path: PathBuf,
) -> Fallible<(P2PNode, Receiver<NetworkMessage>, Receiver<NetworkMessage>)> {
    let (net_tx, _) = std::sync::mpsc::sync_channel(64);
    let (_, msg_wait_rx) = std::sync::mpsc::sync_channel(64);
    let (rpc_tx, rpc_rx) = std::sync::mpsc::sync_channel(64);

    let export_service = StatsExportService::new(StatsServiceMode::NodeMode).unwrap();
    let (mut node, receivers) = P2PNode::new(
        None,
        &get_test_config(port, networks),
        net_tx,
        None,
        node_type,
        Some(export_service),
        rpc_tx,
        Some(data_dir_path),
    );

    // locally-run tests and benches can be polled with a much greater frequency
    node.config.poll_interval = 1;
    node.config.housekeeping_interval = 1;
    node.config.no_net = true;

    node.spawn(receivers);
    Ok((node, msg_wait_rx, rpc_rx))
}

/// Connects `source` and `target` nodes
pub fn connect(source: &mut P2PNode, target: &P2PNode) -> Fallible<()> {
    source.connect(target.self_peer.peer_type, target.internal_addr(), None)
}

pub fn await_handshake(node: &P2PNode) -> Fallible<()> {
    let conn = read_or_die!(node.connection_handler.connections)
        .iter()
        .next()
        .cloned()
        .unwrap();

    loop {
        if conn.is_post_handshake.load(Ordering::Relaxed) {
            break;
        }
    }

    Ok(())
}

/// Waits until
/// `receiver` receive a `peerlist` response packet before timeout is reached.
pub fn await_peerlist_with_timeout(
    receiver: &Receiver<NetworkMessage>,
    timeout: std::time::Duration,
) -> Fallible<()> {
    // Wait for Peerlist response
    if let Ok(NetworkMessage::NetworkResponse(NetworkResponse::PeerList(..), ..)) =
        receiver.recv_timeout(timeout)
    {
        return Ok(());
    }
    bail!("Didn't receive peerlist response message within the timeout period")
}

/// Waits until
/// `receiver` receives a `ping` request packet before the timeout is reached.
pub fn await_ping_with_timeout(
    receiver: &Receiver<NetworkMessage>,
    timeout: std::time::Duration,
) -> Fallible<()> {
    // Wait for Ping request
    if let Ok(NetworkMessage::NetworkRequest(NetworkRequest::Ping(..), ..)) =
        receiver.recv_timeout(timeout)
    {
        return Ok(());
    }
    bail!("Didn't receive ping request message within the timeout period")
}

pub fn await_broadcast_message(
    waiter: &Receiver<RelayOrStopEnvelope<NetworkMessage>>,
) -> Fallible<HybridBuf> {
    loop {
        let msg = waiter.recv()?;
        if let RelayOrStopEnvelope::Relay(NetworkMessage::NetworkPacket(pac, ..)) = msg {
            if let NetworkPacketType::BroadcastedMessage(..) = pac.packet_type {
                return Ok(pac.message.to_owned());
            }
        }
    }
}

pub fn await_direct_message(
    waiter: &Receiver<RelayOrStopEnvelope<NetworkMessage>>,
) -> Fallible<HybridBuf> {
    loop {
        let msg = waiter.recv()?;
        if let RelayOrStopEnvelope::Relay(NetworkMessage::NetworkPacket(pac, ..)) = msg {
            if let NetworkPacketType::DirectMessage(..) = pac.packet_type {
                return Ok(pac.message.to_owned());
            }
        }
    }
}

pub fn await_direct_message_with_timeout(
    waiter: &Receiver<RelayOrStopEnvelope<NetworkMessage>>,
    timeout: std::time::Duration,
) -> Option<HybridBuf> {
    while let Ok(msg) = waiter.recv_timeout(timeout) {
        if let RelayOrStopEnvelope::Relay(NetworkMessage::NetworkPacket(pac, ..)) = msg {
            if let NetworkPacketType::DirectMessage(..) = pac.packet_type {
                return Some(pac.message.to_owned());
            }
        }
    }

    None
}

pub fn consume_pending_messages(waiter: &Receiver<RelayOrStopEnvelope<NetworkMessage>>) {
    let max_wait_time = time::Duration::from_millis(250);
    loop {
        if waiter.recv_timeout(max_wait_time).is_err() {
            break;
        }
    }
}
