//! Node connection handling.

use failure::Fallible;
use mio::{net::TcpStream, Events, Token};
use rand::{
    seq::{index::sample, IteratorRandom},
    Rng,
};
use rayon::iter::{IntoParallelRefMutIterator, ParallelIterator};
use semver::Version;

use crate::{
    common::{get_current_stamp, P2PNodeId, PeerType, RemotePeer},
    configuration as config,
    connection::{ConnChange, Connection, MessageSendingPriority},
    netmsg,
    network::{
        Handshake, NetworkId, NetworkMessage, NetworkPacket, NetworkPayload, NetworkRequest,
        PacketDestination,
    },
    only_fbs,
    p2p::{bans::BanId, maintenance::attempt_bootstrap, P2PNode},
};

use std::{
    cmp, io,
    net::SocketAddr,
    sync::{atomic::Ordering, Arc},
    time::{Duration, Instant},
};

/// The poll token of the node's socket server.
pub const SELF_TOKEN: Token = Token(0);

// a convenience macro to send an object to all connections
macro_rules! send_to_all {
    ($foo_name:ident, $object_type:ty, $req_type:ident) => {
        #[doc = "Send a specified network request to all peers"]
        #[cfg_attr(any(feature = "s11n_serde", feature = "s11n_capnp"), allow(unreachable_code, unused_variables))]
        pub fn $foo_name(&self, object: $object_type) {
            let request = NetworkRequest::$req_type(object);
            let message = netmsg!(NetworkRequest, request);
            let filter = |_: &Connection| true;

            only_fbs!({if let Err(e) = {
                let mut buf = Vec::with_capacity(256);
                message.serialize(&mut buf)
                    .map(|_| buf)
                    .map(|buf| self.send_over_all_connections(&buf, &filter))
            } {
                error!("A network message couldn't be forwarded: {}", e);
            }});
        }
    }
}

/// A macro used to find a connection by the id of its node.
#[macro_export]
macro_rules! find_conn_by_id {
    ($node:expr, $id:expr) => {{
        read_or_die!($node.connections()).values().find(|conn| conn.remote_id() == Some($id))
    }};
}

/// A macro used to find connections by an IP address.
#[macro_export]
macro_rules! find_conns_by_ip {
    ($node:expr, $ip:expr) => {{
        read_or_die!($node.connections()).values().filter(|conn| conn.remote_peer.addr.ip() == $ip)
    }};
}

impl P2PNode {
    send_to_all!(send_ban, BanId, BanNode);

    send_to_all!(send_unban, BanId, UnbanNode);

    send_to_all!(send_joinnetwork, NetworkId, JoinNetwork);

    send_to_all!(send_leavenetwork, NetworkId, LeaveNetwork);

    /// Send a `data` message to all connections adhering to the specified
    /// filter. Returns the number of sent messages.
    pub fn send_over_all_connections(
        &self,
        data: &[u8],
        conn_filter: &dyn Fn(&Connection) -> bool,
    ) -> usize {
        let mut sent_messages = 0usize;
        let data = Arc::from(data);

        for conn in write_or_die!(self.connections()).values_mut().filter(|conn| conn_filter(conn))
        {
            conn.async_send(Arc::clone(&data), MessageSendingPriority::Normal);
            sent_messages += 1;
        }

        sent_messages
    }

    /// Send out ping messages in order to update peer latency statistics.
    pub fn measure_connection_latencies(&self) {
        debug!("Measuring connection latencies");

        for conn in write_or_die!(self.connections()).values_mut() {
            if let Err(e) = conn.send_ping() {
                error!("Can't send a ping to {}: {}", conn, e);
            }
        }
    }

    /// Add a network to the list of node's networks.
    pub fn add_network(&self, network_id: NetworkId) {
        write_or_die!(self.connection_handler.networks).insert(network_id);
    }

    /// Shut down connection with the given poll token.
    pub fn remove_connection(&self, token: Token) -> Option<Connection> {
        let removed_cand = lock_or_die!(self.conn_candidates()).remove(&token);
        let removed_conn = write_or_die!(self.connections()).remove(&token);

        if removed_cand.is_some() {
            removed_cand
        } else if removed_conn.is_some() {
            self.bump_last_peer_update();
            removed_conn
        } else {
            None
        }
    }

    /// Shut down connections with the given poll tokens.
    pub fn remove_connections(&self, tokens: &[Token]) {
        let conn_candidates = &mut lock_or_die!(self.conn_candidates());
        let connections = &mut write_or_die!(self.connections());

        let mut removed = 0;
        for token in tokens {
            if conn_candidates.remove(&token).is_some() {
                continue;
            } else if connections.remove(&token).is_some() {
                removed += 1;
            }
        }
        if removed > 0 {
            self.bump_last_peer_update();
        }
    }

    fn process_network_packet(&self, inner_pkt: NetworkPacket) -> Fallible<usize> {
        let peers_to_skip = match inner_pkt.destination {
            PacketDestination::Direct(..) => vec![],
            PacketDestination::Broadcast(ref dont_relay_to) => {
                if self.config.relay_broadcast_percentage < 1.0 {
                    use rand::seq::SliceRandom;
                    let mut rng = rand::thread_rng();
                    let mut peers = self.get_node_peer_ids();
                    peers.retain(|id| !dont_relay_to.contains(&P2PNodeId(*id)));
                    let peers_to_take = f64::floor(
                        f64::from(peers.len() as u32) * self.config.relay_broadcast_percentage,
                    );
                    peers
                        .choose_multiple(&mut rng, peers_to_take as usize)
                        .map(|id| P2PNodeId(*id))
                        .collect::<Vec<_>>()
                } else {
                    dont_relay_to.to_owned()
                }
            }
        };

        let target = if let PacketDestination::Direct(receiver) = inner_pkt.destination {
            Some(receiver)
        } else {
            None
        };
        let network_id = inner_pkt.network_id;

        let copies = if let Some((btype, btgt, blvl)) = &self.config.breakage {
            if btype == "spam" && (inner_pkt.message[0] == *btgt || *btgt == 99) {
                1 + *blvl
            } else {
                1
            }
        } else {
            1
        };

        let message = netmsg!(NetworkPacket, inner_pkt);
        let mut serialized = Vec::with_capacity(256);
        only_fbs!({
            message.serialize(&mut serialized)?;
        });

        let mut sent = 0;
        if let Some(target_id) = target {
            // direct messages
            let filter =
                |conn: &Connection| conn.remote_peer.peer().map(|p| p.id) == Some(target_id);

            for _ in 0..copies {
                sent += self.send_over_all_connections(&serialized, &filter);
            }
        } else {
            // broadcast messages
            let filter =
                |conn: &Connection| is_valid_broadcast_target(conn, &peers_to_skip, network_id);

            for _ in 0..copies {
                sent += self.send_over_all_connections(&serialized, &filter);
            }
        }

        Ok(sent)
    }

    /// Send queued messages to and then receive any pending messages from all
    /// the node's connections in parallel.
    #[inline]
    pub fn process_network_events(&self, events: &Events) {
        let conn_stats = self.get_peer_stats(Some(PeerType::Node));

        lock_or_die!(self.conn_candidates())
            .par_iter_mut()
            .map(|(_, conn)| conn)
            .chain(write_or_die!(self.connections()).par_iter_mut().map(|(_, conn)| conn))
            .for_each(|conn| {
                if events.iter().any(|event| event.token() == conn.token && event.is_writable()) {
                    conn.low_level.notify_writable();
                }

                if let Err(e) =
                    conn.send_pending_messages().and_then(|_| conn.low_level.flush_socket())
                {
                    error!("[sending to {}] {}", conn, e);
                    if let Ok(_io_err) = e.downcast::<io::Error>() {
                        self.register_conn_change(ConnChange::Removal(conn.token));
                    } else {
                        self.register_conn_change(ConnChange::Expulsion(conn.token));
                    }
                    return;
                }

                if events.iter().any(|event| event.token() == conn.token && event.is_readable()) {
                    match conn.read_stream(&conn_stats) {
                        Err(e) => {
                            error!("[receiving from {}] {}", conn, e);
                            if let Ok(_io_err) = e.downcast::<io::Error>() {
                                self.register_conn_change(ConnChange::Removal(conn.token));
                            } else {
                                self.register_conn_change(ConnChange::Expulsion(conn.token));
                            }
                        }
                        Ok(false) => {
                            // The connection was closed by the peer.
                            self.register_conn_change(ConnChange::Removal(conn.token));
                        }
                        Ok(true) => {}
                    }
                }
            })
    }

    /// Creates a "high-level" handshake request to be sent to new peers.
    pub fn produce_handshake_request(&self) -> Fallible<Vec<u8>> {
        let handshake_request = netmsg!(
            NetworkRequest,
            NetworkRequest::Handshake(Handshake {
                remote_id:   self.self_peer.id,
                remote_port: self.self_peer.port(),
                networks:    read_or_die!(self.networks()).iter().copied().collect(),
                version:     Version::parse(env!("CARGO_PKG_VERSION"))?,
                proof:       vec![],
            })
        );
        let mut serialized = Vec::with_capacity(128);
        only_fbs!({
            handshake_request.serialize(&mut serialized)?;
        });

        Ok(serialized)
    }
}

/// Accept an incoming network connection.
pub fn accept(node: &Arc<P2PNode>) -> Fallible<Token> {
    let (socket, addr) = node.connection_handler.socket_server.accept()?;
    node.stats.conn_received_inc();

    // Lock the candidate list for added safety against duplicate connections
    let mut candidates_lock = lock_or_die!(node.conn_candidates());

    {
        let conn_read_lock = read_or_die!(node.connections());

        if node.self_peer.peer_type == PeerType::Node
            && candidates_lock.len() + conn_read_lock.len()
                >= node.config.hard_connection_limit as usize
        {
            bail!("Too many connections, rejecting attempt from {}", addr);
        }

        if candidates_lock.values().any(|conn| conn.remote_addr() == addr)
            || conn_read_lock.values().any(|conn| conn.remote_addr() == addr)
        {
            bail!("Duplicate connection attempt from {}; rejecting", addr);
        }

        if read_or_die!(node.connection_handler.soft_bans)
            .keys()
            .any(|ip| *ip == BanId::Ip(addr.ip()))
        {
            bail!("Connection attempt from a soft-banned IP ({}); rejecting", addr.ip());
        }
    }

    debug!("Accepting a connection from {}", addr);

    let token = Token(node.connection_handler.next_token.fetch_add(1, Ordering::SeqCst));

    let remote_peer = RemotePeer {
        id: Default::default(),
        addr,
        external_port: addr.port(),
        peer_type: PeerType::Node,
    };

    let mut conn = Connection::new(node, socket, token, remote_peer, false);
    node.register_conn(&mut conn)?;
    candidates_lock.insert(conn.token, conn);

    Ok(token)
}

/// Connect to another node with the specified address and optionally peer id,
/// registering it as the given peer type.
pub fn connect(
    node: &Arc<P2PNode>,
    peer_type: PeerType,
    peer_addr: SocketAddr,
    peer_id: Option<P2PNodeId>,
) -> Fallible<()> {
    debug!(
        "Attempting to connect to {}{}",
        peer_addr,
        if let Some(id) = peer_id {
            format!(" ({})", id)
        } else {
            "".to_owned()
        }
    );

    if peer_type == PeerType::Node {
        let current_peer_count = node.get_peer_stats(Some(PeerType::Node)).len() as u16;
        if current_peer_count >= node.config.max_allowed_nodes {
            bail!(
                "Maximum number of peers reached {}/{}",
                current_peer_count,
                node.config.max_allowed_nodes
            );
        }
    }

    // Don't connect to ourselves
    if node.self_peer.addr == peer_addr || peer_id == Some(node.id()) {
        bail!("Attempted to connect to myself");
    }

    if read_or_die!(node.connection_handler.soft_bans)
        .iter()
        .any(|(ip, _)| *ip == BanId::Ip(peer_addr.ip()) || *ip == BanId::Socket(peer_addr))
    {
        bail!("Refusing to connect to a soft-banned IP ({:?})", peer_addr.ip());
    }

    // Lock the candidate list for added safety against duplicate connections
    let mut candidates_lock = lock_or_die!(node.conn_candidates());

    if candidates_lock.values().any(|cc| cc.remote_addr() == peer_addr) {
        bail!("Already connected to {}", peer_addr.to_string());
    }

    // Don't connect to established peers with a known P2PNodeId or IP+port
    for conn in read_or_die!(node.connections()).values() {
        if conn.remote_addr() == peer_addr || (peer_id.is_some() && conn.remote_id() == peer_id) {
            bail!(
                "Already connected to {}",
                if let Some(id) = peer_id {
                    id.to_string()
                } else {
                    peer_addr.to_string()
                }
            );
        }
    }

    match TcpStream::connect(peer_addr) {
        Ok(socket) => {
            trace!("Connected to {}", peer_addr);
            node.stats.conn_received_inc();

            let token = Token(node.connection_handler.next_token.fetch_add(1, Ordering::SeqCst));

            let remote_peer = RemotePeer {
                id: Default::default(),
                addr: peer_addr,
                external_port: peer_addr.port(),
                peer_type,
            };

            let mut conn = Connection::new(node, socket, token, remote_peer, true);
            node.register_conn(&mut conn)?;
            candidates_lock.insert(conn.token, conn);

            if let Some(ref mut conn) = candidates_lock.get_mut(&token) {
                conn.low_level.send_handshake_message_a()?;
            }

            Ok(())
        }
        Err(e) => {
            if peer_type == PeerType::Node {
                write_or_die!(node.connection_handler.soft_bans).insert(
                    BanId::Socket(peer_addr),
                    Instant::now() + Duration::from_secs(config::UNREACHABLE_EXPIRATION_SECS),
                );
            }
            bail!(e)
        }
    }
}

/// Perform a round of connection maintenance, e.g. removing inactive ones.
pub fn connection_housekeeping(node: &Arc<P2PNode>) {
    debug!("Running connection housekeeping");

    let curr_stamp = get_current_stamp();
    let peer_type = node.peer_type();

    let is_conn_faulty = |conn: &Connection| -> bool {
        if let Some(max_latency) = node.config.max_latency {
            conn.get_latency() >= max_latency
        } else {
            false
        }
    };

    let is_conn_inactive = |conn: &Connection| -> bool {
        (peer_type == PeerType::Node
            && conn.last_seen() + config::MAX_NORMAL_KEEP_ALIVE < curr_stamp)
            || (peer_type == PeerType::Bootstrapper
                && conn.stats.created + config::MAX_BOOTSTRAPPER_KEEP_ALIVE < curr_stamp)
    };

    let is_conn_without_handshake = |conn: &Connection| -> bool {
        conn.stats.created + config::MAX_PREHANDSHAKE_KEEP_ALIVE < curr_stamp
    };

    // remove connections without handshakes
    lock_or_die!(node.conn_candidates()).retain(|_, conn| !is_conn_without_handshake(&conn));

    // remove faulty and inactive connections
    {
        let mut faulty_removed = false;
        write_or_die!(node.connections()).retain(|_, conn| {
            if is_conn_faulty(&conn) || is_conn_inactive(&conn) {
                faulty_removed = true;
                false
            } else {
                true
            }
        });
        if faulty_removed {
            node.bump_last_peer_update();
        }
    }

    // if the number of peers exceeds the desired value, close a random selection of
    // post-handshake connections to lower it
    if peer_type == PeerType::Node {
        let max_allowed_nodes = node.config.max_allowed_nodes;
        let peer_count = node.get_peer_stats(Some(PeerType::Node)).len() as u16;
        if peer_count > max_allowed_nodes {
            let mut rng = rand::thread_rng();
            let to_drop = read_or_die!(node.connections())
                .keys()
                .copied()
                .choose_multiple(&mut rng, (peer_count - max_allowed_nodes) as usize);

            node.remove_connections(&to_drop);
        }
    }

    // periodically lift soft bans
    {
        let mut soft_bans = write_or_die!(node.connection_handler.soft_bans);
        if !soft_bans.is_empty() {
            let now = Instant::now();
            soft_bans.retain(|_, expiry| *expiry > now);
        }
    }

    // reconnect to bootstrappers after a specified amount of time
    if !node.config.no_bootstrap_dns
        && peer_type == PeerType::Node
        && curr_stamp >= node.get_last_bootstrap() + node.config.bootstrapping_interval * 1000
    {
        attempt_bootstrap(node);
    }
}

/// A connetion is applicable for a broadcast if it is not in the exclusion
/// list, belongs to the same network, and doesn't belong to a bootstrapper.
fn is_valid_broadcast_target(
    conn: &Connection,
    peers_to_skip: &[P2PNodeId],
    network_id: NetworkId,
) -> bool {
    let peer_id = conn.remote_peer.id.unwrap(); // safe, post-handshake

    conn.remote_peer.peer_type != PeerType::Bootstrapper
        && !peers_to_skip.contains(&peer_id)
        && conn.remote_end_networks.contains(&network_id)
}

/// Send a direct packet with `msg` contents to the specified peer.
#[inline]
pub fn send_direct_message(
    node: &P2PNode,
    target_id: P2PNodeId,
    network_id: NetworkId,
    msg: Arc<[u8]>,
) -> usize {
    send_message_over_network(node, Some(target_id), vec![], network_id, msg)
}

/// Send a broadcast packet with `msg` contents to the specified peer.
#[inline]
pub fn send_broadcast_message(
    node: &P2PNode,
    dont_relay_to: Vec<P2PNodeId>,
    network_id: NetworkId,
    msg: Arc<[u8]>,
) -> usize {
    send_message_over_network(node, None, dont_relay_to, network_id, msg)
}

#[inline]
fn send_message_over_network(
    node: &P2PNode,
    target_id: Option<P2PNodeId>,
    dont_relay_to: Vec<P2PNodeId>,
    network_id: NetworkId,
    message: Arc<[u8]>,
) -> usize {
    let destination = if let Some(target_id) = target_id {
        PacketDestination::Direct(target_id)
    } else {
        PacketDestination::Broadcast(dont_relay_to)
    };

    let mut message = message.to_vec();

    if let Some((btype, btgt, blvl)) = &node.config.breakage {
        if btype == "fuzz" && (message[0] == *btgt || *btgt == 99) {
            fuzz_packet(&mut message[1..], *blvl);
        }
    }

    // Create packet.
    let packet = NetworkPacket {
        destination,
        network_id,
        message,
    };

    if let Ok(sent_packets) = node.process_network_packet(packet) {
        if sent_packets > 0 {
            trace!("{} peer(s) will receive the packet", sent_packets);
        }
        sent_packets
    } else {
        error!("Couldn't send a packet");
        0
    }
}

fn fuzz_packet(payload: &mut [u8], level: usize) {
    let rng = &mut rand::thread_rng();

    let level = cmp::min(payload.len(), level);

    for i in sample(rng, payload.len(), level).into_iter() {
        payload[i] = rng.gen();
    }
}
