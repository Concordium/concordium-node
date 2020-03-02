use failure::{err_msg, Error, Fallible};
use mio::{net::TcpStream, Events, Token};
use rand::{
    seq::{index::sample, IteratorRandom},
    Rng,
};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use semver::Version;

use crate::{
    common::{get_current_stamp, P2PNodeId, PeerType, RemotePeer},
    configuration as config,
    connection::{send_pending_messages, Connection, DeduplicationQueues, MessageSendingPriority},
    netmsg,
    network::{
        Handshake, NetworkId, NetworkMessage, NetworkMessagePayload, NetworkPacket,
        NetworkPacketType, NetworkRequest,
    },
    p2p::{bans::BanId, maintenance::attempt_bootstrap, P2PNode},
};

use std::{
    cmp::{self, Reverse},
    net::{IpAddr, SocketAddr},
    sync::{
        atomic::{AtomicU16, Ordering},
        Arc,
    },
    time::{Duration, Instant},
};

/// The poll token of the node's socket server.
pub const SELF_TOKEN: Token = Token(0);

// a convenience macro to send an object to all connections
macro_rules! send_to_all {
    ($foo_name:ident, $object_type:ty, $req_type:ident) => {
        #[doc = "Send a specified network request to all peers"]
        pub fn $foo_name(&self, object: $object_type) {
            let request = NetworkRequest::$req_type(object);
            let message = netmsg!(NetworkRequest, request);
            let filter = |_: &Connection| true;

            if let Err(e) = {
                let mut buf = Vec::with_capacity(256);
                message.serialize(&mut buf)
                    .map(|_| buf)
                    .map(|buf| self.send_over_all_connections(&buf, &filter))
            } {
                error!("A network message couldn't be forwarded: {}", e);
            }
        }
    }
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

        for conn in read_or_die!(self.connections())
            .values()
            .filter(|conn| conn.is_post_handshake() && conn_filter(conn))
        {
            conn.async_send(Arc::clone(&data), MessageSendingPriority::Normal);
            sent_messages += 1;
        }

        sent_messages
    }

    /// Send out ping messages in order to update peer latency statistics.
    pub fn measure_connection_latencies(&self) {
        debug!("Measuring connection latencies");

        let connections = read_or_die!(self.connections()).clone();
        for conn in connections.values().filter(|conn| conn.is_post_handshake()) {
            // don't send pings to unresponsive connections so
            // that the latency calculation is not off
            if conn.last_seen() > conn.get_last_ping_sent() {
                if let Err(e) = conn.send_ping() {
                    error!("Can't send a ping to {}: {}", conn, e);
                }
            }
        }
    }

    /// Add a network to the list of node's networks.
    pub fn add_network(&self, network_id: NetworkId) {
        write_or_die!(self.connection_handler.networks).insert(network_id);
    }

    /// Search for a connection by the node id.
    pub fn find_connection_by_id(&self, id: P2PNodeId) -> Option<Arc<Connection>> {
        read_or_die!(self.connections())
            .values()
            .find(|conn| conn.remote_id() == Some(id))
            .map(|conn| Arc::clone(conn))
    }

    /// Search for a connection by the poll token.
    pub fn find_connection_by_token(&self, token: Token) -> Option<Arc<Connection>> {
        read_or_die!(self.connections()).get(&token).map(|conn| Arc::clone(conn))
    }

    /// Search for all connections with the specified IP address.
    pub fn find_connections_by_ip(&self, ip: IpAddr) -> Vec<Arc<Connection>> {
        read_or_die!(self.connections())
            .values()
            .filter(|conn| conn.remote_peer.addr().ip() == ip)
            .map(|conn| Arc::clone(conn))
            .collect()
    }

    /// Shut down connections with the given poll tokens.
    pub fn remove_connections(&self, tokens: &[Token]) -> bool {
        let connections = &mut write_or_die!(self.connections());

        let mut removed = 0;
        let mut update_peer_list = false;
        for token in tokens {
            if let Some(conn) = connections.remove(&token) {
                if conn.is_post_handshake() {
                    update_peer_list = true;
                }
                write_or_die!(conn.low_level).conn_ref = None; // necessary in order for Drop to kick in
                removed += 1;
            }
        }
        if update_peer_list {
            self.bump_last_peer_update();
        }

        removed == tokens.len()
    }

    fn add_connection(&self, conn: Arc<Connection>) {
        write_or_die!(self.connections()).insert(conn.token, conn);
    }

    fn process_network_packet(&self, inner_pkt: NetworkPacket) -> Fallible<usize> {
        let peers_to_skip = match inner_pkt.packet_type {
            NetworkPacketType::DirectMessage(..) => vec![],
            NetworkPacketType::BroadcastedMessage(ref dont_relay_to) => {
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

        let target = if let NetworkPacketType::DirectMessage(receiver) = inner_pkt.packet_type {
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
        message.serialize(&mut serialized)?;

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
    pub fn process_network_events(
        &self,
        events: &Events,
        deduplication_queues: &DeduplicationQueues,
        connections: &mut Vec<(Token, Arc<Connection>)>,
    ) -> (Vec<Token>, Vec<(IpAddr, Error)>) {
        connections.clear();
        // FIXME: it would be cool if we were able to remove this intermediate vector
        for (token, conn) in read_or_die!(self.connections()).iter() {
            connections.push((*token, Arc::clone(&conn)));
        }

        // collect tokens to remove and ips to soft ban, if any
        connections
            .par_iter()
            .filter_map(|(token, conn)| {
                let mut low_level = write_or_die!(conn.low_level);

                if let Err(e) = send_pending_messages(&conn.pending_messages, &mut low_level)
                    .and_then(|_| low_level.flush_socket())
                {
                    error!("{}", e);
                    return Some((*token, (conn.remote_addr().ip(), e)));
                }

                if events
                    .iter()
                    .any(|event| event.token() == *token && event.readiness().is_readable())
                {
                    if let Err(e) = low_level.read_stream(deduplication_queues) {
                        error!("{}", e);
                        Some((*token, (conn.remote_addr().ip(), e)))
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .unzip()
    }

    /// Creates a "high-level" handshake request to be sent to new peers.
    pub fn produce_handshake_request(&self) -> Fallible<Vec<u8>> {
        let handshake_request = netmsg!(
            NetworkRequest,
            NetworkRequest::Handshake(Handshake {
                remote_id:   self.self_peer.id(),
                remote_port: self.self_peer.port(),
                networks:    read_or_die!(self.networks()).iter().copied().collect(),
                version:     Version::parse(env!("CARGO_PKG_VERSION"))?,
                proof:       vec![],
            })
        );
        let mut serialized = Vec::with_capacity(128);
        handshake_request.serialize(&mut serialized)?;

        Ok(serialized)
    }
}

/// Accept an incoming network connection.
pub fn accept(node: &Arc<P2PNode>) -> Fallible<Token> {
    let self_peer = node.self_peer;
    let (socket, addr) = node.connection_handler.socket_server.accept()?;
    node.stats.conn_received_inc();

    {
        let conn_read_lock = read_or_die!(node.connections());

        if node.self_peer.peer_type() == PeerType::Node
            && node.config.hard_connection_limit.is_some()
            && conn_read_lock.values().len() >= node.config.hard_connection_limit.unwrap() as usize
        {
            bail!("Too many connections, rejecting attempt from {:?}", addr);
        }

        if conn_read_lock.values().any(|conn| conn.remote_addr() == addr) {
            bail!("Duplicate connection attempt from {:?}; rejecting", addr);
        }

        if read_or_die!(node.connection_handler.soft_bans)
            .iter()
            .any(|(ip, _)| *ip == BanId::Ip(addr.ip()))
        {
            bail!("Connection attempt from a soft-banned IP ({:?}); rejecting", addr.ip());
        }
    }

    debug!("Accepting new connection from {:?} to {:?}:{}", addr, self_peer.ip(), self_peer.port());

    let token = Token(node.connection_handler.next_token.fetch_add(1, Ordering::SeqCst));

    let remote_peer = RemotePeer {
        id: Default::default(),
        addr,
        peer_external_port: AtomicU16::new(addr.port()),
        peer_type: PeerType::Node,
    };

    let conn = Connection::new(node, socket, token, remote_peer, false);

    conn.register(&node.poll)?;
    node.add_connection(conn);

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
        if current_peer_count > node.config.max_allowed_nodes {
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

    // We purposely take a write lock to ensure that we will block all the way until
    // the connection has been either established or failed, as otherwise we can't
    // be certain the duplicate check can't pass erroneously because two or more
    // calls happen in too rapid succession.
    let mut write_lock_connections = write_or_die!(node.connections());

    // Don't connect to peers with a known P2PNodeId or IP+port
    for conn in write_lock_connections.values() {
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

    match TcpStream::connect(&peer_addr) {
        Ok(socket) => {
            node.stats.conn_received_inc();

            let token = Token(node.connection_handler.next_token.fetch_add(1, Ordering::SeqCst));

            let remote_peer = RemotePeer {
                id: Default::default(),
                addr: peer_addr,
                peer_external_port: AtomicU16::new(peer_addr.port()),
                peer_type,
            };

            let conn = Connection::new(node, socket, token, remote_peer, true);

            conn.register(&node.poll)?;

            write_lock_connections.insert(conn.token, conn);

            if let Some(ref conn) = write_lock_connections.get(&token).map(|conn| Arc::clone(conn))
            {
                write_or_die!(conn.low_level).send_handshake_message_a()?;
            }

            if peer_type == PeerType::Bootstrapper {
                node.update_last_bootstrap();
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
            into_err!(Err(e))
        }
    }
}

/// Perform a round of connection maintenance, e.g. removing inactive ones.
pub fn connection_housekeeping(node: &Arc<P2PNode>) -> Fallible<()> {
    debug!("Running connection housekeeping");

    let curr_stamp = get_current_stamp();
    let peer_type = node.peer_type();

    // deduplicate by P2PNodeId
    {
        let conns = read_or_die!(node.connections()).clone();
        let mut conns = conns.values().filter(|conn| conn.is_post_handshake()).collect::<Vec<_>>();
        conns.sort_by_key(|conn| (conn.remote_id(), Reverse(conn.token)));
        conns.dedup_by_key(|conn| conn.remote_id());
        write_or_die!(node.connections())
            .retain(|_, conn| conns.iter().map(|c| c.token).any(|t| conn.token == t));
    }

    let is_conn_faulty = |conn: &Connection| -> bool {
        if let Some(max_latency) = node.config.max_latency {
            conn.get_last_latency() >= max_latency
        } else {
            false
        }
    };

    let is_conn_inactive = |conn: &Connection| -> bool {
        conn.is_post_handshake()
            && ((peer_type == PeerType::Node
                && conn.last_seen() + config::MAX_NORMAL_KEEP_ALIVE < curr_stamp)
                || (peer_type == PeerType::Bootstrapper
                    && conn.last_seen() + config::MAX_BOOTSTRAPPER_KEEP_ALIVE < curr_stamp))
    };

    let is_conn_without_handshake = |conn: &Connection| -> bool {
        !conn.is_post_handshake()
            && conn.last_seen() + config::MAX_PREHANDSHAKE_KEEP_ALIVE < curr_stamp
    };

    // remove faulty and inactive connections
    write_or_die!(node.connections()).retain(|_, conn| {
        !(is_conn_faulty(&conn) || is_conn_inactive(&conn) || is_conn_without_handshake(&conn))
    });

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
    if peer_type == PeerType::Node
        && curr_stamp >= node.get_last_bootstrap() + node.config.bootstrapping_interval * 1000
    {
        attempt_bootstrap(node);
    }

    Ok(())
}

/// A connetion is applicable for a broadcast if it is not in the exclusion
/// list, belongs to the same network, and doesn't belong to a bootstrapper.
fn is_valid_broadcast_target(
    conn: &Connection,
    peers_to_skip: &[P2PNodeId],
    network_id: NetworkId,
) -> bool {
    let peer_id = read_or_die!(conn.remote_peer.id).unwrap(); // safe, post-handshake

    conn.remote_peer.peer_type() != PeerType::Bootstrapper
        && !peers_to_skip.contains(&peer_id)
        && read_or_die!(conn.remote_end_networks).contains(&network_id)
}

/// Send a direct packet with `msg` contents to the specified peer.
#[inline]
pub fn send_direct_message(
    node: &P2PNode,
    target_id: P2PNodeId,
    network_id: NetworkId,
    msg: Arc<[u8]>,
) -> Fallible<()> {
    send_message_over_network(node, Some(target_id), vec![], network_id, msg, false)
}

/// Send a broadcast packet with `msg` contents to the specified peer.
#[inline]
pub fn send_broadcast_message(
    node: &P2PNode,
    dont_relay_to: Vec<P2PNodeId>,
    network_id: NetworkId,
    msg: Arc<[u8]>,
) -> Fallible<()> {
    send_message_over_network(node, None, dont_relay_to, network_id, msg, true)
}

#[inline]
fn send_message_over_network(
    node: &P2PNode,
    target_id: Option<P2PNodeId>,
    dont_relay_to: Vec<P2PNodeId>,
    network_id: NetworkId,
    message: Arc<[u8]>,
    broadcast: bool,
) -> Fallible<()> {
    let packet_type = if broadcast {
        NetworkPacketType::BroadcastedMessage(dont_relay_to)
    } else {
        let receiver =
            target_id.ok_or_else(|| err_msg("Direct Message requires a valid target id"))?;

        NetworkPacketType::DirectMessage(receiver)
    };

    let mut message = message.to_vec();

    if let Some((btype, btgt, blvl)) = &node.config.breakage {
        if btype == "fuzz" && (message[0] == *btgt || *btgt == 99) {
            fuzz_packet(&mut message[1..], *blvl);
        }
    }

    // Create packet.
    let packet = NetworkPacket {
        packet_type,
        network_id,
        message,
    };

    if let Ok(sent_packets) = node.process_network_packet(packet) {
        if sent_packets > 0 {
            trace!("Sent a packet to {} peers", sent_packets);
        }
    } else {
        error!("Couldn't send a packet");
    }

    Ok(())
}

fn fuzz_packet(payload: &mut [u8], level: usize) {
    let rng = &mut rand::thread_rng();

    let level = cmp::min(payload.len(), level);

    for i in sample(rng, payload.len(), level).into_iter() {
        payload[i] = rng.gen();
    }
}
