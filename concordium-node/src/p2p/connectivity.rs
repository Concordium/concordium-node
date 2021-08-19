//! Node connection handling.

use crate::{
    common::{get_current_stamp, p2p_peer::RemotePeerId, P2PNodeId, PeerType, RemotePeer},
    configuration as config,
    connection::{
        version_adapter::rewrite_outgoing, ConnChange, Connection, MessageSendingPriority,
    },
    lock_or_die, netmsg,
    network::{
        Handshake, NetworkId, NetworkPacket, NetworkRequest, PacketDestination,
        WIRE_PROTOCOL_CURRENT_VERSION, WIRE_PROTOCOL_LEGACY_VERSION, WIRE_PROTOCOL_VERSIONS,
    },
    p2p::{
        bans::{BanId, PersistedBanId},
        maintenance::attempt_bootstrap,
        P2PNode,
    },
    read_or_die, write_or_die,
};
use anyhow::bail;
use mio::{event::Event, net::TcpStream, Events, Token};
use rand::seq::IteratorRandom;
use rayon::iter::{IntoParallelRefMutIterator, ParallelIterator};
use semver::Version;
use std::{
    io,
    net::{IpAddr, SocketAddr},
    sync::{atomic::Ordering, Arc},
    time::{Duration, Instant},
};
use thiserror::Error;

/// The poll token of the node's socket server.
pub const SELF_TOKEN: Token = Token(0);

impl P2PNode {
    /// Broadcast a request to join a network.
    /// Note that this needs a write lock on the node's connections object.
    pub fn send_join_network(&self, id: NetworkId) {
        self.broadcast_network_request(NetworkRequest::JoinNetwork(id))
    }

    /// Broadcast a request to leave the network.
    /// Note that this needs a write lock on the node's connections object.
    pub fn send_leave_network(&self, id: NetworkId) {
        self.broadcast_network_request(NetworkRequest::LeaveNetwork(id))
    }

    /// Send a network change request to all the peers.
    /// Note that this needs a write lock on the node's connections object.
    pub fn broadcast_network_request(&self, request: NetworkRequest) {
        let message = netmsg!(NetworkRequest, request);
        let mut serialized = Vec::with_capacity(256);
        if let Err(e) = message.serialize(&mut serialized) {
            error!("Could not serialize a network request message: {}", e)
        } else {
            let filter = |_: &Connection| true;
            self.send_over_all_connections(&serialized, &filter);
        }
    }

    /// Send a `data` message to all connections adhering to the specified
    /// filter. Returns the number of sent messages.
    pub fn send_over_all_connections(
        &self,
        data: &[u8],
        conn_filter: &dyn Fn(&Connection) -> bool,
    ) -> usize {
        let mut sent_messages = 0usize;
        let data = Arc::from(data);
        // legacy_data holds a rewritten version of data for legacy peers on a by-need
        // basis. If legacy_data is None, then so far no peers have required the
        // legacy version. If legacy_data is Some(None), then the message is not
        // supported by legacy peers. If legacy_data is Some(Some(v)), then v
        // should be sent to the legacy peers.
        let mut legacy_data: Option<Option<Arc<[u8]>>> = None;

        for conn in write_or_die!(self.connections()).values_mut().filter(|conn| conn_filter(conn))
        {
            if conn.wire_version == WIRE_PROTOCOL_CURRENT_VERSION {
                conn.async_send(Arc::clone(&data), MessageSendingPriority::Normal);
                sent_messages += 1;
            } else {
                assert_eq!(conn.wire_version, WIRE_PROTOCOL_LEGACY_VERSION);
                let send_data = match legacy_data.clone() {
                    None => {
                        // The message for legacy peers has not yet been constructed, so do so.
                        let send_data = match rewrite_outgoing(WIRE_PROTOCOL_LEGACY_VERSION, &data)
                        {
                            Ok(None) => Some(Arc::clone(&data)),
                            Ok(Some(legacy)) => Some(legacy),
                            Err(e) => {
                                debug!("Dropping message for legacy peers: {}", e);
                                None
                            }
                        };
                        legacy_data = Some(send_data.clone());
                        send_data
                    }
                    Some(legacy) => legacy,
                };
                if let Some(send) = send_data {
                    conn.async_send(send, MessageSendingPriority::Normal);
                    sent_messages += 1;
                } else {
                    trace!("Message cannot be sent on legacy connection to {}", conn);
                }
            }
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

    /// Find a connection token of the connection to the given post-handshake
    /// peer, if such a connection exists.
    /// NB: This acquires and releases a read lock on the node's connections.
    pub fn find_conn_token_by_id(&self, id: RemotePeerId) -> Option<Token> {
        read_or_die!(self.connections()).values().find_map(|conn| {
            if conn.remote_peer.local_id == id {
                Some(conn.token())
            } else {
                None
            }
        })
    }

    /// Find a connection to the given address. We assume at most one such
    /// exists.
    /// NB: This acquires and releases a read lock on the node's connections.
    pub fn find_conn_to(&self, addr: SocketAddr) -> Option<Token> {
        lock_or_die!(self.conn_candidates())
            .values()
            .chain(read_or_die!(self.connections()).values())
            .find_map(|conn| {
                if conn.remote_addr() == addr {
                    Some(conn.token())
                } else {
                    None
                }
            })
    }

    /// Find connection tokens for all connections to the given ip address.
    /// This acquires a read lock on the node's connections and
    /// connection_candidates objects.
    pub fn find_conn_tokens_by_ip(&self, ip_addr: IpAddr) -> Vec<Token> {
        lock_or_die!(self.conn_candidates())
            .values()
            .chain(read_or_die!(self.connections()).values())
            .filter_map(|conn| {
                if conn.remote_peer.addr.ip() == ip_addr {
                    Some(conn.token())
                } else {
                    None
                }
            })
            .collect()
    }

    /// Shut down connection with the given poll token.
    /// Returns the remote peer, i.e., the other end, of the just closed
    /// connection, if it exists. None is only returned if no connection
    /// with the given token exists.
    pub fn remove_connection(&self, token: Token) -> Option<RemotePeer> {
        // First attempt to remove connection in the handshake phase.
        if let Some(removed_cand) = lock_or_die!(self.conn_candidates()).remove(&token) {
            Some(removed_cand.remote_peer)
        } else {
            // otherwise try to remove a full peer
            let removed_conn = write_or_die!(self.connections()).remove(&token)?;
            self.bump_last_peer_update();
            Some(removed_conn.remote_peer)
        }
    }

    /// Shut down connections with the given poll tokens.
    /// Returns `true` if any connections were removed, and `false` otherwise.
    pub fn remove_connections(&self, tokens: &[Token]) -> bool {
        // This is not implemented as a simple iteration using remove_connection because
        // that would require more lock acquisitions and calls to bump_last_peer_update.
        let conn_candidates = &mut lock_or_die!(self.conn_candidates());
        let connections = &mut write_or_die!(self.connections());

        let mut removed_peers = false;
        let mut removed_candidates = false;
        for token in tokens {
            if conn_candidates.remove(&token).is_some() {
                removed_candidates = true;
            } else if connections.remove(&token).is_some() {
                removed_peers = true;
            }
        }
        if removed_peers {
            self.bump_last_peer_update();
        }
        removed_candidates || removed_peers
    }

    /// Close connection to the given address, if any.
    pub fn remove_connection_to_addr(&self, addr: SocketAddr) {
        lock_or_die!(self.conn_candidates()).retain(|_, conn| conn.remote_addr() != addr);
        write_or_die!(self.connections()).retain(|_, conn| conn.remote_addr() != addr);
    }

    fn process_network_packet(&self, inner_pkt: NetworkPacket) -> anyhow::Result<usize> {
        let peers_to_skip = match inner_pkt.destination {
            PacketDestination::Direct(..) => vec![],
            PacketDestination::Broadcast(ref dont_relay_to) => {
                if self.config.relay_broadcast_percentage < 1.0 {
                    use rand::seq::SliceRandom;
                    let mut rng = rand::thread_rng();
                    let mut peers = self.get_node_peer_tokens();
                    peers.retain(|token| !dont_relay_to.contains(&token));
                    let peers_to_take = f64::floor(
                        f64::from(peers.len() as u32) * self.config.relay_broadcast_percentage,
                    );
                    peers
                        .choose_multiple(&mut rng, peers_to_take as usize)
                        .copied()
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

        let message = netmsg!(NetworkPacket, inner_pkt);
        let mut serialized = Vec::with_capacity(256);
        message.serialize(&mut serialized)?;

        let mut sent = 0;
        if let Some(target_token) = target {
            // direct messages
            let filter = |conn: &Connection| conn.remote_peer.local_id == target_token;
            sent += self.send_over_all_connections(&serialized, &filter);
        } else {
            // broadcast messages
            let filter =
                |conn: &Connection| is_valid_broadcast_target(conn, &peers_to_skip, network_id);
            sent += self.send_over_all_connections(&serialized, &filter);
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
                if events.iter().any(|event| event.token() == conn.token() && event.is_writable()) {
                    conn.low_level.notify_writable();
                }

                if let Err(e) =
                    conn.send_pending_messages().and_then(|_| conn.low_level.flush_socket())
                {
                    error!("[sending to {}] {}", conn, e);
                    if let Ok(_io_err) = e.downcast::<io::Error>() {
                        self.register_conn_change(ConnChange::RemovalByToken(conn.token()));
                    } else {
                        self.register_conn_change(ConnChange::ExpulsionByToken(conn.token()));
                    }
                    return;
                }

                if events.iter().any(|event| event.token() == conn.token() && event.is_readable()) {
                    match conn.read_stream(&conn_stats) {
                        Err(e) => {
                            error!("[receiving from {}] {}", conn, e);
                            if let Ok(_io_err) = e.downcast::<io::Error>() {
                                self.register_conn_change(ConnChange::RemovalByToken(conn.token()));
                            } else {
                                self.register_conn_change(ConnChange::ExpulsionByToken(
                                    conn.token(),
                                ));
                            }
                            return;
                        }
                        Ok(false) => {
                            // The connection was closed by the peer.
                            debug!("Connection to {} closed by peer", conn);
                            self.register_conn_change(ConnChange::RemovalByToken(conn.token()));
                            return;
                        }
                        Ok(true) => {}
                    }
                }

                let closed_or_error = |event: &Event| {
                    event.token() == conn.token()
                        && (event.is_read_closed() || event.is_write_closed() || event.is_error())
                };

                if events.iter().any(closed_or_error) {
                    // Generally, connections will be closed as a result of a read or write failing
                    // or returning 0 bytes, rather than reaching here. This is more of a back stop,
                    // and might catch a failure sooner in the case where we do not currently have
                    // anything to write.
                    debug!("Closing connection to {}", conn);
                    self.register_conn_change(ConnChange::RemovalByToken(conn.token()));
                }
            })
    }

    /// Creates a "high-level" handshake request to be sent to new peers.
    pub fn produce_handshake_request(&self) -> anyhow::Result<Vec<u8>> {
        let handshake_request = netmsg!(
            NetworkRequest,
            NetworkRequest::Handshake(Handshake {
                remote_id:      self.self_peer.id,
                remote_port:    self.self_peer.port(),
                networks:       read_or_die!(self.networks()).iter().copied().collect(),
                node_version:   Version::parse(env!("CARGO_PKG_VERSION"))?,
                wire_versions:  WIRE_PROTOCOL_VERSIONS.to_vec(),
                genesis_blocks: read_or_die!(self.config.regenesis_arc.blocks).clone(),
                proof:          vec![],
            })
        );
        let mut serialized = Vec::with_capacity(128);
        handshake_request.serialize(&mut serialized)?;

        Ok(serialized)
    }
}

#[derive(Debug, Error)]
pub enum AcceptFailureReason {
    #[error("Too many existing connections. Not accepting an additional one from {addr}.")]
    TooManyConnections {
        addr: SocketAddr,
    },
    #[error("Already connected to IP {ip}.")]
    AlreadyConnectedToIP {
        ip: IpAddr,
    },
    #[error("Duplicate connection attempt from {addr}.")]
    DuplicateConnection {
        addr: SocketAddr,
    },
    #[error("Connection attempt from a banned address.")]
    Banned,
    #[error("Connection attempt from a soft-banned address.")]
    SoftBanned,
    #[error("{err}")]
    Other {
        #[from]
        err: anyhow::Error,
    },
}

/// Attempt to accept an incoming network connection.
/// - If an error occurs, e.g., fail to accept the socket connection, or fail to
///   register with the poll registry return Err
/// - Else return the new connection token that can be used to poll for incoming
///   data.
pub fn accept(
    node: &Arc<P2PNode>,
    socket: TcpStream,
    addr: SocketAddr,
) -> Result<Token, AcceptFailureReason> {
    node.stats.conn_received_inc();

    // if we fail to read the database we allow the connection.
    // This is fine as long as we assume that nobody can corrupt our ban database.
    if node.is_banned(PersistedBanId::Ip(addr.ip())).unwrap_or(false) {
        warn!("Connection attempt from a banned IP {}.", addr.ip());
        return Err(AcceptFailureReason::Banned);
    }

    // Lock the candidate list for added safety against duplicate connections
    let mut candidates_lock = lock_or_die!(node.conn_candidates());

    {
        let conn_read_lock = read_or_die!(node.connections());

        // A reasonable argument might be made to try and accept connections
        // from given addresses even if we have reached the maximum limit.
        // However the remote address will almost certainly not be what we have recorded
        // among the given addresses since it will have some OS generated
        // outgoing socket. We could check that it is coming from a given IP,
        // but at the moment that is not how we identify trusted addresses, so
        // it would violate the general rule and complicate local testing.
        if node.self_peer.peer_type == PeerType::Node
            && candidates_lock.len() + conn_read_lock.len()
                >= node.config.hard_connection_limit as usize
        {
            return Err(AcceptFailureReason::TooManyConnections {
                addr,
            });
        }

        for conn in candidates_lock.values().chain(conn_read_lock.values()) {
            if conn.remote_addr().ip() == addr.ip() {
                if node.config.disallow_multiple_peers_on_ip {
                    return Err(AcceptFailureReason::AlreadyConnectedToIP {
                        ip: addr.ip(),
                    });
                } else if conn.remote_addr().port() == addr.port()
                    || conn.remote_peer.external_port == addr.port()
                {
                    return Err(AcceptFailureReason::DuplicateConnection {
                        addr,
                    });
                }
            }
        }

        if node.connection_handler.is_soft_banned(addr) {
            warn!("Connection attempt from a soft-banned IP ({}); rejecting", addr.ip());
            return Err(AcceptFailureReason::SoftBanned);
        }
    }

    debug!("Accepting a connection from {}", addr);

    let token = Token(node.connection_handler.next_token.fetch_add(1, Ordering::SeqCst));

    let remote_peer = RemotePeer {
        self_id: Default::default(),
        addr,
        local_id: token.into(),
        external_port: addr.port(),
        peer_type: PeerType::Node,
    };

    let conn = Connection::new(node, socket, token, remote_peer, false)?;
    candidates_lock.insert(conn.token(), conn);

    Ok(token)
}

/// Connect to another node with the specified address and optionally peer id,
/// registering it as the given peer type.
pub fn connect(
    node: &Arc<P2PNode>,
    peer_type: PeerType, /* type of the peer we are connecting to. This is a our expectation of
                          * what the peer will be, not what it actually is. */
    peer_addr: SocketAddr,      // address to connect to
    peer_id: Option<P2PNodeId>, // id of the peer we are connecting to, if known
    respect_max_peers: bool,    // whether this should respect the maximum peeers setting or not.
) -> anyhow::Result<()> {
    debug!(
        "Attempting to connect to {}{}",
        peer_addr,
        if let Some(id) = peer_id {
            format!(" ({})", id)
        } else {
            "".to_owned()
        }
    );

    if respect_max_peers && peer_type == PeerType::Node {
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
    if node.self_peer.addr == peer_addr {
        bail!("Attempted to connect to myself");
    }

    // Don't connect to banned IPs.
    if node.is_banned(PersistedBanId::Ip(peer_addr.ip())).unwrap_or(false) {
        bail!("Refusing to connect to a banned IP ({})", peer_addr.ip());
    }

    // Or to soft-banned nodes.
    if node.connection_handler.is_soft_banned(peer_addr) {
        bail!("Refusing to connect to a soft-banned IP ({})", peer_addr.ip());
    }

    // Lock the candidate list for added safety against duplicate connections
    let mut candidates_lock = lock_or_die!(node.conn_candidates());

    // Don't connect to established connections on a given IP + port
    for conn in read_or_die!(node.connections()).values().chain(candidates_lock.values()) {
        if node.config.disallow_multiple_peers_on_ip {
            if conn.remote_addr().ip() == peer_addr.ip() {
                bail!("Already connected to IP {}", peer_addr.ip());
            }
        } else if conn.remote_addr() == peer_addr || conn.remote_peer.external_addr() == peer_addr {
            bail!("Already connected to {}", peer_addr);
        }
    }

    match TcpStream::connect(peer_addr) {
        Ok(socket) => {
            trace!("Connected to {}", peer_addr);
            node.stats.conn_received_inc();

            let token = Token(node.connection_handler.next_token.fetch_add(1, Ordering::SeqCst));

            let remote_peer = RemotePeer {
                self_id: None,
                addr: peer_addr,
                local_id: token.into(),
                external_port: peer_addr.port(),
                peer_type,
            };

            let mut conn = Connection::new(node, socket, token, remote_peer, true)?;
            // send the initial handshake
            conn.low_level.send_handshake_message_a()?;
            // and record the connection candidate. Note that we maintain the
            // connection candidates lock so it is OK to only insert the connection at the
            // end here.
            candidates_lock.insert(conn.token(), conn);

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
/// Return whether we attempted to bootstrap.
pub fn connection_housekeeping(node: &Arc<P2PNode>) -> bool {
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
    // post-handshake non-given connections to lower it
    if peer_type == PeerType::Node {
        let max_allowed_nodes = node.config.max_allowed_nodes;
        let peer_count = node.get_peer_stats(Some(PeerType::Node)).len() as u16;
        if peer_count > max_allowed_nodes {
            // drop connections to any non-given peers.
            let mut rng = rand::thread_rng();
            let to_drop = read_or_die!(node.connections())
                .iter()
                .filter_map(|(&token, conn)| {
                    // only consider non-given connections for removal
                    if node.is_given_connection(conn) {
                        None
                    } else {
                        Some(token)
                    }
                })
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

    // Try to connect to any given addresses we are not connected to.
    for given in node.unconnected_given_addresses() {
        if let Err(e) = connect(node, PeerType::Node, given, None, false) {
            warn!("Cannot establish connection to a given address {}: {}", given, e)
        }
    }

    // Log all the bad events that happened and reset all their counters.
    for (peer_id, invalid_msgs) in lock_or_die!(node.bad_events.invalid_messages).drain() {
        warn!("Received {} invalid messages from peer {}", invalid_msgs, peer_id);
    }
    for (peer_id, dropped) in lock_or_die!(node.bad_events.dropped_high_queue).drain() {
        warn!("Dropped {} high priority messages from peer {}.", dropped, peer_id);
    }
    for (peer_id, dropped) in lock_or_die!(node.bad_events.dropped_low_queue).drain() {
        warn!("Dropped {} low priority messages from peer {}.", dropped, peer_id);
    }

    // Reconnect to bootstrappers after a specified amount of time.
    // It's unclear whether we should always be doing this, even if we have enough
    // peers. But the current logic is to try to bootstrap again, and if we have
    // too many peers drop a subset of them.
    if !node.config.no_bootstrap_dns
        && peer_type == PeerType::Node
        && curr_stamp >= node.get_last_bootstrap() + node.config.bootstrapping_interval * 1000
    {
        attempt_bootstrap(node);
        true
    } else {
        false
    }
}

/// A connection is applicable for a broadcast if it is not in the exclusion
/// list, belongs to the same network, and doesn't belong to a bootstrapper.
fn is_valid_broadcast_target(
    conn: &Connection,
    peers_to_skip: &[RemotePeerId],
    network_id: NetworkId,
) -> bool {
    conn.remote_peer.peer_type != PeerType::Bootstrapper
        && !peers_to_skip.contains(&conn.remote_peer.local_id)
        && conn.remote_end_networks.contains(&network_id)
}

/// Send a direct packet with `msg` contents to the specified peer.
#[inline]
pub fn send_direct_message(
    node: &P2PNode,
    target_id: RemotePeerId,
    network_id: NetworkId,
    msg: Arc<[u8]>,
) -> usize {
    send_message_over_network(node, Some(target_id), vec![], network_id, msg)
}

/// Send a broadcast packet with `msg` contents to the specified peer.
#[inline]
pub fn send_broadcast_message(
    node: &P2PNode,
    dont_relay_to: Vec<RemotePeerId>,
    network_id: NetworkId,
    msg: Arc<[u8]>,
) -> usize {
    send_message_over_network(node, None, dont_relay_to, network_id, msg)
}

#[inline]
fn send_message_over_network(
    node: &P2PNode,
    target_id: Option<RemotePeerId>,
    dont_relay_to: Vec<RemotePeerId>,
    network_id: NetworkId,
    message: Arc<[u8]>,
) -> usize {
    let destination = if let Some(target_id) = target_id {
        PacketDestination::Direct(target_id)
    } else {
        PacketDestination::Broadcast(dont_relay_to)
    };

    let message = message.to_vec();

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
