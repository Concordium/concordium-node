use failure::{err_msg, Error, Fallible};
use mio::{net::TcpStream, Events, Token};
use rand::seq::IteratorRandom;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

#[cfg(feature = "beta")]
use crate::plugins::beta::get_username_from_jwt;
use crate::{
    common::{get_current_stamp, P2PNodeId, PeerType, RemotePeer},
    configuration as config,
    connection::{
        send_pending_messages, Connection, DeduplicationQueues, MessageSendingPriority, P2PEvent,
    },
    network::{
        NetworkId, NetworkMessage, NetworkMessagePayload, NetworkPacket, NetworkPacketType,
        NetworkRequest,
    },
    p2p::{bans::BanId, P2PNode},
};

use std::{
    cmp::Reverse,
    net::{IpAddr, SocketAddr},
    sync::{
        atomic::{AtomicU16, Ordering},
        Arc,
    },
    time::{Duration, Instant},
};

pub const SERVER: Token = Token(0);

// a convenience macro to send an object to all connections
macro_rules! send_to_all {
    ($foo_name:ident, $object_type:ty, $req_type:ident) => {
        pub fn $foo_name(&self, object: $object_type) {
            let request = NetworkRequest::$req_type(object);
            let mut message = NetworkMessage {
                timestamp1: None,
                timestamp2: None,
                payload: NetworkMessagePayload::NetworkRequest(request)
            };
            let filter = |_: &Connection| true;

            if let Err(e) = {
                let mut buf = Vec::with_capacity(256);
                message.serialize(&mut buf)
                    .map(|_| buf)
                    .and_then(|buf| self.send_over_all_connections(buf, &filter))
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

    /// It sends `data` message over all filtered connections.
    ///
    /// # Arguments
    /// * `data` - Raw message.
    /// * `conn_filter` - A closure filtering the connections
    /// # Returns the number of messages queued to be sent
    pub fn send_over_all_connections(
        &self,
        data: Vec<u8>,
        conn_filter: &dyn Fn(&Connection) -> bool,
    ) -> Fallible<usize> {
        let mut sent_messages = 0usize;
        let data = Arc::from(data);

        for conn in read_or_die!(self.connections())
            .values()
            .filter(|conn| conn.is_post_handshake() && conn_filter(conn))
        {
            conn.async_send(Arc::clone(&data), MessageSendingPriority::Normal);
            sent_messages += 1;
        }

        Ok(sent_messages)
    }

    pub fn forward_network_packet(&self, msg: NetworkMessage) -> Fallible<()> {
        if let Err(e) = self.rpc_queue.send(msg) {
            error!("Can't relay a message to the RPC outbound queue: {}", e);
        }

        Ok(())
    }

    pub fn measure_connection_latencies(&self) {
        debug!("Measuring connection latencies");

        let connections = read_or_die!(self.connections()).clone();
        for conn in connections.values().filter(|conn| conn.is_post_handshake()) {
            // don't send pings to lagging connections so
            // that the latency calculation is not invalid
            if conn.last_seen() > conn.get_last_ping_sent() {
                if let Err(e) = conn.send_ping() {
                    error!("Can't send a ping to {}: {}", conn, e);
                }
            }
        }
    }

    pub fn connection_housekeeping(&self) -> Fallible<()> {
        debug!("Running connection housekeeping");

        let curr_stamp = get_current_stamp();
        let peer_type = self.peer_type();

        // deduplicate by P2PNodeId
        {
            let conns = read_or_die!(self.connections()).clone();
            let mut conns =
                conns.values().filter(|conn| conn.is_post_handshake()).collect::<Vec<_>>();
            conns.sort_by_key(|conn| (conn.remote_id(), Reverse(conn.token)));
            conns.dedup_by_key(|conn| conn.remote_id());
            write_or_die!(self.connections())
                .retain(|_, conn| conns.iter().map(|c| c.token).any(|t| conn.token == t));
        }

        let is_conn_faulty = |conn: &Connection| -> bool {
            conn.failed_pkts() >= config::MAX_FAILED_PACKETS_ALLOWED
                || if let Some(max_latency) = self.config.max_latency {
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

        // Kill faulty and inactive connections
        write_or_die!(self.connections()).retain(|_, conn| {
            !(is_conn_faulty(&conn) || is_conn_inactive(&conn) || is_conn_without_handshake(&conn))
        });

        // If the number of peers exceeds the desired value, close a random selection of
        // post-handshake connections to lower it
        if peer_type == PeerType::Node {
            let max_allowed_nodes = self.config.max_allowed_nodes;
            let peer_count = self.get_peer_stats(Some(PeerType::Node)).len() as u16;
            if peer_count > max_allowed_nodes {
                let mut rng = rand::thread_rng();
                let to_drop = read_or_die!(self.connections())
                    .keys()
                    .copied()
                    .choose_multiple(&mut rng, (peer_count - max_allowed_nodes) as usize);

                self.remove_connections(&to_drop);
            }
        }

        // periodically lift soft bans
        {
            let mut soft_bans = write_or_die!(self.connection_handler.soft_bans);
            if !soft_bans.is_empty() {
                let now = Instant::now();
                soft_bans.retain(|_, expiry| *expiry > now);
            }
        }

        // reconnect to bootstrappers after a specified amount of time
        if peer_type == PeerType::Node
            && curr_stamp >= self.get_last_bootstrap() + self.config.bootstrapping_interval * 1000
        {
            self.attempt_bootstrap();
        }

        Ok(())
    }

    pub fn accept(&self) -> Fallible<Token> {
        let self_peer = self.self_peer;
        let (socket, addr) = self.connection_handler.server.accept()?;
        self.stats.conn_received_inc();

        {
            let conn_read_lock = read_or_die!(self.connections());

            if self.self_peer.peer_type() == PeerType::Node
                && self.config.hard_connection_limit.is_some()
                && conn_read_lock.values().len()
                    >= self.config.hard_connection_limit.unwrap() as usize
            {
                bail!("Too many connections, rejecting attempt from {:?}", addr);
            }

            if conn_read_lock.values().any(|conn| conn.remote_addr() == addr) {
                bail!("Duplicate connection attempt from {:?}; rejecting", addr);
            }

            if read_or_die!(self.connection_handler.soft_bans)
                .iter()
                .any(|(ip, _)| *ip == BanId::Ip(addr.ip()))
            {
                bail!("Connection attempt from a soft-banned IP ({:?}); rejecting", addr.ip());
            }
        }

        debug!(
            "Accepting new connection from {:?} to {:?}:{}",
            addr,
            self_peer.ip(),
            self_peer.port()
        );

        let token = Token(self.connection_handler.next_id.fetch_add(1, Ordering::SeqCst));

        let remote_peer = RemotePeer {
            id: Default::default(),
            addr,
            peer_external_port: AtomicU16::new(addr.port()),
            peer_type: PeerType::Node,
        };

        let conn = Connection::new(self, socket, token, remote_peer, false);

        conn.register(&self.poll)?;
        self.add_connection(conn);
        self.log_event(P2PEvent::ConnectEvent(addr));

        Ok(token)
    }

    pub fn connect(
        &self,
        peer_type: PeerType,
        addr: SocketAddr,
        peer_id_opt: Option<P2PNodeId>,
    ) -> Fallible<()> {
        debug!(
            "Attempting to connect to {}{}",
            addr,
            if let Some(id) = peer_id_opt {
                format!(" ({})", id)
            } else {
                "".to_owned()
            }
        );

        if peer_type == PeerType::Node {
            let current_peer_count = self.get_peer_stats(Some(PeerType::Node)).len() as u16;
            if current_peer_count > self.config.max_allowed_nodes {
                bail!(
                    "Maximum number of peers reached {}/{}",
                    current_peer_count,
                    self.config.max_allowed_nodes
                );
            }
        }

        // Don't connect to ourselves
        if self.self_peer.addr == addr || peer_id_opt == Some(self.id()) {
            bail!("Attempted to connect to myself");
        }

        // Don't connect to peers with a known P2PNodeId or IP+port
        for conn in read_or_die!(self.connections()).values() {
            if conn.remote_addr() == addr
                || (peer_id_opt.is_some() && conn.remote_id() == peer_id_opt)
            {
                bail!(
                    "Already connected to {}",
                    if let Some(id) = peer_id_opt {
                        id.to_string()
                    } else {
                        addr.to_string()
                    }
                );
            }
        }

        if read_or_die!(self.connection_handler.soft_bans)
            .iter()
            .any(|(ip, _)| *ip == BanId::Ip(addr.ip()) || *ip == BanId::Socket(addr))
        {
            bail!("Refusing to connect to a soft-banned IP ({:?})", addr.ip());
        }

        self.log_event(P2PEvent::InitiatingConnection(addr));
        match TcpStream::connect(&addr) {
            Ok(socket) => {
                self.stats.conn_received_inc();

                let token = Token(self.connection_handler.next_id.fetch_add(1, Ordering::SeqCst));

                let remote_peer = RemotePeer {
                    id: Default::default(),
                    addr,
                    peer_external_port: AtomicU16::new(addr.port()),
                    peer_type,
                };

                let conn = Connection::new(self, socket, token, remote_peer, true);

                conn.register(&self.poll)?;

                self.add_connection(conn);
                self.log_event(P2PEvent::ConnectEvent(addr));

                if let Some(ref conn) = self.find_connection_by_token(token) {
                    write_or_die!(conn.low_level).send_handshake_message_a()?;
                }

                if peer_type == PeerType::Bootstrapper {
                    self.update_last_bootstrap();
                }

                Ok(())
            }
            Err(e) => {
                if peer_type == PeerType::Node {
                    write_or_die!(self.connection_handler.soft_bans).insert(
                        BanId::Socket(addr),
                        Instant::now() + Duration::from_secs(config::UNREACHABLE_EXPIRATION_SECS),
                    );
                }
                into_err!(Err(e))
            }
        }
    }

    /// It adds this server to `network_id` network.
    pub fn add_network(&self, network_id: NetworkId) {
        write_or_die!(self.connection_handler.networks).insert(network_id);
    }

    pub fn find_connection_by_id(&self, id: P2PNodeId) -> Option<Arc<Connection>> {
        read_or_die!(self.connections())
            .values()
            .find(|conn| conn.remote_id() == Some(id))
            .map(|conn| Arc::clone(conn))
    }

    pub fn find_connection_by_token(&self, token: Token) -> Option<Arc<Connection>> {
        read_or_die!(self.connections()).get(&token).map(|conn| Arc::clone(conn))
    }

    pub fn find_connection_by_ip_addr(&self, addr: SocketAddr) -> Option<Arc<Connection>> {
        read_or_die!(self.connections())
            .values()
            .find(|conn| conn.remote_addr() == addr)
            .map(|conn| Arc::clone(conn))
    }

    pub fn find_connections_by_ip(&self, ip: IpAddr) -> Vec<Arc<Connection>> {
        read_or_die!(self.connections())
            .values()
            .filter(|conn| conn.remote_peer().addr().ip() == ip)
            .map(|conn| Arc::clone(conn))
            .collect()
    }

    pub fn remove_connection(&self, token: Token) -> bool {
        if let Some(conn) = write_or_die!(self.connections()).remove(&token) {
            if conn.is_post_handshake() {
                self.bump_last_peer_update();
            }
            write_or_die!(conn.low_level).conn_ref = None; // necessary in order for Drop to kick in
            true
        } else {
            false
        }
    }

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

    pub fn add_connection(&self, conn: Arc<Connection>) {
        write_or_die!(self.connections()).insert(conn.token, conn);
    }

    fn process_network_packet(
        &self,
        inner_pkt: NetworkPacket,
        source_id: P2PNodeId,
    ) -> Fallible<usize> {
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

        let mut message = NetworkMessage {
            timestamp1: Some(get_current_stamp()),
            timestamp2: None,
            payload:    NetworkMessagePayload::NetworkPacket(inner_pkt),
        };

        let mut serialized = Vec::with_capacity(256);
        message.serialize(&mut serialized)?;

        if let Some(target_id) = target {
            // direct messages
            let filter =
                |conn: &Connection| conn.remote_peer.peer().map(|p| p.id) == Some(target_id);

            self.send_over_all_connections(serialized, &filter)
        } else {
            // broadcast messages
            let filter = |conn: &Connection| {
                is_valid_broadcast_target(conn, source_id, &peers_to_skip, network_id)
            };

            self.send_over_all_connections(serialized, &filter)
        }
    }

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
}

/// Connetion is valid for a broadcast if sender is not target,
/// network_id is owned by connection, and the remote peer is not
/// a bootstrap node.
fn is_valid_broadcast_target(
    conn: &Connection,
    sender: P2PNodeId,
    peers_to_skip: &[P2PNodeId],
    network_id: NetworkId,
) -> bool {
    // safe, used only in a post-handshake context
    let peer_id = read_or_die!(conn.remote_peer.id).unwrap();

    conn.remote_peer.peer_type() != PeerType::Bootstrapper
        && peer_id != sender
        && !peers_to_skip.contains(&peer_id)
        && read_or_die!(conn.remote_end_networks()).contains(&network_id)
}

#[inline]
pub fn send_direct_message(
    node: &P2PNode,
    source_id: P2PNodeId,
    target_id: Option<P2PNodeId>,
    network_id: NetworkId,
    msg: Arc<[u8]>,
) -> Fallible<()> {
    send_message_over_network(node, source_id, target_id, vec![], network_id, msg, false)
}

#[inline]
pub fn send_broadcast_message(
    node: &P2PNode,
    source_id: P2PNodeId,
    dont_relay_to: Vec<P2PNodeId>,
    network_id: NetworkId,
    msg: Arc<[u8]>,
) -> Fallible<()> {
    send_message_over_network(node, source_id, None, dont_relay_to, network_id, msg, true)
}

#[inline]
fn send_message_over_network(
    node: &P2PNode,
    source_id: P2PNodeId,
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

    // Create packet.
    let packet = NetworkPacket {
        packet_type,
        network_id,
        message,
    };

    if let Ok(sent_packets) = node.process_network_packet(packet, source_id) {
        if sent_packets > 0 {
            trace!("Sent a packet to {} peers", sent_packets);
        }
    } else {
        error!("Couldn't send a packet");
    }

    Ok(())
}
