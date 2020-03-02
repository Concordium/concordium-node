mod low_level;
pub mod message_handlers;
#[cfg(test)]
mod tests;

use low_level::ConnectionLowLevel;

use bytesize::ByteSize;
use chrono::prelude::Utc;
use circular_queue::CircularQueue;
use digest::Digest;
use failure::Fallible;
use mio::{tcp::TcpStream, Poll, PollOpt, Ready, Token};
use priority_queue::PriorityQueue;
use semver::Version;
use twox_hash::XxHash64;

use crate::{
    common::{get_current_stamp, p2p_peer::P2PPeer, P2PNodeId, PeerStats, PeerType, RemotePeer},
    dumper::DumpItem,
    netmsg,
    network::{
        Handshake, NetworkId, NetworkMessage, NetworkMessagePayload, NetworkPacket, NetworkRequest,
        NetworkResponse,
    },
    p2p::{bans::BanId, Networks, P2PNode},
};
use concordium_common::PacketType;
use crypto_common::Deserial;

use std::{
    cmp::Reverse,
    collections::HashSet,
    convert::TryFrom,
    fmt,
    io::Cursor,
    net::SocketAddr,
    sync::{
        atomic::{AtomicBool, AtomicU64, Ordering},
        Arc, RwLock,
    },
    time::Instant,
};

// If a message is labelled as having `High` priority it is always pushed to the
// front of the queue in the sinks when sending, and otherwise to the back.
#[derive(PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub enum MessageSendingPriority {
    Normal,
    High,
}

/// Contains the circular queues of hashes of different consensus objects
/// for deduplication purposes.
pub struct DeduplicationQueues {
    pub finalizations: RwLock<CircularQueue<u64>>,
    pub transactions:  RwLock<CircularQueue<u64>>,
    pub blocks:        RwLock<CircularQueue<u64>>,
    pub fin_records:   RwLock<CircularQueue<u64>>,
}

impl DeduplicationQueues {
    /// Creates the deduplication queues of specified sizes: short for blocks
    /// and finalization records and long for finalization messages and
    /// transactions.
    pub fn new(long_size: usize, short_size: usize) -> Arc<Self> {
        Arc::new(Self {
            finalizations: RwLock::new(CircularQueue::with_capacity(long_size)),
            transactions:  RwLock::new(CircularQueue::with_capacity(long_size)),
            blocks:        RwLock::new(CircularQueue::with_capacity(short_size)),
            fin_records:   RwLock::new(CircularQueue::with_capacity(short_size)),
        })
    }
}

/// Contains all the statistics of a connection.
pub struct ConnectionStats {
    pub last_ping_sent:    AtomicU64,
    pub sent_handshake:    AtomicU64,
    pub last_seen:         AtomicU64,
    pub messages_sent:     AtomicU64,
    pub messages_received: AtomicU64,
    pub valid_latency:     AtomicBool,
    pub last_latency:      AtomicU64,
    pub bytes_received:    AtomicU64,
    pub bytes_sent:        AtomicU64,
}

type PendingPriority = (MessageSendingPriority, Reverse<Instant>);

/// A collection of objects related to the connection to a single peer.
pub struct Connection {
    /// A reference to the parent node.
    handler: Arc<P2PNode>,
    /// The poll token of the connection's socket.
    pub token: Token,
    /// The connection's representation as a peer object.
    pub remote_peer: RemotePeer,
    pub low_level: RwLock<ConnectionLowLevel>,
    /// The list of networks the connection belongs to.
    pub remote_end_networks: Arc<RwLock<HashSet<NetworkId>>>,
    /// Indicates whether the connection's handshake has concluded.
    pub is_post_handshake: AtomicBool,
    pub stats: ConnectionStats,
    /// The queue of messages to be sent to the connection.
    pub pending_messages: RwLock<PriorityQueue<Arc<[u8]>, PendingPriority>>,
}

impl PartialEq for Connection {
    fn eq(&self, other: &Self) -> bool { self.token == other.token }
}

impl Eq for Connection {}

impl fmt::Display for Connection {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let target = if let Some(id) = self.remote_id() {
            format!("peer {}", id)
        } else {
            self.remote_addr().to_string()
        };
        write!(f, "{}", target)
    }
}

impl Connection {
    /// Create a new connection object.
    pub fn new(
        handler: &Arc<P2PNode>,
        socket: TcpStream,
        token: Token,
        remote_peer: RemotePeer,
        is_initiator: bool,
    ) -> Arc<Self> {
        let curr_stamp = get_current_stamp();

        let low_level = RwLock::new(ConnectionLowLevel::new(
            socket,
            is_initiator,
            handler.config.socket_read_size,
        ));

        let stats = ConnectionStats {
            messages_received: Default::default(),
            messages_sent:     Default::default(),
            sent_handshake:    Default::default(),
            valid_latency:     Default::default(),
            last_latency:      Default::default(),
            last_ping_sent:    AtomicU64::new(curr_stamp),
            last_seen:         AtomicU64::new(curr_stamp),
            bytes_received:    Default::default(),
            bytes_sent:        Default::default(),
        };

        let conn = Arc::new(Self {
            handler: Arc::clone(handler),
            token,
            remote_peer,
            low_level,
            remote_end_networks: Default::default(),
            is_post_handshake: Default::default(),
            stats,
            pending_messages: RwLock::new(PriorityQueue::with_capacity(1024)),
        });

        write_or_die!(conn.low_level).conn_ref = Some(Arc::clone(&conn));

        conn
    }

    /// Get the connection's latest latency value.
    pub fn get_last_latency(&self) -> u64 { self.stats.last_latency.load(Ordering::Relaxed) }

    /// Set the connection's latest latency value.
    pub fn set_last_latency(&self, value: u64) {
        self.stats.last_latency.store(value, Ordering::Relaxed);
    }

    /// Set the timestamp of when the handshake request was sent to the
    /// connection.
    pub fn set_sent_handshake(&self) {
        self.stats.sent_handshake.store(get_current_stamp(), Ordering::Relaxed)
    }

    /// Get the timestamp of when the latest ping request was sent to the
    /// connection.
    pub fn get_last_ping_sent(&self) -> u64 { self.stats.last_ping_sent.load(Ordering::Relaxed) }

    /// Set the timestamp of when the latest ping request was sent to the
    /// connection.
    fn set_last_ping_sent(&self) {
        self.stats.last_ping_sent.store(get_current_stamp(), Ordering::Relaxed);
    }

    /// Obtain the node id related to the connection, if available.
    pub fn remote_id(&self) -> Option<P2PNodeId> { *read_or_die!(self.remote_peer.id) }

    /// Obtain the type of the peer associated with the connection.
    pub fn remote_peer_type(&self) -> PeerType { self.remote_peer.peer_type() }

    /// Obtain the peer stats of the connection.
    #[inline]
    pub fn remote_peer_stats(&self) -> Fallible<PeerStats> {
        Ok(PeerStats::new(
            self.remote_id()
                .ok_or_else(|| format_err!("Attempted to get the stats of a pre-handshake peer!"))?
                .as_raw(),
            self.remote_addr(),
            self.remote_peer_external_port(),
            self.remote_peer_type(),
            &self.stats,
        ))
    }

    /// Obtain the remote address of the connection.
    pub fn remote_addr(&self) -> SocketAddr { self.remote_peer.addr() }

    /// Obtain the external port of the connection.
    pub fn remote_peer_external_port(&self) -> u16 {
        self.remote_peer.peer_external_port.load(Ordering::Relaxed)
    }

    /// Check whether the handshake with the connection has concluded.
    #[inline]
    pub fn is_post_handshake(&self) -> bool { self.is_post_handshake.load(Ordering::Relaxed) }

    /// Obtain the timestamp of when the connection was interacted with last.
    pub fn last_seen(&self) -> u64 { self.stats.last_seen.load(Ordering::Relaxed) }

    /// It registers the connection's socket for read and write ops using *edge*
    /// notifications.
    #[inline]
    pub fn register(&self, poll: &Poll) -> Fallible<()> {
        into_err!(poll.register(
            &read_or_die!(self.low_level).socket,
            self.token,
            Ready::readable() | Ready::writable(),
            PollOpt::edge()
        ))
    }

    #[inline]
    fn is_packet_duplicate(
        &self,
        packet: &mut NetworkPacket,
        deduplication_queues: &DeduplicationQueues,
    ) -> Fallible<bool> {
        ensure!(!packet.message.is_empty());
        let packet_type = PacketType::try_from(
            u8::deserial(&mut Cursor::new(&packet.message[..1]))
                .expect("Writing to buffer is safe."),
        );

        let is_duplicate = match packet_type {
            Ok(PacketType::FinalizationMessage) => {
                dedup_with(&packet.message, &mut write_or_die!(deduplication_queues.finalizations))?
            }
            Ok(PacketType::Transaction) => {
                dedup_with(&packet.message, &mut write_or_die!(deduplication_queues.transactions))?
            }
            Ok(PacketType::Block) => {
                dedup_with(&packet.message, &mut write_or_die!(deduplication_queues.blocks))?
            }
            Ok(PacketType::FinalizationRecord) => {
                dedup_with(&packet.message, &mut write_or_die!(deduplication_queues.fin_records))?
            }
            _ => false,
        };

        Ok(is_duplicate)
    }

    #[inline]
    fn process_message(
        &self,
        message: Arc<[u8]>,
        deduplication_queues: &DeduplicationQueues,
    ) -> Fallible<()> {
        self.update_last_seen();
        self.stats.messages_received.fetch_add(1, Ordering::Relaxed);
        self.stats.bytes_received.fetch_add(message.len() as u64, Ordering::Relaxed);
        self.handler.connection_handler.total_received.fetch_add(1, Ordering::Relaxed);
        self.handler.stats.pkt_received_inc();

        if cfg!(feature = "network_dump") {
            self.send_to_dump(message.clone(), true);
        }

        let mut message = NetworkMessage::deserialize(&message)?;

        if let NetworkMessagePayload::NetworkPacket(ref mut packet) = message.payload {
            // disregard packets when in bootstrapper mode
            if self.handler.self_peer.peer_type == PeerType::Bootstrapper {
                return Ok(());
            }
            // deduplicate the incoming packet payload
            if self.is_packet_duplicate(packet, deduplication_queues)? {
                return Ok(());
            }
        }

        let is_msg_processable = match message.payload {
            NetworkMessagePayload::NetworkRequest(NetworkRequest::Handshake(..), ..) => true,
            _ => self.is_post_handshake(),
        };

        let is_msg_forwardable = match message.payload {
            NetworkMessagePayload::NetworkRequest(ref request, ..) => match request {
                NetworkRequest::BanNode(..) | NetworkRequest::UnbanNode(..) => {
                    !self.handler.config.no_trust_bans
                }
                _ => false,
            },
            _ => false,
        };

        // forward applicable messages to other connections
        if is_msg_forwardable {
            if let NetworkMessagePayload::NetworkRequest(..) = message.payload {
                if let Err(e) = self.forward_network_message(&message) {
                    error!("Couldn't forward a network message: {}", e);
                }
            }
        }

        // process the incoming message if applicable
        if is_msg_processable {
            self.handle_incoming_message(message)?;
        } else {
            bail!(
                "Refusing to process or forward the incoming message ({:?}) before a handshake",
                message,
            );
        };

        Ok(())
    }

    /// Concludes the connection's handshake process.
    pub fn promote_to_post_handshake(&self, id: P2PNodeId, peer_port: u16) {
        *write_or_die!(self.remote_peer.id) = Some(id);
        self.remote_peer.peer_external_port.store(peer_port, Ordering::SeqCst);
        self.is_post_handshake.store(true, Ordering::SeqCst);
        self.handler.bump_last_peer_update();
    }

    pub fn local_end_networks(&self) -> &RwLock<Networks> { self.handler.networks() }

    /// It queues a network request
    #[inline]
    pub fn async_send(&self, message: Arc<[u8]>, priority: MessageSendingPriority) {
        write_or_die!(self.pending_messages).push(message, (priority, Reverse(Instant::now())));
    }

    #[inline]
    pub fn update_last_seen(&self) {
        if self.handler.peer_type() != PeerType::Bootstrapper {
            self.stats.last_seen.store(get_current_stamp(), Ordering::Relaxed);
        }
    }

    pub fn add_remote_end_network(&self, network: NetworkId) {
        write_or_die!(self.remote_end_networks).insert(network);
    }

    pub fn add_remote_end_networks(&self, networks: &HashSet<NetworkId>) {
        write_or_die!(self.remote_end_networks).extend(networks.iter())
    }

    pub fn remove_remote_end_network(&self, network: NetworkId) {
        write_or_die!(self.remote_end_networks).remove(&network);
    }

    fn send_to_dump(&self, buf: Arc<[u8]>, inbound: bool) {
        if let Some(ref sender) = &*read_or_die!(self.handler.connection_handler.log_dumper) {
            let di = DumpItem::new(Utc::now(), inbound, self.remote_peer.addr().ip(), buf);
            let _ = sender.send(di);
        }
    }

    pub fn produce_handshake_request(&self) -> Fallible<Vec<u8>> {
        let handshake_request = netmsg!(
            NetworkRequest,
            NetworkRequest::Handshake(Handshake {
                remote_id:   self.handler.self_peer.id(),
                remote_port: self.handler.self_peer.port(),
                networks:    read_or_die!(self.handler.networks()).iter().copied().collect(),
                version:     Version::parse(env!("CARGO_PKG_VERSION"))?,
                proof:       vec![],
            })
        );
        let mut serialized = Vec::with_capacity(128);
        handshake_request.serialize(&mut serialized)?;

        Ok(serialized)
    }

    pub fn send_ping(&self) -> Fallible<()> {
        trace!("Sending a ping to {}", self);

        let ping = netmsg!(NetworkRequest, NetworkRequest::Ping);
        let mut serialized = Vec::with_capacity(64);
        ping.serialize(&mut serialized)?;
        self.async_send(Arc::from(serialized), MessageSendingPriority::High);

        self.set_last_ping_sent();

        Ok(())
    }

    pub fn send_pong(&self) -> Fallible<()> {
        trace!("Sending a pong to {}", self);

        let pong = netmsg!(NetworkResponse, NetworkResponse::Pong);
        let mut serialized = Vec::with_capacity(64);
        pong.serialize(&mut serialized)?;
        self.async_send(Arc::from(serialized), MessageSendingPriority::High);

        Ok(())
    }

    pub fn send_peer_list_resp(&self, nets: &HashSet<NetworkId>) -> Fallible<()> {
        let requestor =
            self.remote_peer.peer().ok_or_else(|| format_err!("handshake not concluded yet"))?;

        let peer_list_resp = match self.handler.peer_type() {
            PeerType::Bootstrapper => {
                const BOOTSTRAP_PEER_COUNT: usize = 100;
                let random_nodes =
                    match self.handler.config.partition_network_for_time {
                        Some(time) => {
                            if (Utc::now().timestamp_millis()
                                - self.handler.start_time.timestamp_millis())
                                as usize
                                >= time
                            {
                                safe_read!(self.handler.connection_handler.buckets)?
                                    .get_random_nodes(&requestor, BOOTSTRAP_PEER_COUNT, nets, false)
                            } else {
                                safe_read!(self.handler.connection_handler.buckets)?
                                    .get_random_nodes(&requestor, BOOTSTRAP_PEER_COUNT, nets, true)
                            }
                        }
                        _ => safe_read!(self.handler.connection_handler.buckets)?.get_random_nodes(
                            &requestor,
                            BOOTSTRAP_PEER_COUNT,
                            nets,
                            false,
                        ),
                    };

                if !random_nodes.is_empty()
                    && random_nodes.len()
                        >= usize::from(self.handler.config.bootstrapper_wait_minimum_peers)
                {
                    Some(netmsg!(NetworkResponse, NetworkResponse::PeerList(random_nodes)))
                } else {
                    None
                }
            }
            PeerType::Node => {
                let nodes = self
                    .handler
                    .get_peer_stats(Some(PeerType::Node))
                    .iter()
                    .filter(|stat| P2PNodeId(stat.id) != requestor.id)
                    .map(|stat| {
                        P2PPeer::from(stat.peer_type, P2PNodeId(stat.id), stat.external_address())
                    })
                    .collect::<Vec<_>>();

                if !nodes.is_empty() {
                    Some(netmsg!(NetworkResponse, NetworkResponse::PeerList(nodes)))
                } else {
                    None
                }
            }
        };

        if let Some(resp) = peer_list_resp {
            debug!("Sending my PeerList to peer {}", requestor.id());

            let mut serialized = Vec::with_capacity(256);
            resp.serialize(&mut serialized)?;
            self.async_send(Arc::from(serialized), MessageSendingPriority::Normal);

            Ok(())
        } else {
            debug!("I don't have any peers to share with peer {}", requestor.id());
            Ok(())
        }
    }

    fn forward_network_message(&self, msg: &NetworkMessage) -> Fallible<()> {
        let mut serialized = Vec::with_capacity(256);
        msg.serialize(&mut serialized)?;

        let conn_filter = |conn: &Connection| match msg.payload {
            NetworkMessagePayload::NetworkRequest(ref request, ..) => match request {
                NetworkRequest::BanNode(peer_to_ban) => {
                    conn != self
                        && match peer_to_ban {
                            BanId::NodeId(id) => {
                                conn.remote_peer.peer().map_or(true, |x| x.id() != *id)
                            }
                            BanId::Ip(addr) => conn.remote_peer.addr().ip() != *addr,
                            _ => unimplemented!("Socket address bans don't propagate"),
                        }
                }
                _ => true,
            },
            _ => unreachable!("Only network requests are ever forwarded"),
        };

        self.handler.send_over_all_connections(&serialized, &conn_filter);
        Ok(())
    }
}

impl Drop for Connection {
    fn drop(&mut self) {
        debug!("Closing the connection to {}", self);

        // Report number of peers to stats export engine
        if self.is_post_handshake() {
            self.handler.stats.peers_dec();
        }
    }
}

// returns a bool indicating if the message is a duplicate
#[inline]
fn dedup_with(message: &[u8], queue: &mut CircularQueue<u64>) -> Fallible<bool> {
    let mut hash = [0u8; 8];
    hash.copy_from_slice(&XxHash64::digest(message));
    let num = u64::from_le_bytes(hash);

    if !queue.iter().any(|n| n == &num) {
        trace!("Message {:x} is unique, adding to dedup queue", num);
        queue.push(num);
        Ok(false)
    } else {
        trace!("Message {:x} is a duplicate", num);
        Ok(true)
    }
}

#[inline]
pub fn send_pending_messages(
    pending_messages: &RwLock<PriorityQueue<Arc<[u8]>, PendingPriority>>,
    low_level: &mut ConnectionLowLevel,
) -> Fallible<()> {
    let mut pending_messages = write_or_die!(pending_messages);

    while let Some((msg, _)) = pending_messages.pop() {
        trace!(
            "Attempting to send {} to {}",
            ByteSize(msg.len() as u64).to_string_as(true),
            low_level.conn()
        );

        if let Err(err) = low_level.write_to_socket(msg) {
            bail!("Can't send a raw network request to {}: {}", low_level.conn(), err);
        }
    }

    Ok(())
}
