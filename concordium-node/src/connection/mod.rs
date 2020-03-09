//! Connection handling.

mod low_level;
pub mod message_handlers;
#[cfg(test)]
mod tests;

use low_level::ConnectionLowLevel;

use bytesize::ByteSize;
use circular_queue::CircularQueue;
use digest::Digest;
use failure::Fallible;
use mio::{tcp::TcpStream, Poll, PollOpt, Ready, Token};
use priority_queue::PriorityQueue;
use twox_hash::XxHash64;

#[cfg(feature = "network_dump")]
use crate::dumper::DumpItem;
use crate::{
    common::{get_current_stamp, p2p_peer::P2PPeer, P2PNodeId, PeerType, RemotePeer},
    configuration::MAX_PEER_NETWORKS,
    connection::low_level::ReadResult,
    netmsg,
    network::{
        NetworkId, NetworkMessage, NetworkPacket, NetworkPayload, NetworkRequest, NetworkResponse,
        Networks,
    },
    p2p::P2PNode,
};

use concordium_common::PacketType;
use crypto_common::Deserial;

use std::{
    cmp::Reverse,
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

/// Designates the sending priority of outgoing messages.
// If a message is labelled as having `High` priority it is always pushed to the
// front of the queue in the sinks when sending, and otherwise to the back.
#[derive(PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub enum MessageSendingPriority {
    /// Queued FIFO-style.
    Normal,
    /// Sent before all `Normal` messages.
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
    pub created:           u64,
    pub last_ping_sent:    AtomicU64,
    pub last_seen:         AtomicU64,
    pub messages_sent:     AtomicU64,
    pub messages_received: AtomicU64,
    pub valid_latency:     AtomicBool,
    pub last_latency:      AtomicU64,
    pub bytes_received:    AtomicU64,
    pub bytes_sent:        AtomicU64,
}

type PendingPriority = (MessageSendingPriority, Reverse<Instant>);

/// Specifies the type of change to be applied to the list of connections.
pub enum ConnChange {
    /// To be soft-banned and removed from the list of connections.
    Expulsion(Token),
    /// Prospect node address to attempt to connect to.
    NewConn(SocketAddr),
    /// Prospect peers to possibly connect to.
    NewPeers(Vec<P2PPeer>),
    /// Promotion to post-handshake.
    Promotion(Token),
    /// To be removed from the list of connections.
    Removal(Token),
}

/// A collection of objects related to the connection to a single peer.
pub struct Connection {
    /// A reference to the parent node.
    handler: Arc<P2PNode>,
    /// The poll token of the connection's socket.
    pub token: Token,
    /// The connection's representation as a peer object.
    pub remote_peer: RemotePeer,
    /// Low-level connection objects.
    pub low_level: ConnectionLowLevel,
    /// The list of networks the connection belongs to.
    pub remote_end_networks: Arc<RwLock<Networks>>,
    pub stats: ConnectionStats,
    /// The queue of messages to be sent to the connection.
    pub pending_messages: PriorityQueue<Arc<[u8]>, PendingPriority>,
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
    ) -> Self {
        let curr_stamp = get_current_stamp();

        let low_level = ConnectionLowLevel::new(
            handler,
            socket,
            is_initiator,
            handler.config.socket_read_size,
            handler.config.socket_write_size,
        );

        let stats = ConnectionStats {
            created:           get_current_stamp(),
            messages_received: Default::default(),
            messages_sent:     Default::default(),
            valid_latency:     Default::default(),
            last_latency:      Default::default(),
            last_ping_sent:    AtomicU64::new(curr_stamp),
            last_seen:         AtomicU64::new(curr_stamp),
            bytes_received:    Default::default(),
            bytes_sent:        Default::default(),
        };

        Self {
            handler: Arc::clone(handler),
            token,
            remote_peer,
            low_level,
            remote_end_networks: Default::default(),
            stats,
            pending_messages: PriorityQueue::with_capacity(1024),
        }
    }

    /// Get the connection's latest latency value.
    pub fn get_last_latency(&self) -> u64 { self.stats.last_latency.load(Ordering::Relaxed) }

    /// Set the connection's latest latency value.
    pub fn set_last_latency(&self, value: u64) {
        self.stats.last_latency.store(value, Ordering::Relaxed);
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
    pub fn remote_id(&self) -> Option<P2PNodeId> { self.remote_peer.id }

    /// Obtain the type of the peer associated with the connection.
    pub fn remote_peer_type(&self) -> PeerType { self.remote_peer.peer_type }

    /// Obtain the remote address of the connection.
    pub fn remote_addr(&self) -> SocketAddr { self.remote_peer.addr }

    /// Obtain the external port of the connection.
    pub fn remote_peer_external_port(&self) -> u16 { self.remote_peer.peer_external_port }

    /// Obtain the timestamp of when the connection was interacted with last.
    pub fn last_seen(&self) -> u64 { self.stats.last_seen.load(Ordering::Relaxed) }

    /// It registers the connection's socket for read and write ops using *edge*
    /// notifications.
    #[inline]
    pub fn register(&self, poll: &Poll) -> Fallible<()> {
        into_err!(poll.register(
            &self.low_level.socket,
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
        let destination = PacketType::try_from(
            u8::deserial(&mut Cursor::new(&packet.message[..1]))
                .expect("Writing to buffer is safe."),
        );

        let is_duplicate = match destination {
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

    /// Keeps reading from the socket as long as there is data to be read
    /// and the operation is not blocking.
    #[inline]
    pub fn read_stream(&mut self, dedup_queues: &DeduplicationQueues) -> Fallible<()> {
        loop {
            match self.low_level.read_from_socket()? {
                ReadResult::Complete(msg) => self.process_message(Arc::from(msg), dedup_queues)?,
                ReadResult::Incomplete | ReadResult::WouldBlock => return Ok(()),
            }
        }
    }

    #[inline]
    fn process_message(
        &mut self,
        bytes: Arc<[u8]>,
        deduplication_queues: &DeduplicationQueues,
    ) -> Fallible<()> {
        self.update_last_seen();
        self.stats.messages_received.fetch_add(1, Ordering::Relaxed);
        self.stats.bytes_received.fetch_add(bytes.len() as u64, Ordering::Relaxed);
        self.handler.connection_handler.total_received.fetch_add(1, Ordering::Relaxed);
        self.handler.stats.pkt_received_inc();

        #[cfg(feature = "network_dump")]
        {
            self.send_to_dump(bytes.clone(), true);
        }

        let mut message = NetworkMessage::deserialize(&bytes)?;

        if let NetworkPayload::NetworkPacket(ref mut packet) = message.payload {
            // disregard packets when in bootstrapper mode
            if self.handler.self_peer.peer_type == PeerType::Bootstrapper {
                return Ok(());
            }
            // deduplicate the incoming packet payload
            if self.is_packet_duplicate(packet, deduplication_queues)? {
                return Ok(());
            }
        }

        // process the incoming message
        self.handle_incoming_message(message, bytes)
    }

    /// Concludes the connection's handshake process.
    pub fn promote_to_post_handshake(&mut self, id: P2PNodeId, peer_port: u16) {
        self.remote_peer.id = Some(id);
        self.remote_peer.peer_external_port = peer_port;
        self.handler.stats.peers_inc();
        self.handler.bump_last_peer_update();
        if self.remote_peer.peer_type == PeerType::Bootstrapper {
            self.handler.update_last_bootstrap();
        }
        self.handler.register_conn_change(ConnChange::Promotion(self.token));
    }

    /// Queues a message to be sent to the connection.
    #[inline]
    pub fn async_send(&mut self, message: Arc<[u8]>, priority: MessageSendingPriority) {
        self.pending_messages.push(message, (priority, Reverse(Instant::now())));
    }

    /// Update the timestamp of when the connection was seen last.
    #[inline]
    pub fn update_last_seen(&self) {
        if self.handler.peer_type() != PeerType::Bootstrapper {
            self.stats.last_seen.store(get_current_stamp(), Ordering::Relaxed);
        }
    }

    /// Register connection's remote end networks.
    pub fn populate_remote_end_networks(&self, peer: P2PPeer, networks: &Networks) {
        write_or_die!(self.remote_end_networks).extend(networks.iter());

        if self.remote_peer.peer_type != PeerType::Bootstrapper {
            write_or_die!(self.handler.buckets()).insert_into_bucket(peer, networks.to_owned());
        }
    }

    /// Add a single network to the connection's remote end networks.
    pub fn add_remote_end_network(&self, network: NetworkId) -> Fallible<()> {
        let curr_networks = &mut write_or_die!(self.remote_end_networks);
        ensure!(curr_networks.len() < MAX_PEER_NETWORKS, "refusing to add any more networks");

        curr_networks.insert(network);

        let peer = self.remote_peer.peer().ok_or_else(|| format_err!("missing handshake"))?;
        write_or_die!(self.handler.buckets()).update_network_ids(peer, curr_networks.to_owned());
        Ok(())
    }

    /// Remove a network from the connection's remote end networks.
    pub fn remove_remote_end_network(&self, network: NetworkId) -> Fallible<()> {
        write_or_die!(self.remote_end_networks).remove(&network);

        let peer = self.remote_peer.peer().ok_or_else(|| format_err!("missing handshake"))?;
        write_or_die!(self.handler.buckets())
            .update_network_ids(peer, read_or_die!(self.remote_end_networks).to_owned());
        Ok(())
    }

    #[cfg(feature = "network_dump")]
    fn send_to_dump(&self, buf: Arc<[u8]>, inbound: bool) {
        if let Some(ref sender) = &*read_or_die!(self.handler.connection_handler.log_dumper) {
            let di = DumpItem::new(inbound, self.remote_peer.addr.ip(), buf);
            let _ = sender.send(di);
        }
    }

    /// Send a ping to the connection.
    pub fn send_ping(&mut self) -> Fallible<()> {
        trace!("Sending a ping to {}", self);

        let ping = netmsg!(NetworkRequest, NetworkRequest::Ping);
        let mut serialized = Vec::with_capacity(56);
        ping.serialize(&mut serialized)?;
        self.async_send(Arc::from(serialized), MessageSendingPriority::High);

        self.set_last_ping_sent();

        Ok(())
    }

    /// Send a pong to the connection.
    pub fn send_pong(&mut self) -> Fallible<()> {
        trace!("Sending a pong to {}", self);

        let pong = netmsg!(NetworkResponse, NetworkResponse::Pong);
        let mut serialized = Vec::with_capacity(56);
        pong.serialize(&mut serialized)?;
        self.async_send(Arc::from(serialized), MessageSendingPriority::High);

        Ok(())
    }

    /// Send a response to a request for peers to the connection.
    pub fn send_peer_list_resp(&mut self, nets: Networks) -> Fallible<()> {
        let requestor =
            self.remote_peer.peer().ok_or_else(|| format_err!("handshake not concluded yet"))?;

        let peer_list_resp = match self.handler.peer_type() {
            PeerType::Bootstrapper => {
                let get_100_random_nodes = |partition: bool| -> Fallible<Vec<P2PPeer>> {
                    Ok(safe_read!(self.handler.buckets())?
                        .get_random_nodes(&requestor, 100, &nets, partition))
                };
                let random_nodes = match self.handler.config.partition_network_for_time {
                    Some(time) if (self.handler.get_uptime() as usize) < time => {
                        get_100_random_nodes(true)?
                    }
                    _ => get_100_random_nodes(false)?,
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
                        P2PPeer::from((stat.peer_type, P2PNodeId(stat.id), stat.external_address()))
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
            debug!("Sending a PeerList to peer {}", requestor.id);

            let mut serialized = Vec::with_capacity(256);
            resp.serialize(&mut serialized)?;
            self.async_send(Arc::from(serialized), MessageSendingPriority::Normal);

            Ok(())
        } else {
            debug!("I don't have any peers to share with peer {}", requestor.id);
            Ok(())
        }
    }
}

impl Drop for Connection {
    fn drop(&mut self) {
        debug!("Closing the connection to {}", self);

        // update peer stats if it was post-handshake
        if self.remote_id().is_some() {
            self.handler.stats.peers_dec();
        }
    }
}

/// Returns a bool indicating whether the message is a duplicate.
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

/// Processes a queue with pending messages, writing them to the socket.
#[inline]
pub fn send_pending_messages(conn: &mut Connection) -> Fallible<()> {
    while let Some((msg, _)) = conn.pending_messages.pop() {
        trace!("Attempting to send {} to {}", ByteSize(msg.len() as u64).to_string_as(true), conn);

        if let Err(err) = conn.low_level.write_to_socket(msg.clone()) {
            bail!("Can't send a raw network request: {}", err);
        } else {
            conn.handler.connection_handler.total_sent.fetch_add(1, Ordering::Relaxed);
            conn.handler.stats.pkt_sent_inc();
            conn.stats.messages_sent.fetch_add(1, Ordering::Relaxed);
            conn.stats.bytes_sent.fetch_add(msg.len() as u64, Ordering::Relaxed);

            #[cfg(feature = "network_dump")]
            {
                conn.send_to_dump(msg, false);
            }
        }
    }

    Ok(())
}
