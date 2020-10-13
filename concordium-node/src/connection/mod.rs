//! Connection handling.

mod low_level;
pub mod message_handlers;
#[cfg(test)]
mod tests;

use low_level::ConnectionLowLevel;

use bytesize::ByteSize;
use circular_queue::CircularQueue;
use failure::Fallible;
use mio::{net::TcpStream, Token};

#[cfg(feature = "network_dump")]
use crate::dumper::DumpItem;
use crate::{
    common::{
        get_current_stamp,
        p2p_peer::{P2PPeer, PeerStats},
        P2PNodeId, PeerType, RemotePeer,
    },
    configuration::MAX_PEER_NETWORKS,
    connection::low_level::ReadResult,
    netmsg,
    network::{
        NetworkId, NetworkMessage, NetworkPacket, NetworkPayload, NetworkRequest, NetworkResponse,
        Networks,
    },
    only_fbs,
    p2p::P2PNode,
};

use concordium_common::PacketType;
use crypto_common::Deserial;

use std::{
    collections::VecDeque,
    convert::TryFrom,
    fmt,
    io::Cursor,
    net::SocketAddr,
    ops::{Index, IndexMut},
    str::FromStr,
    sync::{
        atomic::{AtomicU64, Ordering},
        Arc, RwLock,
    },
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

/// This enum defines the hashing algorithms we support for deduplication
#[derive(Debug, Clone, Copy)]
pub enum DeduplicationHashAlgorithm {
    /// XxHash64
    XxHash64,
    // SHA256
    Sha256,
}

impl FromStr for DeduplicationHashAlgorithm {
    type Err = failure::Error;

    fn from_str(algorithm: &str) -> Result<Self, Self::Err> {
        match algorithm {
            "xxhash64" => Ok(DeduplicationHashAlgorithm::XxHash64),
            "sha256" => Ok(DeduplicationHashAlgorithm::Sha256),
            _ => bail!("Could not parse deduplication hashing algorithm"),
        }
    }
}

/// Trait used by a deduplication queue implementation
pub trait DeduplicationQueue: Send + Sync {
    /// Check if element exists, and if not insert it - return status is whether
    /// or not message was a duplicate
    fn check_and_insert(&mut self, input: &[u8]) -> Fallible<bool>;
    /// Invalidate the entry in the queue if a key is found
    fn invalidate_if_exists(&mut self, input: &[u8]);
}

/// XxHash64 deduplication struct
pub struct DeduplicationQueueXxHash64 {
    /// Random seed generated per queue when constructed
    seed: u64,
    /// The queue itself
    queue: CircularQueue<u64>,
}

impl DeduplicationQueueXxHash64 {
    /// Constructs a new XxHash64 deduplication queue with a random seed
    pub fn new(capacity: usize) -> Self {
        use rand::Rng;
        Self {
            seed:  rand::thread_rng().gen::<u64>(),
            queue: CircularQueue::with_capacity(capacity),
        }
    }

    /// Hash an input given as a byte slice
    fn hash(&self, input: &[u8]) -> u64 {
        use std::hash::Hasher;
        use twox_hash::XxHash64;
        let mut hasher = XxHash64::with_seed(self.seed);
        hasher.write(&input);
        hasher.finish()
    }
}

impl DeduplicationQueue for DeduplicationQueueXxHash64 {
    fn check_and_insert(&mut self, input: &[u8]) -> Fallible<bool> {
        let num = self.hash(&input);
        if !self.queue.iter().any(|n| n == &num) {
            trace!("Message XxHash64 {:x} is unique, adding to dedup queue", num);
            self.queue.push(num);
            Ok(false)
        } else {
            trace!("Message XxHash64 {:x} is a duplicate", num);
            Ok(true)
        }
    }

    fn invalidate_if_exists(&mut self, input: &[u8]) {
        let num = self.hash(&input);
        if let Some(old_val) = self.queue.iter_mut().find(|val| **val == num) {
            // flip all bits in the old hash; we can't just remove the value from the queue
            *old_val = !*old_val;
        }
    }
}

/// SHA256 deduplication struct
pub struct DeduplicationQueueSha256 {
    /// The queue itself
    queue: CircularQueue<[u8; 32]>,
}

impl DeduplicationQueueSha256 {
    /// Constructs a new SHA256 deduplication queue
    pub fn new(capacity: usize) -> Self {
        Self {
            queue: CircularQueue::with_capacity(capacity),
        }
    }

    /// Hash an input given as a byte slice
    fn hash(&self, input: &[u8]) -> [u8; 32] {
        use sha2::{Digest, Sha256};
        Sha256::digest(input).into()
    }
}

impl DeduplicationQueue for DeduplicationQueueSha256 {
    fn check_and_insert(&mut self, input: &[u8]) -> Fallible<bool> {
        let hash = self.hash(&input);
        if !self.queue.iter().any(|n| *n == hash) {
            trace!("Message SHA256 {:X?} is unique, adding to dedup queue", &hash[..]);
            self.queue.push(hash);
            Ok(false)
        } else {
            trace!("Message SHA256 {:X?} is a duplicate", &hash[..]);
            Ok(true)
        }
    }

    fn invalidate_if_exists(&mut self, input: &[u8]) {
        let hash = self.hash(&input);
        if let Some(old_val) = self.queue.iter_mut().find(|val| **val == hash) {
            // zero out all bits in the old hash; we can't just remove the value from the
            // queue
            *old_val = Default::default();
        }
    }
}

/// Contains the circular queues of hashes of different consensus objects
/// for deduplication purposes.
pub struct DeduplicationQueues {
    pub finalizations: RwLock<Box<dyn DeduplicationQueue>>,
    pub transactions:  RwLock<Box<dyn DeduplicationQueue>>,
    pub blocks:        RwLock<Box<dyn DeduplicationQueue>>,
    pub fin_records:   RwLock<Box<dyn DeduplicationQueue>>,
}

impl DeduplicationQueues {
    /// Creates the deduplication queues of specified sizes: short for blocks
    /// and finalization records and long for finalization messages and
    /// transactions.
    pub fn new(algorithm: DeduplicationHashAlgorithm, long_size: usize, short_size: usize) -> Self {
        match algorithm {
            DeduplicationHashAlgorithm::XxHash64 => Self {
                finalizations: RwLock::new(Box::new(DeduplicationQueueXxHash64::new(long_size))),
                transactions:  RwLock::new(Box::new(DeduplicationQueueXxHash64::new(long_size))),
                blocks:        RwLock::new(Box::new(DeduplicationQueueXxHash64::new(short_size))),
                fin_records:   RwLock::new(Box::new(DeduplicationQueueXxHash64::new(short_size))),
            },
            DeduplicationHashAlgorithm::Sha256 => Self {
                finalizations: RwLock::new(Box::new(DeduplicationQueueSha256::new(long_size))),
                transactions:  RwLock::new(Box::new(DeduplicationQueueSha256::new(long_size))),
                blocks:        RwLock::new(Box::new(DeduplicationQueueSha256::new(short_size))),
                fin_records:   RwLock::new(Box::new(DeduplicationQueueSha256::new(short_size))),
            },
        }
    }
}

/// Contains all the statistics of a connection.
pub struct ConnectionStats {
    pub created:           u64,
    pub last_seen:         AtomicU64,
    pub last_ping:         AtomicU64,
    pub last_pong:         AtomicU64,
    pub messages_sent:     AtomicU64,
    pub messages_received: AtomicU64,
    pub bytes_received:    AtomicU64,
    pub bytes_sent:        AtomicU64,
}

/// Specifies the type of change to be applied to the list of connections.
pub enum ConnChange {
    /// To be soft-banned and removed from the list of connections.
    Expulsion(Token),
    /// Prospect node address to attempt to connect to.
    NewConn(SocketAddr, PeerType),
    /// Prospect peers to possibly connect to.
    NewPeers(Vec<P2PPeer>),
    /// Promotion to post-handshake.
    Promotion(Token),
    /// To be removed from the list of connections.
    Removal(Token),
}

/// Message queues, indexed by priority.
pub struct MessageQueues {
    pub low:  VecDeque<Arc<[u8]>>,
    pub high: VecDeque<Arc<[u8]>>,
}

impl Index<MessageSendingPriority> for MessageQueues {
    type Output = VecDeque<Arc<[u8]>>;

    fn index(&self, priority: MessageSendingPriority) -> &Self::Output {
        match priority {
            MessageSendingPriority::Normal => &self.low,
            MessageSendingPriority::High => &self.high,
        }
    }
}

impl IndexMut<MessageSendingPriority> for MessageQueues {
    fn index_mut(&mut self, priority: MessageSendingPriority) -> &mut Self::Output {
        match priority {
            MessageSendingPriority::Normal => &mut self.low,
            MessageSendingPriority::High => &mut self.high,
        }
    }
}

impl MessageQueues {
    /// Create queues with the specified initial capacities.
    pub fn new(low_capacity: usize, high_capacity: usize) -> Self {
        Self {
            low:  VecDeque::with_capacity(low_capacity),
            high: VecDeque::with_capacity(high_capacity),
        }
    }

    /// Add a message to the queue with the appropriate priority.
    pub fn enqueue(&mut self, priority: MessageSendingPriority, message: Arc<[u8]>) {
        self[priority].push_back(message);
    }

    /// Dequeue a message, taking from the high priority queue first.
    pub fn dequeue(&mut self) -> Option<Arc<[u8]>> {
        match self.high.pop_front() {
            Some(message) => Some(message),
            None => self.low.pop_front(),
        }
    }
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
    pub remote_end_networks: Networks,
    pub stats: ConnectionStats,
    /// The queue of messages to be sent to the connection.
    pub pending_messages: MessageQueues,
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
            last_ping:         Default::default(),
            last_pong:         Default::default(),
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
            pending_messages: MessageQueues::new(1024, 128),
        }
    }

    /// Obtain the connection's latency.
    pub fn get_latency(&self) -> u64 {
        let last_ping = self.stats.last_ping.load(Ordering::SeqCst);
        let last_pong = self.stats.last_pong.load(Ordering::SeqCst);

        if last_ping > 0 && last_pong > 0 && last_pong > last_ping {
            last_pong - last_ping
        } else {
            0
        }
    }

    /// Obtain the node id related to the connection, if available.
    pub fn remote_id(&self) -> Option<P2PNodeId> { self.remote_peer.id }

    /// Obtain the type of the peer associated with the connection.
    pub fn remote_peer_type(&self) -> PeerType { self.remote_peer.peer_type }

    /// Obtain the remote address of the connection.
    pub fn remote_addr(&self) -> SocketAddr { self.remote_peer.addr }

    /// Obtain the external port of the connection.
    pub fn remote_peer_external_port(&self) -> u16 { self.remote_peer.external_port }

    /// Obtain the timestamp of when the connection was interacted with last.
    pub fn last_seen(&self) -> u64 { self.stats.last_seen.load(Ordering::Relaxed) }

    #[inline]
    fn is_packet_duplicate(&self, packet: &mut NetworkPacket) -> Fallible<bool> {
        use super::network::PacketDestination;
        ensure!(!packet.message.is_empty());
        let packet_type = PacketType::try_from(
            u8::deserial(&mut Cursor::new(&packet.message[..1]))
                .expect("Writing to buffer is safe."),
        );

        if let PacketDestination::Direct(_) = packet.destination {
            return Ok(false);
        }

        let deduplication_queues = &self.handler.connection_handler.deduplication_queues;

        let is_duplicate = match packet_type {
            Ok(PacketType::FinalizationMessage) => dedup_with(
                &packet.message,
                &mut **write_or_die!(deduplication_queues.finalizations),
            )?,
            Ok(PacketType::Transaction) => dedup_with(
                &packet.message,
                &mut **write_or_die!(deduplication_queues.transactions),
            )?,
            Ok(PacketType::Block) => {
                dedup_with(&packet.message, &mut **write_or_die!(deduplication_queues.blocks))?
            }
            Ok(PacketType::FinalizationRecord) => {
                dedup_with(&packet.message, &mut **write_or_die!(deduplication_queues.fin_records))?
            }
            _ => false,
        };

        Ok(is_duplicate)
    }

    /// Keeps reading from the socket as long as there is data to be read
    /// and the operation is not blocking.
    /// The return value indicates if the connection is still open.
    #[inline]
    pub fn read_stream(&mut self, conn_stats: &[PeerStats]) -> Fallible<bool> {
        loop {
            match self.low_level.read_from_socket()? {
                ReadResult::Complete(msg) => self.process_message(Arc::from(msg), conn_stats)?,
                ReadResult::Incomplete => {}
                ReadResult::WouldBlock => return Ok(true),
                ReadResult::Closed => return Ok(false),
            }
        }
    }

    #[inline]
    fn process_message(&mut self, bytes: Arc<[u8]>, conn_stats: &[PeerStats]) -> Fallible<()> {
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
            if self.is_packet_duplicate(packet)? {
                return Ok(());
            }
        }

        // process the incoming message
        self.handle_incoming_message(message, bytes, conn_stats)
    }

    /// Concludes the connection's handshake process.
    pub fn promote_to_post_handshake(&mut self, id: P2PNodeId, peer_port: u16, nets: &Networks) {
        self.remote_peer.id = Some(id);
        self.remote_peer.external_port = peer_port;
        self.handler.stats.peers_inc();
        if self.remote_peer.peer_type == PeerType::Bootstrapper {
            self.handler.update_last_bootstrap();
        }
        let remote_peer = P2PPeer::from((
            self.remote_peer.peer_type,
            id,
            SocketAddr::new(self.remote_peer.addr.ip(), peer_port),
        ));
        self.populate_remote_end_networks(remote_peer, nets);
        self.handler.register_conn_change(ConnChange::Promotion(self.token));
        debug!("Concluded handshake with peer {}", id);
    }

    /// Queues a message to be sent to the connection.
    #[inline]
    pub fn async_send(&mut self, message: Arc<[u8]>, priority: MessageSendingPriority) {
        self.pending_messages.enqueue(priority, message);
    }

    /// Update the timestamp of when the connection was seen last.
    #[inline]
    pub fn update_last_seen(&self) {
        if self.handler.peer_type() != PeerType::Bootstrapper {
            self.stats.last_seen.store(get_current_stamp(), Ordering::Relaxed);
        }
    }

    /// Register connection's remote end networks.
    pub fn populate_remote_end_networks(&mut self, peer: P2PPeer, networks: &Networks) {
        self.remote_end_networks.extend(networks.iter());

        if self.remote_peer.peer_type != PeerType::Bootstrapper {
            write_or_die!(self.handler.buckets()).insert_into_bucket(peer, networks.to_owned());
        }
    }

    /// Add a single network to the connection's remote end networks.
    pub fn add_remote_end_network(&mut self, network: NetworkId) -> Fallible<()> {
        ensure!(
            self.remote_end_networks.len() < MAX_PEER_NETWORKS,
            "refusing to add any more networks"
        );

        self.remote_end_networks.insert(network);
        let peer = self.remote_peer.peer().ok_or_else(|| format_err!("missing handshake"))?;
        write_or_die!(self.handler.buckets())
            .update_network_ids(peer, self.remote_end_networks.to_owned());
        Ok(())
    }

    /// Remove a network from the connection's remote end networks.
    pub fn remove_remote_end_network(&mut self, network: NetworkId) -> Fallible<()> {
        self.remote_end_networks.remove(&network);

        let peer = self.remote_peer.peer().ok_or_else(|| format_err!("missing handshake"))?;
        write_or_die!(self.handler.buckets())
            .update_network_ids(peer, self.remote_end_networks.to_owned());
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

        only_fbs!(ping.serialize(&mut serialized)?);
        self.stats.last_ping.store(get_current_stamp(), Ordering::SeqCst);

        self.async_send(Arc::from(serialized), MessageSendingPriority::High);

        Ok(())
    }

    /// Send a pong to the connection.
    pub fn send_pong(&mut self) -> Fallible<()> {
        trace!("Sending a pong to {}", self);

        let pong = netmsg!(NetworkResponse, NetworkResponse::Pong);
        let mut serialized = Vec::with_capacity(56);
        only_fbs!(pong.serialize(&mut serialized)?);
        self.async_send(Arc::from(serialized), MessageSendingPriority::High);

        Ok(())
    }

    /// Send a response to a request for peers to the connection.
    pub fn send_peer_list_resp(
        &mut self,
        nets: Networks,
        conn_stats: &[PeerStats],
    ) -> Fallible<()> {
        let requestor =
            self.remote_peer.peer().ok_or_else(|| format_err!("handshake not concluded yet"))?;

        let peer_list_resp = match self.handler.peer_type() {
            PeerType::Bootstrapper => {
                let get_random_nodes = |partition: bool| -> Fallible<Vec<P2PPeer>> {
                    Ok(read_or_die!(self.handler.buckets()).get_random_nodes(
                        &requestor,
                        self.handler.config.bootstrapper_peer_list_size,
                        &nets,
                        partition,
                    ))
                };
                let random_nodes = match self.handler.config.partition_network_for_time {
                    Some(time) if (self.handler.get_uptime() as usize) < time => {
                        get_random_nodes(true)?
                    }
                    _ => get_random_nodes(false)?,
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
                let nodes = conn_stats
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
            only_fbs!(resp.serialize(&mut serialized)?);
            self.async_send(Arc::from(serialized), MessageSendingPriority::Normal);

            Ok(())
        } else {
            debug!("I don't have any peers to share with peer {}", requestor.id);
            Ok(())
        }
    }

    /// Processes a queue with pending messages, writing them to the socket.
    #[inline]
    pub fn send_pending_messages(&mut self) -> Fallible<()> {
        while let Some(msg) = self.pending_messages.dequeue() {
            trace!(
                "Attempting to send {} to {}",
                ByteSize(msg.len() as u64).to_string_as(true),
                self
            );

            self.low_level.write_to_socket(msg.clone())?;

            self.handler.connection_handler.total_sent.fetch_add(1, Ordering::Relaxed);
            self.handler.stats.pkt_sent_inc();
            self.stats.messages_sent.fetch_add(1, Ordering::Relaxed);
            self.stats.bytes_sent.fetch_add(msg.len() as u64, Ordering::Relaxed);

            #[cfg(feature = "network_dump")]
            {
                self.send_to_dump(msg, false);
            }
        }

        Ok(())
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
fn dedup_with(message: &[u8], queue: &mut dyn DeduplicationQueue) -> Fallible<bool> {
    queue.check_and_insert(message)
}
