#[macro_use]
pub mod fails;
pub mod message_handlers;

mod async_adapter;
pub use async_adapter::{FrameSink, FrameStream, HandshakeStreamSink, Readiness};

mod p2p_event;

// If a message is labelled as having `High` priority it is always pushed to the
// front of the queue in the sinks when sending, and otherwise to the back
#[derive(PartialEq, Eq, Clone, Copy)]
pub enum MessageSendingPriority {
    High,
    Normal,
}

pub use crate::p2p::{Networks, P2PNode};

pub use p2p_event::P2PEvent;

use crate::{
    common::{
        counter::{TOTAL_MESSAGES_RECEIVED_COUNTER, TOTAL_MESSAGES_SENT_COUNTER},
        get_current_stamp,
        p2p_peer::P2PPeer,
        NetworkRawRequest, P2PNodeId, PeerStats, PeerType, RemotePeer,
    },
    dumper::DumpItem,
    network::{
        Buckets, NetworkId, NetworkMessage, NetworkPacketType, NetworkRequest, NetworkResponse,
        ProtocolMessageType, NETWORK_MESSAGE_PROTOCOL_TYPE_IDX,
    },
    p2p::banned_nodes::BannedNode,
};

use concordium_common::{
    hybrid_buf::HybridBuf,
    serial::{serialize_into_buffer, E},
    PacketType, Serial,
};

use byteorder::ReadBytesExt;
use chrono::prelude::Utc;
use circular_queue::CircularQueue;
use digest::Digest;
use failure::Fallible;
use mio::{tcp::TcpStream, Poll, PollOpt, Ready, Token};
use snow::Keypair;
use twox_hash::XxHash64;

use std::{
    collections::HashSet,
    convert::TryFrom,
    fmt,
    io::{Seek, SeekFrom},
    net::SocketAddr,
    pin::Pin,
    sync::{
        atomic::{AtomicBool, AtomicU32, AtomicU64, Ordering},
        Arc, RwLock,
    },
};

pub struct DeduplicationQueues {
    pub finalizations: CircularQueue<[u8; 8]>,
    pub transactions:  CircularQueue<[u8; 8]>,
    pub blocks:        CircularQueue<[u8; 8]>,
    pub fin_records:   CircularQueue<[u8; 8]>,
}

impl DeduplicationQueues {
    pub fn default() -> Self {
        const SHORT_DEDUP_SIZE: usize = 16;
        const LONG_QUEUE_SIZE: usize = 16 * 1024;

        Self {
            finalizations: CircularQueue::with_capacity(LONG_QUEUE_SIZE),
            transactions:  CircularQueue::with_capacity(LONG_QUEUE_SIZE),
            blocks:        CircularQueue::with_capacity(SHORT_DEDUP_SIZE),
            fin_records:   CircularQueue::with_capacity(SHORT_DEDUP_SIZE),
        }
    }
}

pub struct ConnectionStats {
    pub last_ping_sent:    AtomicU64,
    pub sent_handshake:    AtomicU64,
    pub last_seen:         AtomicU64,
    pub failed_pkts:       AtomicU32,
    pub messages_sent:     Arc<AtomicU64>,
    pub messages_received: Arc<AtomicU64>,
    pub valid_latency:     Arc<AtomicBool>,
    pub last_latency:      Arc<AtomicU64>,
}

pub struct Connection {
    handler_ref:             Pin<Arc<P2PNode>>,
    pub token:               Token,
    pub remote_peer:         RemotePeer,
    pub low_level:           RwLock<ConnectionLowLevel>,
    pub remote_end_networks: Arc<RwLock<HashSet<NetworkId>>>,
    pub is_post_handshake:   AtomicBool,
    pub stats:               ConnectionStats,
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
        write!(f, "connection to {}", target)
    }
}

impl Connection {
    pub fn handler(&self) -> &P2PNode { &self.handler_ref }

    #[allow(clippy::too_many_arguments)]
    pub fn new(
        handler: &P2PNode,
        socket: TcpStream,
        token: Token,
        remote_peer: RemotePeer,
        local_peer_type: PeerType,
        key_pair: Keypair,
        is_initiator: bool,
        noise_params: snow::params::NoiseParams,
    ) -> Arc<Self> {
        let curr_stamp = get_current_stamp();

        let low_level = RwLock::new(ConnectionLowLevel::new(
            local_peer_type,
            socket,
            key_pair,
            is_initiator,
            noise_params,
        ));

        let stats = ConnectionStats {
            messages_received: Default::default(),
            messages_sent:     Default::default(),
            sent_handshake:    Default::default(),
            valid_latency:     Default::default(),
            last_latency:      Default::default(),
            failed_pkts:       Default::default(),
            last_ping_sent:    AtomicU64::new(curr_stamp),
            last_seen:         AtomicU64::new(curr_stamp),
        };

        let conn = Arc::new(Self {
            handler_ref: handler.self_ref.clone().unwrap(), // safe, always available
            token,
            remote_peer,
            low_level,
            remote_end_networks: Default::default(),
            is_post_handshake: Default::default(),
            stats,
        });

        write_or_die!(conn.low_level).conn_ref = Some(Pin::new(Arc::clone(&conn)));

        conn
    }

    pub fn get_last_latency(&self) -> u64 { self.stats.last_latency.load(Ordering::SeqCst) }

    pub fn set_last_latency(&self, value: u64) {
        self.stats.last_latency.store(value, Ordering::SeqCst);
    }

    pub fn set_sent_handshake(&self) {
        self.stats
            .sent_handshake
            .store(get_current_stamp(), Ordering::SeqCst)
    }

    pub fn get_last_ping_sent(&self) -> u64 { self.stats.last_ping_sent.load(Ordering::SeqCst) }

    pub fn set_last_ping_sent(&self) {
        self.stats
            .last_ping_sent
            .store(get_current_stamp(), Ordering::SeqCst);
    }

    pub fn remote_peer(&self) -> RemotePeer { self.remote_peer.clone() }

    pub fn remote_id(&self) -> Option<P2PNodeId> { *read_or_die!(self.remote_peer.id) }

    pub fn remote_peer_type(&self) -> PeerType { self.remote_peer.peer_type() }

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

    pub fn remote_addr(&self) -> SocketAddr { self.remote_peer.addr() }

    pub fn remote_peer_external_port(&self) -> u16 {
        self.remote_peer.peer_external_port.load(Ordering::SeqCst)
    }

    pub fn is_post_handshake(&self) -> bool { self.is_post_handshake.load(Ordering::SeqCst) }

    pub fn last_seen(&self) -> u64 { self.stats.last_seen.load(Ordering::SeqCst) }

    pub fn get_messages_received(&self) -> u64 {
        self.stats.messages_received.load(Ordering::SeqCst)
    }

    pub fn get_messages_sent(&self) -> u64 { self.stats.messages_sent.load(Ordering::SeqCst) }

    pub fn failed_pkts(&self) -> u32 { self.stats.failed_pkts.load(Ordering::SeqCst) }

    /// It registers the connection socket, for read and write ops using *edge*
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

    fn is_message_duplicate(
        &self,
        message: &mut HybridBuf,
        deduplication_queues: &mut DeduplicationQueues,
    ) -> Fallible<bool> {
        let packet_type = PacketType::try_from(message.read_u16::<E>()?);

        let is_duplicate = match packet_type {
            Ok(PacketType::FinalizationMessage) => {
                dedup_with(message, &mut deduplication_queues.finalizations)?
            }
            Ok(PacketType::Transaction) => {
                dedup_with(message, &mut deduplication_queues.transactions)?
            }
            Ok(PacketType::Block) => dedup_with(message, &mut deduplication_queues.blocks)?,
            Ok(PacketType::FinalizationRecord) => {
                dedup_with(message, &mut deduplication_queues.fin_records)?
            }
            _ => false,
        };

        Ok(is_duplicate)
    }

    fn process_message(
        &self,
        mut message: HybridBuf,
        deduplication_queues: &mut DeduplicationQueues,
    ) -> Fallible<()> {
        self.update_last_seen();
        self.stats.messages_received.fetch_add(1, Ordering::Relaxed);
        TOTAL_MESSAGES_RECEIVED_COUNTER.fetch_add(1, Ordering::Relaxed);
        if let Some(ref service) = self.handler().stats_export_service {
            service.pkt_received_inc();
        };

        // avoid allocations by not deserializing into a NetworkMessage before dedup
        message.seek(SeekFrom::Start(NETWORK_MESSAGE_PROTOCOL_TYPE_IDX as u64))?;
        let protocol_type = ProtocolMessageType::try_from(message.read_u8()?)?;
        if protocol_type == ProtocolMessageType::Packet {
            let _ = NetworkPacketType::deserial(&mut message)?;
            let _ = NetworkId::deserial(&mut message)?;

            // deduplicate the incoming packet payload
            if self.is_message_duplicate(&mut message, deduplication_queues)? {
                return Ok(());
            }
        }
        message.rewind()?;

        let message = NetworkMessage::deserial(&mut message)?;

        let is_msg_processable = match message {
            NetworkMessage::NetworkRequest(NetworkRequest::Handshake(..), ..)
            | NetworkMessage::NetworkResponse(NetworkResponse::Handshake(..), ..) => true,
            _ => self.is_post_handshake(),
        };

        // process the incoming message if applicable
        if is_msg_processable {
            self.handle_incoming_message(&message);
        } else {
            bail!("Refusing to process or forward any incoming messages before a handshake");
        };

        let is_msg_forwardable = match message {
            NetworkMessage::NetworkRequest(ref request, ..) => match request {
                NetworkRequest::BanNode(..) | NetworkRequest::UnbanNode(..) => {
                    !self.handler().config.no_trust_bans
                }
                _ => false,
            },
            NetworkMessage::NetworkResponse(..) => false,
            NetworkMessage::NetworkPacket(..) => {
                self.handler().is_rpc_online.load(Ordering::Relaxed)
            }
            NetworkMessage::InvalidMessage => false,
        };

        // forward applicable messages to other connections
        if is_msg_forwardable {
            if let NetworkMessage::NetworkPacket(..) = message {
                self.handler().forward_network_packet(message)
            } else {
                self.forward_network_message(&message)
            }
        } else {
            Ok(())
        }
    }

    pub fn buckets(&self) -> &RwLock<Buckets> { &self.handler().connection_handler.buckets }

    pub fn promote_to_post_handshake(&self, id: P2PNodeId, peer_port: u16) -> Fallible<()> {
        self.is_post_handshake.store(true, Ordering::SeqCst);
        *write_or_die!(self.remote_peer.id) = Some(id);
        self.remote_peer
            .peer_external_port
            .store(peer_port, Ordering::SeqCst);

        self.handler().bump_last_peer_update();

        Ok(())
    }

    pub fn remote_end_networks(&self) -> &RwLock<HashSet<NetworkId>> { &self.remote_end_networks }

    pub fn local_end_networks(&self) -> &RwLock<Networks> { self.handler().networks() }

    /// It queues a network request
    #[inline(always)]
    pub fn async_send(&self, input: HybridBuf, priority: MessageSendingPriority) -> Fallible<()> {
        let request = NetworkRawRequest {
            token: self.token,
            data:  input,
        };

        if priority == MessageSendingPriority::High {
            into_err!(self
                .handler()
                .connection_handler
                .network_messages_hi
                .send(request))
        } else {
            into_err!(self
                .handler()
                .connection_handler
                .network_messages_lo
                .send(request))
        }
    }

    /// It sends `input` through `socket`.
    /// This functions returns (almost) immediately, because it does NOT wait
    /// for real write. Function `ConnectionPrivate::ready` will make ensure to
    /// write chunks of the message
    #[inline(always)]
    pub fn async_send_from_poll_loop(&self, input: HybridBuf) -> Fallible<Readiness<usize>> {
        TOTAL_MESSAGES_SENT_COUNTER.fetch_add(1, Ordering::Relaxed);
        self.stats.messages_sent.fetch_add(1, Ordering::Relaxed);
        if let Some(ref stats) = self.handler().stats_export_service {
            stats.pkt_sent_inc();
        }

        self.send_to_dump(&input, false);

        write_or_die!(self.low_level).write_to_sink(input)
    }

    pub fn update_last_seen(&self) {
        if self.handler().peer_type() != PeerType::Bootstrapper {
            self.stats
                .last_seen
                .store(get_current_stamp(), Ordering::SeqCst);
        }
    }

    #[inline]
    pub fn add_remote_end_network(&self, network: NetworkId) {
        write_or_die!(self.remote_end_networks).insert(network);
    }

    #[inline]
    pub fn add_remote_end_networks(&self, networks: &HashSet<NetworkId>) {
        write_or_die!(self.remote_end_networks).extend(networks.iter())
    }

    pub fn remove_remote_end_network(&self, network: NetworkId) {
        write_or_die!(self.remote_end_networks).remove(&network);
    }

    fn send_to_dump(&self, buf: &HybridBuf, inbound: bool) {
        if let Some(ref sender) = self.handler().connection_handler.log_dumper {
            let di = DumpItem::new(
                Utc::now(),
                inbound,
                self.remote_peer().addr().ip(),
                buf.clone(),
            );
            let _ = sender.send(di);
        }
    }

    pub fn send_handshake_request(&self) -> Fallible<()> {
        debug!("Sending a handshake request to {}", self.remote_addr());

        let handshake_request = NetworkMessage::NetworkRequest(
            NetworkRequest::Handshake(
                self.handler().self_peer.id(),
                self.handler().self_peer.port(),
                read_or_die!(self.handler().networks())
                    .iter()
                    .copied()
                    .collect(),
                vec![],
            ),
            Some(get_current_stamp()),
            None,
        );

        self.async_send(
            serialize_into_buffer(&handshake_request, 256)?,
            MessageSendingPriority::High,
        )?;

        self.set_sent_handshake();

        Ok(())
    }

    pub fn send_handshake_response(&self, remote_node_id: P2PNodeId) -> Fallible<()> {
        debug!("Sending a handshake response to peer {}", remote_node_id);

        let handshake_msg = NetworkMessage::NetworkResponse(
            NetworkResponse::Handshake(
                self.handler().self_peer.id(),
                self.handler().self_peer.port(),
                read_or_die!(self.remote_end_networks).to_owned(),
                vec![],
            ),
            Some(get_current_stamp()),
            None,
        );

        self.async_send(
            serialize_into_buffer(&handshake_msg, 128)?,
            MessageSendingPriority::High,
        )
    }

    pub fn send_ping(&self) -> Fallible<()> {
        trace!("Sending a ping on {}", self);

        let ping_msg =
            NetworkMessage::NetworkRequest(NetworkRequest::Ping, Some(get_current_stamp()), None);

        self.async_send(
            serialize_into_buffer(&ping_msg, 64)?,
            MessageSendingPriority::High,
        )?;

        self.set_last_ping_sent();

        Ok(())
    }

    pub fn send_pong(&self) -> Fallible<()> {
        trace!("Sending a pong on {}", self);

        let pong_msg =
            NetworkMessage::NetworkResponse(NetworkResponse::Pong, Some(get_current_stamp()), None);

        self.async_send(
            serialize_into_buffer(&pong_msg, 64)?,
            MessageSendingPriority::High,
        )
    }

    pub fn send_peer_list_resp(&self, nets: &HashSet<NetworkId>) -> Fallible<()> {
        let requestor = self.remote_peer().peer().unwrap();

        let peer_list_resp = match self.handler().peer_type() {
            PeerType::Bootstrapper => {
                const BOOTSTRAP_PEER_COUNT: usize = 100;

                let random_nodes = safe_read!(self.handler().connection_handler.buckets)?
                    .get_random_nodes(&requestor, BOOTSTRAP_PEER_COUNT, nets);

                if !random_nodes.is_empty()
                    && random_nodes.len()
                        >= usize::from(self.handler().config.bootstrapper_wait_minimum_peers)
                {
                    Some(NetworkMessage::NetworkResponse(
                        NetworkResponse::PeerList(random_nodes),
                        Some(get_current_stamp()),
                        None,
                    ))
                } else {
                    None
                }
            }
            PeerType::Node => {
                let nodes = self
                    .handler()
                    .get_peer_stats(Some(PeerType::Node))
                    .iter()
                    .filter(|stat| P2PNodeId(stat.id) != requestor.id)
                    .map(|stat| {
                        P2PPeer::from(stat.peer_type, P2PNodeId(stat.id), stat.external_address())
                    })
                    .collect::<Vec<_>>();

                if !nodes.is_empty() {
                    Some(NetworkMessage::NetworkResponse(
                        NetworkResponse::PeerList(nodes),
                        Some(get_current_stamp()),
                        None,
                    ))
                } else {
                    None
                }
            }
        };

        if let Some(resp) = peer_list_resp {
            debug!("Sending my PeerList to peer {}", requestor.id());

            self.async_send(
                serialize_into_buffer(&resp, 256)?,
                MessageSendingPriority::Normal,
            )
        } else {
            debug!(
                "I don't have any peers to share with peer {}",
                requestor.id()
            );
            Ok(())
        }
    }

    fn forward_network_message(&self, msg: &NetworkMessage) -> Fallible<()> {
        let conn_filter = |conn: &Connection| match msg {
            NetworkMessage::NetworkRequest(request, ..) => match request {
                NetworkRequest::BanNode(peer_to_ban) => match peer_to_ban {
                    BannedNode::ById(id) => {
                        conn.remote_peer().peer().map_or(true, |x| x.id() != *id)
                    }
                    BannedNode::ByAddr(addr) => conn
                        .remote_peer()
                        .peer()
                        .map_or(true, |peer| peer.ip() != *addr),
                },
                _ => true,
            },
            _ => unreachable!("Only network requests are ever forwarded"),
        };

        match serialize_into_buffer(msg, 256) {
            Ok(data) => {
                for conn in read_or_die!(self.handler().connections())
                    .values()
                    .filter(|&conn| {
                        conn.is_post_handshake() && conn.as_ref() != self && conn_filter(conn)
                    })
                {
                    if let Err(e) = conn.async_send(data.clone(), MessageSendingPriority::Normal) {
                        error!("Can't forward a network message to {}: {}", conn, e);
                    }
                }
            }
            Err(e) => {
                error!("A network message couldn't be forwarded: {}", e);
            }
        }

        Ok(())
    }

    #[cfg(test)]
    pub fn validate_packet_type_test(&self, msg: &[u8]) -> Readiness<bool> {
        write_or_die!(self.low_level)
            .message_stream
            .validate_packet_type(msg)
    }
}

impl Drop for Connection {
    fn drop(&mut self) {
        debug!("Closing {}", self);

        // Report number of peers to stats export engine
        if let Some(ref service) = self.handler().stats_export_service {
            if self.is_post_handshake() {
                service.peers_dec();
            }
        }
    }
}

pub struct ConnectionLowLevel {
    pub conn_ref:   Option<Pin<Arc<Connection>>>,
    socket:         TcpStream,
    message_sink:   FrameSink,
    message_stream: FrameStream,
}

impl ConnectionLowLevel {
    fn conn(&self) -> &Connection {
        &self.conn_ref.as_ref().unwrap() // safe; always available
    }

    fn new(
        peer_type: PeerType,
        socket: TcpStream,
        key_pair: Keypair,
        is_initiator: bool,
        noise_params: snow::params::NoiseParams,
    ) -> Self {
        let handshaker = Arc::new(RwLock::new(HandshakeStreamSink::new(
            noise_params.clone(),
            key_pair,
            is_initiator,
        )));

        if let Err(e) = socket.set_linger(Some(std::time::Duration::from_secs(0))) {
            error!(
                "Can't set SOLINGER to 0 for socket {:?} due to {}",
                socket, e
            );
        }

        ConnectionLowLevel {
            conn_ref: None,
            socket,
            message_sink: FrameSink::new(Arc::clone(&handshaker)),
            message_stream: FrameStream::new(peer_type, handshaker),
        }
    }

    #[inline(always)]
    pub fn read_stream(&mut self, deduplication_queues: &mut DeduplicationQueues) -> Fallible<()> {
        loop {
            let read_result = self.message_stream.read(&mut self.socket);
            match read_result {
                Ok(readiness) => match readiness {
                    Readiness::Ready(message) => {
                        self.conn().send_to_dump(&message, true);
                        if let Err(e) = self.conn().process_message(message, deduplication_queues) {
                            bail!("Can't read the stream: {}", e);
                        }
                    }
                    Readiness::NotReady => break,
                },
                Err(e) => bail!("Can't read the stream: {}", e),
            }
        }

        Ok(())
    }

    #[inline(always)]
    pub fn flush_sink(&mut self) -> Fallible<Readiness<usize>> {
        self.message_sink.flush(&mut self.socket)
    }

    #[inline(always)]
    fn write_to_sink(&mut self, input: HybridBuf) -> Fallible<Readiness<usize>> {
        self.message_sink.write(input, &mut self.socket)
    }
}

// returns a bool indicating if the message is a duplicate
fn dedup_with(message: &mut HybridBuf, queue: &mut CircularQueue<[u8; 8]>) -> Fallible<bool> {
    let mut hash = [0u8; 8];
    hash.copy_from_slice(&XxHash64::digest(&message.remaining_bytes()?));

    if !queue.iter().any(|h| h == &hash) {
        queue.push(hash);
        Ok(false)
    } else {
        Ok(true)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        common::PeerType,
        connection::Readiness,
        test_utils::{
            await_handshake, connect, make_node_and_sync, next_available_port, setup_logger,
        },
    };
    use failure::Fallible;
    use rand::{distributions::Standard, thread_rng, Rng};
    use std::iter;

    const PACKAGE_INITIAL_BUFFER_SZ: usize = 1024;
    const PACKAGE_MAX_BUFFER_SZ: usize = 4096;

    pub struct BytesMutConn {
        pkt_buffer: Vec<u8>,
    }

    /// Simulate allocation/deallocation of `Connection.pkt_buffer`.
    fn check_bytes_mut_drop(pkt_size: usize) {
        assert!(pkt_size > PACKAGE_MAX_BUFFER_SZ);

        // 1. Allocate buffer with initial capacity.
        let mut a1 = BytesMutConn {
            pkt_buffer: Vec::with_capacity(PACKAGE_INITIAL_BUFFER_SZ),
        };

        // 2. Simulate reception of X bytes.
        let content: Vec<u8> = thread_rng().sample_iter(&Standard).take(pkt_size).collect();

        for chunk in content.chunks(1024) {
            a1.pkt_buffer.extend_from_slice(chunk);
        }
        assert_eq!(pkt_size, a1.pkt_buffer.len());
        assert!(a1.pkt_buffer.capacity() >= pkt_size);

        // 3. Reset
        a1.pkt_buffer = Vec::with_capacity(PACKAGE_INITIAL_BUFFER_SZ);
        assert_eq!(PACKAGE_INITIAL_BUFFER_SZ, a1.pkt_buffer.capacity());
        assert_eq!(0, a1.pkt_buffer.len());
    }

    #[test]
    fn check_bytes_mut_drop_128k() { check_bytes_mut_drop(128 * 1024); }

    #[test]
    fn check_bytes_mut_drop_512k() { check_bytes_mut_drop(512 * 1024); }

    #[test]
    fn check_bytes_mut_drop_8m() { check_bytes_mut_drop(8 * 1024 * 1024); }

    // This test stops the event loop because it needs a connection to be tested.
    // Connections are not simple objects and require complex objects i.e.
    // TcpStream, so the implementation creates a pair of nodes and connects them.
    //
    // The pkt_buffer inside a connection can be filled with events that trigger
    // processes in the event loop. Therefore, the safe way to work with this is
    // deregistering it from the event loop. This way we keep the connection alive
    // and the buffer is not filled by other threads.
    #[test]
    fn test_validate_packet_type() -> Fallible<()> {
        setup_logger();

        // Create connections
        let node = make_node_and_sync(next_available_port(), vec![100], PeerType::Node)?;
        let bootstrapper =
            make_node_and_sync(next_available_port(), vec![100], PeerType::Bootstrapper)?;
        connect(&node, &bootstrapper)?;
        await_handshake(&node)?;

        let conn_node = node.find_connection_by_id(bootstrapper.id()).unwrap();
        let conn_bootstrapper = bootstrapper.find_connection_by_id(node.id()).unwrap();

        // Assert that a Node accepts every packet
        match conn_node.validate_packet_type_test(&[]) {
            Readiness::Ready(true) => {}
            _ => bail!("Unwanted packet type"),
        }
        match conn_node
            .validate_packet_type_test(&iter::repeat(0).take(13).chain(Some(2)).collect::<Vec<_>>())
        {
            Readiness::Ready(true) => {}
            _ => bail!("Unwanted packet type"),
        }
        match conn_node
            .validate_packet_type_test(&iter::repeat(0).take(13).chain(Some(1)).collect::<Vec<_>>())
        {
            Readiness::Ready(true) => {}
            _ => bail!("Unwanted packet type"),
        }
        match conn_node.validate_packet_type_test(&iter::repeat(0).take(14).collect::<Vec<_>>()) {
            Readiness::Ready(true) => {}
            _ => bail!("Unwanted packet type"),
        }
        // Assert that a Boostrapper reports as unknown packets that are too small
        match conn_bootstrapper.validate_packet_type_test(&[]) {
            Readiness::NotReady => {}
            _ => bail!("Unwanted packet type"),
        }
        // Assert that a Bootstrapper reports as Invalid messages that are packets
        match conn_bootstrapper
            .validate_packet_type_test(&iter::repeat(0).take(13).chain(Some(2)).collect::<Vec<_>>())
        {
            Readiness::Ready(false) => {}
            _ => bail!("Unwanted packet type"),
        }
        // Assert that a Bootstrapper accepts Request and Response messages
        match conn_bootstrapper
            .validate_packet_type_test(&iter::repeat(0).take(13).chain(Some(1)).collect::<Vec<_>>())
        {
            Readiness::Ready(true) => {}
            _ => bail!("Unwanted packet type"),
        }
        match conn_bootstrapper
            .validate_packet_type_test(&iter::repeat(0).take(14).collect::<Vec<_>>())
        {
            Readiness::Ready(true) => {}
            _ => bail!("Unwanted packet type"),
        }
        Ok(())
    }

}
