#[macro_use]
pub mod fails;
pub mod message_handlers;

mod async_adapter;
pub use async_adapter::{FrameSink, FrameStream, HandshakeStreamSink, Readiness};

mod p2p_event;

// If a message is labelled as having `High` priority it is always pushed to the
// front of the queue in the sinks when sending, and otherwise to the back
pub enum MessageSendingPriority {
    High,
    Normal,
}

mod connection_private;

pub use crate::{connection::connection_private::ConnectionPrivate, p2p::P2PNode};

pub use p2p_event::P2PEvent;

use crate::{
    common::{
        counter::{TOTAL_MESSAGES_RECEIVED_COUNTER, TOTAL_MESSAGES_SENT_COUNTER},
        get_current_stamp,
        serialization::{Deserializable, ReadArchiveAdapter},
        NetworkRawRequest, P2PNodeId, PeerStats, PeerType, RemotePeer,
    },
    connection::message_handlers::handle_incoming_message,
    dumper::DumpItem,
    network::{Buckets, NetworkId, NetworkMessage},
};

use concordium_common::hybrid_buf::HybridBuf;

use chrono::prelude::Utc;
use failure::Fallible;
use mio::{tcp::TcpStream, Event, Poll, PollOpt, Ready, Token};
use snow::Keypair;

use std::{
    collections::HashSet,
    net::SocketAddr,
    pin::Pin,
    sync::{
        atomic::{AtomicBool, AtomicU32, AtomicU64, Ordering},
        Arc, RwLock,
    },
};

pub struct Connection {
    handler_ref:               Pin<Arc<P2PNode>>,
    pub token:                 Token,
    pub remote_peer:           RemotePeer,
    pub dptr:                  Arc<RwLock<ConnectionPrivate>>,
    pub remote_end_networks:   Arc<RwLock<HashSet<NetworkId>>>,
    pub is_post_handshake:     Arc<AtomicBool>,
    pub messages_sent:         Arc<AtomicU64>,
    pub messages_received:     Arc<AtomicU64>,
    pub last_ping_sent:        Arc<AtomicU64>,
    pub sent_handshake:        Arc<AtomicU64>,
    pub sent_ping:             Arc<AtomicU64>,
    pub last_latency_measured: Arc<AtomicU64>,
    pub last_seen:             Arc<AtomicU64>,
    pub failed_pkts:           Arc<AtomicU32>,
}

impl Drop for Connection {
    fn drop(&mut self) {
        debug!(
            "Closing connection {} ({}:{})",
            usize::from(self.token),
            self.remote_addr().ip(),
            self.remote_addr().port()
        );

        if let Err(e) = self.handler().deregister_connection(self) {
            error!(
                "Connection {} couldn't be closed: {:?}",
                usize::from(self.token),
                e
            );
        }

        if let Some(id) = self.remote_id() {
            write_or_die!(self.handler().active_peer_stats).remove(&id.as_raw());
        }

        // Report number of peers to stats export engine
        if let Some(ref service) = self.handler().stats_export_service {
            if self.is_post_handshake() {
                service.peers_dec();
            }
        }
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

        let dptr = Arc::new(RwLock::new(ConnectionPrivate::new(
            local_peer_type,
            socket,
            key_pair,
            is_initiator,
            noise_params,
        )));

        let conn = Arc::new(Self {
            handler_ref: handler.self_ref.clone().unwrap(), // safe, always available
            token,
            remote_peer,
            dptr,
            remote_end_networks: Default::default(),
            is_post_handshake: Default::default(),
            messages_received: Default::default(),
            messages_sent: Default::default(),
            last_ping_sent: Arc::new(AtomicU64::new(curr_stamp)),
            sent_handshake: Default::default(),
            sent_ping: Default::default(),
            last_latency_measured: Default::default(),
            last_seen: Arc::new(AtomicU64::new(curr_stamp)),
            failed_pkts: Default::default(),
        });

        write_or_die!(conn.dptr).conn_ref = Some(Pin::new(Arc::clone(&conn)));

        conn
    }

    pub fn get_last_latency_measured(&self) -> u64 {
        self.last_latency_measured.load(Ordering::SeqCst)
    }

    pub fn set_measured_handshake_sent(&self) {
        self.sent_handshake
            .store(get_current_stamp(), Ordering::SeqCst)
    }

    pub fn set_measured_ping_sent(&self) {
        self.sent_ping.store(get_current_stamp(), Ordering::SeqCst)
    }

    pub fn get_last_ping_sent(&self) -> u64 { self.last_ping_sent.load(Ordering::SeqCst) }

    pub fn set_last_ping_sent(&self) {
        self.last_ping_sent
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
            self.remote_peer_type(),
            Arc::clone(&self.messages_sent),
            Arc::clone(&self.messages_received),
            Arc::clone(&self.last_latency_measured),
        ))
    }

    pub fn remote_addr(&self) -> SocketAddr { self.remote_peer.addr() }

    pub fn is_post_handshake(&self) -> bool { self.is_post_handshake.load(Ordering::SeqCst) }

    pub fn last_seen(&self) -> u64 { self.last_seen.load(Ordering::SeqCst) }

    pub fn get_messages_received(&self) -> u64 { self.messages_received.load(Ordering::SeqCst) }

    pub fn get_messages_sent(&self) -> u64 { self.messages_sent.load(Ordering::SeqCst) }

    pub fn failed_pkts(&self) -> u32 { self.failed_pkts.load(Ordering::SeqCst) }

    /// It registers the connection socket, for read and write ops using *edge*
    /// notifications.
    #[inline]
    pub fn register(&self, poll: &Poll) -> Fallible<()> {
        into_err!(poll.register(
            &read_or_die!(self.dptr).socket,
            self.token,
            Ready::readable() | Ready::writable(),
            PollOpt::edge()
        ))
    }

    #[inline]
    pub fn deregister(&self, poll: &Poll) -> Fallible<()> {
        map_io_error_to_fail!(poll.deregister(&read_or_die!(self.dptr).socket))
    }

    /// This function is called when `poll` indicates that `socket` is ready to
    /// write or/and read.
    pub fn ready(&self, ev: &Event) -> Fallible<()> {
        write_or_die!(self.dptr).read_from_stream(ev)
    }

    /// It decodes message from `buf` and processes it using its message
    /// handlers.
    fn process_message(&self, message: HybridBuf) -> Fallible<()> {
        let mut archive = ReadArchiveAdapter::new(message, self.remote_peer());
        let message = NetworkMessage::deserialize(&mut archive)?;

        self.update_last_seen();
        self.messages_received.fetch_add(1, Ordering::Relaxed);
        TOTAL_MESSAGES_RECEIVED_COUNTER.fetch_add(1, Ordering::Relaxed);
        if let Some(ref service) = self.handler().stats_export_service {
            service.pkt_received_inc();
        };

        handle_incoming_message(self.handler(), self, &message);
        self.handler().forward_network_message(&message)?;

        Ok(())
    }

    pub fn buckets(&self) -> Arc<RwLock<Buckets>> {
        Arc::clone(&self.handler().connection_handler.buckets)
    }

    pub fn promote_to_post_handshake(&self, id: P2PNodeId) -> Fallible<()> {
        self.is_post_handshake.store(true, Ordering::SeqCst);
        *write_or_die!(self.remote_peer.id) = Some(id);

        // register peer's stats in the P2PNode
        write_or_die!(self.handler().active_peer_stats)
            .insert(id.as_raw(), self.remote_peer_stats()?);

        Ok(())
    }

    pub fn remote_end_networks(&self) -> Arc<RwLock<HashSet<NetworkId>>> {
        Arc::clone(&self.remote_end_networks)
    }

    pub fn local_end_networks(&self) -> Arc<RwLock<HashSet<NetworkId>>> {
        self.handler().networks()
    }

    /// It queues a network request
    #[inline]
    pub fn async_send(&self, input: HybridBuf, priority: MessageSendingPriority) -> Fallible<()> {
        let request = NetworkRawRequest {
            token: self.token,
            data: input,
            priority,
        };
        into_err!(self
            .handler()
            .connection_handler
            .network_request_sender
            .send(request))
    }

    /// It sends `input` through `socket`.
    /// This functions returns (almost) immediately, because it does NOT wait
    /// for real write. Function `ConnectionPrivate::ready` will make ensure to
    /// write chunks of the message
    #[inline]
    pub fn async_send_from_poll_loop(
        &self,
        input: HybridBuf,
        priority: MessageSendingPriority,
    ) -> Fallible<Readiness<usize>> {
        TOTAL_MESSAGES_SENT_COUNTER.fetch_add(1, Ordering::Relaxed);
        if let Some(ref stats) = self.handler().stats_export_service {
            stats.pkt_sent_inc();
        }

        self.send_to_dump(&input, false);
        write_or_die!(self.dptr).write_to_sink(input, priority)
    }

    pub fn update_last_seen(&self) {
        if self.handler().peer_type() != PeerType::Bootstrapper {
            self.last_seen.store(get_current_stamp(), Ordering::SeqCst);
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
                self.remote_peer(),
                self.remote_peer().addr().ip(),
                buf.clone(),
            );
            let _ = sender.send(di);
        }
    }

    #[cfg(test)]
    pub fn validate_packet_type_test(&self, msg: &[u8]) -> Readiness<bool> {
        write_or_die!(self.dptr)
            .message_stream
            .validate_packet_type(msg)
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
        let (node, _) = make_node_and_sync(next_available_port(), vec![100], PeerType::Node)?;
        let (bootstrapper, _) =
            make_node_and_sync(next_available_port(), vec![100], PeerType::Bootstrapper)?;
        connect(&node, &bootstrapper)?;
        await_handshake(&node)?;
        // Deregister connection on the node side
        let conn_node = node.find_connection_by_id(bootstrapper.id()).unwrap();
        node.deregister_connection(&conn_node)?;
        // Deregister connection on the bootstrapper side
        let conn_bootstrapper = bootstrapper.find_connection_by_id(node.id()).unwrap();
        bootstrapper.deregister_connection(&conn_bootstrapper)?;
        // Assert that a Node accepts every packet
        match conn_node.validate_packet_type_test(&[]) {
            Readiness::Ready(true) => {}
            _ => bail!("Unwanted packet type"),
        }
        match conn_node
            .validate_packet_type_test(&iter::repeat(0).take(23).chain(Some(2)).collect::<Vec<_>>())
        {
            Readiness::Ready(true) => {}
            _ => bail!("Unwanted packet type"),
        }
        match conn_node
            .validate_packet_type_test(&iter::repeat(0).take(23).chain(Some(1)).collect::<Vec<_>>())
        {
            Readiness::Ready(true) => {}
            _ => bail!("Unwanted packet type"),
        }
        match conn_node.validate_packet_type_test(&iter::repeat(0).take(24).collect::<Vec<_>>()) {
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
            .validate_packet_type_test(&iter::repeat(0).take(23).chain(Some(2)).collect::<Vec<_>>())
        {
            Readiness::Ready(false) => {}
            _ => bail!("Unwanted packet type"),
        }
        // Assert that a Bootstrapper accepts Request and Response messages
        match conn_bootstrapper
            .validate_packet_type_test(&iter::repeat(0).take(23).chain(Some(1)).collect::<Vec<_>>())
        {
            Readiness::Ready(true) => {}
            _ => bail!("Unwanted packet type"),
        }
        match conn_bootstrapper
            .validate_packet_type_test(&iter::repeat(0).take(24).collect::<Vec<_>>())
        {
            Readiness::Ready(true) => {}
            _ => bail!("Unwanted packet type"),
        }
        Ok(())
    }

}
