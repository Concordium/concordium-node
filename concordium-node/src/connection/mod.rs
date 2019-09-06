#[macro_use]
pub mod fails;
pub mod network_handler;

mod async_adapter;
pub use async_adapter::{FrameSink, FrameStream, HandshakeStreamSink, Readiness};

mod p2p_event;

#[derive(PartialEq, Clone, Copy)]
pub enum ConnectionStatus {
    PreHandshake,
    PostHandshake,
    Closing,
    Closed,
}

// If a message is labelled as having `High` priority it is always pushed to the
// front of the queue in the sinks when sending, and otherwise to the back
pub enum MessageSendingPriority {
    High,
    Normal,
}

mod connection_builder;
pub use connection_builder::ConnectionBuilder;

mod connection_default_handlers;
mod connection_handshake_handlers;
mod connection_private;
mod handler_utils;

pub use crate::{connection::connection_private::ConnectionPrivate, p2p::P2PNode};

pub use network_handler::MessageHandler;
pub use p2p_event::P2PEvent;

use crate::{
    common::{
        counter::TOTAL_MESSAGES_RECEIVED_COUNTER,
        get_current_stamp,
        serialization::{Deserializable, ReadArchiveAdapter},
        NetworkRawRequest, P2PNodeId, P2PPeer, PeerType, RemotePeer,
    },
    connection::{
        connection_default_handlers::network_message_handle,
        connection_handshake_handlers::handshake_handle,
        network_handler::message_processor::{
            collapse_process_result, MessageProcessor, ProcessResult,
        },
    },
    network::{Buckets, NetworkId, NetworkMessage},
};
use concordium_common::stats_export_service::StatsExportService;

use concordium_common::{
    functor::{FuncResult, UnitFunction},
    hybrid_buf::HybridBuf,
};

use failure::Fallible;
use mio::{Event, Poll, Token};
use std::{
    collections::HashSet,
    net::SocketAddr,
    pin::Pin,
    sync::{
        atomic::{AtomicU64, Ordering},
        Arc, RwLock,
    },
};

/// This macro clones `dptr` and moves it into callback closure.
/// That closure is just a call to `fn` Fn.
macro_rules! handle_by_private {
    ($dptr:expr, $fn:ident) => {{
        let dptr_cloned = Arc::clone(&$dptr);
        make_atomic_callback!(move |m: &NetworkMessage| { $fn(&dptr_cloned, m) })
    }};
}

#[derive(Clone)]
pub struct Connection {
    handler_ref: Pin<Arc<P2PNode>>,

    // Counters
    pub messages_sent:     Arc<AtomicU64>,
    pub messages_received: Arc<AtomicU64>,
    last_ping_sent:        Arc<AtomicU64>,

    token: Token,

    /// It stores internal info used in handles. In this way,
    /// handler's function will only need two arguments: this shared object, and
    /// the message which is going to be processed.
    dptr: Arc<RwLock<ConnectionPrivate>>,

    // Message handlers
    pub pre_handshake_message_processor:  MessageProcessor,
    pub post_handshake_message_processor: MessageProcessor,
    pub common_message_processor:         MessageProcessor,
}

impl Connection {
    pub fn handler(&self) -> &Pin<Arc<P2PNode>> { &self.handler_ref }

    // Setup handshake handler
    pub fn setup_pre_handshake(&self) {
        self.pre_handshake_message_processor
            .add(&self.common_message_processor)
            .add_action(handle_by_private!(self.dptr, handshake_handle));
    }

    fn make_update_last_seen_handler<T: Send>(&self) -> UnitFunction<T> {
        let priv_conn = Arc::clone(&self.dptr);

        make_atomic_callback!(move |_: &T| -> FuncResult<()> {
            safe_read!(priv_conn)?.update_last_seen();
            Ok(())
        })
    }

    pub fn setup_post_handshake(&self) {
        let last_seen_handler = self.make_update_last_seen_handler();

        self.post_handshake_message_processor
            .add(&self.common_message_processor)
            .add_action(handle_by_private!(self.dptr, network_message_handle))
            .add_action(last_seen_handler);
    }

    pub fn get_last_latency_measured(&self) -> u64 {
        read_or_die!(self.dptr)
            .last_latency_measured
            .load(Ordering::SeqCst)
    }

    pub fn set_measured_handshake_sent(&self) {
        read_or_die!(self.dptr)
            .sent_handshake
            .store(get_current_stamp(), Ordering::SeqCst)
    }

    pub fn set_measured_ping_sent(&self) { read_or_die!(self.dptr).set_measured_ping_sent(); }

    pub fn get_last_ping_sent(&self) -> u64 { self.last_ping_sent.load(Ordering::SeqCst) }

    pub fn set_last_ping_sent(&self) {
        self.last_ping_sent
            .store(get_current_stamp(), Ordering::SeqCst);
    }

    pub fn local_peer(&self) -> P2PPeer { self.handler().self_peer }

    pub fn remote_peer(&self) -> RemotePeer { read_or_die!(self.dptr).remote_peer() }

    pub fn local_id(&self) -> P2PNodeId { self.local_peer().id() }

    pub fn remote_id(&self) -> Option<P2PNodeId> {
        match read_or_die!(self.dptr).remote_peer() {
            RemotePeer::PostHandshake(ref remote_peer) => Some(remote_peer.id()),
            _ => None,
        }
    }

    pub fn local_peer_type(&self) -> PeerType { self.local_peer().peer_type() }

    pub fn remote_peer_type(&self) -> PeerType { read_or_die!(self.dptr).remote_peer.peer_type() }

    pub fn local_addr(&self) -> SocketAddr { self.local_peer().addr }

    pub fn remote_addr(&self) -> SocketAddr { read_or_die!(self.dptr).remote_peer().addr() }

    pub fn is_post_handshake(&self) -> bool {
        read_or_die!(self.dptr).remote_peer.is_post_handshake()
    }

    pub fn last_seen(&self) -> u64 { read_or_die!(self.dptr).last_seen() }

    pub fn get_messages_received(&self) -> u64 { self.messages_received.load(Ordering::SeqCst) }

    pub fn get_messages_sent(&self) -> u64 { self.messages_sent.load(Ordering::SeqCst) }

    pub fn failed_pkts(&self) -> u32 { read_or_die!(self.dptr).failed_pkts }

    /// It registers the connection socket, for read and write ops using *edge*
    /// notifications.
    #[inline]
    pub fn register(&self, poll: &Poll) -> Fallible<()> { read_or_die!(self.dptr).register(poll) }

    #[inline]
    pub fn deregister(&self, poll: &Poll) -> Fallible<()> {
        read_or_die!(self.dptr).deregister(poll)
    }

    #[inline]
    pub fn is_closed(&self) -> bool {
        let status = read_or_die!(self.dptr).status;
        status == ConnectionStatus::Closed || status == ConnectionStatus::Closing
    }

    #[inline]
    pub fn close(&self) { write_or_die!(self.dptr).status = ConnectionStatus::Closing; }

    #[inline]
    pub fn status(&self) -> ConnectionStatus { read_or_die!(self.dptr).status }

    #[inline]
    pub fn shutdown(&self) -> Fallible<()> { write_or_die!(self.dptr).shutdown() }

    pub fn ready(
        &self,
        ev: &Event,
    ) -> Result<ProcessResult, Vec<Result<ProcessResult, failure::Error>>> {
        let messages_result = write_or_die!(self.dptr).ready(ev);
        match messages_result {
            Ok(messages) => collapse_process_result(self, messages),
            Err(error) => Err(vec![Err(error)]),
        }
    }

    /// It decodes message from `buf` and processes it using its message
    /// handlers.
    fn process_message(&self, message: HybridBuf) -> Fallible<ProcessResult> {
        let mut archive = ReadArchiveAdapter::new(message, self.remote_peer());
        let message = NetworkMessage::deserialize(&mut archive)?;

        self.messages_received.fetch_add(1, Ordering::Relaxed);
        TOTAL_MESSAGES_RECEIVED_COUNTER.fetch_add(1, Ordering::Relaxed);
        if let Some(ref service) = &self.stats_export_service() {
            service.pkt_received_inc();
        };

        // Process message by message handler.
        if read_or_die!(self.dptr).status == ConnectionStatus::PostHandshake {
            self.post_handshake_message_processor
                .process_message(&message)
        } else {
            self.pre_handshake_message_processor
                .process_message(&message)
        }
    }

    #[cfg(test)]
    pub fn validate_packet_type_test(&self, msg: &[u8]) -> Readiness<bool> {
        write_or_die!(self.dptr).validate_packet_type(msg)
    }

    pub fn stats_export_service(&self) -> Option<StatsExportService> {
        self.handler().stats_export_service().clone()
    }

    pub fn buckets(&self) -> Arc<RwLock<Buckets>> {
        Arc::clone(&self.handler().connection_handler.buckets)
    }

    #[inline]
    pub fn promote_to_post_handshake(&self, id: P2PNodeId, addr: SocketAddr) -> Fallible<()> {
        write_or_die!(self.dptr).promote_to_post_handshake(id, addr)
    }

    pub fn remote_end_networks(&self) -> HashSet<NetworkId> {
        read_or_die!(self.dptr).remote_end_networks.clone()
    }

    pub fn local_end_networks(&self) -> Arc<RwLock<HashSet<NetworkId>>> {
        Arc::clone(&read_or_die!(self.dptr).local_end_networks)
    }

    #[inline]
    pub fn token(&self) -> Token { self.token }

    /// It queues network request
    #[inline]
    pub fn async_send(&self, input: HybridBuf, priority: MessageSendingPriority) -> Fallible<()> {
        let request = NetworkRawRequest {
            token: self.token(),
            data: input,
            priority,
        };
        into_err!(self
            .handler()
            .connection_handler
            .network_request_sender
            .send(request))
    }

    #[inline]
    pub fn async_send_from_poll_loop(
        &self,
        input: HybridBuf,
        priority: MessageSendingPriority,
    ) -> Fallible<Readiness<usize>> {
        write_or_die!(self.dptr).async_send(input, priority)
    }

    pub fn add_notification(&self, func: UnitFunction<NetworkMessage>) {
        self.pre_handshake_message_processor
            .add_notification(func.clone());
        self.post_handshake_message_processor.add_notification(func);
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
        let (mut node, w1) = make_node_and_sync(next_available_port(), vec![100], PeerType::Node)?;
        let (bootstrapper, _) =
            make_node_and_sync(next_available_port(), vec![100], PeerType::Bootstrapper)?;
        connect(&mut node, &bootstrapper)?;
        await_handshake(&w1)?;
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
