#[macro_use]
pub mod fails;
pub mod network_handler;

mod async_adapter;
pub use async_adapter::{
    default_noise_params, FrameSink, FrameStream, HandshakeStreamSink, Readiness,
};

mod p2p_event;
mod seen_messages_list;

#[derive(PartialEq, Clone, Copy)]
pub enum ConnectionStatus {
    PreHandshake,
    PostHandshake,
    Closing,
    Closed,
}

mod connection_builder;
pub use connection_builder::ConnectionBuilder;

mod connection_default_handlers;
mod connection_handshake_handlers;
mod connection_private;
mod handler_utils;

pub use crate::connection::connection_private::ConnectionPrivate;

pub use network_handler::{
    MessageHandler, NetworkPacketCW, NetworkRequestCW, NetworkResponseCW, PacketHandler,
    RequestHandler, ResponseHandler,
};
pub use p2p_event::P2PEvent;
pub use seen_messages_list::SeenMessagesList;

use crate::{
    common::{
        counter::TOTAL_MESSAGES_RECEIVED_COUNTER,
        get_current_stamp,
        serialization::{Deserializable, ReadArchiveAdapter},
        NetworkRawRequest, P2PNodeId, P2PPeer, PeerType, RemotePeer,
    },
    connection::{
        connection_default_handlers::*,
        connection_handshake_handlers::*,
        network_handler::message_processor::{
            collapse_process_result, MessageProcessor, ProcessResult,
        },
    },
    dumper::DumpItem,
    network::{Buckets, NetworkId, NetworkMessage, NetworkRequest, NetworkResponse},
    stats_export_service::StatsExportService,
};

use concordium_common::{
    functor::{FuncResult, UnitFunction},
    UCursor,
};

use failure::{Error, Fallible};
use mio::{Event, Poll, Token};
use std::{
    cell::RefCell,
    collections::HashSet,
    net::SocketAddr,
    rc::Rc,
    sync::{atomic::Ordering, mpsc::Sender, Arc, RwLock},
};

/// This macro clones `dptr` and moves it into callback closure.
/// That closure is just a call to `fn` Fn.
macro_rules! handle_by_private {
    ($dptr:expr, $message_type:ty, $fn:ident) => {{
        let dptr_cloned = Rc::clone(&$dptr);
        make_atomic_callback!(move |m: $message_type| { $fn(&dptr_cloned, m) })
    }};
}

pub struct Connection {
    // Counters
    messages_sent:     u64,
    messages_received: u64,
    last_ping_sent:    u64,

    blind_trusted_broadcast: bool,

    network_request_sender: Sender<NetworkRawRequest>,

    /// It stores internal info used in handles. In this way,
    /// handler's function will only need two arguments: this shared object, and
    /// the message which is going to be processed.
    dptr: Rc<RefCell<ConnectionPrivate>>,

    // Message handlers
    pub pre_handshake_message_processor:  MessageProcessor,
    pub post_handshake_message_processor: MessageProcessor,
    pub common_message_processor:         Rc<RefCell<MessageProcessor>>,
}

impl Connection {
    pub fn set_network_request_sender(&mut self, sender: Sender<NetworkRawRequest>) {
        self.network_request_sender = sender;
    }

    // Setup handshake handler
    pub fn setup_pre_handshake(&mut self) {
        let cloned_message_processor = Rc::clone(&self.common_message_processor);
        self.pre_handshake_message_processor
            .add(&cloned_message_processor.borrow())
            .add_request_action(handle_by_private!(
                self.dptr,
                &NetworkRequest,
                handshake_request_handle
            ))
            .add_response_action(handle_by_private!(
                self.dptr,
                &NetworkResponse,
                handshake_response_handle
            ));
    }

    fn make_update_last_seen_handler<T>(&self) -> UnitFunction<T> {
        let priv_conn = Rc::clone(&self.dptr);

        make_atomic_callback!(move |_: &T| -> FuncResult<()> {
            priv_conn.borrow_mut().update_last_seen();
            Ok(())
        })
    }

    fn make_request_handler(&mut self) -> RequestHandler {
        let update_last_seen_handler = self.make_update_last_seen_handler();

        let mut rh = RequestHandler::new();
        rh.add_ping_callback(handle_by_private!(
            self.dptr,
            &NetworkRequest,
            default_network_request_ping_handle
        ))
        .add_find_node_callback(handle_by_private!(
            self.dptr,
            &NetworkRequest,
            default_network_request_find_node_handle
        ))
        .add_get_peers_callback(handle_by_private!(
            self.dptr,
            &NetworkRequest,
            default_network_request_get_peers
        ))
        .add_join_network_callback(handle_by_private!(
            self.dptr,
            &NetworkRequest,
            default_network_request_join_network
        ))
        .add_leave_network_callback(handle_by_private!(
            self.dptr,
            &NetworkRequest,
            default_network_request_leave_network
        ))
        .add_handshake_callback(make_atomic_callback!(move |m: &NetworkRequest| {
            default_network_request_handshake(m)
        }))
        .add_ban_node_callback(Arc::clone(&update_last_seen_handler))
        .add_unban_node_callback(Arc::clone(&update_last_seen_handler))
        .add_join_network_callback(Arc::clone(&update_last_seen_handler))
        .add_leave_network_callback(Arc::clone(&update_last_seen_handler));

        rh
    }

    fn make_response_handler(&mut self) -> ResponseHandler {
        let mut rh = ResponseHandler::new();

        rh.add_find_node_callback(handle_by_private!(
            self.dptr,
            &NetworkResponse,
            default_network_response_find_node
        ))
        .add_pong_callback(handle_by_private!(
            self.dptr,
            &NetworkResponse,
            default_network_response_pong
        ))
        .add_handshake_callback(make_atomic_callback!(move |m: &NetworkResponse| {
            default_network_response_handshake(m)
        }))
        .add_peer_list_callback(handle_by_private!(
            self.dptr,
            &NetworkResponse,
            default_network_response_peer_list
        ));

        rh
    }

    pub fn setup_post_handshake(&mut self) {
        let request_handler = self.make_request_handler();
        let response_handler = self.make_response_handler();
        let last_seen_response_handler = self.make_update_last_seen_handler();
        let last_seen_packet_handler = self.make_update_last_seen_handler();
        let cloned_message_processor = Rc::clone(&self.common_message_processor);
        let dptr_1 = Rc::clone(&self.dptr);
        let dptr_2 = Rc::clone(&self.dptr);

        self.post_handshake_message_processor
            .add(&cloned_message_processor.borrow())
            .add_request_action(make_atomic_callback!(move |req: &NetworkRequest| {
                request_handler.process_message(req).map_err(Error::from)
            }))
            .add_response_action(make_atomic_callback!(move |res: &NetworkResponse| {
                response_handler.process_message(res).map_err(Error::from)
            }))
            .add_response_action(last_seen_response_handler)
            .add_packet_action(last_seen_packet_handler)
            .set_unknown_handler(Rc::new(move || default_unknown_message(&dptr_1)))
            .set_invalid_handler(Rc::new(move || default_invalid_message(&dptr_2)));
    }

    // =============================

    pub fn get_last_latency_measured(&self) -> Option<u64> {
        let latency: u64 = self.dptr.borrow().last_latency_measured;
        if latency != u64::max_value() {
            Some(latency)
        } else {
            None
        }
    }

    pub fn set_measured_handshake_sent(&mut self) {
        self.dptr.borrow_mut().sent_handshake = get_current_stamp()
    }

    pub fn set_measured_ping_sent(&mut self) { self.dptr.borrow_mut().set_measured_ping_sent(); }

    pub fn get_last_ping_sent(&self) -> u64 { self.last_ping_sent }

    pub fn set_last_ping_sent(&mut self) { self.last_ping_sent = get_current_stamp(); }

    pub fn local_id(&self) -> P2PNodeId { self.dptr.borrow().local_peer.id() }

    pub fn remote_id(&self) -> Option<P2PNodeId> {
        match self.dptr.borrow().remote_peer() {
            RemotePeer::PostHandshake(ref remote_peer) => Some(remote_peer.id()),
            _ => None,
        }
    }

    pub fn is_post_handshake(&self) -> bool { self.dptr.borrow().remote_peer.is_post_handshake() }

    pub fn local_addr(&self) -> SocketAddr { self.dptr.borrow().local_peer.addr }

    pub fn remote_addr(&self) -> SocketAddr { self.dptr.borrow().remote_peer().addr() }

    pub fn last_seen(&self) -> u64 { self.dptr.borrow().last_seen() }

    pub fn local_peer(&self) -> P2PPeer { self.dptr.borrow().local_peer.clone() }

    pub fn remote_peer(&self) -> RemotePeer { self.dptr.borrow().remote_peer().to_owned() }

    pub fn get_messages_received(&self) -> u64 { self.messages_received }

    pub fn get_messages_sent(&self) -> u64 { self.messages_sent }

    pub fn failed_pkts(&self) -> u32 { self.dptr.borrow().failed_pkts }

    /// It registers the connection socket, for read and write ops using *edge*
    /// notifications.
    #[inline]
    pub fn register(&self, poll: &mut Poll) -> Fallible<()> { self.dptr.borrow().register(poll) }

    #[inline]
    pub fn deregister(&self, poll: &mut Poll) -> Fallible<()> {
        self.dptr.borrow().deregister(poll)
    }

    pub fn blind_trusted_broadcast(&self) -> bool { self.blind_trusted_broadcast }

    #[inline]
    pub fn is_closed(&self) -> bool {
        let status = self.dptr.borrow().status;
        status == ConnectionStatus::Closed || status == ConnectionStatus::Closing
    }

    #[inline]
    pub fn close(&mut self) { self.dptr.borrow_mut().status = ConnectionStatus::Closing; }

    #[inline]
    pub fn status(&self) -> ConnectionStatus { self.dptr.borrow().status }

    #[inline]
    pub fn shutdown(&mut self) -> Fallible<()> { self.dptr.borrow_mut().shutdown() }

    pub fn ready(
        &mut self,
        ev: &Event,
    ) -> Result<ProcessResult, Vec<Result<ProcessResult, failure::Error>>> {
        let messages_result = self.dptr.borrow_mut().ready(ev);
        match messages_result {
            Ok(messages) => collapse_process_result(self, messages),
            Err(error) => Err(vec![Err(error)]),
        }
    }

    #[inline]
    pub fn set_log_dumper(&mut self, log_dumper: Option<Sender<DumpItem>>) {
        self.dptr.borrow_mut().set_log_dumper(log_dumper);
    }

    /// It decodes message from `buf` and processes it using its message
    /// handlers.
    fn process_message(&mut self, message: UCursor) -> Fallible<ProcessResult> {
        let mut archive =
            ReadArchiveAdapter::new(message, self.remote_peer().clone(), self.remote_addr().ip());
        let message = NetworkMessage::deserialize(&mut archive)?;
        let outer = Arc::new(message);

        self.messages_received += 1;
        TOTAL_MESSAGES_RECEIVED_COUNTER.fetch_add(1, Ordering::Relaxed);
        if let Some(ref service) = &self.stats_export_service() {
            if let Ok(mut slock) = safe_write!(service) {
                slock.pkt_received_inc();
            }
        };

        // Process message by message handler.
        if self.dptr.borrow().status == ConnectionStatus::PostHandshake {
            self.post_handshake_message_processor
                .process_message(&outer)
        } else {
            self.pre_handshake_message_processor.process_message(&outer)
        }
    }

    #[cfg(test)]
    pub fn validate_packet_type_test(&mut self, msg: &[u8]) -> Readiness<bool> {
        self.dptr.borrow_mut().validate_packet_type(msg)
    }

    pub fn stats_export_service(&self) -> Option<Arc<RwLock<StatsExportService>>> {
        self.dptr.borrow().stats_export_service.clone()
    }

    pub fn local_peer_type(&self) -> PeerType { self.dptr.borrow().local_peer.peer_type() }

    pub fn remote_peer_type(&self) -> PeerType { self.dptr.borrow().remote_peer.peer_type() }

    pub fn buckets(&self) -> Arc<RwLock<Buckets>> { Arc::clone(&self.dptr.borrow().buckets) }

    #[inline]
    pub fn promote_to_post_handshake(&mut self, id: P2PNodeId, addr: SocketAddr) -> Fallible<()> {
        self.dptr.borrow_mut().promote_to_post_handshake(id, addr)
    }

    pub fn remote_end_networks(&self) -> HashSet<NetworkId> {
        self.dptr.borrow().remote_end_networks.clone()
    }

    pub fn local_end_networks(&self) -> Arc<RwLock<HashSet<NetworkId>>> {
        Arc::clone(&self.dptr.borrow().local_end_networks)
    }

    #[inline]
    pub fn token(&self) -> Token { self.dptr.borrow().token }

    /// It queues network request
    #[inline]
    pub fn async_send(&self, input: UCursor) -> Fallible<()> {
        let request = NetworkRawRequest {
            token: self.token(),
            data:  input,
        };
        into_err!(self.network_request_sender.send(request))
    }

    #[inline]
    pub fn async_send_from_poll_loop(&mut self, input: UCursor) -> Fallible<Readiness<usize>> {
        self.dptr.borrow_mut().async_send(input)
    }

    pub fn add_notification(&mut self, func: UnitFunction<NetworkMessage>) {
        self.pre_handshake_message_processor
            .add_notification(Arc::clone(&func));
        self.post_handshake_message_processor.add_notification(func);
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        common::PeerType,
        connection::Readiness,
        test_utils::{
            connect_and_wait_handshake, make_node_and_sync, next_available_port, setup_logger,
        },
    };
    use failure::Fallible;
    use rand::{distributions::Standard, thread_rng, Rng};
    use std::{iter, sync::Arc};

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
        let (mut node, w1) =
            make_node_and_sync(next_available_port(), vec![100], false, PeerType::Node)?;
        let (bootstrapper, _) = make_node_and_sync(
            next_available_port(),
            vec![100],
            false,
            PeerType::Bootstrapper,
        )?;
        connect_and_wait_handshake(&mut node, &bootstrapper, &w1)?;

        // Deregister connection on the node side
        let tls = Arc::clone(&node.tls_server);
        let priv_tls = safe_read!(tls)?.get_private_tls();
        let priv_tls = safe_read!(priv_tls)?;
        let conn_node = priv_tls.find_connection_by_id(bootstrapper.id()).unwrap();
        node.deregister_connection(conn_node)?;
        let mut conn_node = conn_node.borrow_mut();

        // Deregister connection on the bootstrapper side
        let tls = Arc::clone(&bootstrapper.tls_server);
        let priv_tls = safe_read!(tls)?.get_private_tls();
        let priv_tls = safe_read!(priv_tls)?;
        let conn_bootstrapper = priv_tls.find_connection_by_id(node.id()).unwrap();
        bootstrapper.deregister_connection(conn_bootstrapper)?;
        let mut conn_bootstrapper = conn_bootstrapper.borrow_mut();

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
