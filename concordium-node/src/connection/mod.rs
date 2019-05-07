pub mod network_handler;

mod p2p_event;
mod seen_messages_list;
mod writev_adapter;

mod connection_default_handlers;
mod connection_handshake_handlers;
mod connection_private;
pub mod fails;
mod handler_utils;

use concordium_common::functor::FuncResult;
use rustls::Session;

/// It is a common trait for `rustls::ClientSession` and `rustls::ServerSession`
pub trait CommonSession: Session + std::io::Write + std::io::Read {}

impl<T> CommonSession for T where T: Session + std::io::Write + std::io::Read {}

pub use self::{
    network_handler::{
        EmptyCW, MessageHandler, MessageManager, NetworkPacketCW, NetworkRequestCW,
        NetworkResponseCW, PacketHandler, RequestHandler, ResponseHandler,
    },
    p2p_event::P2PEvent,
    seen_messages_list::SeenMessagesList,
};

use byteorder::{ByteOrder, NetworkEndian, WriteBytesExt};
use bytes::BytesMut;
use std::{
    cell::RefCell,
    collections::HashSet,
    convert::TryFrom,
    net::{Shutdown, SocketAddr},
    rc::Rc,
    sync::{atomic::Ordering, mpsc::Sender, Arc, RwLock},
};

use mio::{net::TcpStream, Event, Poll, PollOpt, Ready, Token};
use rustls::{ClientSession, ServerSession};

use failure::{bail, Error, Fallible};

use concordium_common::{
    functor::{FunctorResult, UnitFunction},
    UCursor,
};

use crate::{
    common::{
        counter::TOTAL_MESSAGES_RECEIVED_COUNTER,
        get_current_stamp,
        serialization::{Deserializable, ReadArchiveAdapter},
        P2PNodeId, P2PPeer, PeerType, RemotePeer,
    },
    network::{
        Buckets, NetworkId, NetworkMessage, NetworkRequest, NetworkResponse,
        PROTOCOL_HEADER_LENGTH, PROTOCOL_MAX_MESSAGE_SIZE, PROTOCOL_MESSAGE_LENGTH,
        PROTOCOL_MESSAGE_TYPE_LENGTH, PROTOCOL_WHOLE_PACKET_SIZE,
    },
    stats_export_service::StatsExportService,
};

#[cfg(not(target_os = "windows"))]
use crate::connection::writev_adapter::WriteVAdapter;
use crate::{
    connection::{
        connection_default_handlers::*,
        connection_handshake_handlers::*,
        connection_private::{ConnectionPrivate, ConnectionPrivateBuilder},
    },
    network::protocol_message_type::ProtocolMessageType,
};

/// This macro clones `dptr` and moves it into callback closure.
/// That closure is just a call to `fn` Fn.
macro_rules! handle_by_private {
    ($dptr:expr, $message_type:ty, $fn:ident) => {{
        let dptr_cloned = Rc::clone(&$dptr);
        make_atomic_callback!(move |m: $message_type| { $fn(&dptr_cloned, m) })
    }};
}

const PACKAGE_INITIAL_BUFFER_SZ: usize = 1024;
const PACKAGE_MAX_BUFFER_SZ: usize = 4096;

#[derive(PartialEq)]
pub enum ConnectionStatus {
    Untrusted,
    Established,
}

pub struct ConnectionBuilder {
    pub socket:              Option<TcpStream>,
    token:                   Option<Token>,
    blind_trusted_broadcast: Option<bool>,
    priv_conn_builder:       ConnectionPrivateBuilder,
}

impl Default for ConnectionBuilder {
    fn default() -> Self { ConnectionBuilder::new() }
}

impl ConnectionBuilder {
    pub fn new() -> ConnectionBuilder {
        ConnectionBuilder {
            socket:                  None,
            token:                   None,
            blind_trusted_broadcast: None,
            priv_conn_builder:       ConnectionPrivateBuilder::new(),
        }
    }

    pub fn build(self) -> Fallible<Connection> {
        let curr_stamp = get_current_stamp();
        let priv_conn = self.priv_conn_builder.build()?;
        if let (Some(socket), Some(token), Some(blind_trusted_broadcast)) =
            (self.socket, self.token, self.blind_trusted_broadcast)
        {
            let mut lself = Connection {
                socket,
                token,
                closing: false,
                closed: false,
                expected_size: 0,
                pkt_buffer: BytesMut::with_capacity(PACKAGE_INITIAL_BUFFER_SZ),
                messages_received: 0,
                messages_sent: 0,
                pkt_validated: false,
                pkt_valid: false,
                last_ping_sent: curr_stamp,
                dptr: Rc::new(RefCell::new(priv_conn)),
                message_handler: MessageHandler::new(),
                common_message_handler: Rc::new(RefCell::new(MessageHandler::new())),
                blind_trusted_broadcast,
                status: ConnectionStatus::Untrusted,
            };

            lself.setup_handshake_handler();
            Ok(lself)
        } else {
            bail!(fails::MissingFieldsConnectionBuilder)
        }
    }

    pub fn set_socket(mut self, s: TcpStream) -> ConnectionBuilder {
        self.socket = Some(s);
        self
    }

    pub fn set_token(mut self, t: Token) -> ConnectionBuilder {
        self.token = Some(t);
        self
    }

    pub fn set_local_peer(mut self, p: P2PPeer) -> ConnectionBuilder {
        self.priv_conn_builder = self.priv_conn_builder.set_local_peer(p);
        self
    }

    pub fn set_remote_peer(mut self, p: RemotePeer) -> ConnectionBuilder {
        self.priv_conn_builder = self.priv_conn_builder.set_remote_peer(p);
        self
    }

    pub fn set_local_end_networks(
        mut self,
        local_end_nets: Arc<RwLock<HashSet<NetworkId>>>,
    ) -> ConnectionBuilder {
        self.priv_conn_builder = self
            .priv_conn_builder
            .set_local_end_networks(local_end_nets);
        self
    }

    pub fn set_buckets(mut self, buckets: Arc<RwLock<Buckets>>) -> ConnectionBuilder {
        self.priv_conn_builder = self.priv_conn_builder.set_buckets(buckets);
        self
    }

    pub fn set_server_session(mut self, ss: Option<ServerSession>) -> ConnectionBuilder {
        self.priv_conn_builder = self.priv_conn_builder.set_server_session(ss);
        self
    }

    pub fn set_client_session(mut self, cs: Option<ClientSession>) -> ConnectionBuilder {
        self.priv_conn_builder = self.priv_conn_builder.set_client_session(cs);
        self
    }

    pub fn set_stats_export_service(
        mut self,
        pe: Option<Arc<RwLock<StatsExportService>>>,
    ) -> ConnectionBuilder {
        self.priv_conn_builder = self.priv_conn_builder.set_stats_export_service(pe);
        self
    }

    pub fn set_event_log(mut self, el: Option<Sender<P2PEvent>>) -> ConnectionBuilder {
        self.priv_conn_builder = self.priv_conn_builder.set_event_log(el);
        self
    }

    pub fn set_blind_trusted_broadcast(mut self, btb: bool) -> ConnectionBuilder {
        self.blind_trusted_broadcast = Some(btb);
        self.priv_conn_builder = self.priv_conn_builder.set_blind_trusted_broadcast(btb);
        self
    }
}

pub struct Connection {
    pub socket:              TcpStream,
    token:                   Token,
    pub closing:             bool,
    closed:                  bool,
    pkt_validated:           bool,
    pkt_valid:               bool,
    expected_size:           u32,
    pkt_buffer:              BytesMut,
    messages_sent:           u64,
    messages_received:       u64,
    last_ping_sent:          u64,
    blind_trusted_broadcast: bool,

    /// It stores internal info used in handles. In this way,
    /// handler's function will only need two arguments: this shared object, and
    /// the message which is going to be processed.
    dptr: Rc<RefCell<ConnectionPrivate>>,
    pub message_handler: MessageHandler,
    pub common_message_handler: Rc<RefCell<MessageHandler>>,
    status: ConnectionStatus,
}

impl Connection {
    // Setup handshake handler
    fn setup_handshake_handler(&mut self) {
        let cloned_message_handler = Rc::clone(&self.common_message_handler);
        self.message_handler
            .add_callback(make_atomic_callback!(move |msg: &NetworkMessage| {
                cloned_message_handler
                    .borrow()
                    .process_message(msg)
                    .map_err(Error::from)
            }))
            .add_request_callback(handle_by_private!(
                self.dptr,
                &NetworkRequest,
                handshake_request_handle
            ))
            .add_response_callback(handle_by_private!(
                self.dptr,
                &NetworkResponse,
                handshake_response_handle
            ));
    }

    // Setup message handler
    // ============================

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

    fn setup_message_handler(&mut self) {
        let request_handler = self.make_request_handler();
        let response_handler = self.make_response_handler();
        let last_seen_response_handler = self.make_update_last_seen_handler();
        let last_seen_packet_handler = self.make_update_last_seen_handler();
        let cloned_message_handler = Rc::clone(&self.common_message_handler);
        let dptr_1 = Rc::clone(&self.dptr);
        let dptr_2 = Rc::clone(&self.dptr);
        self.message_handler = MessageHandler::new();
        self.message_handler
            .add_callback(make_atomic_callback!(move |msg: &NetworkMessage| {
                cloned_message_handler
                    .borrow()
                    .process_message(msg)
                    .map_err(Error::from)
            }))
            .add_request_callback(make_atomic_callback!(move |req: &NetworkRequest| {
                request_handler.process_message(req).map_err(Error::from)
            }))
            .add_response_callback(make_atomic_callback!(move |res: &NetworkResponse| {
                response_handler.process_message(res).map_err(Error::from)
            }))
            .add_response_callback(last_seen_response_handler)
            .add_packet_callback(last_seen_packet_handler)
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

    fn clear_buffer(&mut self) {
        self.pkt_buffer.clear();
        self.expected_size = 0;
        self.pkt_valid = false;
        self.pkt_validated = false;
    }

    fn pkt_validated(&self) -> bool { self.pkt_validated }

    fn pkt_valid(&self) -> bool { self.pkt_valid }

    fn set_validated(&mut self) { self.pkt_validated = true; }

    fn set_valid(&mut self) { self.pkt_valid = true }

    pub fn failed_pkts(&self) -> u32 { self.dptr.borrow().failed_pkts }

    fn setup_buffer(&mut self, expected_size: u32) {
        // It resets packet buffer, to keep memory footprint low per connection.
        // Otherwise, it reuses the current buffer.
        if self.pkt_buffer.capacity() > PACKAGE_MAX_BUFFER_SZ {
            self.pkt_buffer = BytesMut::with_capacity(PACKAGE_INITIAL_BUFFER_SZ);
        } else {
            self.pkt_buffer.clear();
        }

        self.expected_size = expected_size;
        self.pkt_valid = false;
        self.pkt_validated = false;
    }

    /// It registers the connection socket, for read and write ops using *edge*
    /// notifications.
    pub fn register(&self, poll: &mut Poll) -> Fallible<()> {
        into_err!(poll.register(
            &self.socket,
            self.token,
            Ready::readable() | Ready::writable(),
            PollOpt::edge()
        ))
    }

    pub fn blind_trusted_broadcast(&self) -> bool { self.blind_trusted_broadcast }

    pub fn is_closed(&self) -> bool { self.closed }

    pub fn close(&mut self) {
        self.dptr.borrow_mut().tls_session.send_close_notify();
        self.closing = true;
    }

    pub fn shutdown(&mut self) -> Fallible<()> {
        self.socket.shutdown(Shutdown::Both)?;
        Ok(())
    }

    pub fn ready(&mut self, ev: &Event) -> Fallible<()> {
        let ev_readiness = ev.readiness();

        if ev_readiness.is_readable() {
            // Process pending reads.
            while let Ok(size) = self.do_tls_read() {
                if size == 0 {
                    break;
                }
                self.try_plain_read()?;
            }
        }

        if ev_readiness.is_writable() {
            let written_bytes = self.flush_tls()?;
            if written_bytes > 0 {
                trace!(
                    "EV readiness is WRITABLE, {} bytes were written",
                    written_bytes
                );
            }
        }

        let session_wants_read = self.dptr.borrow().tls_session.wants_read();
        if self.closing && !session_wants_read {
            let _ = self.socket.shutdown(Shutdown::Both);
            self.closed = true;
        }

        Ok(())
    }

    fn do_tls_read(&mut self) -> Fallible<usize> {
        let lptr = &mut self.dptr.borrow_mut();

        if lptr.tls_session.wants_read() {
            match lptr.tls_session.read_tls(&mut self.socket) {
                Ok(size) => {
                    if size > 0 {
                        into_err!(lptr.tls_session.process_new_packets())?;
                    }
                    Ok(size)
                }
                Err(read_err) => {
                    if read_err.kind() == std::io::ErrorKind::WouldBlock {
                        Ok(0)
                    } else {
                        self.closing = true;
                        into_err!(Err(read_err))
                    }
                }
            }
        } else {
            Ok(0)
        }
    }

    fn try_plain_read(&mut self) -> Fallible<()> {
        // Read and process all available plaintext.
        let mut read_buffer = Vec::new();

        let read_status = self
            .dptr
            .borrow_mut()
            .tls_session
            .read_to_end(&mut read_buffer);
        match read_status {
            Ok(_) => {
                trace!("plaintext read {:?}", read_buffer.len());

                let mut buf = read_buffer.as_slice();
                while !buf.is_empty() {
                    buf = self.incoming_plaintext(buf);
                }

                Ok(())
            }
            Err(read_err) => {
                self.closing = true;
                bail!(read_err);
            }
        }
    }

    fn write_to_tls(&mut self, bytes: &[u8]) -> Fallible<()> {
        self.messages_sent += 1;
        into_err!(self.dptr.borrow_mut().tls_session.write_all(bytes))
    }

    /// It decodes message from `buf` and processes it using its message
    /// handlers.
    fn process_complete_packet(&mut self, buf: Vec<u8>) -> FunctorResult<()> {
        let buf_cursor = UCursor::from(buf);

        let mut archive = ReadArchiveAdapter::new(
            buf_cursor,
            self.remote_peer().clone(),
            self.remote_addr().ip(),
        );
        let message = NetworkMessage::deserialize(&mut archive).map_err(|e| vec![e])?;
        let outer = Arc::new(message);

        self.messages_received += 1;
        TOTAL_MESSAGES_RECEIVED_COUNTER.fetch_add(1, Ordering::Relaxed);
        if let Some(ref service) = &self.stats_export_service() {
            if let Ok(mut slock) = safe_write!(service) {
                slock.pkt_received_inc();
            }
        };

        // Process message by message handler.
        self.message_handler.process_message(&outer)
    }

    fn validate_packet_type(&self, buf: &[u8]) -> Fallible<()> {
        if self.local_peer_type() == PeerType::Bootstrapper {
            if let Ok(msg_id_str) = std::str::from_utf8(&buf[..PROTOCOL_MESSAGE_TYPE_LENGTH]) {
                match ProtocolMessageType::try_from(msg_id_str) {
                    Ok(ProtocolMessageType::DirectMessage)
                    | Ok(ProtocolMessageType::BroadcastedMessage) => bail!(fails::PeerError {
                        message: "Wrong data type message received for node",
                    }),
                    _ => return Ok(()),
                }
            }
        }
        Ok(())
    }

    fn validate_packet(&mut self) {
        if !self.pkt_validated() && self.pkt_buffer.len() >= PROTOCOL_MESSAGE_LENGTH {
            let pkt_header: &[u8] =
                &self.pkt_buffer[PROTOCOL_HEADER_LENGTH..][..PROTOCOL_MESSAGE_TYPE_LENGTH];

            if self.validate_packet_type(pkt_header).is_err() {
                info!("Received network packet message, not wanted - disconnecting peer");
                self.clear_buffer();
                self.close();
            } else {
                self.set_valid();
                self.set_validated();
            }
        }
    }

    /// It appends data from `buf` into `self.pkt_buffer` taking account the
    /// `expected_size` for this message.
    /// As soon as message is completed, it is processed.
    ///
    /// # Return
    /// An slice with pending bytes from `buf` to be processed.
    fn append_and_process_message<'a>(&mut self, buf: &'a [u8]) -> &'a [u8] {
        // 1. We know message size: Append and check for packet completion
        let pending_read = self.expected_size - self.pkt_buffer.len() as u32;
        let max_to_append = std::cmp::min(buf.len(), pending_read as usize);

        self.pkt_buffer.extend_from_slice(&buf[..max_to_append]);
        trace!(
            "Append packet with {} bytes, new size is {} bytes",
            max_to_append,
            self.pkt_buffer.len()
        );

        if self.pkt_buffer.len() == self.expected_size as usize {
            trace!("Completed packet with {} size", self.pkt_buffer.len());
            self.validate_packet();
            if self.pkt_valid() || !self.pkt_validated() {
                let res = self.process_complete_packet(self.pkt_buffer.to_vec());
                self.drop_conn_if_unwanted(res)
            }
            self.clear_buffer();
        }

        &buf[max_to_append..]
    }

    fn drop_conn_if_unwanted(&mut self, process_result: FunctorResult<()>) {
        if let Err(e) = process_result {
            if let Some(f) = e
                .errors
                .iter()
                .find(|ref x| x.downcast_ref::<fails::UnwantedMessageError>().is_some())
            {
                error!("Dropping connection: {}", f);
                self.close();
            } else {
                e.errors.iter().for_each(|x| error!("{}", x));
            }
        } else if self.status == ConnectionStatus::Untrusted {
            self.setup_message_handler();
            debug!(
                "Succesfully executed handshake between {:?} and {:?}",
                self.local_peer(),
                self.remote_peer()
            );
            self.status = ConnectionStatus::Established;
        }
    }

    /// It tries to load the expected size for the next message.
    fn append_and_check_size<'a>(&mut self, buf: &'a [u8]) -> &'a [u8] {
        let mut message_offset = 0;

        // Prefetch 'expected size' into `pkt_buffer`.
        if self.pkt_buffer.len() < PROTOCOL_WHOLE_PACKET_SIZE {
            message_offset = std::cmp::min(
                buf.len(),
                PROTOCOL_WHOLE_PACKET_SIZE - self.pkt_buffer.len(),
            );
            self.pkt_buffer.extend_from_slice(&buf[..message_offset]);
        }

        // If we've got enough after prefetching, then we could got further
        if self.pkt_buffer.len() >= PROTOCOL_WHOLE_PACKET_SIZE {
            trace!(
                "Trying to read size from raw buffer of {} bytes",
                self.pkt_buffer.len()
            );

            #[cfg(test)]
            const_assert!(PROTOCOL_WHOLE_PACKET_SIZE >= 4);
            let expected_size =
                NetworkEndian::read_u32(&self.pkt_buffer[..PROTOCOL_WHOLE_PACKET_SIZE]);

            if self.expected_size as usize <= PROTOCOL_MAX_MESSAGE_SIZE {
                // Clean buffer and start to process know-size message.
                self.setup_buffer(expected_size);
                trace!("New message with expected size {} bytes", expected_size);

                self.append_and_process_message(&buf[message_offset..])
            } else {
                // force to drop connection, because remote peer is breaking the protocol.
                error!(
                    "Expected Message size is more that {} bytes, dropping that connection",
                    self.expected_size
                );
                self.close();
                &[]
            }
        } else {
            debug!(
                "We received only {} bytes, but we need at least {}",
                self.pkt_buffer.len(),
                PROTOCOL_MAX_MESSAGE_SIZE
            );
            &[]
        }
    }

    /// It tries to build a complete message before process it.
    /// This call should be used iteratively because it only works build a
    /// process one message at a time.
    ///
    /// # Return
    /// A slice of pending bytes to be processed.
    fn incoming_plaintext<'a>(&mut self, buf: &'a [u8]) -> &'a [u8] {
        trace!("Received plaintext");

        if self.expected_size > 0 {
            self.append_and_process_message(buf)
        } else {
            self.append_and_check_size(buf)
        }
    }

    pub fn serialize_bytes(&mut self, pkt: &[u8]) -> Fallible<usize> {
        trace!("Serializing data to connection {} bytes", pkt.len());
        let mut size_vec = Vec::with_capacity(4);

        size_vec.write_u32::<NetworkEndian>(pkt.len() as u32)?;
        self.write_to_tls(&size_vec[..])?;
        self.write_to_tls(pkt)?;

        self.flush_tls()
    }

    /// It tries to write into socket all pending to write.
    /// It returns how many bytes were writte into the socket.
    #[cfg(not(target_os = "windows"))]
    fn flush_tls(&mut self) -> Fallible<usize> {
        let wants_write =
            !self.closed && !self.closing && self.dptr.borrow().tls_session.wants_write();
        if wants_write {
            trace!(
                "{}/{} is attempting to write to socket {:?}",
                self.local_id(),
                self.local_addr(),
                self.socket
            );

            let mut wr = WriteVAdapter::new(&mut self.socket);
            let mut lptr = self.dptr.borrow_mut();
            match lptr.tls_session.writev_tls(&mut wr) {
                Err(e) => match e.kind() {
                    std::io::ErrorKind::WouldBlock
                    | std::io::ErrorKind::WriteZero
                    | std::io::ErrorKind::Interrupted => into_err!(Err(e)),
                    _ => {
                        self.closed = true;
                        into_err!(Err(e))
                    }
                },
                Ok(size) => Ok(size),
            }
        } else {
            Ok(0)
        }
    }

    #[cfg(target_os = "windows")]
    fn flush_tls(&mut self) -> Fallible<usize> {
        let wants_write =
            !self.closed && !self.closing && self.dptr.borrow().tls_session.wants_write();
        if wants_write {
            trace!(
                "{}/{} is attempting to write to socket {:?}",
                self.local_id(),
                self.local_addr(),
                self.socket
            );

            let mut lptr = self.dptr.borrow_mut();
            match lptr.tls_session.write_tls(&mut self.socket) {
                Err(e) => match e.kind() {
                    std::io::ErrorKind::WouldBlock
                    | std::io::ErrorKind::WriteZero
                    | std::io::ErrorKind::Interrupted => into_err!(Err(e)),
                    _ => {
                        self.closed = true;
                        into_err!(Err(e))
                    }
                },
                Ok(size) => Ok(size),
            }
        } else {
            Ok(0)
        }
    }

    pub fn stats_export_service(&self) -> Option<Arc<RwLock<StatsExportService>>> {
        self.dptr.borrow().stats_export_service.clone()
    }

    pub fn local_peer_type(&self) -> PeerType { self.dptr.borrow().local_peer.peer_type() }

    pub fn remote_peer_type(&self) -> PeerType { self.dptr.borrow().remote_peer.peer_type() }

    pub fn buckets(&self) -> Arc<RwLock<Buckets>> { Arc::clone(&self.dptr.borrow().buckets) }

    pub fn promote_to_post_handshake(&mut self, id: P2PNodeId, addr: SocketAddr) -> Fallible<()> {
        self.dptr.borrow_mut().promote_to_post_handshake(id, addr)
    }

    pub fn remote_end_networks(&self) -> HashSet<NetworkId> {
        self.dptr.borrow().remote_end_networks.clone()
    }

    pub fn local_end_networks(&self) -> Arc<RwLock<HashSet<NetworkId>>> {
        Arc::clone(&self.dptr.borrow().local_end_networks)
    }

    pub fn token(&self) -> Token { self.token }
}

#[cfg(test)]
mod tests {
    use super::{PACKAGE_INITIAL_BUFFER_SZ, PACKAGE_MAX_BUFFER_SZ};
    use bytes::BytesMut;
    use rand::{distributions::Standard, thread_rng, Rng};

    pub struct BytesMutConn {
        pkt_buffer: BytesMut,
    }

    /// Simulate allocation/deallocation of `Connection.pkt_buffer`.
    fn check_bytes_mut_drop(pkt_size: usize) {
        assert!(pkt_size > PACKAGE_MAX_BUFFER_SZ);

        // 1. Allocate buffer with initial capacity.
        let mut a1 = BytesMutConn {
            pkt_buffer: BytesMut::with_capacity(PACKAGE_INITIAL_BUFFER_SZ),
        };

        // 2. Simulate reception of X bytes.
        let content: Vec<u8> = thread_rng().sample_iter(&Standard).take(pkt_size).collect();

        for chunk in content.chunks(1024) {
            a1.pkt_buffer.extend_from_slice(chunk);
        }
        assert_eq!(pkt_size, a1.pkt_buffer.len());
        assert!(a1.pkt_buffer.capacity() >= pkt_size);

        // 3. Reset
        a1.pkt_buffer = BytesMut::with_capacity(PACKAGE_INITIAL_BUFFER_SZ);
        assert_eq!(PACKAGE_INITIAL_BUFFER_SZ, a1.pkt_buffer.capacity());
        assert_eq!(0, a1.pkt_buffer.len());
    }

    #[test]
    fn check_bytes_mut_drop_128k() { check_bytes_mut_drop(128 * 1024); }

    #[test]
    fn check_bytes_mut_drop_512k() { check_bytes_mut_drop(512 * 1024); }

    #[test]
    fn check_bytes_mut_drop_8m() { check_bytes_mut_drop(8 * 1024 * 1024); }

}
