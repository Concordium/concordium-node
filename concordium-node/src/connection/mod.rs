pub mod network_handler;

mod p2p_event;
mod seen_messages_list;
mod writev_adapter;

mod connection_default_handlers;
mod connection_handshake_handlers;
mod connection_private;
pub mod fails;
mod handler_utils;

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

use byteorder::{NetworkEndian, ReadBytesExt, WriteBytesExt};
use bytes::{BufMut, BytesMut};
use std::{
    cell::RefCell,
    collections::HashSet,
    convert::TryFrom,
    io::Cursor,
    net::{Shutdown, SocketAddr},
    rc::Rc,
    sync::{atomic::Ordering, mpsc::Sender, Arc, RwLock},
};

use mio::{net::TcpStream, Event, Poll, PollOpt, Ready, Token};
use rustls::{ClientSession, ServerSession};

use failure::{bail, Error, Fallible};

use crate::{
    common::{
        counter::TOTAL_MESSAGES_RECEIVED_COUNTER,
        functor::{AFunctorCW, FunctorError, FunctorResult},
        serialization::{ Deserializable, IOReadArchiveAdapter },
        get_current_stamp, P2PNodeId, P2PPeer, PeerType, RemotePeer, UCursor,
    },
    network::{
        Buckets, NetworkId, NetworkMessage, NetworkRequest, NetworkResponse,
        PROTOCOL_HEADER_LENGTH, PROTOCOL_MESSAGE_LENGTH, PROTOCOL_MESSAGE_TYPE_LENGTH,
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

macro_rules! drop_conn_if_unwanted {
    ($process:expr, $self:ident) => {
        if let Err(e) = $process {
            match e.downcast::<fails::UnwantedMessageError>() {
                Ok(f) => {
                    error!("Dropping connection: {}", f);
                    $self.close();
                }
                Err(e) => {
                    if let Ok(f) = e.downcast::<FunctorError>() {
                        f.errors.iter().for_each(|x| {
                            if x.to_string().contains("SendError(..)") {
                                trace!("Send Error in incoming plaintext");
                            } else {
                                error!("{}", x);
                            }
                        });
                    }
                }
            }
        } else if $self.status == ConnectionStatus::Untrusted {
            $self.setup_message_handler();
            debug!(
                "Succesfully executed handshake between {:?} and {:?}",
                $self.local_peer(),
                $self.remote_peer()
            );
            $self.status = ConnectionStatus::Established;
        }
    };
}

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
                currently_read: 0,
                expected_size: 0,
                pkt_buffer: None,
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
    currently_read:          u32,
    pkt_validated:           bool,
    pkt_valid:               bool,
    expected_size:           u32,
    pkt_buffer:              Option<BytesMut>,
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

    fn make_update_last_seen_handler<T>(&self) -> AFunctorCW<T> {
        let priv_conn = Rc::clone(&self.dptr);

        make_atomic_callback!(move |_: &T| -> FunctorResult {
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

    fn append_buffer(&mut self, new_data: &[u8]) {
        if let Some(ref mut buf) = self.pkt_buffer {
            buf.reserve(new_data.len());
            buf.put_slice(new_data);
            self.currently_read += new_data.len() as u32;
        }
    }

    fn update_buffer_read_stats(&mut self, buf_len: u32) { self.currently_read += buf_len; }

    pub fn local_peer(&self) -> P2PPeer { self.dptr.borrow().local_peer.clone() }

    pub fn remote_peer(&self) -> RemotePeer { self.dptr.borrow().remote_peer().to_owned() }

    pub fn get_messages_received(&self) -> u64 { self.messages_received }

    pub fn get_messages_sent(&self) -> u64 { self.messages_sent }

    fn clear_buffer(&mut self) {
        if let Some(ref mut buf) = self.pkt_buffer {
            buf.clear();
        }
        self.currently_read = 0;
        self.expected_size = 0;
        self.pkt_buffer = None;
        self.pkt_valid = false;
        self.pkt_validated = false;
    }

    fn pkt_validated(&self) -> bool { self.pkt_validated }

    fn pkt_valid(&self) -> bool { self.pkt_valid }

    fn set_validated(&mut self) { self.pkt_validated = true; }

    fn set_valid(&mut self) { self.pkt_valid = true }

    pub fn failed_pkts(&self) -> u32 { self.dptr.borrow().failed_pkts }

    fn setup_buffer(&mut self) {
        self.pkt_buffer = Some(BytesMut::with_capacity(1024));
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

    pub fn close(&mut self) { self.closing = true; }

    pub fn shutdown(&mut self) -> Fallible<()> {
        self.socket.shutdown(Shutdown::Both)?;
        Ok(())
    }

    pub fn ready(
        &mut self,
        poll: &mut Poll,
        ev: &Event,
        packets_queue: &Sender<Arc<NetworkMessage>>,
    ) -> Fallible<()> {
        let ev_readiness = ev.readiness();

        if ev_readiness.is_readable() {
            // Process pending reads.
            while let Ok(size) = self.do_tls_read() {
                if size == 0 {
                    break;
                }
                self.try_plain_read(poll, packets_queue)?;
            }
        }

        if ev_readiness.is_writable() {
            let written_bytes = self.flush_tls()?;
            if written_bytes > 0 {
                debug!(
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

    fn try_plain_read(
        &mut self,
        poll: &mut Poll,
        packets_queue: &Sender<Arc<NetworkMessage>>,
    ) -> Fallible<()> {
        // Read and process all available plaintext.
        let mut buf = Vec::new();

        let read_status = self.dptr.borrow_mut().tls_session.read_to_end(&mut buf);
        match read_status {
            Ok(_) => {
                if !buf.is_empty() {
                    trace!("plaintext read {:?}", buf.len());
                    self.incoming_plaintext(poll, packets_queue, &buf)
                } else {
                    Ok(())
                }
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
    fn process_complete_packet(&mut self, buf: Vec<u8>) -> FunctorResult {
        let buf_cursor = UCursor::from(buf);

        let mut archive = IOReadArchiveAdapter::new( buf_cursor, self.remote_peer().clone(), self.remote_addr().ip());
        let message = NetworkMessage::deserialize( &mut archive)?;
        let outer = Arc::new( message);

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
        if !self.pkt_validated() {
            let buff = if let Some(ref bytebuf) = self.pkt_buffer {
                if bytebuf.len() >= PROTOCOL_MESSAGE_LENGTH {
                    Some(bytebuf[PROTOCOL_HEADER_LENGTH..][..PROTOCOL_MESSAGE_TYPE_LENGTH].to_vec())
                } else {
                    None
                }
            } else {
                None
            };
            if let Some(ref bufdata) = buff {
                if self.validate_packet_type(bufdata).is_err() {
                    info!("Received network packet message, not wanted - disconnecting peer");
                    self.clear_buffer();
                    self.close();
                } else {
                    self.set_valid();
                    self.set_validated();
                }
            }
        }
    }

    fn read_with_matching_sizes(
        &mut self,
        poll: &mut Poll,
        packets_queue: &Sender<Arc<NetworkMessage>>,
        buf: &[u8],
    ) -> Fallible<()> {
        trace!("Completed packet with {} size", self.currently_read);
        if self.pkt_valid() || !self.pkt_validated() {
            let mut buffered = Vec::new();
            if let Some(ref mut buf) = self.pkt_buffer {
                buffered = buf[..].to_vec();
            }
            self.validate_packet_type(&buffered)?;
            drop_conn_if_unwanted!(self.process_complete_packet(buffered), self)
        }

        self.clear_buffer();
        self.incoming_plaintext(poll, packets_queue, buf)?;
        Ok(())
    }

    fn read_with_less_than_needed(&mut self, buf: &[u8]) -> Fallible<()> {
        if self.pkt_valid() || !self.pkt_validated() {
            self.append_buffer(&buf);
        } else {
            self.update_buffer_read_stats(buf.len() as u32);
        }
        if self.expected_size == self.currently_read {
            trace!("Completed packet with {} size", self.currently_read);
            if self.pkt_valid() || !self.pkt_validated() {
                let mut buffered = Vec::new();
                if let Some(ref mut buf) = self.pkt_buffer {
                    buffered = buf[..].to_vec();
                }
                self.validate_packet_type(&buffered)?;
                drop_conn_if_unwanted!(self.process_complete_packet(buffered), self)
            }
            self.clear_buffer();
        }
        Ok(())
    }

    fn read_with_more_than_needed(
        &mut self,
        poll: &mut Poll,
        packets_queue: &Sender<Arc<NetworkMessage>>,
        buf: &[u8],
    ) -> Fallible<()> {
        trace!("Got more buffer than needed");
        let to_take = self.expected_size - self.currently_read;
        if self.pkt_valid() || !self.pkt_validated() {
            self.append_buffer(&buf[..to_take as usize]);
            let mut buffered = Vec::new();
            if let Some(ref mut buf) = self.pkt_buffer {
                buffered = buf[..].to_vec();
            }
            self.validate_packet_type(&buffered)?;
            drop_conn_if_unwanted!(self.process_complete_packet(buffered), self)
        }
        self.clear_buffer();
        self.incoming_plaintext(poll, &packets_queue, &buf[to_take as usize..])?;
        Ok(())
    }

    fn read_packet_size(
        &mut self,
        poll: &mut Poll,
        packets_queue: &Sender<Arc<NetworkMessage>>,
        buf: &[u8],
    ) -> Fallible<()> {
        trace!("Trying to read size");
        let _buf = &buf[..4].to_vec();
        let mut size_bytes = Cursor::new(_buf);
        self.expected_size = size_bytes
            .read_u32::<NetworkEndian>()
            .expect("Couldn't read from buffer on incoming plaintext");
        if self.expected_size > 268_435_456 {
            error!("Packet can't be bigger than 256MB");
            self.expected_size = 0;
            self.incoming_plaintext(poll, &packets_queue, &buf[4..])?;
        } else {
            self.setup_buffer();
            if buf.len() > 4 {
                trace!("Got enough to read it...");
                self.incoming_plaintext(poll, &packets_queue, &buf[4..])?;
            }
        };
        Ok(())
    }

    fn incoming_plaintext(
        &mut self,
        poll: &mut Poll,
        packets_queue: &Sender<Arc<NetworkMessage>>,
        buf: &[u8],
    ) -> Fallible<()> {
        trace!("Received plaintext");
        self.validate_packet();
        if self.expected_size > 0 && self.currently_read == self.expected_size {
            self.read_with_matching_sizes(poll, packets_queue, buf)
        } else if self.expected_size > 0
            && buf.len() <= (self.expected_size as usize - self.currently_read as usize)
        {
            self.read_with_less_than_needed(buf)
        } else if self.expected_size > 0
            && buf.len() > (self.expected_size as usize - self.currently_read as usize)
        {
            self.read_with_more_than_needed(poll, packets_queue, buf)
        } else if buf.len() >= 4 {
            self.read_packet_size(poll, packets_queue, buf)
        } else {
            bail!(fails::NotEnoughBytesToRead)
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
    fn flush_tls(&mut self) -> Fallible<usize> {
        let wants_write =
            self.dptr.borrow().tls_session.wants_write() && !self.closed && !self.closing;
        if wants_write {
            debug!(
                "{}/{} is attempting to write to socket {:?}",
                self.local_id(),
                self.local_addr(),
                self.socket
            );

            let mut wr = WriteVAdapter::new(&mut self.socket);
            let mut lptr = self.dptr.borrow_mut();
            match lptr.tls_session.writev_tls(&mut wr) {
                Err(e) => match e.kind() {
                    std::io::ErrorKind::WouldBlock => {
                        Err(failure::Error::from_boxed_compat(Box::new(e)))
                    }
                    std::io::ErrorKind::WriteZero => {
                        Err(failure::Error::from_boxed_compat(Box::new(e)))
                    }
                    std::io::ErrorKind::Interrupted => {
                        Err(failure::Error::from_boxed_compat(Box::new(e)))
                    }
                    _ => {
                        self.closed = true;
                        Err(failure::Error::from_boxed_compat(Box::new(e)))
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
