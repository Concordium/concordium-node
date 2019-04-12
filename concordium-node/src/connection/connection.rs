use byteorder::{NetworkEndian, ReadBytesExt, WriteBytesExt};
use bytes::{BufMut, BytesMut};
use std::{
    cell::RefCell,
    collections::HashSet,
    io::Cursor,
    net::{IpAddr, Shutdown},
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
        get_current_stamp, ConnectionType, P2PNodeId, P2PPeer, UCursor,
    },
    connection::{MessageHandler, P2PEvent, P2PNodeMode, RequestHandler, ResponseHandler},
    network::{
        Buckets, NetworkMessage, NetworkRequest, NetworkResponse,
        PROTOCOL_MESSAGE_TYPE_BROADCASTED_MESSAGE, PROTOCOL_MESSAGE_TYPE_DIRECT_MESSAGE,
        PROTOCOL_HEADER_LENGTH, PROTOCOL_MESSAGE_LENGTH, PROTOCOL_MESSAGE_TYPE_LENGTH,
    },
    prometheus_exporter::PrometheusServer,
};

use super::fails;
#[cfg(not(target_os = "windows"))]
use crate::connection::writev_adapter::WriteVAdapter;
use crate::connection::{
    connection_default_handlers::*, connection_handshake_handlers::*,
    connection_private::ConnectionPrivate,
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
    ($process:expr, $self:ident, $poll:ident) => {
        if let Err(e) = $process {
            match e.downcast::<fails::UnwantedMessageError>() {
                Ok(f) => {
                    error!("Dropping connection: {}", f);
                    $self.close($poll)?;
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
                $self.get_self_peer(),
                $self.get_remote_peer()
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

pub struct Connection {
    socket:                  TcpStream,
    token:                   Token,
    pub closing:             bool,
    closed:                  bool,
    currently_read:          u32,
    pkt_validated:           bool,
    pkt_valid:               bool,
    peer_ip:                 IpAddr,
    peer_port:               u16,
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
    pub fn new(
        connection_type: ConnectionType,
        socket: TcpStream,
        token: Token,
        tls_server_session: Option<ServerSession>,
        tls_client_session: Option<ClientSession>,
        own_id: P2PNodeId,
        self_peer: P2PPeer,
        peer_ip: IpAddr,
        peer_port: u16,
        mode: P2PNodeMode,
        prometheus_exporter: Option<Arc<RwLock<PrometheusServer>>>,
        event_log: Option<Sender<P2PEvent>>,
        own_networks: Arc<RwLock<HashSet<u16>>>,
        buckets: Arc<RwLock<Buckets>>,
        blind_trusted_broadcast: bool,
    ) -> Self {
        let curr_stamp = get_current_stamp();
        let priv_conn = Rc::new(RefCell::new(ConnectionPrivate::new(
            connection_type,
            mode,
            own_id,
            self_peer,
            own_networks,
            buckets,
            tls_server_session,
            tls_client_session,
            prometheus_exporter,
            event_log,
            blind_trusted_broadcast,
        )));

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
            peer_ip,
            peer_port,
            pkt_validated: false,
            pkt_valid: false,
            last_ping_sent: curr_stamp,
            dptr: priv_conn,
            message_handler: MessageHandler::new(),
            common_message_handler: Rc::new(RefCell::new(MessageHandler::new())),
            blind_trusted_broadcast,
            status: ConnectionStatus::Untrusted,
        };

        lself.setup_handshake_handler();
        lself
    }

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
            .add_unknown_callback(handle_by_private!(self.dptr, &(), default_unknown_message))
            .add_invalid_callback(handle_by_private!(self.dptr, &(), default_invalid_message));
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

    pub fn id(&self) -> P2PNodeId { self.dptr.borrow().own_id }

    pub fn ip(&self) -> IpAddr { self.peer_ip }

    pub fn port(&self) -> u16 { self.peer_port }

    pub fn last_seen(&self) -> u64 { self.dptr.borrow().last_seen() }

    fn append_buffer(&mut self, new_data: &[u8]) {
        if let Some(ref mut buf) = self.pkt_buffer {
            buf.reserve(new_data.len());
            buf.put_slice(new_data);
            self.currently_read += new_data.len() as u32;
        }
    }

    fn update_buffer_read_stats(&mut self, buf_len: u32) { self.currently_read += buf_len; }

    pub fn get_self_peer(&self) -> P2PPeer { self.dptr.borrow().self_peer.clone() }

    fn get_remote_peer(&self) -> Option<P2PPeer> { self.dptr.borrow().peer().to_owned() }

    pub fn get_messages_received(&self) -> u64 { self.messages_received }

    pub fn get_messages_sent(&self) -> u64 { self.messages_sent }

    fn clear_buffer(&mut self) {
        if let Some(ref mut buf) = self.pkt_buffer {
            buf.clear();
        }
        self.currently_read = 0;
        self.expected_size = 0;
        self.pkt_buffer = None;
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

    #[allow(unused)]
    pub fn blind_trusted_broadcast(&self) -> bool { self.blind_trusted_broadcast }

    pub fn is_closed(&self) -> bool { self.closed }

    pub fn close(&mut self, poll: &mut Poll) -> Fallible<()> {
        self.closing = true;
        poll.deregister(&self.socket)?;
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

        // Manage clossing event.
        if self.closing {
            self.close(poll).unwrap_or_else(|e| error!("{}", e));
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
        let outer = Arc::new(NetworkMessage::deserialize(
            self.get_remote_peer(),
            self.ip(),
            buf_cursor,
        ));
        self.messages_received += 1;
        TOTAL_MESSAGES_RECEIVED_COUNTER.fetch_add(1, Ordering::Relaxed);
        if let Some(ref prom) = &self.prometheus_exporter() {
            if let Ok(mut plock) = safe_write!(prom) {
                plock.pkt_received_inc().unwrap_or_else(|e| error!("{}", e));
            }
        };

        // Process message by message handler.
        self.message_handler.process_message(&outer)
    }

    fn validate_packet(&mut self, poll: &mut Poll) {
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
                if self.mode() == P2PNodeMode::BootstrapperMode {
                    let msg_num = String::from_utf8(bufdata.to_vec())
                        .expect("Unable to get string from utf8");
                    if msg_num == PROTOCOL_MESSAGE_TYPE_DIRECT_MESSAGE
                        || msg_num == PROTOCOL_MESSAGE_TYPE_BROADCASTED_MESSAGE
                    {
                        info!("Received network packet message, not wanted - disconnecting peer");
                        &self.clear_buffer();
                        &self.close(poll);
                    }
                } else {
                    self.set_valid();
                    self.set_validated();
                }
            }
        }
    }

    fn incoming_plaintext(
        &mut self,
        poll: &mut Poll,
        packets_queue: &Sender<Arc<NetworkMessage>>,
        buf: &[u8],
    ) -> Fallible<()> {
        trace!("Received plaintext");
        self.validate_packet(poll);
        if self.expected_size > 0 && self.currently_read == self.expected_size {
            trace!("Completed packet with {} size", self.currently_read);
            if self.pkt_valid() || !self.pkt_validated() {
                let mut buffered = Vec::new();
                if let Some(ref mut buf) = self.pkt_buffer {
                    buffered = buf[..].to_vec();
                }
                drop_conn_if_unwanted!(self.process_complete_packet(buffered), self, poll)
            }

            self.clear_buffer();
            self.incoming_plaintext(poll, packets_queue, buf)?;
        } else if self.expected_size > 0
            && buf.len() <= (self.expected_size as usize - self.currently_read as usize)
        {
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
                    drop_conn_if_unwanted!(self.process_complete_packet(buffered), self, poll)
                }
                self.clear_buffer();
            }
        } else if self.expected_size > 0
            && buf.len() > (self.expected_size as usize - self.currently_read as usize)
        {
            trace!("Got more buffer than needed");
            let to_take = self.expected_size - self.currently_read;
            if self.pkt_valid() || !self.pkt_validated() {
                self.append_buffer(&buf[..to_take as usize]);
                let mut buffered = Vec::new();
                if let Some(ref mut buf) = self.pkt_buffer {
                    buffered = buf[..].to_vec();
                }
                drop_conn_if_unwanted!(self.process_complete_packet(buffered), self, poll)
            }
            self.clear_buffer();
            self.incoming_plaintext(poll, &packets_queue, &buf[to_take as usize..])?;
        } else if buf.len() >= 4 {
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
            }
        }
        Ok(())
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
        let wants_write = self.dptr.borrow().tls_session.wants_write() && !self.closed && !self.closing;
        if wants_write {
            debug!(
                "{}/{}:{} is attempting to write to socket {:?}",
                self.id(),
                self.ip(),
                self.port(),
                self.socket
            );

            let mut wr = WriteVAdapter::new(&mut self.socket);
            let mut lptr = self.dptr.borrow_mut();
            match lptr.tls_session.writev_tls(&mut wr) {
                Err(e) => {
                    match e.kind() {
                        std::io::ErrorKind::WouldBlock => Err(failure::Error::from_boxed_compat(Box::new(e))),
                        std::io::ErrorKind::WriteZero => Err(failure::Error::from_boxed_compat(Box::new(e))),
                        std::io::ErrorKind::Interrupted => Err(failure::Error::from_boxed_compat(Box::new(e))),
                        _ => {
                            self.closed = true;
                            Err(failure::Error::from_boxed_compat(Box::new(e)))
                        }
                    }
                }
                Ok(size) => Ok(size) 
            }
        } else {
            Ok(0)
        }
    }

    pub fn prometheus_exporter(&self) -> Option<Arc<RwLock<PrometheusServer>>> {
        self.dptr.borrow().prometheus_exporter.clone()
    }

    pub fn mode(&self) -> P2PNodeMode { self.dptr.borrow().mode }

    pub fn buckets(&self) -> Arc<RwLock<Buckets>> { Arc::clone(&self.dptr.borrow().buckets) }

    pub fn own_id(&self) -> P2PNodeId { self.dptr.borrow().own_id }

    pub fn peer(&self) -> Option<P2PPeer> { self.dptr.borrow().peer().to_owned() }

    pub fn set_peer(&mut self, peer: P2PPeer) { self.dptr.borrow_mut().set_peer(peer); }

    pub fn networks(&self) -> HashSet<u16> { self.dptr.borrow().networks.clone() }

    pub fn connection_type(&self) -> ConnectionType { self.dptr.borrow().connection_type }

    pub fn own_networks(&self) -> Arc<RwLock<HashSet<u16>>> {
        Arc::clone(&self.dptr.borrow().own_networks)
    }

    pub fn token(&self) -> &Token { &self.token }
}
