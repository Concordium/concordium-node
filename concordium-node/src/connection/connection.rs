use bytes::{ BufMut, BytesMut };
use byteorder::{NetworkEndian, ReadBytesExt, WriteBytesExt};
use std::sync::{ Arc, Mutex, RwLock };
use std::sync::mpsc::{ Sender };
use std::net::{Shutdown, IpAddr };
use std::rc::{ Rc };
use std::cell::{ RefCell };
use std::io::{ Cursor, Result };
use atomic_counter::AtomicCounter;

use mio::{ Poll, PollOpt, Ready, Token, Event, net::TcpStream };
use rustls::{ ServerSession, ClientSession };
use crate::prometheus_exporter::{ PrometheusServer };

use crate::errors::{ ResultExtWrapper, ErrorKindWrapper, ErrorWrapper };
use error_chain::ChainedError;

use crate::common::{ ConnectionType, P2PNodeId, P2PPeer, get_current_stamp };
use crate::common::counter::{ TOTAL_MESSAGES_RECEIVED_COUNTER };
use crate::common::functor::{ AFunctorCW, FunctorResult };
use crate::network::{ NetworkMessage, NetworkRequest, NetworkResponse, Buckets,
    PROTOCOL_MESSAGE_TYPE_DIRECT_MESSAGE, PROTOCOL_MESSAGE_TYPE_BROADCASTED_MESSAGE };

use crate::connection::{
    MessageHandler, RequestHandler, ResponseHandler,
    P2PEvent, P2PNodeMode };

use crate::connection::connection_private::{ ConnectionPrivate };
#[cfg(not(target_os = "windows"))]
use crate::connection::writev_adapter::{ WriteVAdapter };
use crate::connection::connection_default_handlers::*;


/// This macro clones `dptr` and moves it into callback closure.
/// That closure is just a call to `fn` Fn.
macro_rules! handle_by_private {
    ($dptr:expr, $message_type:ty, $fn: ident) => {{
        let dptr_cloned = $dptr.clone();
        make_atomic_callback!( move |m: $message_type| {
            $fn( &dptr_cloned, m)
        })
    }}
}

pub struct Connection {
    socket: TcpStream,
    token: Token,
    pub closing: bool,
    closed: bool,
    currently_read: u32,
    pkt_validated: bool,
    pkt_valid: bool,
    peer_ip: IpAddr,
    peer_port: u16,
    expected_size: u32,
    pkt_buffer: Option<BytesMut>,
    messages_sent: u64,
    messages_received: u64,
    last_ping_sent: u64,
    blind_trusted_broadcast: bool,


    /// It stores internal info used in handles. In this way,
    /// handler's function will only need two arguments: this shared object, and
    /// the message which is going to be processed.
    dptr: Rc< RefCell< ConnectionPrivate >>,
    pub message_handler: MessageHandler
}

impl Connection {
    pub fn new(connection_type: ConnectionType,
           socket: TcpStream,
           token: Token,
           tls_server_session: Option< ServerSession>,
           tls_client_session: Option< ClientSession>,
           own_id: P2PNodeId,
           self_peer: P2PPeer,
           peer_ip: IpAddr,
           peer_port: u16,
           mode: P2PNodeMode,
           prometheus_exporter: Option<Arc<Mutex<PrometheusServer>>>,
           event_log: Option<Sender<P2PEvent>>,
           own_networks: Arc<Mutex<Vec<u16>>>,
           buckets: Arc< RwLock< Buckets > >,
           blind_trusted_broadcast: bool,)
           -> Self {

        let curr_stamp = get_current_stamp();
        let priv_conn = Rc::new( RefCell::new( ConnectionPrivate::new(
                connection_type, mode, own_id, self_peer, own_networks, buckets,
                tls_server_session, tls_client_session,
                prometheus_exporter, event_log, blind_trusted_broadcast)));

        let mut lself = Connection {
                     socket: socket,
                     token,
                     closing: false,
                     closed: false,
                     currently_read: 0,
                     expected_size: 0,
                     pkt_buffer: None,
                     messages_received: 0,
                     messages_sent: 0,
                     peer_ip: peer_ip,
                     peer_port: peer_port,
                     pkt_validated: false,
                     pkt_valid: false,
                     last_ping_sent: curr_stamp,
                     dptr: priv_conn,
                     message_handler: MessageHandler::new(),
                     blind_trusted_broadcast,
        };

        lself.setup_message_handler();
        lself
    }

    // Setup message handler
    // ============================

    fn make_update_last_seen_handler<T>(&self) -> AFunctorCW<T>{
        let priv_conn = self.dptr.clone();

        make_atomic_callback!( move |_: &T| -> FunctorResult {
            priv_conn.borrow_mut().update_last_seen();
            Ok(())
        })
    }

    fn make_request_handler(&mut self) -> RequestHandler {
        let update_last_seen_handler = self.make_update_last_seen_handler();

        let mut rh = RequestHandler::new();
        rh.add_ping_callback(
                handle_by_private!( self.dptr, &NetworkRequest,
                                    default_network_request_ping_handle))
            .add_find_node_callback(
                handle_by_private!( self.dptr, &NetworkRequest,
                                    default_network_request_find_node_handle))
            .add_get_peers_callback(
                handle_by_private!( self.dptr, &NetworkRequest,
                                    default_network_request_get_peers))
            .add_join_network_callback(
                handle_by_private!( self.dptr, &NetworkRequest,
                                    default_network_request_join_network))
            .add_leave_network_callback(
                handle_by_private!( self.dptr, &NetworkRequest,
                                    default_network_request_leave_network))
            .add_handshake_callback(
                handle_by_private!( self.dptr, &NetworkRequest,
                                    default_network_request_handshake))
            .add_ban_node_callback( update_last_seen_handler.clone())
            .add_unban_node_callback( update_last_seen_handler.clone())
            .add_join_network_callback( update_last_seen_handler.clone())
            .add_leave_network_callback( update_last_seen_handler.clone());

        rh
    }

    fn make_response_handler(&mut self) -> ResponseHandler {
        let mut rh = ResponseHandler::new();

        rh.add_find_node_callback(
                handle_by_private!( self.dptr, &NetworkResponse,
                                    default_network_response_find_node))
            .add_pong_callback(
                handle_by_private!( self.dptr, &NetworkResponse,
                                    default_network_response_pong))
            .add_handshake_callback(
                handle_by_private!( self.dptr, &NetworkResponse,
                                    default_network_response_handshake))
            .add_peer_list_callback(
                handle_by_private!( self.dptr, &NetworkResponse,
                                    default_network_response_peer_list));

        rh
    }

    fn setup_message_handler(&mut self) {
        let request_handler = self.make_request_handler();
        let response_handler = self.make_response_handler();
        let last_seen_response_handler = self.make_update_last_seen_handler();
        let last_seen_packet_handler = self.make_update_last_seen_handler();

        self.message_handler
            .add_request_callback( make_atomic_callback!(
                move |req: &NetworkRequest| { (request_handler)(req) }))
            .add_response_callback(  make_atomic_callback!(
                move |res: &NetworkResponse| { (response_handler)(res) }))
            .add_response_callback( last_seen_response_handler)
            .add_packet_callback( last_seen_packet_handler)
            .add_unknown_callback(
                handle_by_private!( self.dptr, &(), default_unknown_message))
            .add_invalid_callback(
                handle_by_private!( self.dptr, &(), default_invalid_message));
    }

    // =============================

    pub fn get_last_latency_measured(&self) -> Option<u64> {
        let latency : u64 = self.dptr.borrow().last_latency_measured;
        if latency != u64::max_value() {
            Some( latency)
        } else {
            None
        }
    }

    pub fn set_measured_handshake_sent(&mut self) {
        self.dptr.borrow_mut().sent_handshake = get_current_stamp()
    }

    pub fn set_measured_ping_sent(&mut self) {
        self.dptr.borrow_mut().set_measured_ping_sent();
    }

    pub fn get_last_ping_sent(&self) -> u64 {
        self.last_ping_sent
    }

    pub fn set_last_ping_sent(&mut self) {
        self.last_ping_sent = get_current_stamp();
    }

    pub fn ip(&self) -> IpAddr {
        self.peer_ip.clone()
    }

    pub fn port(&self) -> u16 {
        self.peer_port.clone()
    }

    pub fn last_seen(&self) -> u64 {
        self.dptr.borrow().last_seen()
    }

    fn append_buffer(&mut self, new_data: &[u8]) {
        if let Some(ref mut buf) = self.pkt_buffer {
            buf.reserve(new_data.len());
            buf.put_slice(new_data);
            self.currently_read += new_data.len() as u32;
        }
    }

    fn update_buffer_read_stats(&mut self, buf_len: u32) {
        self.currently_read += buf_len;
    }

    pub fn get_self_peer(&self) -> P2PPeer {
        self.dptr.borrow().self_peer.clone()
    }

    fn get_peer(&self) -> Option<P2PPeer> {
        self.dptr.borrow().peer().clone()
    }

    pub fn get_messages_received(&self) -> u64 {
        self.messages_received
    }

    pub fn get_messages_sent(&self) -> u64 {
        self.messages_sent
    }

    fn clear_buffer(&mut self) {
        if let Some(ref mut buf) = self.pkt_buffer {
            buf.clear();
        }
        self.currently_read = 0;
        self.expected_size = 0;
        self.pkt_buffer = None;
    }

    fn pkt_validated(&self) -> bool {
        self.pkt_validated
    }

    fn pkt_valid(&self) -> bool {
        self.pkt_valid
    }

    fn set_validated(&mut self) {
        self.pkt_validated = true;
    }

    fn set_valid(&mut self) {
        self.pkt_valid = true
    }

    pub fn failed_pkts(&self) -> u32 {
        self.dptr.borrow().failed_pkts
    }

    fn setup_buffer(&mut self) {
        self.pkt_buffer = Some(BytesMut::with_capacity(1024));
        self.pkt_valid = false;
        self.pkt_validated = false;
    }

    /// It registers the connection socket, for read and write ops using *edge* notifications.
    pub fn register(&self, poll: &mut Poll) -> ResultExtWrapper<()> {
        poll.register(
                &self.socket,
                self.token,
                Ready::readable() | Ready::writable(),
                PollOpt::edge())
            .map_err( |e| ErrorWrapper::from_kind( ErrorKindWrapper::InternalIOError(e)))
    }

    #[allow(unused)]
    pub fn blind_trusted_broadcast(&self) -> bool {
        self.blind_trusted_broadcast
    }

    pub fn is_closed(&self) -> bool {
        self.closed
    }

    pub fn close(&mut self, poll: &mut Poll) -> ResultExtWrapper<()> {
        self.closing = true;
        poll.deregister(&self.socket)?;
        self.socket.shutdown(Shutdown::Both)?;
        Ok(())
    }

    pub fn ready(&mut self,
             poll: &mut Poll,
             ev: &Event,
             packets_queue: &Sender<Arc<Box<NetworkMessage>>>)
             -> ResultExtWrapper<()>
    {
        let ev_readiness = ev.readiness();

        if ev_readiness.is_readable()
        {
            // Process pending reads.
            while let Ok(size) = self.do_tls_read()
            {
                if size == 0 {
                    break;
                }
                self.try_plain_read(poll, &packets_queue);
            }
        }

        if ev_readiness.is_writable()
        {
            self.flush_tls()?;
        }

        // Manage clossing event.
        if self.closing {
            self.close(poll).map_err(|e| error!("{}", e)).ok();
        }

        let session_wants_read = self.dptr.borrow().tls_session.wants_read();
        if self.closing && ! session_wants_read {
            let _ = self.socket.shutdown(Shutdown::Both);
            self.closed = true;
        }

        Ok(())
    }

    fn do_tls_read(&mut self) -> ResultExtWrapper<usize> {
        let lptr = &mut self.dptr.borrow_mut();

        if lptr.tls_session.wants_read()
        {
            match lptr.tls_session.read_tls(&mut self.socket)
            {
                Ok(size) => {
                    if size > 0 {
                        lptr.tls_session.process_new_packets()?;
                    }
                    Ok(size)
                },
                Err(read_err) => {
                    if read_err.kind() == std::io::ErrorKind::WouldBlock {
                        Ok(0)
                    } else {
                        self.closing = true;
                        Err( ErrorKindWrapper::InternalIOError( read_err).into())
                    }
                }
            }
        }
        else {
            Ok(0)
        }
    }

    fn try_plain_read(&mut self,
                      poll: &mut Poll,
                      packets_queue: &Sender<Arc<Box<NetworkMessage>>>) {
        // Read and process all available plaintext.
        let mut buf = Vec::new();

        let read_status = self.dptr.borrow_mut().tls_session.read_to_end( &mut buf);
        match read_status
        {
            Ok(_) => {
                if !buf.is_empty() {
                    trace!("plaintext read {:?}", buf.len());
                    self.incoming_plaintext(poll, &packets_queue, &buf);
                }
            },
            Err(read_err) => {
                error!("plaintext read failed: {:?}", read_err);
                self.closing = true;
            }
        }
    }

    fn write_to_tls(&mut self, bytes: &[u8]) -> Result<()>
    {
        self.messages_sent += 1;
        self.dptr.borrow_mut().tls_session.write_all(bytes)
    }

    /// It decodes message from `buf` and processes it using its message handlers.
    fn process_complete_packet(&mut self, buf: &[u8]) {
        let outer = Arc::new(box NetworkMessage::deserialize(self.get_peer(), self.ip(), &buf));
        self.messages_received += 1;
        TOTAL_MESSAGES_RECEIVED_COUNTER.inc();
        if let Some(ref prom) = &self.prometheus_exporter() {
            match prom.lock() {
                Err(_) => {}
                Ok(mut plock) => {
                    plock.pkt_received_inc()
                        .map_err(|e| error!("{}", e))
                        .ok();
                }
            }
        };

        // Process message by message handler.
        if let Err(e) = (self.message_handler)(&outer) {
            warn!( "Message handler {}", e.display_chain().to_string());
        }
    }

    fn validate_packet(&mut self, poll: &mut Poll) {
        if !self.pkt_validated() {
            let buff = if let Some(ref bytebuf) = self.pkt_buffer {
                if bytebuf.len() >= 132 {
                    Some(bytebuf[24..28].to_vec())
                } else {
                    None
                }
            } else {
                None
            };
            match buff {
                Some(ref bufdata) => {
                    if self.mode() == P2PNodeMode::BootstrapperMode
                       || self.mode() == P2PNodeMode::BootstrapperPrivateMode
                    {
                        let msg_num = String::from_utf8(bufdata.to_vec()).expect("Unable to get string from utf8");
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
                _ => {}
            }
        }
    }

    fn incoming_plaintext(&mut self,
                          poll: &mut Poll,
                          packets_queue: &Sender<Arc<Box<NetworkMessage>>>,
                          buf: &[u8]) {
        trace!("Received plaintext");
        self.validate_packet(poll);
        if self.expected_size > 0 && self.currently_read == self.expected_size {
            trace!("Completed packet with {} size", self.currently_read);
            if self.pkt_valid() || !self.pkt_validated() {
                let mut buffered = Vec::new();
                if let Some(ref mut buf) = self.pkt_buffer {
                    buffered = buf[..].to_vec();
                }
                self.process_complete_packet( &buffered);
            }
            self.clear_buffer();
            self.incoming_plaintext(poll, packets_queue, buf);
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
                    self.process_complete_packet( &buffered);
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
                self.process_complete_packet( &buffered);
            }
            self.clear_buffer();
            self.incoming_plaintext(poll, &packets_queue, &buf[to_take as usize..]);
        } else if buf.len() >= 4 {
            trace!("Trying to read size");
            let _buf = &buf[..4].to_vec();
            let mut size_bytes = Cursor::new(_buf);
            self.expected_size = size_bytes.read_u32::<NetworkEndian>().expect("Couldn't read from buffer on incoming plaintext");
            if self.expected_size > 268_435_456 {
                error!("Packet can't be bigger than 256MB");
                self.expected_size = 0;
                self.incoming_plaintext(poll, &packets_queue, &buf[4..]);
            } else {
                self.setup_buffer();
                if buf.len() > 4 {
                    trace!("Got enough to read it...");
                    self.incoming_plaintext(poll, &packets_queue, &buf[4..]);
                }
            }
        }
    }

    pub fn serialize_bytes(&mut self, pkt: &[u8]) -> Result<usize> {
        trace!("Serializing data to connection {} bytes", pkt.len());
        let mut size_vec = Vec::with_capacity(4);

        size_vec.write_u32::<NetworkEndian>(pkt.len() as u32)?;
        self.write_to_tls(&size_vec[..])?;
        self.write_to_tls(pkt)?;

        self.flush_tls()
    }

    fn flush_tls(&mut self) -> Result<usize>
    {
        let rc = {
            let mut lptr = self.dptr.borrow_mut();
            if lptr.tls_session.wants_write() {
                let mut wr = WriteVAdapter::new( &mut self.socket);
                lptr.tls_session.writev_tls( &mut wr)
            } else {
                Ok(0)
            }
        };

        if rc.is_err() {
            error!("write failed {:?}", rc);
            self.closing = true;
        }

        rc
    }

    pub fn prometheus_exporter(&self) -> Option<Arc<Mutex<PrometheusServer>>> {
        self.dptr.borrow().prometheus_exporter.clone()
    }

    pub fn mode(&self) -> P2PNodeMode {
        self.dptr.borrow().mode
    }

    pub fn buckets(&self) -> Arc< RwLock< Buckets >> {
        self.dptr.borrow().buckets.clone()
    }

    pub fn own_id(&self) -> P2PNodeId {
        self.dptr.borrow().own_id.clone()
    }

    pub fn peer(&self) -> Option<P2PPeer> {
        self.dptr.borrow().peer().clone()
    }

    pub fn set_peer(&mut self, peer: P2PPeer) {
        self.dptr.borrow_mut().set_peer( peer);
    }

    pub fn networks(&self) -> Vec<u16> {
        self.dptr.borrow().networks.clone()
    }

    pub fn connection_type(&self) -> ConnectionType {
        self.dptr.borrow().connection_type
    }

    pub fn own_networks(&self) -> Arc<Mutex<Vec<u16>>> {
        self.dptr.borrow().own_networks.clone()
    }

    pub fn token(&self) -> &Token {
        &self.token
    }
}
