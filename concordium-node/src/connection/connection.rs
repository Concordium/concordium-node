use bytes::{ BufMut, BytesMut };
use byteorder::{NetworkEndian, ReadBytesExt, WriteBytesExt};
use std::sync::{ Arc, Mutex, RwLock };
use std::sync::mpsc::{ Sender };
use std::sync::atomic::{ AtomicU64, Ordering };
use std::net::{Shutdown, IpAddr };
use std::rc::{ Rc };
use std::io::{ Error, ErrorKind, Cursor, Result };
use atomic_counter::AtomicCounter;

use mio::{ Poll, PollOpt, Ready, Token, Event, net::TcpStream };
use rustls::{ ServerSession, ClientSession, Session };
use prometheus_exporter::{ PrometheusServer };

use errors::{ ResultExtWrapper, ResultExt };
use errors::ErrorKindWrapper::{ InternalIOError, NetworkError };
use error_chain::ChainedError;

use common::{ ConnectionType, P2PNodeId, P2PPeer, get_current_stamp };
use common::counter::{ TOTAL_MESSAGES_RECEIVED_COUNTER, TOTAL_MESSAGES_SENT_COUNTER };
use network::{ NetworkMessage, NetworkPacket, NetworkRequest, NetworkResponse, Buckets, 
    PROTOCOL_MESSAGE_TYPE_DIRECT_MESSAGE, PROTOCOL_MESSAGE_TYPE_BROADCASTED_MESSAGE };

use connection::{ 
    P2PEvent, SeenMessagesList, P2PNodeMode, ConnClientSession, ConnServerSession, 
    NetworkRequestSafeFn, ConnSession };
use connection::request_handler::{ RequestHandler };
use connection::message_handler::{ MessageHandler };
use connection::writev_adapter::{ WriteVAdapter };
use connection::connection_default_handlers::*;

pub const BOOTSTRAP_PEER_COUNT: usize = 100;

pub struct Connection {
    pub connection_type: ConnectionType,
    socket: TcpStream,
    token: Token,
    pub closing: bool,
    closed: bool,
    tls_server_session: ConnServerSession,
    tls_client_session: ConnClientSession,
    initiated_by_me: bool,
    own_id: P2PNodeId,
    pub peer: Option<P2PPeer>,
    currently_read: u32,
    pkt_validated: bool,
    pkt_valid: bool,
    failed_pkts: u32,
    peer_ip: IpAddr,
    peer_port: u16,
    expected_size: u32,
    pkt_buffer: Option<BytesMut>,
    last_seen: Rc<AtomicU64>,
    self_peer: P2PPeer,
    messages_sent: u64,
    messages_received: u64,
    mode: P2PNodeMode,
    prometheus_exporter: Option<Arc<Mutex<PrometheusServer>>>,
    pub networks: Vec<u16>,
    event_log: Option<Sender<P2PEvent>>,
    pub own_networks: Arc<Mutex<Vec<u16>>>,
    seen_messages: SeenMessagesList,
    sent_ping: Option<u64>,
    sent_handshake: Option<u64>,
    last_ping_sent: u64,
    last_latency_measured: Option<u64>,

    buckets: Arc< RwLock < Buckets > >,
    message_handler: MessageHandler
}

impl Connection {
    pub fn new(connection_type: ConnectionType,
           socket: TcpStream,
           token: Token,
           tls_server_session: Option< ServerSession>,
           tls_client_session: Option< ClientSession>,
           initiated_by_me: bool,
           own_id: P2PNodeId,
           self_peer: P2PPeer,
           peer_ip: IpAddr,
           peer_port: u16,
           mode: P2PNodeMode,
           prometheus_exporter: Option<Arc<Mutex<PrometheusServer>>>,
           event_log: Option<Sender<P2PEvent>>,
           own_networks: Arc<Mutex<Vec<u16>>>,
           seen_messages: SeenMessagesList,
           buckets: Arc< RwLock< Buckets > >)
           -> Self {
        let curr_stamp = get_current_stamp();
        let mut lself = Connection { connection_type,
                     socket: socket,
                     token,
                     closing: false,
                     closed: false,
                     tls_server_session: if let Some(s) = tls_server_session { Some(Arc::new( RwLock::new(s)))} else { None },
                     tls_client_session: if let Some(c) = tls_client_session { Some(Arc::new( RwLock::new(c)))} else { None },
                     initiated_by_me,
                     own_id,
                     peer: None,
                     currently_read: 0,
                     expected_size: 0,
                     pkt_buffer: None,
                     last_seen: Rc::new( AtomicU64::new( curr_stamp)),
                     self_peer: self_peer,
                     messages_received: 0,
                     messages_sent: 0,
                     peer_ip: peer_ip,
                     peer_port: peer_port,
                     mode: mode,
                     pkt_validated: false,
                     pkt_valid: false,
                     failed_pkts: 0,
                     prometheus_exporter: prometheus_exporter,
                     networks: vec![],
                     event_log: event_log,
                     own_networks: own_networks,
                     seen_messages: seen_messages,
                     sent_ping: None,
                     sent_handshake: None,
                     last_latency_measured: None,
                     last_ping_sent: curr_stamp, 
                     buckets: buckets,
                     message_handler: MessageHandler::new()
        };

        lself.message_handler = lself.make_message_handler();
        lself
    }

    // Setup message handler
    // ============================
    
    fn make_default_network_request_ping_handle(&mut self) -> NetworkRequestSafeFn {
        let mode = self.mode.clone();
        let last_seen = self.last_seen.clone();
        let prom = self.prometheus_exporter.clone();
        let self_peer = self.get_self_peer();
        let session = self.session().clone();

        make_callback!( move |_req: &NetworkRequest| { 
            default_network_request_ping_handle(
                &session, &self_peer, mode, &last_seen, &prom)
        })
    }

    fn make_default_network_request_find_node_handle(&self) -> NetworkRequestSafeFn {
        let mode = self.mode.clone();
        let last_seen = self.last_seen.clone();
        let self_peer = self.get_self_peer();
        let session = self.session().clone();
        let buckets = self.buckets.clone();

        make_callback!( move |req: &NetworkRequest| { 
            default_network_request_find_node_handle(
                    &session, &self_peer, mode, &last_seen, &buckets, req)
        })
    }

    fn make_default_network_request_ban_node_handler(&self) -> NetworkRequestSafeFn {
        make_callback!( move |_req: &NetworkRequest| { 
            Ok(())
        })
    }


    fn make_request_handler(&mut self) -> RequestHandler {

        RequestHandler::new()
            .add_ping_callback( self.make_default_network_request_ping_handle())
            .add_find_node_callback( self.make_default_network_request_find_node_handle())
            .add_ban_node_callback( self.make_default_network_request_ban_node_handler())
            .add_unban_node_callback( make_callback!(
                move | _req: &NetworkRequest| {
                    Ok(())
            }))
            .add_handshake_callback( make_callback!(
                move | _req: &NetworkRequest| {
                    Ok(())
            }))
            .add_get_peers_callback( make_callback!(
                move | _req: &NetworkRequest| {
                    Ok(())
            }))
            .add_join_network_callback( make_callback!(
                move | _req: &NetworkRequest| {
                    Ok(())
            }))
            .add_leave_network_callback( make_callback!(
                move | _req: &NetworkRequest| {
                    Ok(())
            }))
    }

    fn make_message_handler(&mut self) -> MessageHandler {
        let request_handler = self.make_request_handler();

        MessageHandler::new()
            .add_request_callback( make_callback!(
                move |req: &NetworkRequest| { (request_handler)(req) }))
    }

    // =============================

    pub fn get_last_latency_measured(&self) -> Option<u64> {
        self.last_latency_measured.clone()
    }

    fn set_measured_ping(&mut self) {
        if self.sent_ping.is_some() {
            self.last_latency_measured =
                Some(get_current_stamp() - self.sent_ping.unwrap());
            self.sent_ping = None;
        }
    }

    fn set_measured_handshake(&mut self) {
        if self.sent_handshake.is_some() {
            self.last_latency_measured =
                Some(get_current_stamp() - self.sent_handshake.unwrap());
            self.sent_handshake = None;
        }
    }

    pub fn set_measured_ping_sent(&mut self) {
        if self.sent_ping.is_none() {
            self.sent_ping = Some(get_current_stamp())
        }
    }

    pub fn set_measured_handshake_sent(&mut self) {
        if self.sent_handshake.is_none() {
            self.sent_handshake = Some(get_current_stamp())
        }
    }

    fn log_event(&mut self, event: P2PEvent) {
        match self.event_log {
            Some(ref mut x) => {
                match x.send(event) {
                    Ok(_) => {}
                    Err(e) => error!("Couldn't send event {:?}", e),
                };
            }
            _ => {}
        }
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

    fn update_last_seen(&mut self) {
        self.last_seen.store( get_current_stamp(), Ordering::Relaxed);
    }

    pub fn last_seen(&self) -> u64 {
        self.last_seen.load( Ordering::Relaxed)
    }

    fn add_networks(&mut self, networks: &Vec<u16>) {
        for ele in networks {
            if !self.networks.contains(ele) {
                self.networks.push(*ele);
            }
        }
    }

    fn remove_network(&mut self, network: &u16) {
        self.networks.retain(|x| x != network);
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
        self.self_peer.clone()
    }

    fn get_peer(&self) -> Option<P2PPeer> {
        self.peer.clone()
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

    fn failed_pkts_inc(&mut self) {
        self.failed_pkts += 1;
    }

    pub fn failed_pkts(&self) -> u32 {
        self.failed_pkts
    }

    fn setup_buffer(&mut self) {
        self.pkt_buffer = Some(BytesMut::with_capacity(1024));
        self.pkt_valid = false;
        self.pkt_validated = false;
    }

    pub fn register(&self, poll: &mut Poll) -> ResultExtWrapper<()> {
        match poll.register(&self.socket,
                            self.token,
                            self.event_set(),
                            PollOpt::level() | PollOpt::oneshot())
        {
            Ok(_) => Ok(()),
            Err(e) => Err(InternalIOError(e).into()),
        }
    }

    fn reregister(&self, poll: &mut Poll) -> ResultExtWrapper<()> {
        match poll.reregister(&self.socket,
                              self.token,
                              self.event_set(),
                              PollOpt::level() | PollOpt::oneshot())
        {
            Ok(_) => Ok(()),
            Err(e) => Err(InternalIOError(e).into()),
        }
    }

    fn event_set(&self) -> Ready {
        let mut rd: bool = false;
        let mut _wr: bool;

        if let Some(ref session) = self.session() {
            if let Some(ref locked_session) = session.read().ok() {
                rd = locked_session.wants_read();
                _wr = locked_session.wants_write();
            }
        };

        //Don't trust it .. It's broken and inconsistent
        _wr = true;

        if rd && _wr {
            Ready::readable() | Ready::writable()
        } else if _wr {
            Ready::writable()
        } else {
            Ready::readable()
        }
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
             -> ResultExtWrapper<()> {
        if ev.readiness().is_readable() {
            self.do_tls_read().map_err(|e| error!("{}", e)).ok();
            self.try_plain_read(poll, &packets_queue);
        }

        if ev.readiness().is_writable() {
            self.do_tls_write().map_err(|e| error!("{}", e)).ok();
        }

        if self.closing {
            self.close(poll).map_err(|e| error!("{}", e)).ok();
        }

        if let Some(ref session) = self.session() {
            if let Some(ref locked_session) = session.read().ok() {
                if self.closing && ! locked_session.wants_read() {
                    let _ = self.socket.shutdown(Shutdown::Both);
                    self.closed = true;
                } else {
                    self.reregister(poll).map_err(|e| error!("{}", e)).ok();
                }
            }
        }

        Ok(())
    }

    fn do_tls_read(&mut self) -> ResultExtWrapper<(usize)> {
        let local_session = self.session();

        let rc = if let Some(ref session) = local_session {
            if let Some(ref mut locked_session) = session.write().ok() {
                locked_session.read_tls(&mut self.socket)
            } else {
                Err( Error::new(ErrorKind::Other, "Session cannot be locked"))
            }
        } else {
            Err(Error::new( ErrorKind::Other, "Session is empty"))
        };
        
        if rc.is_err() {
            let err = &rc.unwrap_err();

            if let ErrorKind::WouldBlock = err.kind() {
                return Err(NetworkError(format!("{}:{}/blocked {:?}",
                                                                  self.ip().to_string(),
                                                                  self.port(),
                                                                  err)).into());
            }

            //error!("read error {}:{}/{:?}", self.ip().to_string(), self.port(), err);
            self.closing = true;
            return Err(NetworkError(format!("{}:{}/read error {:?}",
                                                              self.ip().to_string(),
                                                              self.port(),
                                                              err)).into());
        }

        if let Ok(size) = rc {
            if size == 0 {
                debug!("eof");
                self.closing = true;
                return Err(NetworkError("eof".to_string()).into());
            }
        }

        // Process newly-received TLS messages.
        let processed = if let Some(session) = local_session {
            if let Some(mut locked_session) = session.write().ok() {
                Ok( locked_session.process_new_packets())
            } else {
                Err(NetworkError(format!("{}:{} Session cannot be locked",
                                         self.ip().to_string(),
                                         self.port())))
            }
        } else {
            Err(NetworkError(format!("{}:{} Session is not found",
                                     self.ip().to_string(),
                                     self.port())))
        };

        if processed.is_err() {
            error!("cannot process packet: {:?}", processed);
            self.closing = true;
            return Err(NetworkError(format!("Can't process packet {:?}",
                                                              processed)).into());
        }

        rc.chain_err(|| NetworkError("couldn't read from TLS socket".to_string()))
    }

    fn try_plain_read(&mut self,
                      poll: &mut Poll,
                      packets_queue: &Sender<Arc<Box<NetworkMessage>>>) {
        // Read and process all available plaintext.
        let mut buf = Vec::new();

        let rc = 
            if let Some(ref session) = self.session() {
                if let Some(ref mut locked_session) = session.write().ok() {
                    locked_session.read_to_end(&mut buf)
                } else {
                    Err(Error::new(ErrorKind::Other, "Session cannot be locked!"))
                }
            } else {
                Err(Error::new(ErrorKind::Other, "Couldn't find session!"))
            };

        if rc.is_err() {
            error!("plaintext read failed: {:?}", rc);
            self.closing = true;
            return;
        }

        if !buf.is_empty() {
            trace!("plaintext read {:?}", buf.len());
            self.incoming_plaintext(poll, &packets_queue, &buf);
        }
    }

    fn session(&self) -> ConnSession {
       sessionAs!( self, Session)
    }

    fn write_all(&mut self, bytes: &[u8]) -> Result<()> {
        if let Some(ref session) = self.session() {
            if let Some(ref mut locked_session) = session.write().ok() {
                self.messages_sent += 1;
                locked_session.write_all(bytes)
            } else {
                Err(Error::new(ErrorKind::Other, "Couldn't find session!"))
            }
        } else {
            Err(Error::new(ErrorKind::Other, "Couldn't find session!"))
        }
    }

    fn process_complete_packet(&mut self,
                               buf: &[u8],
                               packet_queue: &Sender<Arc<Box<NetworkMessage>>>) {
        let outer = Arc::new(box NetworkMessage::deserialize(self.get_peer(), self.ip(), &buf));
        let self_peer = self.get_self_peer().clone();
        self.messages_received += 1;
        TOTAL_MESSAGES_RECEIVED_COUNTER.inc();
        if let Some(ref prom) = &self.prometheus_exporter {
            prom.lock()
                .unwrap()
                .pkt_received_inc()
                .map_err(|e| error!("{}", e))
                .ok();
        };

        if let Err(e) = (self.message_handler)(&outer) {
            warn!( "Message handler {}", e.display_chain().to_string());
        }

        match *outer.clone() {
            box NetworkMessage::NetworkRequest(ref x, _, _) => {
                match x {
                    NetworkRequest::Ping(_) => { }
                    NetworkRequest::FindNode(_, _x) => { }
                    NetworkRequest::BanNode(_, _) => {
                        debug!("Got request for BanNode");
                        if self.mode != P2PNodeMode::BootstrapperMode
                           && self.mode != P2PNodeMode::BootstrapperPrivateMode
                        {
                            self.update_last_seen();
                        }
                        match packet_queue.send(outer.clone()) {
                            Ok(_) => {}
                            Err(e) => error!("Couldn't send to packet_queue, {:?}", e),
                        };
                    }
                    NetworkRequest::UnbanNode(_, _) => {
                        debug!("Got request for UnbanNode");
                        if self.mode != P2PNodeMode::BootstrapperMode
                           && self.mode != P2PNodeMode::BootstrapperPrivateMode
                        {
                            self.update_last_seen();
                        }
                        match packet_queue.send(outer.clone()) {
                            Ok(_) => {}
                            Err(e) => error!("Couldn't send to packet_queue, {:?}", e),
                        };
                    }
                    NetworkRequest::Handshake(sender, nets, _) => {
                        debug!("Got request for Handshake");
                        self.update_last_seen();
                        let my_nets = self.own_networks.lock().unwrap().clone();
                        self.serialize_bytes(
                                        &NetworkResponse::Handshake(self_peer.clone(),
                                                                    my_nets,
                                                                    vec![]).serialize()).unwrap();
                        self.serialize_bytes(
                                        &NetworkRequest::Ping(self_peer.clone()).serialize()).unwrap();
                        TOTAL_MESSAGES_SENT_COUNTER.add(2);
                        self.set_measured_ping_sent();
                        self.add_networks(nets);
                        self.peer = Some(sender.clone());
                        if self.mode == P2PNodeMode::BootstrapperPrivateMode
                           || self.mode == P2PNodeMode::NormalPrivateMode
                        {
                            self.buckets.write().unwrap()
                                .insert_into_bucket(sender, &self.own_id, nets.clone());
                        } else if sender.ip().is_global()
                                  && !sender.ip().is_multicast()
                                  && !sender.ip().is_documentation()
                        {
                            self.buckets.write().unwrap()
                                .insert_into_bucket(sender, &self.own_id, nets.clone());
                        }
                        if let Some(ref prom) = &self.prometheus_exporter {
                            let mut _prom = prom.lock().unwrap();
                            _prom.peers_inc().map_err(|e| error!("{}", e)).ok();
                            _prom.pkt_sent_inc_by(2).map_err(|e| error!("{}", e)).ok();
                        };
                        if self.mode == P2PNodeMode::BootstrapperMode
                           || self.mode == P2PNodeMode::BootstrapperPrivateMode
                        {
                            debug!("Running in bootstrapper mode, so instantly sending peers {} random peers",
                                   BOOTSTRAP_PEER_COUNT);
                            let random_nodes = self.buckets.read().unwrap()
                                .get_random_nodes(&sender, BOOTSTRAP_PEER_COUNT, &nets);
                            self.serialize_bytes( &NetworkResponse::PeerList(
                                    self_peer, random_nodes).serialize()).unwrap();
                            if let Some(ref prom) = &self.prometheus_exporter {
                                prom.lock()
                                    .unwrap()
                                    .pkt_sent_inc()
                                    .map_err(|e| error!("{}", e))
                                    .ok();
                            };
                            TOTAL_MESSAGES_SENT_COUNTER.inc();
                        }
                        match packet_queue.send(outer.clone()) {
                            Ok(_) => {}
                            Err(e) => error!("Couldn't send to packet_queue, {:?}", e),
                        };
                    }
                    NetworkRequest::GetPeers(ref sender, ref networks) => {
                        debug!("Got request for GetPeers");
                        if self.mode != P2PNodeMode::BootstrapperMode
                           && self.mode != P2PNodeMode::BootstrapperPrivateMode
                        {
                            self.update_last_seen();
                        }
                        let nodes = self.buckets.read().unwrap()
                            .get_all_nodes(Some(&sender), networks);
                        TOTAL_MESSAGES_SENT_COUNTER.inc();
                        if let Some(ref prom) = &self.prometheus_exporter {
                            prom.lock()
                                .unwrap()
                                .pkt_sent_inc()
                                .map_err(|e| error!("{}", e))
                                .ok();
                        };
                        self.serialize_bytes( &NetworkResponse::PeerList(self_peer, nodes).serialize()).unwrap();
                    }
                    NetworkRequest::JoinNetwork(sender, network) => {
                        self.add_networks(&vec![*network]);
                        match self.get_peer() {
                            Some(peer) => {
                                self.buckets.write().unwrap().
                                    update_network_ids(&peer, self.networks.clone());
                            }
                            _ => {}
                        }
                        self.log_event(P2PEvent::JoinedNetwork(sender.clone(), *network));
                        if self.mode != P2PNodeMode::BootstrapperMode
                           && self.mode != P2PNodeMode::BootstrapperPrivateMode
                        {
                            self.update_last_seen();
                        }
                    }
                    NetworkRequest::LeaveNetwork(sender, ref network) => {
                        self.remove_network(network);
                        match self.get_peer() {
                            Some(peer) => {
                                self.buckets.write().unwrap()
                                    .update_network_ids(&peer, self.networks.clone());
                            }
                            _ => {}
                        }
                        self.log_event(P2PEvent::LeftNetwork(sender.clone(), *network));
                        if self.mode != P2PNodeMode::BootstrapperMode
                           && self.mode != P2PNodeMode::BootstrapperPrivateMode
                        {
                            self.update_last_seen();
                        }
                    }
                }
            }
            box NetworkMessage::NetworkResponse(ref x, _, _) => {
                match x {
                    NetworkResponse::FindNode(_, peers) => {
                        debug!("Got response to FindNode");
                        if self.mode != P2PNodeMode::BootstrapperMode
                           && self.mode != P2PNodeMode::BootstrapperPrivateMode
                        {
                            self.update_last_seen();
                        }
                        //Process the received node list
                        let mut ref_buckets = self.buckets.write().unwrap();
                        for peer in peers.iter() {
                            ref_buckets.insert_into_bucket(peer, &self.own_id, vec![]);
                        }
                    }
                    NetworkResponse::Pong(_) => {
                        debug!("Got response for ping");
                        self.set_measured_ping();
                        if self.mode != P2PNodeMode::BootstrapperMode
                           && self.mode != P2PNodeMode::BootstrapperPrivateMode
                        {
                            self.update_last_seen();
                        }
                    }
                    NetworkResponse::PeerList(_, peers) => {
                        debug!("Got response to PeerList");
                        if self.mode != P2PNodeMode::BootstrapperMode
                           && self.mode != P2PNodeMode::BootstrapperPrivateMode
                        {
                            self.update_last_seen();
                        }
                        //Process the received node list
                        let mut ref_buckets = self.buckets.write().unwrap();
                        for peer in peers.iter() {
                            ref_buckets.insert_into_bucket(peer, &self.own_id, vec![]);
                        }
                        match packet_queue.send(outer.clone()) {
                            Ok(_) => {}
                            Err(e) => error!("Couldn't send to packet_queue, {:?}", e),
                        };
                    }
                    NetworkResponse::Handshake(peer, nets, _) => {
                        debug!("Got response to Handshake");
                        self.set_measured_handshake();
                        self.update_last_seen();
                        self.add_networks(nets);
                        self.peer = Some(peer.clone());
                        let bucket_sender = P2PPeer::from(self.connection_type,
                                                          peer.id().clone(),
                                                          peer.ip().clone(),
                                                          peer.port());
                        self.buckets.write().unwrap()
                            .insert_into_bucket(&bucket_sender, &self.own_id, nets.clone());
                        if let Some(ref prom) = &self.prometheus_exporter {
                            prom.lock()
                                .unwrap()
                                .peers_inc()
                                .map_err(|e| error!("{}", e))
                                .ok();
                        };
                        for ele in nets {
                            self.log_event(P2PEvent::JoinedNetwork(peer.clone(), *ele));
                        }
                    }
                }
            }
            box NetworkMessage::NetworkPacket(ref x, _, _) => {
                match x {
                    NetworkPacket::DirectMessage(ref sender,
                                                 ref msgid,
                                                 _,
                                                 ref network_id,
                                                 ref msg) => {
                        if !self.seen_messages.contains(msgid) {
                            self.seen_messages.append(&msgid);
                            if self.own_networks.lock().unwrap().contains(network_id) {
                                if self.mode != P2PNodeMode::BootstrapperMode
                                   && self.mode != P2PNodeMode::BootstrapperPrivateMode
                                {
                                    self.update_last_seen();
                                }
                                debug!("Received direct message of size {}", msg.len());
                                match packet_queue.send(outer.clone()) {
                                    Ok(_) => {
                                        self.seen_messages.append(&msgid);
                                    }
                                    Err(e) => error!("Couldn't send to packet_queue, {:?}", e),
                                };
                            } else {
                                if let Some(ref prom) = &self.prometheus_exporter {
                                    prom.lock()
                                        .unwrap()
                                        .invalid_network_pkts_received_inc()
                                        .map_err(|e| error!("{}", e))
                                        .ok();
                                };
                            }
                        } else {
                            info!("Dropping duplicate direct packet {}/{}/{}",
                                   sender.id().to_string(),
                                   network_id,
                                   msgid);
                        }
                    }
                    NetworkPacket::BroadcastedMessage(ref sender,
                                                      ref msgid,
                                                      ref network_id,
                                                      ref msg) => {
                        if !self.seen_messages.contains(msgid) {
                            if self.own_networks.lock().unwrap().contains(network_id) {
                                if self.mode != P2PNodeMode::BootstrapperMode
                                   && self.mode != P2PNodeMode::BootstrapperPrivateMode
                                {
                                    self.update_last_seen();
                                }
                                debug!("Received broadcast message of size {}", msg.len());
                                match packet_queue.send(outer.clone()) {
                                    Ok(_) => {
                                        self.seen_messages.append(&msgid);
                                    }
                                    Err(e) => error!("Couldn't send to packet_queue, {:?}", e),
                                };
                            } else {
                                if let Some(ref prom) = &self.prometheus_exporter {
                                    prom.lock()
                                        .unwrap()
                                        .invalid_network_pkts_received_inc()
                                        .map_err(|e| error!("{}", e))
                                        .ok();
                                };
                            }
                        } else {
                            info!("Dropping duplicate broadcast packet {}/{}/{}",
                                   sender.id().to_string(),
                                   network_id,
                                   msgid);
                        }
                    }
                }
            }
            box NetworkMessage::UnknownMessage => {
                self.failed_pkts_inc();
                debug!("Unknown message received!");
                if self.mode != P2PNodeMode::BootstrapperMode
                   && self.mode != P2PNodeMode::BootstrapperPrivateMode
                {
                    self.update_last_seen();
                }
                trace!("Contents were: {:?}",
                       String::from_utf8(buf.to_vec()).unwrap());
                if let Some(ref prom) = &self.prometheus_exporter {
                    prom.lock()
                        .unwrap()
                        .unknown_pkts_received_inc()
                        .map_err(|e| error!("{}", e))
                        .ok();
                };
            }
            box NetworkMessage::InvalidMessage => {
                self.failed_pkts_inc();
                debug!("Invalid message received!");
                if self.mode != P2PNodeMode::BootstrapperMode
                   && self.mode != P2PNodeMode::BootstrapperPrivateMode
                {
                    self.update_last_seen();
                }
                trace!("Contents were: {:?}",
                       String::from_utf8(buf.to_vec()).unwrap());
                if let Some(ref prom) = &self.prometheus_exporter {
                    prom.lock()
                        .unwrap()
                        .invalid_pkts_received_inc()
                        .map_err(|e| error!("{}", e))
                        .ok();
                };
            }
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
                    if self.mode == P2PNodeMode::BootstrapperMode
                       || self.mode == P2PNodeMode::BootstrapperPrivateMode
                    {
                        let msg_num = String::from_utf8(bufdata.to_vec()).unwrap();
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
                self.process_complete_packet( &buffered, &packets_queue);
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
                    self.process_complete_packet( &buffered, &packets_queue);
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
                self.process_complete_packet( &buffered, &packets_queue);
            }
            self.clear_buffer();
            self.incoming_plaintext(poll, &packets_queue, &buf[to_take as usize..]);
        } else if buf.len() >= 4 {
            trace!("Trying to read size");
            let _buf = &buf[..4].to_vec();
            let mut size_bytes = Cursor::new(_buf);
            self.expected_size = size_bytes.read_u32::<NetworkEndian>().unwrap();
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
    
    pub fn serialize_bytes(&mut self, pkt: &[u8]) -> ResultExtWrapper<()> {
        trace!("Serializing data to connection {} bytes", pkt.len());
        let mut size_vec = Vec::with_capacity(4);

        match size_vec.write_u32::<NetworkEndian>(pkt.len() as u32) {
            Ok(()) => {}
            Err(e) => {
                if let Some(inner_err) = e.into_inner() {
                    info!("{}", inner_err);
                }
            }
        };
        match self.write_all(&size_vec[..]) {
            Ok(()) => {}
            Err(e) => {
                if let Some(inner_err) = e.into_inner() {
                    info!("{}", inner_err);
                }
            }
        };
        match self.write_all(pkt) {
            Ok(()) => {}
            Err(e) => {
                if let Some(inner_err) = e.into_inner() {
                    info!("{}", inner_err);
                }
            }
        };
        Ok(())
    }

    #[cfg(not(target_os = "windows"))]
    fn do_tls_write(&mut self) -> ResultExtWrapper<(usize)> {
         
        let rc = if let Some(ref session) = self.session() {
            if let Some(ref mut locked_session) = session.write().ok() {
                let mut wr = WriteVAdapter::new(&mut self.socket);
                locked_session.writev_tls( &mut wr)
            } else { 
                Err(Error::new(ErrorKind::Other, format!("Couldn't find session! {}:{}", file!(), line!()))) 
            }
        } else { 
            Err(Error::new(ErrorKind::Other, format!("Couldn't find session! {}:{}", file!(), line!()))) 
        };

        if rc.is_err() {
            error!("write failed {:?}", rc);
            self.closing = true;
        }
        rc.chain_err(|| NetworkError("couldn't write TLS to socket".to_string()))
    }

    #[cfg(target_os = "windows")]
    fn do_tls_write(&mut self) -> ResultExtWrapper<(usize)> {
        let rc = match self.initiated_by_me {
            true => {
                match self.tls_client_session {
                    Some(ref mut x) => x.write_tls(&mut self.socket),
                    None => Err(Error::new(ErrorKind::Other, "Couldn't find session!")),
                }
            }
            false => {
                match self.tls_server_session {
                    Some(ref mut x) => x.write_tls(&mut self.socket),
                    None => Err(Error::new(ErrorKind::Other, "Couldn't find session!")),
                }
            }
        };

        if rc.is_err() {
            error!("write failed {:?}", rc);
            self.closing = true;
        }
        rc.chain_err(|| NetworkError("couldn't write TLS to socket".to_string()))
    }
}
