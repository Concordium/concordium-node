use mio::{ Poll, PollOpt, Ready, Token, Event, net::TcpStream };
use bytes::{ BufMut, BytesMut };
use vecio::{ Rawv };
use errors::{ ResultExtWrapper, ResultExt };
use rustls::{ TLSError, ServerSession, ClientSession, Session };
use std::io::{ Error, ErrorKind, Cursor, Result, Read, Write };
use std::net::{Shutdown, IpAddr };
use byteorder::{ NetworkEndian, ReadBytesExt, WriteBytesExt };
use std::sync::{ Arc, Mutex };
use atomic_counter::{ AtomicCounter, RelaxedCounter };
use std::sync::mpsc::{ Sender };
use prometheus_exporter::{ PrometheusServer };
use errors::ErrorKindWrapper::{ InternalIOError, NetworkError };

use common;
use common::{ ConnectionType, P2PNodeId, P2PPeer};
use network::{ 
    PROTOCOL_MESSAGE_TYPE_BROADCASTED_MESSAGE, PROTOCOL_MESSAGE_TYPE_DIRECT_MESSAGE,
    NetworkMessage, NetworkPacket, NetworkRequest, NetworkResponse, Buckets
};

/// Helper macro to create callbacks from raw function pointers or closures.
#[macro_export]
macro_rules! make_callback {
    ($callback:expr) => {
        Arc::new ( Mutex::new ( Box::new( $callback )))
    }
}

pub mod parse_handler;
pub mod packet_handler;
pub mod message_handler;

pub const BOOTSTRAP_PEER_COUNT: usize = 100;

lazy_static! {
    static ref TOTAL_MESSAGES_RECEIVED_COUNTER: RelaxedCounter = { RelaxedCounter::new(0) };
    pub static ref TOTAL_MESSAGES_SENT_COUNTER: RelaxedCounter = { RelaxedCounter::new(0) };
}

pub struct WriteVAdapter<'a> {
    rawv: &'a mut Rawv,
}

impl<'a> WriteVAdapter<'a> {
    pub fn new(rawv: &'a mut Rawv) -> WriteVAdapter<'a> {
        WriteVAdapter { rawv }
    }
}

impl<'a> rustls::WriteV for WriteVAdapter<'a> {
    fn writev(&mut self, bytes: &[&[u8]]) -> Result<usize> {
        self.rawv.writev(bytes)
    }
}

#[derive(Debug, Clone)]
pub struct SeenMessagesList {
    seen_msgs: Arc<Mutex<Vec<String>>>,
}

impl SeenMessagesList {
    pub fn new() -> Self {
        SeenMessagesList { seen_msgs: Arc::new(Mutex::new(Vec::new())), }
    }

    pub fn contains(&self, msgid: &String) -> bool {
        if let Ok(ref mut list) = self.seen_msgs.lock() {
            return list.contains(msgid);
        }
        false
    }

    pub fn append(&self, msgid: &String) -> bool {
        if let Ok(ref mut list) = self.seen_msgs.lock() {
            if !list.contains(msgid) {
                if list.len() >= 1000 {
                    list.remove(0);
                    list.push(msgid.clone().to_owned());
                } else {
                    list.push(msgid.clone().to_owned());
                }
            }
            true
        } else {
            false
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum P2PNodeMode {
    NormalMode,
    NormalPrivateMode,
    BootstrapperMode,
    BootstrapperPrivateMode,
}

#[derive(Clone, Debug, PartialEq)]
pub enum P2PEvent {
    ConnectEvent(String, u16),
    DisconnectEvent(String),
    ReceivedMessageEvent(P2PNodeId),
    SentMessageEvent(P2PNodeId),
    InitiatingConnection(IpAddr, u16),
    JoinedNetwork(P2PPeer, u16),
    LeftNetwork(P2PPeer, u16),
}

pub struct Connection {
    pub connection_type: ConnectionType,
    socket: TcpStream,
    token: Token,
    pub closing: bool,
    closed: bool,
    tls_server_session: Option<ServerSession>,
    tls_client_session: Option<ClientSession>,
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
    pub last_seen: u64,
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
}

impl Connection {
    pub fn new(connection_type: ConnectionType,
           socket: TcpStream,
           token: Token,
           tls_server_session: Option<ServerSession>,
           tls_client_session: Option<ClientSession>,
           initiated_by_me: bool,
           own_id: P2PNodeId,
           self_peer: P2PPeer,
           peer_ip: IpAddr,
           peer_port: u16,
           mode: P2PNodeMode,
           prometheus_exporter: Option<Arc<Mutex<PrometheusServer>>>,
           event_log: Option<Sender<P2PEvent>>,
           own_networks: Arc<Mutex<Vec<u16>>>,
           seen_messages: SeenMessagesList)
           -> Connection {
        Connection { connection_type,
                     socket,
                     token,
                     closing: false,
                     closed: false,
                     tls_server_session,
                     tls_client_session,
                     initiated_by_me,
                     own_id,
                     peer: None,
                     currently_read: 0,
                     expected_size: 0,
                     pkt_buffer: None,
                     last_seen: common::get_current_stamp(),
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
                     last_ping_sent: common::get_current_stamp(), }
    }

    pub fn get_last_latency_measured(&self) -> Option<u64> {
        self.last_latency_measured.clone()
    }

    fn set_measured_ping(&mut self) {
        if self.sent_ping.is_some() {
            self.last_latency_measured =
                Some(common::get_current_stamp() - self.sent_ping.unwrap());
            self.sent_ping = None;
        }
    }

    fn set_measured_handshake(&mut self) {
        if self.sent_handshake.is_some() {
            self.last_latency_measured =
                Some(common::get_current_stamp() - self.sent_handshake.unwrap());
            self.sent_handshake = None;
        }
    }

    pub fn set_measured_ping_sent(&mut self) {
        if self.sent_ping.is_none() {
            self.sent_ping = Some(common::get_current_stamp())
        }
    }

    pub fn set_measured_handshake_sent(&mut self) {
        if self.sent_handshake.is_none() {
            self.sent_handshake = Some(common::get_current_stamp())
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
        self.last_ping_sent = common::get_current_stamp();
    }

    pub fn ip(&self) -> IpAddr {
        self.peer_ip.clone()
    }

    pub fn port(&self) -> u16 {
        self.peer_port.clone()
    }

    fn update_last_seen(&mut self) {
        self.last_seen = common::get_current_stamp();
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
        let mut _rd = false;
        let mut _wr = false;
        match self.initiated_by_me {
            true => {
                _rd = match self.tls_client_session {
                    Some(ref x) => x.wants_read(),
                    _ => false,
                };
                _wr = match self.tls_client_session {
                    Some(ref x) => x.wants_write(),
                    _ => false,
                };
            }
            false => {
                _rd = match self.tls_server_session {
                    Some(ref x) => x.wants_read(),
                    _ => false,
                };
                _wr = match self.tls_server_session {
                    Some(ref x) => x.wants_write(),
                    _ => false,
                };
            }
        };

        //Don't trust it .. It's broken and inconsistent
        _wr = true;

        if _rd && _wr {
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
             buckets: &mut Buckets,
             packets_queue: &Sender<Arc<Box<NetworkMessage>>>)
             -> ResultExtWrapper<()> {
        if ev.readiness().is_readable() {
            self.do_tls_read().map_err(|e| error!("{}", e)).ok();
            self.try_plain_read(poll, &packets_queue, buckets);
        }

        if ev.readiness().is_writable() {
            self.do_tls_write().map_err(|e| error!("{}", e)).ok();
        }

        if self.closing {
            self.close(poll).map_err(|e| error!("{}", e)).ok();
        }

        match self.initiated_by_me {
            true => {
                if self.closing && !match self.tls_client_session {
                    Some(ref x) => x.wants_read(),
                    _ => false,
                } {
                    let _ = self.socket.shutdown(Shutdown::Both);
                    self.closed = true;
                } else {
                    self.reregister(poll).map_err(|e| error!("{}", e)).ok();
                }
            }
            false => {
                if self.closing && !match self.tls_server_session {
                    Some(ref x) => x.wants_read(),
                    _ => false,
                } {
                    let _ = self.socket.shutdown(Shutdown::Both);
                    self.closed = true;
                } else {
                    self.reregister(poll).map_err(|e| error!("{}", e)).ok();
                }
            }
        };
        Ok(())
    }

    fn do_tls_read(&mut self) -> ResultExtWrapper<(usize)> {
        let rc = match self.initiated_by_me {
            true => {
                match self.tls_client_session {
                    Some(ref mut x) => x.read_tls(&mut self.socket),
                    None => Err(Error::new(ErrorKind::Other, "Couldn't find session!")),
                }
            }
            false => {
                match self.tls_server_session {
                    Some(ref mut x) => x.read_tls(&mut self.socket),
                    None => Err(Error::new(ErrorKind::Other, "Couldn't find session!")),
                }
            }
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
        let processed = match self.initiated_by_me {
            true => {
                match self.tls_client_session {
                    Some(ref mut x) => x.process_new_packets(),
                    None => Err(TLSError::General(String::from("Couldn't find session!"))),
                }
            }
            false => {
                match self.tls_server_session {
                    Some(ref mut x) => x.process_new_packets(),
                    None => Err(TLSError::General(String::from("Couldn't find session!"))),
                }
            }
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
                      packets_queue: &Sender<Arc<Box<NetworkMessage>>>,
                      mut buckets: &mut Buckets) {
        // Read and process all available plaintext.
        let mut buf = Vec::new();

        let rc = match self.initiated_by_me {
            true => {
                match self.tls_client_session {
                    Some(ref mut x) => x.read_to_end(&mut buf),
                    None => Err(Error::new(ErrorKind::Other, "Couldn't find session!")),
                }
            }
            false => {
                match self.tls_server_session {
                    Some(ref mut x) => x.read_to_end(&mut buf),
                    None => Err(Error::new(ErrorKind::Other, "Couldn't find session!")),
                }
            }
        };

        if rc.is_err() {
            error!("plaintext read failed: {:?}", rc);
            self.closing = true;
            return;
        }

        if !buf.is_empty() {
            trace!("plaintext read {:?}", buf.len());
            self.incoming_plaintext(poll, &packets_queue, &mut buckets, &buf);
        }
    }

    fn write_all(&mut self, bytes: &[u8]) -> Result<()> {
        match self.initiated_by_me {
            true => {
                match self.tls_client_session {
                    Some(ref mut x) => {
                        self.messages_sent += 1;
                        x.write_all(bytes)
                    }
                    None => Err(Error::new(ErrorKind::Other, "Couldn't find session!")),
                }
            }
            false => {
                match self.tls_server_session {
                    Some(ref mut x) => {
                        self.messages_sent += 1;
                        x.write_all(bytes)
                    }
                    None => Err(Error::new(ErrorKind::Other, "Couldn't find session!")),
                }
            }
        }
    }

    fn process_complete_packet(&mut self,
                               buckets: &mut Buckets,
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
        match *outer.clone() {
            box NetworkMessage::NetworkRequest(ref x, _, _) => {
                match x {
                    NetworkRequest::Ping(_) => {
                        //Respond with pong
                        debug!("Got request for ping");
                        if self.mode != P2PNodeMode::BootstrapperMode
                           && self.mode != P2PNodeMode::BootstrapperPrivateMode
                        {
                            self.update_last_seen();
                        }
                        TOTAL_MESSAGES_SENT_COUNTER.inc();
                        if let Some(ref prom) = &self.prometheus_exporter {
                            prom.lock()
                                .unwrap()
                                .pkt_sent_inc()
                                .map_err(|e| error!("{}", e))
                                .ok();
                        };
                        self.serialize_bytes( &NetworkResponse::Pong(self_peer).serialize()).unwrap();
                    }
                    NetworkRequest::FindNode(_, x) => {
                        //Return list of nodes
                        debug!("Got request for FindNode");
                        if self.mode != P2PNodeMode::BootstrapperMode
                           && self.mode != P2PNodeMode::BootstrapperPrivateMode
                        {
                            self.update_last_seen();
                        }
                        let nodes = buckets.closest_nodes(x);
                        self.serialize_bytes( &NetworkResponse::FindNode(self_peer, nodes).serialize()).unwrap();
                    }
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
                            buckets.insert_into_bucket(sender, &self.own_id, nets.clone());
                        } else if sender.ip().is_global()
                                  && !sender.ip().is_multicast()
                                  && !sender.ip().is_documentation()
                        {
                            buckets.insert_into_bucket(sender, &self.own_id, nets.clone());
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
                            self.serialize_bytes( &NetworkResponse::PeerList(self_peer, buckets.get_random_nodes(&sender, BOOTSTRAP_PEER_COUNT, &nets)).serialize()).unwrap();
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
                        let nodes = buckets.get_all_nodes(Some(&sender), networks);
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
                                buckets.update_network_ids(&peer, self.networks.clone());
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
                                buckets.update_network_ids(&peer, self.networks.clone());
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
                        for peer in peers.iter() {
                            buckets.insert_into_bucket(peer, &self.own_id, vec![]);
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
                        for peer in peers.iter() {
                            buckets.insert_into_bucket(peer, &self.own_id, vec![]);
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
                        buckets.insert_into_bucket(&bucket_sender, &self.own_id, nets.clone());
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
                          buckets: &mut Buckets,
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
                self.process_complete_packet(buckets, &buffered, &packets_queue);
            }
            self.clear_buffer();
            self.incoming_plaintext(poll, packets_queue, buckets, buf);
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
                    self.process_complete_packet(buckets, &buffered, &packets_queue);
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
                self.process_complete_packet(buckets, &buffered, &packets_queue);
            }
            self.clear_buffer();
            self.incoming_plaintext(poll, &packets_queue, buckets, &buf[to_take as usize..]);
        } else if buf.len() >= 4 {
            trace!("Trying to read size");
            let _buf = &buf[..4].to_vec();
            let mut size_bytes = Cursor::new(_buf);
            self.expected_size = size_bytes.read_u32::<NetworkEndian>().unwrap();
            if self.expected_size > 268_435_456 {
                error!("Packet can't be bigger than 256MB");
                self.expected_size = 0;
                self.incoming_plaintext(poll, &packets_queue, buckets, &buf[4..]);
            } else {
                self.setup_buffer();
                if buf.len() > 4 {
                    trace!("Got enough to read it...");
                    self.incoming_plaintext(poll, &packets_queue, buckets, &buf[4..]);
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
        let rc = match self.initiated_by_me {
            true => {
                match self.tls_client_session {
                    Some(ref mut x) => x.writev_tls(&mut WriteVAdapter::new(&mut self.socket)),
                    None => Err(Error::new(ErrorKind::Other, "Couldn't find session!")),
                }
            }
            false => {
                match self.tls_server_session {
                    Some(ref mut x) => x.writev_tls(&mut WriteVAdapter::new(&mut self.socket)),
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
