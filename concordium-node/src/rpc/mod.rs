mod fails;

use crate::{
    common::{ConnectionType, P2PNodeId, P2PPeer},
    configuration,
    db::P2PDB,
    failure::{Error, Fallible},
    network::{NetworkMessage, NetworkPacketType},
};

use crate::{
    common::counter::{TOTAL_MESSAGES_RECEIVED_COUNTER, TOTAL_MESSAGES_SENT_COUNTER},
    p2p::P2PNode,
    proto::*,
};
use ::grpcio::{self, Environment, ServerBuilder};
use consensus_sys::consensus::ConsensusContainer;
use futures::future::Future;
use std::{
    cell::RefCell,
    net::IpAddr,
    str::FromStr,
    sync::{atomic::Ordering, mpsc, Arc, RwLock},
    time::{SystemTime, UNIX_EPOCH},
};

pub struct RpcServerImplShared {
    pub server:                 Option<grpcio::Server>,
    pub subscription_queue_out: mpsc::Receiver<NetworkMessage>,
    pub subscription_queue_in:  mpsc::Sender<NetworkMessage>,
}

impl RpcServerImplShared {
    pub fn new() -> Self {
        let (sender, receiver) = mpsc::channel::<NetworkMessage>();

        RpcServerImplShared {
            server:                 None,
            subscription_queue_out: receiver,
            subscription_queue_in:  sender,
        }
    }

    pub fn queue_message(&mut self, msg: &NetworkMessage) -> Fallible<()> {
        self.subscription_queue_in
            .send(msg.to_owned())
            .map_err(|_| Error::from(fails::QueueingError))
    }

    pub fn stop_server(&mut self) -> Fallible<()> {
        if let Some(ref mut srv) = self.server {
            srv.shutdown()
                .wait()
                .map_err(fails::GeneralRpcError::from)?
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct RpcServerImpl {
    node:         RefCell<P2PNode>,
    listen_port:  u16,
    listen_addr:  String,
    access_token: String,
    db:           P2PDB,
    consensus:    Option<ConsensusContainer>,
    dptr:         Arc<RwLock<RpcServerImplShared>>,
}

unsafe impl Send for RpcServerImpl {}

impl RpcServerImpl {
    pub fn new(
        node: P2PNode,
        db: P2PDB,
        consensus: Option<ConsensusContainer>,
        conf: &configuration::RpcCliConfig,
    ) -> Self {
        RpcServerImpl {
            node: RefCell::new(node),
            listen_addr: conf.rpc_server_addr.clone(),
            listen_port: conf.rpc_server_port,
            access_token: conf.rpc_server_token.clone(),
            db,
            consensus,
            dptr: Arc::new(RwLock::new(RpcServerImplShared::new())),
        }
    }

    #[inline]
    pub fn queue_message(&self, msg: &NetworkMessage) -> Fallible<()> {
        safe_write!(self.dptr)?.queue_message(msg)
    }

    #[inline]
    pub fn set_server(&self, server: grpcio::Server) -> Fallible<()> {
        safe_write!(self.dptr)?.server = Some(server);
        Ok(())
    }

    pub fn start_server(&mut self) -> Fallible<()> {
        let self_clone = self.clone();
        let env = Arc::new(Environment::new(1));
        let (listen_addr, listen_port) = (self_clone.listen_addr.clone(), self_clone.listen_port);
        let service = create_p2_p(self_clone);
        info!("RPC started on {}:{}", listen_addr, listen_port);
        let mut server = ServerBuilder::new(env)
            .register_service(service)
            .bind(listen_addr, listen_port)
            .build()
            .map_err(|_| fails::ServerBuildError)?;

        server.start();
        self.set_server(server)
    }

    #[inline]
    pub fn stop_server(&mut self) -> Fallible<()> { safe_write!(self.dptr)?.stop_server() }

    fn send_message_with_error(&self, req: &SendMessageRequest) -> Fallible<SuccessResponse> {
        let mut r: SuccessResponse = SuccessResponse::new();

        if req.has_message() && req.has_broadcast() {
            // TODO avoid double-copy
            let msg = req.get_message().get_value().to_vec();

            if req.has_node_id() && !req.get_broadcast().get_value() && req.has_network_id() {
                let id = P2PNodeId::from_str(&req.get_node_id().get_value().to_string())?;

                info!("Sending direct message to: {}", id);
                r.set_value(
                    self.node
                        .borrow_mut()
                        .send_message(
                            Some(id),
                            req.get_network_id().get_value() as u16,
                            None,
                            msg,
                            false,
                        )
                        .map_err(|e| error!("{}", e))
                        .is_ok(),
                );
            } else if req.get_broadcast().get_value() {
                info!("Sending broadcast message");
                r.set_value(
                    self.node
                        .borrow_mut()
                        .send_message(
                            None,
                            req.get_network_id().get_value() as u16,
                            None,
                            msg,
                            true,
                        )
                        .map_err(|e| error!("{}", e))
                        .is_ok(),
                );
            }
        } else {
            r.set_value(false);
        }
        Ok(r)
    }
}

macro_rules! authenticate {
    ($ctx:expr, $req:expr, $sink:expr, $access_token:expr, $inner:block) => {
        match $ctx
            .request_headers()
            .iter()
            .find(|&val| val.0 == "authentication")
        {
            Some(val) => {
                match String::from_utf8(val.1.to_vec()) {
                    Ok(at) => {
                        if at == *$access_token {
                            $inner
                        } else {
                            let f = $sink
                                .fail(::grpcio::RpcStatus::new(
                                    ::grpcio::RpcStatusCode::Unauthenticated,
                                    Some("Missing or incorrect token provided".to_string()),
                                ))
                                .map_err(move |e| error!("failed to reply {:?}: {:?}", $req, e));
                            $ctx.spawn(f);
                        }
                    }
                    Err(e) => {
                        let f = $sink
                            .fail(::grpcio::RpcStatus::new(
                                ::grpcio::RpcStatusCode::InvalidArgument,
                                Some(e.to_string()),
                            ))
                            .map_err(move |e| error!("failed to reply {:?}: {:?}", $req, e));
                        $ctx.spawn(f);
                    }
                };
            }
            _ => {
                let f = $sink
                    .fail(::grpcio::RpcStatus::new(
                        ::grpcio::RpcStatusCode::Unauthenticated,
                        Some("Missing or incorrect token provided".to_string()),
                    ))
                    .map_err(move |e| error!("failed to reply {:?}: {:?}", $req, e));
                $ctx.spawn(f);
            }
        };
    };
}

macro_rules! successful_json_response {
    ($self:ident, $ctx:ident, $req:ident, $sink:ident, $inner_match:expr) => {
        if let Some(ref consensus) = $self.consensus {
            match $inner_match(consensus) {
                Some(ref res) => {
                    let mut r: SuccessfulJsonPayloadResponse = SuccessfulJsonPayloadResponse::new();
                    r.set_json_value(res.to_owned());
                    let f = $sink
                        .success(r)
                        .map_err(move |e| error!("failed to reply {:?}: {:?}", $req, e));
                    $ctx.spawn(f);
                }
                _ => {
                    let f = $sink
                        .fail(::grpcio::RpcStatus::new(
                            ::grpcio::RpcStatusCode::Internal,
                            None,
                        ))
                        .map_err(move |e| error!("failed to reply {:?}: {:?}", $req, e));
                    $ctx.spawn(f);
                }
            }
        }
    };
}

macro_rules! successful_byte_response {
    ($self:ident, $ctx:ident, $req:ident, $sink:ident, $inner_match:expr) => {
        if let Some(ref consensus) = $self.consensus {
            match $inner_match(consensus) {
                Some(res) => {
                    let mut r: SuccessfulBytePayloadResponse = SuccessfulBytePayloadResponse::new();
                    r.set_payload(res);
                    let f = $sink
                        .success(r)
                        .map_err(move |e| error!("failed to reply {:?}: {:?}", $req, e));
                    $ctx.spawn(f);
                }
                _ => {
                    let f = $sink
                        .fail(::grpcio::RpcStatus::new(
                            ::grpcio::RpcStatusCode::Internal,
                            None,
                        ))
                        .map_err(move |e| error!("failed to reply {:?}: {:?}", $req, e));
                    $ctx.spawn(f);
                }
            }
        }
    };
}

impl P2P for RpcServerImpl {
    fn peer_connect(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: PeerConnectRequest,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let mut r: SuccessResponse = SuccessResponse::new();
            if req.has_ip() && req.has_port() {
                let ip = IpAddr::from_str(req.get_ip().get_value()).expect(
                    &format!(
                        "incorrect IP in peer connect request, current value: {:?}",
                        req.get_ip().get_value()
                    )
                    .to_string(),
                );
                let port = req.get_port().get_value() as u16;
                r.set_value(
                    self.node
                        .borrow_mut()
                        .connect(ConnectionType::Node, ip, port, None)
                        .map_err(|e| error!("{}", e))
                        .is_ok(),
                );
            } else {
                r.set_value(false);
            }
            let f = sink
                .success(r)
                .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn peer_version(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<StringResponse>,
    ) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let mut r: StringResponse = StringResponse::new();
            r.set_value(self.node.borrow_mut().get_version());
            let f = sink
                .success(r)
                .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn peer_uptime(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<NumberResponse>,
    ) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let mut r: NumberResponse = NumberResponse::new();
            r.set_value(self.node.borrow_mut().get_uptime() as u64);
            let f = sink
                .success(r)
                .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn peer_total_received(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<NumberResponse>,
    ) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let mut r: NumberResponse = NumberResponse::new();
            r.set_value(TOTAL_MESSAGES_RECEIVED_COUNTER.load(Ordering::Relaxed) as u64);
            let f = sink
                .success(r)
                .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn peer_total_sent(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<NumberResponse>,
    ) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let mut r: NumberResponse = NumberResponse::new();
            r.set_value(TOTAL_MESSAGES_SENT_COUNTER.load(Ordering::Relaxed) as u64);
            let f = sink
                .success(r)
                .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn send_message(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: SendMessageRequest,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let f = match self.send_message_with_error(&req) {
                Ok(r) => sink.success(r),
                Err(e) => sink.fail(grpcio::RpcStatus::new(
                    grpcio::RpcStatusCode::InvalidArgument,
                    Some(
                        e.name()
                            .expect("Unwrapping of an error name failed")
                            .to_string(),
                    ),
                )),
            };
            let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn send_transaction(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: SendTransactionRequest,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        authenticate!(ctx, req, sink, &self.access_token, {
            match self.consensus {
                Some(ref res) => match res.send_transaction(req.get_payload()) {
                    0 => {
                        let mut r: SuccessResponse = SuccessResponse::new();
                        r.set_value(true);
                        let f = sink
                            .success(r)
                            .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
                        ctx.spawn(f);
                    }
                    _ => {
                        let f = sink
                            .fail(::grpcio::RpcStatus::new(
                                ::grpcio::RpcStatusCode::Internal,
                                None,
                            ))
                            .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
                        ctx.spawn(f);
                    }
                },
                _ => {
                    let f = sink
                        .fail(::grpcio::RpcStatus::new(
                            ::grpcio::RpcStatusCode::Internal,
                            None,
                        ))
                        .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
                    ctx.spawn(f);
                }
            }
        });
    }

    fn join_network(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: NetworkChangeRequest,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let mut r: SuccessResponse = SuccessResponse::new();
            if req.has_network_id()
                && req.get_network_id().get_value() > 0
                && req.get_network_id().get_value() < 100_000
            {
                info!(
                    "Attempting to join network {}",
                    req.get_network_id().get_value()
                );
                r.set_value(
                    self.node
                        .borrow_mut()
                        .send_joinnetwork(req.get_network_id().get_value() as u16)
                        .map_err(|e| error!("{}", e))
                        .is_ok(),
                );
            } else {
                r.set_value(false);
            }
            let f = sink
                .success(r)
                .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn leave_network(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: NetworkChangeRequest,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let mut r: SuccessResponse = SuccessResponse::new();
            if req.has_network_id()
                && req.get_network_id().get_value() > 0
                && req.get_network_id().get_value() < 100_000
            {
                info!(
                    "Attempting to leave network {}",
                    req.get_network_id().get_value()
                );
                r.set_value(
                    self.node
                        .borrow_mut()
                        .send_leavenetwork(req.get_network_id().get_value() as u16)
                        .map_err(|e| error!("{}", e))
                        .is_ok(),
                );
            } else {
                r.set_value(false);
            }
            let f = sink
                .success(r)
                .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn peer_stats(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<PeerStatsResponse>,
    ) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let f = match self.node.borrow_mut().get_peer_stats(&vec![]) {
                Ok(data) => {
                    let data: Vec<_> = data
                        .iter()
                        .map(|x| {
                            let mut peer_resp = PeerStatsResponse_PeerStats::new();
                            peer_resp.set_node_id(x.id.to_owned());
                            peer_resp.set_packets_sent(x.sent);
                            peer_resp.set_packets_received(x.received);
                            x.measured_latency()
                                .map_or_else(|| {}, |val| peer_resp.set_measured_latency(val));
                            peer_resp
                        })
                        .collect();
                    let mut resp = PeerStatsResponse::new();
                    resp.set_peerstats(::protobuf::RepeatedField::from_vec(data));
                    sink.success(resp)
                }
                Err(e) => sink.fail(grpcio::RpcStatus::new(
                    grpcio::RpcStatusCode::Aborted,
                    Some(e.name().expect("Couldn't extract error name").to_string()),
                )),
            };
            let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn peer_list(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<PeerListResponse>,
    ) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let borrowed_node = self.node.borrow_mut();
            let f = match borrowed_node.get_peer_stats(&vec![]) {
                Ok(data) => {
                    let data: Vec<_> = data
                        .iter()
                        .map(|x| {
                            let mut peer_resp = PeerElement::new();
                            let mut node_id = ::protobuf::well_known_types::StringValue::new();
                            node_id.set_value(format!("{}", x.id()));
                            peer_resp.set_node_id(node_id);
                            let mut ip = ::protobuf::well_known_types::StringValue::new();
                            ip.set_value(x.ip().to_string());
                            peer_resp.set_ip(ip);
                            let mut port = ::protobuf::well_known_types::Int32Value::new();
                            port.set_value(x.port() as i32);
                            peer_resp.set_port(port);
                            peer_resp
                        })
                        .collect();
                    let mut resp = PeerListResponse::new();
                    let node_type = match &format!("{:?}", borrowed_node.get_node_mode())[..] {
                        "NormalMode" | "NormalPrivateMode" => "Normal",
                        "BootstrapperMode" | "BootstrapperPrivateMode" => "Bootstrapper",
                        _ => panic!(),
                    };
                    resp.set_node_type(node_type.to_string());
                    resp.set_peer(::protobuf::RepeatedField::from_vec(data));
                    sink.success(resp)
                }
                Err(e) => sink.fail(grpcio::RpcStatus::new(
                    grpcio::RpcStatusCode::Aborted,
                    Some(e.name().expect("Couldn't extract error name").to_string()),
                )),
            };
            let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn node_info(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<NodeInfoResponse>,
    ) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let mut resp = NodeInfoResponse::new();
            let mut node_id = ::protobuf::well_known_types::StringValue::new();
            node_id.set_value(format!("{}", self.node.borrow().get_own_id().to_string()));
            resp.set_node_id(node_id);
            let curtime = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .expect("Time went backwards")
                .as_secs();
            resp.set_current_localtime(curtime);
            // TODO: use enums for matching
            let node_type = match &format!("{:?}", self.node.borrow().get_node_mode())[..] {
                "NormalMode" | "NormalPrivateMode" => "Normal",
                "BootstrapperMode" | "BootstrapperPrivateMode" => "Bootstrapper",
                _ => panic!(),
            };
            resp.set_node_type(node_type.to_string());
            let f = sink
                .success(resp)
                .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn subscription_start(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let mut r: SuccessResponse = SuccessResponse::new();
            r.set_value(true);
            let f = sink
                .success(r)
                .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn subscription_stop(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let mut r: SuccessResponse = SuccessResponse::new();
            r.set_value(true);
            let f = sink
                .success(r)
                .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn subscription_poll(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<P2PNetworkMessage>,
    ) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let mut r: P2PNetworkMessage = P2PNetworkMessage::new();

            if let Ok(read_lock_dptr) = self.dptr.read() {
                if let Ok(msg) = read_lock_dptr.subscription_queue_out.try_recv() {
                    if let NetworkMessage::NetworkPacket(ref packet, ..) = msg {
                        let mut inner_msg = packet.message.to_owned();
                        let view_inner_msg = inner_msg.read_all_into_view().unwrap();
                        let msg = view_inner_msg.as_slice().to_vec();

                        match packet.packet_type {
                            NetworkPacketType::DirectMessage(..) => {
                                let mut i_msg = MessageDirect::new();
                                i_msg.set_data(msg);
                                r.set_message_direct(i_msg);
                            }
                            NetworkPacketType::BroadcastedMessage => {
                                let mut i_msg = MessageBroadcast::new();
                                i_msg.set_data(msg);
                                r.set_message_broadcast(i_msg);
                            }
                        };

                        r.set_network_id(packet.network_id as u32);
                        r.set_message_id(packet.message_id.to_owned());
                        r.set_sender(format!("{}", packet.peer.id()));
                    } else {
                        r.set_message_none(MessageNone::new());
                    }
                }
            } else {
                r.set_message_none(MessageNone::new())
            }
            let f = sink
                .success(r)
                .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn ban_node(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: PeerElement,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let mut r: SuccessResponse = SuccessResponse::new();
            let f = if req.has_node_id() && req.has_ip() && req.has_port() {
                let req_id = req.get_node_id().get_value().to_string();
                let node_id = P2PNodeId::from_str(&req_id);
                let ip = IpAddr::from_str(&req.get_ip().get_value().to_string());
                let port = req.get_port().get_value() as u16;
                if node_id.is_ok() && ip.is_ok() {
                    let mut node = self.node.borrow_mut();
                    let peer =
                        P2PPeer::from(ConnectionType::Node, node_id.unwrap(), ip.unwrap(), port);
                    if node.ban_node(peer.clone()).is_ok() {
                        let db_done = self.db.insert_ban(
                            &peer.id().to_string(),
                            &peer.ip().to_string(),
                            peer.port(),
                        );
                        if db_done {
                            r.set_value(node.send_ban(peer.clone()).is_ok());
                        } else {
                            node.unban_node(peer.clone())
                                .map_err(|e| error!("{}", e))
                                .ok();
                            r.set_value(false);
                        }
                    }
                    sink.success(r)
                } else {
                    sink.fail(grpcio::RpcStatus::new(
                        grpcio::RpcStatusCode::InvalidArgument,
                        Some("invalid node_id or ip".to_string()),
                    ))
                }
            } else {
                sink.fail(grpcio::RpcStatus::new(
                    grpcio::RpcStatusCode::InvalidArgument,
                    Some("missing node_id, ip or port".to_string()),
                ))
            };
            let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn unban_node(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: PeerElement,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let mut r: SuccessResponse = SuccessResponse::new();
            let f = if req.has_node_id() && req.has_ip() && req.has_port() {
                let req_id = req.get_node_id().get_value().to_string();
                let node_id = P2PNodeId::from_str(&req_id);
                let ip = IpAddr::from_str(&req.get_ip().get_value().to_string());
                let port = req.get_port().get_value() as u16;
                if node_id.is_ok() && ip.is_ok() {
                    let mut node = self.node.borrow_mut();
                    let peer =
                        P2PPeer::from(ConnectionType::Node, node_id.unwrap(), ip.unwrap(), port);
                    if node.unban_node(peer.clone()).is_ok() {
                        let db_done = self.db.delete_ban(
                            peer.id().to_string(),
                            peer.ip().to_string(),
                            peer.port(),
                        );
                        if db_done {
                            r.set_value(node.send_unban(peer.clone()).is_ok());
                        } else {
                            node.ban_node(peer.clone())
                                .map_err(|e| error!("{}", e))
                                .ok();
                            r.set_value(false);
                        }
                    }
                    sink.success(r)
                } else {
                    sink.fail(grpcio::RpcStatus::new(
                        grpcio::RpcStatusCode::InvalidArgument,
                        Some("invalid node_id or ip".to_string()),
                    ))
                }
            } else {
                sink.fail(grpcio::RpcStatus::new(
                    grpcio::RpcStatusCode::InvalidArgument,
                    Some("missing node_id, ip or port".to_string()),
                ))
            };
            let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn get_consensus_status(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<SuccessfulJsonPayloadResponse>,
    ) {
        authenticate!(ctx, req, sink, &self.access_token, {
            successful_json_response!(self, ctx, req, sink, |consensus: &ConsensusContainer| {
                consensus.get_consensus_status()
            });
        });
    }

    fn get_branches(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<SuccessfulJsonPayloadResponse>,
    ) {
        authenticate!(ctx, req, sink, &self.access_token, {
            successful_json_response!(self, ctx, req, sink, |consensus: &ConsensusContainer| {
                consensus.get_branches()
            });
        });
    }

    fn get_block_info(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: BlockHash,
        sink: ::grpcio::UnarySink<SuccessfulJsonPayloadResponse>,
    ) {
        authenticate!(ctx, req, sink, &self.access_token, {
            successful_json_response!(self, ctx, req, sink, |consensus: &ConsensusContainer| {
                consensus.get_block_info(&req.get_block_hash())
            });
        });
    }

    fn get_ancestors(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: BlockHashAndAmount,
        sink: ::grpcio::UnarySink<SuccessfulJsonPayloadResponse>,
    ) {
        authenticate!(ctx, req, sink, &self.access_token, {
            successful_json_response!(self, ctx, req, sink, |consensus: &ConsensusContainer| {
                consensus.get_ancestors(&req.get_block_hash(), req.get_amount())
            });
        });
    }

    fn get_last_final_account_list(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<SuccessfulBytePayloadResponse>,
    ) {
        authenticate!(ctx, req, sink, &self.access_token, {
            successful_byte_response!(self, ctx, req, sink, |consensus: &ConsensusContainer| {
                consensus.get_last_final_account_list()
            });
        });
    }

    fn get_last_final_instances(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<SuccessfulBytePayloadResponse>,
    ) {
        authenticate!(ctx, req, sink, &self.access_token, {
            successful_byte_response!(self, ctx, req, sink, |consensus: &ConsensusContainer| {
                consensus.get_last_final_instances()
            });
        });
    }

    fn get_last_final_account_info(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: AccountAddress,
        sink: ::grpcio::UnarySink<SuccessfulBytePayloadResponse>,
    ) {
        authenticate!(ctx, req, sink, &self.access_token, {
            successful_byte_response!(self, ctx, req, sink, |consensus: &ConsensusContainer| {
                consensus.get_last_final_account_info(req.get_payload())
            });
        });
    }

    fn get_last_final_instance_info(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: ContractInstanceAddress,
        sink: ::grpcio::UnarySink<SuccessfulBytePayloadResponse>,
    ) {
        authenticate!(ctx, req, sink, &self.access_token, {
            successful_byte_response!(self, ctx, req, sink, |consensus: &ConsensusContainer| {
                consensus.get_last_final_instance_info(req.get_payload())
            });
        });
    }
}
