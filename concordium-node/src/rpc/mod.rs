mod fails;

#[cfg(feature = "benchmark")]
use crate::utils;
use crate::{
    common::{
        counter::{TOTAL_MESSAGES_RECEIVED_COUNTER, TOTAL_MESSAGES_SENT_COUNTER},
        P2PNodeId, PeerType,
    },
    configuration,
    failure::Fallible,
    network::{request::RequestedElementType, NetworkId, NetworkMessage, NetworkPacketType},
    p2p::{
        banned_nodes::{remove_ban, BannedNode},
        p2p_node::{send_broadcast_message, send_direct_message},
        P2PNode,
    },
    proto::*,
};

use concordium_common::{
    hybrid_buf::HybridBuf, ConsensusFfiResponse, PacketType, SerializeToBytes,
};
use concordium_consensus::consensus::{ConsensusContainer, CALLBACK_QUEUE};
use concordium_global_state::tree::messaging::{ConsensusMessage, DistributionMode, MessageType};
use futures::future::Future;
use grpcio::{self, Environment, ServerBuilder};

use std::{
    convert::TryFrom,
    net::{IpAddr, SocketAddr},
    str::FromStr,
    sync::{atomic::Ordering, mpsc, Arc, Mutex},
    time::{SystemTime, UNIX_EPOCH},
};

pub struct RpcServerImpl {
    node:         Arc<P2PNode>,
    listen_port:  u16,
    listen_addr:  String,
    access_token: String,
    consensus:    Option<ConsensusContainer>,
    server:       Arc<Mutex<Option<grpcio::Server>>>,
    receiver:     Option<mpsc::Receiver<NetworkMessage>>,
}

// a trick implementation so we can have a lockless Receiver
impl Clone for RpcServerImpl {
    fn clone(&self) -> Self {
        RpcServerImpl {
            node:         Arc::clone(&self.node),
            listen_port:  self.listen_port,
            listen_addr:  self.listen_addr.clone(),
            access_token: self.access_token.clone(),
            consensus:    self.consensus.clone(),
            server:       self.server.clone(),
            receiver:     None,
        }
    }
}

impl RpcServerImpl {
    pub fn new(
        node: Arc<P2PNode>,
        consensus: Option<ConsensusContainer>,
        conf: &configuration::RpcCliConfig,
        subscription_queue_out: mpsc::Receiver<NetworkMessage>,
    ) -> Self {
        RpcServerImpl {
            node: Arc::clone(&node),
            listen_addr: conf.rpc_server_addr.clone(),
            listen_port: conf.rpc_server_port,
            access_token: conf.rpc_server_token.clone(),
            consensus,
            server: Default::default(),
            receiver: Some(subscription_queue_out),
        }
    }

    #[inline]
    pub fn set_server(&mut self, server: grpcio::Server) -> Fallible<()> {
        let mut srv = safe_lock!(self.server)?;
        *srv = Some(server);
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
        self.set_server(server)?;

        Ok(())
    }

    #[inline]
    pub fn stop_server(&mut self) -> Fallible<()> {
        if let Some(ref mut srv) = *safe_lock!(self.server)? {
            srv.shutdown()
                .wait()
                .map_err(fails::GeneralRpcError::from)?;
        }
        Ok(())
    }

    fn send_message_with_error(&self, req: &SendMessageRequest) -> Fallible<SuccessResponse> {
        let mut r: SuccessResponse = SuccessResponse::new();

        if req.has_message() && req.has_broadcast() {
            // TODO avoid double-copy
            let msg = HybridBuf::try_from(req.get_message().get_value())?;
            let network_id = NetworkId::from(req.get_network_id().get_value() as u16);

            if req.has_node_id() && !req.get_broadcast().get_value() && req.has_network_id() {
                let id = P2PNodeId::from_str(&req.get_node_id().get_value().to_string())?;

                trace!("Sending direct message to: {}", id);
                r.set_value(
                    send_direct_message(&self.node, Some(id), network_id, None, msg)
                        .map_err(|e| error!("{}", e))
                        .is_ok(),
                );
            } else if req.get_broadcast().get_value() {
                trace!("Sending broadcast message");
                r.set_value(
                    send_broadcast_message(&self.node, vec![], network_id, None, msg)
                        .map_err(|e| error!("{}", e))
                        .is_ok(),
                );
            }
        } else {
            r.set_value(false);
        }
        Ok(r)
    }

    fn receive_network_msg(&self) -> Option<NetworkMessage> {
        self.receiver.as_ref().unwrap().try_recv().ok()
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
                        if at == $access_token {
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
            let res = $inner_match(consensus);
            let mut r: SuccessfulJsonPayloadResponse = SuccessfulJsonPayloadResponse::new();
            r.set_json_value(res.to_owned());
            let f = $sink
                .success(r)
                .map_err(move |e| error!("failed to reply {:?}: {:?}", $req, e));
            $ctx.spawn(f);
        }
    };
}

macro_rules! successful_bool_response {
    ($self:ident, $ctx:ident, $req:ident, $sink:ident, $inner_match:expr) => {
        if let Some(ref consensus) = $self.consensus {
            let res = $inner_match(consensus);
            let mut r: SuccessResponse = SuccessResponse::new();
            r.set_value(res.to_owned());
            let f = $sink
                .success(r)
                .map_err(move |e| error!("failed to reply {:?}: {:?}", $req, e));
            $ctx.spawn(f);
        }
    };
}

macro_rules! successful_byte_response {
    ($self:ident, $ctx:ident, $req:ident, $sink:ident, $inner_match:expr) => {
        if let Some(ref consensus) = $self.consensus {
            let res = $inner_match(consensus);
            let mut r: SuccessfulBytePayloadResponse = SuccessfulBytePayloadResponse::new();
            r.set_payload(res);
            let f = $sink
                .success(r)
                .map_err(move |e| error!("failed to reply {:?}: {:?}", $req, e));
            $ctx.spawn(f);
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
        authenticate!(ctx, req, sink, self.access_token, {
            let mut r: SuccessResponse = SuccessResponse::new();
            let f = if req.has_ip() && req.has_port() {
                let ip = IpAddr::from_str(req.get_ip().get_value()).unwrap_or_else(|_| {
                    panic!(
                        "incorrect IP in peer connect request, current value: {:?}",
                        req.get_ip().get_value()
                    )
                });
                let port = req.get_port().get_value() as u16;
                let addr = SocketAddr::new(ip, port);
                r.set_value(self.node.connect(PeerType::Node, addr, None).is_ok());
                sink.success(r)
            } else {
                r.set_value(false);
                sink.success(r)
            };
            let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn peer_version(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<StringResponse>,
    ) {
        authenticate!(ctx, req, sink, self.access_token, {
            let mut r: StringResponse = StringResponse::new();
            r.set_value(crate::VERSION.to_owned());
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
        authenticate!(ctx, req, sink, self.access_token, {
            let mut r: NumberResponse = NumberResponse::new();
            let f = {
                let uptime = self.node.get_uptime() as u64;
                r.set_value(uptime);
                sink.success(r)
            };
            let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn peer_total_received(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<NumberResponse>,
    ) {
        authenticate!(ctx, req, sink, self.access_token, {
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
        authenticate!(ctx, req, sink, self.access_token, {
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
        authenticate!(ctx, req, sink, self.access_token, {
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
        authenticate!(ctx, req, sink, self.access_token, {
            match self.consensus {
                Some(ref consensus) => {
                    let payload = req.get_payload();

                    let request = ConsensusMessage::new(
                        MessageType::Inbound(self.node.id().0, DistributionMode::Broadcast),
                        PacketType::Transaction,
                        Arc::from(payload),
                        vec![],
                    );
                    let gs_result = CALLBACK_QUEUE.send_message(request);
                    let consensus_result = consensus.send_transaction(payload);

                    match (gs_result, consensus_result) {
                        (Ok(_), ConsensusFfiResponse::Success) => {
                            let mut r: SuccessResponse = SuccessResponse::new();
                            r.set_value(true);
                            let f = sink
                                .success(r)
                                .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
                            ctx.spawn(f);
                        }
                        (_, e) => {
                            let f = sink
                                .fail(::grpcio::RpcStatus::new(
                                    ::grpcio::RpcStatusCode::Internal,
                                    Some(format!(
                                        "Got non-success response from FFI interface {:?}",
                                        e
                                    )),
                                ))
                                .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
                            ctx.spawn(f);
                        }
                    }
                }
                _ => {
                    let f = sink
                        .fail(::grpcio::RpcStatus::new(
                            ::grpcio::RpcStatusCode::Internal,
                            Some(String::from("Consensus container is not initialized!")),
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
        authenticate!(ctx, req, sink, self.access_token, {
            let mut r: SuccessResponse = SuccessResponse::new();
            let f = if req.has_network_id()
                && req.get_network_id().get_value() > 0
                && req.get_network_id().get_value() < 100_000
            {
                info!(
                    "Attempting to join network {}",
                    req.get_network_id().get_value()
                );
                let network_id = NetworkId::from(req.get_network_id().get_value() as u16);
                self.node.send_joinnetwork(network_id);
                r.set_value(true);
                sink.success(r)
            } else {
                r.set_value(false);
                sink.success(r)
            };
            let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn leave_network(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: NetworkChangeRequest,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        authenticate!(ctx, req, sink, self.access_token, {
            let mut r: SuccessResponse = SuccessResponse::new();
            let f = if req.has_network_id()
                && req.get_network_id().get_value() > 0
                && req.get_network_id().get_value() < 100_000
            {
                info!(
                    "Attempting to leave network {}",
                    req.get_network_id().get_value()
                );
                let network_id = NetworkId::from(req.get_network_id().get_value() as u16);
                self.node.send_leavenetwork(network_id);
                r.set_value(true);
                sink.success(r)
            } else {
                r.set_value(false);
                sink.success(r)
            };
            let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn peer_stats(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: PeersRequest,
        sink: ::grpcio::UnarySink<PeerStatsResponse>,
    ) {
        authenticate!(ctx, req, sink, self.access_token, {
            let peer_stats = self.node.get_peer_stats();

            let f = {
                let data = peer_stats
                    .iter()
                    .filter(|peer| match peer.peer_type {
                        PeerType::Node => true,
                        PeerType::Bootstrapper => req.get_include_bootstrappers(),
                    })
                    .map(|peer| {
                        let mut peer_resp = PeerStatsResponse_PeerStats::new();
                        peer_resp.set_node_id(P2PNodeId(peer.id).to_string());
                        peer_resp.set_packets_sent(peer.sent.load(Ordering::Relaxed));
                        peer_resp.set_packets_received(peer.received.load(Ordering::Relaxed));

                        let latency = peer.measured_latency.load(Ordering::Relaxed);
                        if latency > 0 {
                            peer_resp.set_measured_latency(latency);
                        }

                        peer_resp
                    })
                    .collect();
                let mut resp = PeerStatsResponse::new();
                resp.set_peerstats(::protobuf::RepeatedField::from_vec(data));
                sink.success(resp)
            };
            let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn peer_list(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: PeersRequest,
        sink: ::grpcio::UnarySink<PeerListResponse>,
    ) {
        authenticate!(ctx, req, sink, self.access_token, {
            let f = {
                let peer_type = self.node.peer_type();
                let data = self
                    .node
                    .get_peer_stats()
                    .iter()
                    .filter(|peer| match peer.peer_type {
                        PeerType::Node => true,
                        PeerType::Bootstrapper => req.get_include_bootstrappers(),
                    })
                    .map(|peer| {
                        let mut peer_resp = PeerElement::new();
                        let mut node_id = ::protobuf::well_known_types::StringValue::new();
                        node_id.set_value(peer.id.to_string());
                        peer_resp.set_node_id(node_id);
                        let mut ip = ::protobuf::well_known_types::StringValue::new();
                        ip.set_value(peer.addr.ip().to_string());
                        peer_resp.set_ip(ip);
                        let mut port = ::protobuf::well_known_types::UInt32Value::new();
                        port.set_value(peer.addr.port().into());
                        peer_resp.set_port(port);
                        peer_resp
                    })
                    .collect();
                let mut resp = PeerListResponse::new();
                let peer_type = match &format!("{:?}", peer_type)[..] {
                    "Node" => "Node",
                    "Bootstrapper" => "Bootstrapper",
                    _ => panic!(),
                };
                resp.set_peer_type(peer_type.to_string());
                resp.set_peer(::protobuf::RepeatedField::from_vec(data));
                sink.success(resp)
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
        authenticate!(ctx, req, sink, self.access_token, {
            let mut resp = NodeInfoResponse::new();
            let mut node_id = ::protobuf::well_known_types::StringValue::new();
            let f = {
                let (id, peer_type) = (self.node.id(), self.node.peer_type());

                node_id.set_value(id.to_string());
                resp.set_node_id(node_id);
                let curtime = SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .expect("Time went backwards")
                    .as_secs();
                resp.set_current_localtime(curtime);
                resp.set_peer_type(peer_type.to_string());
                match self.consensus {
                    Some(ref consensus) => {
                        resp.set_consensus_baker_running(consensus.is_baking());
                        resp.set_consensus_running(true);
                        resp.set_consensus_type(consensus.consensus_type.to_string());
                    }
                    None => {
                        resp.set_consensus_baker_running(false);
                        resp.set_consensus_running(false);
                        resp.set_consensus_type("Inactive".to_owned());
                    }
                }
                sink.success(resp)
            };
            let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn subscription_start(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        authenticate!(ctx, req, sink, self.access_token, {
            let f = {
                self.node.rpc_subscription_start();
                let mut r: SuccessResponse = SuccessResponse::new();
                r.set_value(true);
                sink.success(r)
            };
            let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn subscription_stop(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        authenticate!(ctx, req, sink, self.access_token, {
            let f = {
                let mut r: SuccessResponse = SuccessResponse::new();
                r.set_value(self.node.rpc_subscription_stop());
                sink.success(r)
            };
            let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn subscription_poll(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<P2PNetworkMessage>,
    ) {
        authenticate!(ctx, req, sink, self.access_token, {
            let mut r: P2PNetworkMessage = P2PNetworkMessage::new();

            let f = {
                if let Some(network_msg) = self.receive_network_msg() {
                    if let NetworkMessage::NetworkPacket(ref packet, ..) = network_msg {
                        let mut inner_msg = packet.message.to_owned();
                        if let Ok(view_inner_msg) = inner_msg.remaining_bytes() {
                            let msg = view_inner_msg.into_owned();

                            match packet.packet_type {
                                NetworkPacketType::DirectMessage(..) => {
                                    let mut i_msg = MessageDirect::new();
                                    i_msg.set_data(msg);
                                    r.set_message_direct(i_msg);
                                }
                                NetworkPacketType::BroadcastedMessage(..) => {
                                    let mut i_msg = MessageBroadcast::new();
                                    i_msg.set_data(msg);
                                    r.set_message_broadcast(i_msg);
                                }
                            };

                            r.set_network_id(u32::from(packet.network_id.id));
                            r.set_message_id(packet.message_id.to_vec());
                            r.set_sender(packet.peer.id().to_string());
                        } else {
                            r.set_message_none(MessageNone::new());
                        }
                    }
                } else {
                    r.set_message_none(MessageNone::new());
                }
                sink.success(r)
            };
            let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn ban_node(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: PeerElement,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        authenticate!(ctx, req, sink, self.access_token, {
            let mut r: SuccessResponse = SuccessResponse::new();

            let banned_node = if req.has_node_id() && !req.has_ip() {
                P2PNodeId::from_str(&req.get_node_id().get_value().to_string())
                    .ok()
                    .map(BannedNode::ById)
            } else if req.has_ip() && !req.has_node_id() {
                IpAddr::from_str(&req.get_ip().get_value().to_string())
                    .ok()
                    .map(BannedNode::ByAddr)
            } else {
                None
            };

            let f = if let Some(to_ban) = banned_node {
                match self.node.ban_node(to_ban) {
                    Ok(_) => {
                        r.set_value(true);
                    }
                    Err(e) => {
                        error!("{}", e);
                        r.set_value(false);
                    }
                }

                sink.success(r)
            } else {
                sink.fail(grpcio::RpcStatus::new(
                    grpcio::RpcStatusCode::InvalidArgument,
                    Some("Missing banned IP or address".to_string()),
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
        authenticate!(ctx, req, sink, self.access_token, {
            let mut r: SuccessResponse = SuccessResponse::new();

            let banned_node = if req.has_node_id() && !req.has_ip() {
                P2PNodeId::from_str(&req.get_node_id().get_value().to_string())
                    .ok()
                    .map(BannedNode::ById)
            } else if req.has_ip() && !req.has_node_id() {
                IpAddr::from_str(&req.get_ip().get_value().to_string())
                    .ok()
                    .map(BannedNode::ByAddr)
            } else {
                None
            };

            let f = if let Some(to_unban) = banned_node {
                let store_key = to_unban.serialize();
                match remove_ban(&self.node.kvs, &store_key)
                    .and_then(|_| self.node.unban_node(to_unban))
                {
                    Ok(_) => {
                        self.node.send_unban(to_unban);
                        r.set_value(true);
                    }
                    Err(e) => {
                        error!("{}", e);
                        r.set_value(false);
                    }
                }

                sink.success(r)
            } else {
                sink.fail(grpcio::RpcStatus::new(
                    grpcio::RpcStatusCode::InvalidArgument,
                    Some("Missing banned ID or address".to_string()),
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
        authenticate!(ctx, req, sink, self.access_token, {
            successful_json_response!(self, ctx, req, sink, |consensus: &ConsensusContainer| {
                consensus.get_consensus_status()
            });
        });
    }

    fn start_baker(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        authenticate!(ctx, req, sink, self.access_token, {
            successful_bool_response!(self, ctx, req, sink, |consensus: &ConsensusContainer| {
                consensus.start_baker()
            });
        });
    }

    fn stop_baker(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        authenticate!(ctx, req, sink, self.access_token, {
            successful_bool_response!(self, ctx, req, sink, |consensus: &ConsensusContainer| {
                consensus.stop_baker()
            });
        });
    }

    fn get_branches(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<SuccessfulJsonPayloadResponse>,
    ) {
        authenticate!(ctx, req, sink, self.access_token, {
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
        authenticate!(ctx, req, sink, self.access_token, {
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
        authenticate!(ctx, req, sink, self.access_token, {
            successful_json_response!(self, ctx, req, sink, |consensus: &ConsensusContainer| {
                consensus.get_ancestors(&req.get_block_hash(), req.get_amount())
            });
        });
    }

    fn get_account_list(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: BlockHash,
        sink: ::grpcio::UnarySink<SuccessfulJsonPayloadResponse>,
    ) {
        authenticate!(ctx, req, sink, self.access_token, {
            successful_json_response!(self, ctx, req, sink, |consensus: &ConsensusContainer| {
                consensus.get_account_list(&req.get_block_hash())
            });
        });
    }

    fn get_instances(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: BlockHash,
        sink: ::grpcio::UnarySink<SuccessfulJsonPayloadResponse>,
    ) {
        authenticate!(ctx, req, sink, self.access_token, {
            successful_json_response!(self, ctx, req, sink, |consensus: &ConsensusContainer| {
                consensus.get_instances(&req.get_block_hash())
            });
        });
    }

    fn get_account_info(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: GetAddressInfoRequest,
        sink: ::grpcio::UnarySink<SuccessfulJsonPayloadResponse>,
    ) {
        authenticate!(ctx, req, sink, self.access_token, {
            successful_json_response!(self, ctx, req, sink, |consensus: &ConsensusContainer| {
                consensus.get_account_info(&req.get_block_hash(), &req.get_address())
            });
        });
    }

    fn get_instance_info(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: GetAddressInfoRequest,
        sink: ::grpcio::UnarySink<SuccessfulJsonPayloadResponse>,
    ) {
        authenticate!(ctx, req, sink, self.access_token, {
            successful_json_response!(self, ctx, req, sink, |consensus: &ConsensusContainer| {
                consensus.get_instance_info(&req.get_block_hash(), &req.get_address())
            });
        });
    }

    fn get_reward_status(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: BlockHash,
        sink: ::grpcio::UnarySink<SuccessfulJsonPayloadResponse>,
    ) {
        authenticate!(ctx, req, sink, self.access_token, {
            successful_json_response!(self, ctx, req, sink, |consensus: &ConsensusContainer| {
                consensus.get_reward_status(&req.get_block_hash())
            });
        });
    }

    fn get_birk_parameters(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: BlockHash,
        sink: ::grpcio::UnarySink<SuccessfulJsonPayloadResponse>,
    ) {
        authenticate!(ctx, req, sink, self.access_token, {
            successful_json_response!(self, ctx, req, sink, |consensus: &ConsensusContainer| {
                consensus.get_birk_parameters(&req.get_block_hash())
            });
        });
    }

    fn get_module_list(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: BlockHash,
        sink: ::grpcio::UnarySink<SuccessfulJsonPayloadResponse>,
    ) {
        authenticate!(ctx, req, sink, self.access_token, {
            successful_json_response!(self, ctx, req, sink, |consensus: &ConsensusContainer| {
                consensus.get_module_list(&req.get_block_hash())
            });
        });
    }

    fn get_module_source(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: GetModuleSourceRequest,
        sink: ::grpcio::UnarySink<SuccessfulBytePayloadResponse>,
    ) {
        authenticate!(ctx, req, sink, self.access_token, {
            successful_byte_response!(self, ctx, req, sink, |consensus: &ConsensusContainer| {
                consensus.get_module_source(&req.get_block_hash(), &req.get_module_ref())
            });
        });
    }

    fn get_banned_peers(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<PeerListResponse>,
    ) {
        authenticate!(ctx, req, sink, self.access_token, {
            let mut r: PeerListResponse = PeerListResponse::new();
            let f = {
                r.set_peer(::protobuf::RepeatedField::from_vec(
                    if let Ok(banlist) = self.node.get_banlist() {
                        banlist
                            .into_iter()
                            .map(|banned_node| {
                                let mut pe = PeerElement::new();
                                let mut node_id = ::protobuf::well_known_types::StringValue::new();
                                node_id.set_value(match banned_node {
                                    BannedNode::ById(id) => id.to_string(),
                                    _ => "*".to_owned(),
                                });
                                pe.set_node_id(node_id);
                                let mut ip = ::protobuf::well_known_types::StringValue::new();
                                ip.set_value(match banned_node {
                                    BannedNode::ByAddr(addr) => addr.to_string(),
                                    _ => "*".to_owned(),
                                });
                                pe.set_ip(ip);
                                pe
                            })
                            .collect::<Vec<PeerElement>>()
                    } else {
                        error!("Can't load the banlist");
                        Vec::new()
                    },
                ));
                sink.success(r)
            };
            let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn shutdown(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        authenticate!(ctx, req, sink, self.access_token, {
            let f = {
                let mut r: SuccessResponse = SuccessResponse::new();
                r.set_value(self.node.close());
                sink.success(r)
            };
            let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    #[cfg(feature = "benchmark")]
    fn tps_test(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: TpsRequest,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        authenticate!(ctx, req, sink, self.access_token, {
            let f = {
                let (network_id, id, dir) = (
                    NetworkId::from(req.network_id as u16),
                    req.id.clone(),
                    req.directory.clone(),
                );
                let _node_list = self.node.get_peer_stats();
                if !_node_list
                    .into_iter()
                    .any(|s| P2PNodeId(s.id).to_string() == id)
                {
                    sink.fail(grpcio::RpcStatus::new(
                        grpcio::RpcStatusCode::FailedPrecondition,
                        Some("I don't have the required peers!".to_string()),
                    ))
                } else {
                    let test_messages = utils::get_tps_test_messages(Some(dir));
                    let mut r: SuccessResponse = SuccessResponse::new();
                    let result = !(test_messages.into_iter().map(|message| {
                        let out_bytes_len = message.len();
                        let to_send = P2PNodeId::from_str(&id).ok();
                        match send_direct_message(
                            &self.node,
                            to_send,
                            network_id,
                            None,
                            HybridBuf::try_from(message).unwrap(),
                        ) {
                            Ok(_) => {
                                info!("Sent TPS test bytes of len {}", out_bytes_len);
                                Ok(())
                            }
                            Err(_) => {
                                error!("Couldn't send TPS test message!");
                                Err(())
                            }
                        }
                    }))
                    .any(|res| res.is_err());
                    r.set_value(result);
                    sink.success(r)
                }
            };
            let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    #[cfg(not(feature = "benchmark"))]
    fn tps_test(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: TpsRequest,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        let f = sink
            .fail(grpcio::RpcStatus::new(
                grpcio::RpcStatusCode::Unavailable,
                Some("Feature not activated".to_string()),
            ))
            .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
        ctx.spawn(f);
    }

    #[cfg(not(feature = "network_dump"))]
    fn dump_start(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: DumpRequest,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        let f = sink
            .fail(grpcio::RpcStatus::new(
                grpcio::RpcStatusCode::Unavailable,
                Some("Feature not activated".to_string()),
            ))
            .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
        ctx.spawn(f);
    }

    #[cfg(feature = "network_dump")]
    fn dump_start(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: DumpRequest,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        authenticate!(ctx, req, sink, self.access_token, {
            let f = if let Ok(locked_node) = self.node.lock() {
                let mut r = SuccessResponse::new();
                let file_path = req.get_file().to_owned();
                if locked_node
                    .activate_dump(
                        if file_path.is_empty() {
                            "dump"
                        } else {
                            &file_path
                        },
                        req.get_raw(),
                    )
                    .is_ok()
                {
                    r.set_value(true);
                } else {
                    r.set_value(false);
                }
                sink.success(r)
            } else {
                sink.fail(grpcio::RpcStatus::new(
                    grpcio::RpcStatusCode::ResourceExhausted,
                    Some("Node can't be locked".to_string()),
                ))
            };
            let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    #[cfg(not(feature = "network_dump"))]
    fn dump_stop(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        let f = sink
            .fail(grpcio::RpcStatus::new(
                grpcio::RpcStatusCode::Unavailable,
                Some("Feature not activated".to_string()),
            ))
            .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
        ctx.spawn(f);
    }

    #[cfg(feature = "network_dump")]
    fn dump_stop(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        authenticate!(ctx, req, sink, self.access_token, {
            let f = if let Ok(locked_node) = self.node.lock() {
                let mut r = SuccessResponse::new();
                r.set_value(locked_node.stop_dump().is_ok());

                sink.success(r)
            } else {
                sink.fail(grpcio::RpcStatus::new(
                    grpcio::RpcStatusCode::ResourceExhausted,
                    Some("Node can't be locked".to_string()),
                ))
            };
            let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn retransmit_request(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: RetransmitRequestMessage,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        authenticate!(ctx, req, sink, self.access_token, {
            let f = {
                if req.has_since() {
                    let mut r: SuccessResponse = SuccessResponse::new();
                    self.node.send_retransmit(
                        RequestedElementType::from(req.get_element_type() as u8),
                        req.get_since().get_value(),
                        NetworkId::from(req.get_network_id() as u16),
                    );
                    r.set_value(true);
                    sink.success(r)
                } else {
                    sink.fail(grpcio::RpcStatus::new(
                        grpcio::RpcStatusCode::InvalidArgument,
                        Some("`Since` argument can not be ommited".to_string()),
                    ))
                }
            };
            let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn get_skov_stats(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<SuccesfulStructResponse>,
    ) {
        authenticate!(ctx, req, sink, self.access_token, {
            let f = if let Some(stats) = &self.node.stats_export_service {
                let mut r: SuccesfulStructResponse = SuccesfulStructResponse::new();
                let mut s = GRPCSkovStats::new();
                let stat_values = stats.get_gs_stats();
                s.set_gs_block_receipt(stat_values.0 as u32);
                s.set_gs_block_entry(stat_values.1 as u32);
                s.set_gs_block_query(stat_values.2 as u32);
                s.set_gs_finalization_receipt(stat_values.3 as u32);
                s.set_gs_finalization_entry(stat_values.4 as u32);
                s.set_gs_finalization_query(stat_values.5 as u32);
                r.set_gs_stats(s);
                sink.success(r)
            } else {
                sink.fail(grpcio::RpcStatus::new(
                    grpcio::RpcStatusCode::ResourceExhausted,
                    Some("Stats server can't be locked".to_string()),
                ))
            };
            let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn hook_transaction(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: TransactionHash,
        sink: ::grpcio::UnarySink<SuccessfulJsonPayloadResponse>,
    ) {
        authenticate!(ctx, req, sink, self.access_token, {
            successful_json_response!(self, ctx, req, sink, |consensus: &ConsensusContainer| {
                consensus.hook_transaction(&req.get_transaction_hash())
            });
        });
    }
}

#[cfg(test)]
mod tests {
    #[cfg(feature = "benchmark")]
    use crate::test_utils::await_direct_message;
    use crate::{
        common::{P2PNodeId, PeerType},
        configuration,
        p2p::p2p_node::send_broadcast_message,
        proto::concordium_p2p_rpc_grpc::P2PClient,
        rpc::RpcServerImpl,
        test_utils::{
            await_broadcast_message, await_handshake, connect, get_test_config, make_node_and_sync,
            make_node_and_sync_with_rpc, next_available_port, setup_logger,
        },
    };
    use chrono::prelude::Utc;
    use concordium_common::hybrid_buf::HybridBuf;
    use failure::Fallible;
    use grpcio::{ChannelBuilder, EnvBuilder};
    use std::{convert::TryFrom, sync::Arc};

    // Same as create_node_rpc_call_option but also outputs the Message receiver
    fn create_node_rpc_call_option_waiter(
        nt: PeerType,
    ) -> (P2PClient, RpcServerImpl, grpcio::CallOption) {
        let conf = configuration::parse_config().expect("Can't parse the config file!");
        let app_prefs = configuration::AppPreferences::new(
            conf.common.config_dir.to_owned(),
            conf.common.data_dir.to_owned(),
        );

        let (node, _, rpc_rx) = make_node_and_sync_with_rpc(
            next_available_port(),
            vec![100],
            nt,
            app_prefs.get_user_app_dir(),
        )
        .unwrap();

        let rpc_port = next_available_port();
        let mut config = get_test_config(8888, vec![100]);
        config.cli.rpc.rpc_server_port = rpc_port;
        config.cli.rpc.rpc_server_addr = "127.0.0.1".to_owned();
        config.cli.rpc.rpc_server_token = "rpcadmin".to_owned();
        let mut rpc_server = RpcServerImpl::new(node, None, &config.cli.rpc, rpc_rx);
        rpc_server.start_server().expect("rpc");

        let env = Arc::new(EnvBuilder::new().build());
        let ch = ChannelBuilder::new(env).connect(&format!("127.0.0.1:{}", rpc_port));

        let client = P2PClient::new(ch);

        let mut req_meta_builder = ::grpcio::MetadataBuilder::new();
        req_meta_builder
            .add_str("Authentication", "rpcadmin")
            .unwrap();
        let meta_data = req_meta_builder.build();

        let call_options = ::grpcio::CallOption::default().headers(meta_data);

        (client, rpc_server, call_options)
    }

    // Creates P2PClient, RpcServImpl and CallOption instances.
    // The intended use is for spawning nodes for testing gRPC api.
    // The port number is safe as it uses a AtomicUsize for respecting the order.
    fn create_node_rpc_call_option(nt: PeerType) -> (P2PClient, RpcServerImpl, grpcio::CallOption) {
        create_node_rpc_call_option_waiter(nt)
    }

    #[test]
    pub fn test_grpc_noauth() -> Fallible<()> {
        let (client, _, _) = create_node_rpc_call_option(PeerType::Node);
        match client.peer_version(&crate::proto::Empty::new()) {
            Err(::grpcio::Error::RpcFailure(ref x)) => {
                assert_eq!(x.status, grpcio::RpcStatusCode::Unauthenticated)
            }
            _ => panic!("Wrong rejection"),
        }

        Ok(())
    }

    /// Ignore this test for now, because `conn.async_send` only enqueues the
    /// send request. That new request will be processed inside poll-event.
    #[ignore]
    #[test]
    fn test_peer_connect() -> Fallible<()> {
        let (client, _, callopts) = create_node_rpc_call_option(PeerType::Node);
        let mut ip = protobuf::well_known_types::StringValue::new();
        ip.set_value("127.0.0.1".to_owned());
        let mut port_pb = protobuf::well_known_types::Int32Value::new();
        port_pb.set_value(1);
        let mut pcr = crate::proto::PeerConnectRequest::new();
        pcr.set_ip(ip.clone());
        pcr.set_port(port_pb);
        // test it can not connect to inexistent peer
        assert!(!client
            .peer_connect_opt(&pcr, callopts.clone())
            .unwrap()
            .get_value());

        let port = next_available_port();
        let (_node, _) = make_node_and_sync(port, vec![100], PeerType::Node)?;

        let mut port_pb = protobuf::well_known_types::Int32Value::new();
        port_pb.set_value(port as i32);
        let mut pcr = crate::proto::PeerConnectRequest::new();
        pcr.set_ip(ip);
        pcr.set_port(port_pb);

        // test it can connect to existing peer
        assert!(client.peer_connect_opt(&pcr, callopts).unwrap().get_value());

        Ok(())
    }

    #[test]
    fn test_peer_version() -> Fallible<()> {
        let (client, _, callopts) = create_node_rpc_call_option(PeerType::Node);
        let emp = crate::proto::Empty::new();
        assert_eq!(
            client.peer_version_opt(&emp, callopts).unwrap().get_value(),
            crate::VERSION.to_owned()
        );
        Ok(())
    }

    #[test]
    fn test_peer_uptime() -> Fallible<()> {
        let t0 = Utc::now().timestamp_millis() as u64;
        let (client, _, callopts) = create_node_rpc_call_option(PeerType::Node);
        let emp = crate::proto::Empty::new();
        let t1 = Utc::now().timestamp_millis() as u64;
        let nt1 = client
            .peer_uptime_opt(&emp.clone(), callopts.clone())?
            .get_value();
        let t2 = Utc::now().timestamp_millis() as u64;
        let nt2 = client
            .peer_uptime_opt(&emp.clone(), callopts.clone())?
            .get_value();
        let t3 = Utc::now().timestamp_millis() as u64;
        // t0 - n0 - t1 - n1 - t2 - n2 - t3
        // nt{n} := n{n} - n0
        assert!(nt1 <= (t2 - t0));
        assert!((nt2 - nt1) <= (t3 - t1));
        assert!(nt2 <= (t3 - t0));
        Ok(())
    }

    #[test]
    fn test_peer_total_received() -> Fallible<()> {
        let (client, rpc_serv, callopts) = create_node_rpc_call_option(PeerType::Node);
        let port = next_available_port();
        let (node2, _) = make_node_and_sync(port, vec![100], PeerType::Node)?;
        connect(&node2, &rpc_serv.node)?;
        await_handshake(&node2)?;
        let emp = crate::proto::Empty::new();
        let rcv = client
            .peer_total_received_opt(&emp.clone(), callopts.clone())?
            .get_value();
        assert!(rcv > 0);
        Ok(())
    }

    #[test]
    fn test_peer_total_sent() -> Fallible<()> {
        let (client, rpc_serv, callopts) = create_node_rpc_call_option(PeerType::Node);
        let port = next_available_port();
        let (node2, _) = make_node_and_sync(port, vec![100], PeerType::Node)?;
        connect(&node2, &rpc_serv.node)?;
        await_handshake(&node2)?;
        let emp = crate::proto::Empty::new();
        let snt = client
            .peer_total_sent_opt(&emp.clone(), callopts.clone())?
            .get_value();
        assert!(snt > 0);
        Ok(())
    }

    #[test]
    fn test_send_message() -> Fallible<()> {
        setup_logger();

        let (client, rpc_serv, callopts) = create_node_rpc_call_option(PeerType::Node);
        let port = next_available_port();
        let (node2, wt1) = make_node_and_sync(port, vec![100], PeerType::Node)?;
        connect(&node2, &rpc_serv.node)?;
        await_handshake(&node2)?;
        let mut message = protobuf::well_known_types::BytesValue::new();
        message.set_value(b"Hey".to_vec());
        let mut node_id = protobuf::well_known_types::StringValue::new();
        node_id.set_value(node2.id().to_string());
        let mut network_id = protobuf::well_known_types::Int32Value::new();
        network_id.set_value(100);
        let mut broadcast = protobuf::well_known_types::BoolValue::new();
        broadcast.set_value(true);
        let mut smr = crate::proto::SendMessageRequest::new();
        // smr.set_node_id(node_id);
        smr.set_network_id(network_id);
        smr.set_message(message);
        smr.set_broadcast(broadcast);
        client.send_message_opt(&smr, callopts)?;
        assert_eq!(
            &*await_broadcast_message(&wt1).unwrap().remaining_bytes()?,
            b"Hey"
        );
        Ok(())
    }

    // test_send_transaction is not implemented as it is more of an integration test
    // rather that a unit test. The corresponding flow test is in
    // `tests/consensus-tests.rs`

    #[test]
    fn test_join_network() -> Fallible<()> {
        setup_logger();

        let (client, rpc_serv, callopts) = create_node_rpc_call_option(PeerType::Node);
        let port = next_available_port();
        let (node2, _) = make_node_and_sync(port, vec![100], PeerType::Node)?;
        connect(&node2, &rpc_serv.node)?;
        await_handshake(&node2)?;
        let mut net = protobuf::well_known_types::Int32Value::new();
        net.set_value(10);
        let mut ncr = crate::proto::NetworkChangeRequest::new();
        ncr.set_network_id(net);
        assert!(client.join_network_opt(&ncr, callopts)?.get_value());
        Ok(())
    }

    #[test]
    fn test_leave_network() -> Fallible<()> {
        let (client, rpc_serv, callopts) = create_node_rpc_call_option(PeerType::Node);
        let port = next_available_port();
        let (node2, _) = make_node_and_sync(port, vec![100], PeerType::Node)?;
        connect(&node2, &rpc_serv.node)?;
        await_handshake(&node2)?;
        let mut net = protobuf::well_known_types::Int32Value::new();
        net.set_value(100);
        let mut ncr = crate::proto::NetworkChangeRequest::new();
        ncr.set_network_id(net);
        assert!(client.leave_network_opt(&ncr, callopts)?.get_value());
        Ok(())
    }

    #[test]
    fn test_peer_stats() -> Fallible<()> {
        let (client, rpc_serv, callopts) = create_node_rpc_call_option(PeerType::Node);
        let req = crate::proto::PeersRequest::new();
        let rcv = client
            .peer_stats_opt(&req.clone(), callopts.clone())?
            .get_peerstats()
            .to_vec();
        assert!(rcv.is_empty());
        let port = next_available_port();
        let (node2, _) = make_node_and_sync(port, vec![100], PeerType::Node)?;
        connect(&node2, &rpc_serv.node)?;
        await_handshake(&node2)?;
        let req = crate::proto::PeersRequest::new();
        let rcv = client
            .peer_stats_opt(&req.clone(), callopts.clone())?
            .get_peerstats()
            .to_vec();
        assert!(rcv.len() == 1);
        assert_eq!(rcv[0].node_id, node2.id().to_string());
        Ok(())
    }

    #[test]
    fn test_peer_list() -> Fallible<()> {
        let (client, rpc_serv, callopts) = create_node_rpc_call_option(PeerType::Node);
        let req = crate::proto::PeersRequest::new();
        let rcv = client.peer_list_opt(&req.clone(), callopts.clone())?;
        assert!(rcv.get_peer().to_vec().is_empty());
        assert_eq!(rcv.get_peer_type(), "Node");
        let port = next_available_port();
        let (node2, _) = make_node_and_sync(port, vec![100], PeerType::Node)?;
        connect(&node2, &rpc_serv.node)?;
        await_handshake(&node2)?;
        let req = crate::proto::PeersRequest::new();
        let rcv = client
            .peer_list_opt(&req.clone(), callopts.clone())?
            .get_peer()
            .to_vec();
        assert!(rcv.len() == 1);
        let elem = rcv[0].clone();
        assert_eq!(
            P2PNodeId(str::parse::<u64>(elem.node_id.unwrap().get_value()).unwrap()).to_string(),
            node2.id().to_string()
        );
        assert_eq!(
            elem.ip.unwrap().get_value(),
            node2.internal_addr().ip().to_string()
        );
        Ok(())
    }

    #[test]
    pub fn test_grpc_peer_list_node_type() -> Fallible<()> {
        let types = [PeerType::Node, PeerType::Bootstrapper];
        types
            .into_iter()
            .map(|m| grpc_peer_list_node_type_str(*m))
            .collect::<Fallible<Vec<()>>>()?;

        Ok(())
    }

    fn grpc_peer_list_node_type_str(peer_type: PeerType) -> Fallible<()> {
        let (client, _, callopts) = create_node_rpc_call_option(peer_type);
        let reply = client
            .peer_list_opt(&crate::proto::PeersRequest::new(), callopts)
            .expect("rpc");
        assert_eq!(reply.peer_type, peer_type.to_string());
        Ok(())
    }

    #[test]
    fn test_node_info() -> Fallible<()> {
        let instant1 = (Utc::now().timestamp_millis() as u64) / 1000;
        let (client, rpc_serv, callopts) = create_node_rpc_call_option(PeerType::Node);
        let reply = client
            .node_info_opt(&crate::proto::Empty::new(), callopts)
            .expect("rpc");
        let instant2 = (Utc::now().timestamp_millis() as u64) / 1000;
        assert!((reply.current_localtime >= instant1) && (reply.current_localtime <= instant2));
        assert_eq!(reply.peer_type, "Node");
        assert_eq!(
            reply.node_id.unwrap().get_value(),
            rpc_serv.node.id().to_string()
        );
        Ok(())
    }

    #[test]
    fn test_subscription_start() -> Fallible<()> {
        let (client, _, callopts) = create_node_rpc_call_option(PeerType::Node);
        assert!(client
            .subscription_start_opt(&crate::proto::Empty::new(), callopts)
            .unwrap()
            .get_value());
        Ok(())
    }

    #[test]
    fn test_subscription_stop() -> Fallible<()> {
        let (client, _, callopts) = create_node_rpc_call_option(PeerType::Node);
        // assert!(!client
        // .subscription_stop_opt(&crate::proto::Empty::new(), callopts.clone())
        // .unwrap()
        // .get_value());
        client
            .subscription_start_opt(&crate::proto::Empty::new(), callopts.clone())
            .unwrap();
        assert!(client
            .subscription_stop_opt(&crate::proto::Empty::new(), callopts)
            .unwrap()
            .get_value());
        Ok(())
    }

    #[test]
    #[ignore] // TODO: decide how to handle this one
    fn test_subscription_poll() -> Fallible<()> {
        let (client, rpc_serv, callopts) = create_node_rpc_call_option_waiter(PeerType::Node);
        let port = next_available_port();
        let (node2, _) = make_node_and_sync(port, vec![100], PeerType::Node)?;
        connect(&node2, &rpc_serv.node)?;
        await_handshake(&node2)?;
        client.subscription_start_opt(&crate::proto::Empty::new(), callopts.clone())?;
        send_broadcast_message(
            &node2,
            vec![],
            crate::network::NetworkId::from(100),
            None,
            HybridBuf::try_from(&b"Hey"[..])?,
        )?;
        // await_broadcast_message(&wt1).expect("Message sender disconnected");
        let ans = client.subscription_poll_opt(&crate::proto::Empty::new(), callopts.clone())?;
        if let crate::proto::P2PNetworkMessage_oneof_payload::message_broadcast(b) =
            ans.payload.expect(
                "Received empty message from the rpc subscription when expecting a broadcast \
                 message",
            )
        {
            assert_eq!(b.data, b"Hey");
        } else {
            bail!("Wrong message received");
        }
        Ok(())
    }

    // Ban node/unban node/get banned peers are not easily testable as they involve
    // the database. The banning functionalities of a P2PNode are tested in
    // `p2p2::tests::test_banned_functionalities`. The process succeds but it
    // encounters a problem when inserting in the database as it's a default dummy
    // one.

    // Some tests involve a baker so they might be tested as flow tests:
    // - Consensus status
    // - Get branches
    // - Get block info
    // - Get ancestors
    // - Get last final account list
    // - Get last final instances
    // - Get last final account info
    // - Get last final instance info

    #[test]
    fn test_shutdown() -> Fallible<()> {
        let (client, _, callopts) = create_node_rpc_call_option(PeerType::Node);
        assert!(client
            .shutdown_opt(&crate::proto::Empty::new(), callopts)
            .expect("rpc")
            .get_value());
        Ok(())
    }

    #[test]
    #[cfg(feature = "benchmark")]
    #[ignore] // TODO: decide how to handle this one
    fn test_tps_tests() -> Fallible<()> {
        let data = "Hey";
        std::fs::create_dir_all("/tmp/blobs")?;
        std::fs::write("/tmp/blobs/test", data)?;
        let (client, rpc_serv, callopts) = create_node_rpc_call_option(PeerType::Node);
        let port = next_available_port();
        let (node2, wt2) = make_node_and_sync(port, vec![100], PeerType::Node)?;
        connect(&node2, &rpc_serv.node)?;
        await_handshake(&node2)?;
        let mut req = crate::proto::TpsRequest::new();
        req.set_network_id(100);
        req.set_id(node2.id().to_string());
        req.set_directory("/tmp/blobs".to_string());
        client.tps_test_opt(&req, callopts)?;
        assert_eq!(&await_direct_message(&wt2)?.remaining_bytes()?[..], b"Hey");
        Ok(())
    }

    #[test]
    #[cfg(not(feature = "benchmark"))]
    fn test_tps_tests() -> Fallible<()> {
        let data = "Hey";
        std::fs::create_dir_all("/tmp/blobs")?;
        std::fs::write("/tmp/blobs/test", data)?;
        let (client, rpc_serv, callopts) = create_node_rpc_call_option(PeerType::Node);
        let port = next_available_port();
        let (node2, _) = make_node_and_sync(port, vec![100], PeerType::Node)?;
        connect(&node2, &rpc_serv.node)?;
        await_handshake(&node2)?;
        let mut req = crate::proto::TpsRequest::new();
        req.set_network_id(100);
        req.set_id(node2.id().to_string());
        req.set_directory("/tmp/blobs".to_string());
        if let Err(grpcio::Error::RpcFailure(s)) = client.tps_test_opt(&req, callopts) {
            if grpcio::RpcStatusCode::Unavailable == s.status
                && s.details.unwrap() == "Feature not activated"
            {
                return Ok(());
            }
        }
        bail!("grpc: TPS test should have been deactivated but doesn't answer with the propererror")
    }

}
