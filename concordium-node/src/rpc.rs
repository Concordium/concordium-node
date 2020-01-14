#[cfg(feature = "benchmark")]
use crate::utils;
use crate::{
    common::{P2PNodeId, PeerType},
    configuration,
    failure::{Error, Fallible},
    network::{NetworkId, NetworkMessage, NetworkMessagePayload, NetworkPacketType},
    p2p::{
        bans::BanId,
        connectivity::{send_broadcast_message, send_direct_message},
        P2PNode,
    },
};

use byteorder::{BigEndian, WriteBytesExt};
use concordium_common::{ConsensusFfiResponse, ConsensusIsInCommitteeResponse, PacketType};
use consensus_rust::consensus::{ConsensusContainer, CALLBACK_QUEUE};
use futures::future::Future;
use globalstate_rust::tree::messaging::{ConsensusMessage, MessageType};
use tonic::{self, transport::{server::{Router, Unimplemented}, Server}, Request, Response, Status, Code};

use std::{
    convert::TryInto,
    io::Write,
    net::{IpAddr, SocketAddr},
    str::FromStr,
    sync::{atomic::Ordering, Arc, Mutex},
    time::{SystemTime, UNIX_EPOCH},
};

tonic::include_proto!("concordium");
use p2p_server::*;

#[derive(Clone)]
pub struct RpcServerImpl {
    node: Arc<P2PNode>,
    listen_port: u16,
    listen_addr: String,
    access_token: String,
    baker_private_data_json_file: Option<String>,
    consensus: Option<ConsensusContainer>,
    server: Arc<Mutex<Option<Router<P2pServer<RpcServerImpl>, Unimplemented>>>>,
}

impl RpcServerImpl {
    pub fn new(
        node: Arc<P2PNode>,
        consensus: Option<ConsensusContainer>,
        conf: &configuration::RpcCliConfig,
        baker_private_data_json_file: Option<String>,
    ) -> Self {
        RpcServerImpl {
            node: Arc::clone(&node),
            listen_addr: conf.rpc_server_addr.clone(),
            listen_port: conf.rpc_server_port,
            access_token: conf.rpc_server_token.clone(),
            baker_private_data_json_file,
            consensus,
            server: Default::default(),
        }
    }

    pub async fn start_server(&mut self) -> Fallible<()> {
        let addr = SocketAddr::from((IpAddr::from_str(&self.listen_addr)?, self.listen_port));
        let self_clone = self.clone();

        let server = Server::builder().add_service(P2pServer::new(self_clone));

        server.serve(addr).await?;

        Ok(())
    }

    #[inline]
    pub fn stop_server(&mut self) -> Fallible<()> {
        safe_lock!(self.server)?.take();
        Ok(())
    }
}

macro_rules! authenticate {
    ($req:expr, $access_token:expr) => {
        if let Some(val) = $req.metadata().get("authentication") {
            match String::from_utf8(val.as_bytes().to_owned()) {
                Ok(at) if at == $access_token => {}
                _ => {
                    error!("failed to reply to {:?}: invalid authentication token", $req);
                    return Err(Status::new(
                        Code::Unauthenticated,
                        "invalid authentication token",
                    ));
                }
            }
        } else {
            error!("failed to reply to {:?}: missing authentication token", $req);
            return Err(Status::new(
                Code::Unauthenticated,
                "missing authentication token",
            ));
        }
    };
}

macro_rules! successful_json_response {
    ($self:ident, $req_name:expr, $foo:expr) => {
        if let Some(ref consensus) = $self.consensus {
            Ok(Response::new(SuccessfulJsonPayloadResponse { json_value: $foo(consensus) }))
        } else {
            warn!("Can't respond to a {} request due to stopped Consensus", $req_name);
            Err(Status::new(Code::Internal, "Consensus container is not initialized!"))
        }
    };
}

macro_rules! successful_bool_response {
    ($self:ident, $req_name:expr, $foo:expr) => {
        if let Some(ref consensus) = $self.consensus {
            Ok(Response::new(SuccessResponse { value: $foo(consensus) }))
        } else {
            warn!("Can't respond to a {} request due to stopped Consensus", $req_name);
            Err(Status::new(Code::Internal, "Consensus container is not initialized!"))
        }
    };
}
/*
macro_rules! successful_byte_response {
    ($self:ident, $ctx:ident, $req:ident, $sink:ident, $inner_match:expr) => {
        if let Some(ref consensus) = $self.consensus {
            let res = $inner_match(consensus);
            let mut r: SuccessfulBytePayloadResponse = SuccessfulBytePayloadResponse::new();
            r.set_payload(res);
            let f =
                $sink.success(r).map_err(move |e| error!("failed to reply {:?}: {:?}", $req, e));
            $ctx.spawn(f);
        }
    };
}
*/
#[tonic::async_trait]
impl P2p for RpcServerImpl {
    async fn subscription_start(
        &self,
        req: Request<Empty>,
    ) -> Result<Response<SuccessResponse>, Status> {
        authenticate!(req, self.access_token);
        self.node.rpc_subscription_start();
        Ok(Response::new(SuccessResponse { value: true }))
    }

    async fn subscription_stop(
        &self,
        req: Request<Empty>,
    ) -> Result<Response<SuccessResponse>, Status> {
        authenticate!(req, self.access_token);
        self.node.rpc_subscription_stop();
        Ok(Response::new(SuccessResponse { value: true }))
    }

    async fn peer_connect(
        &self,
        req: Request<PeerConnectRequest>,
    ) -> Result<Response<SuccessResponse>, Status> {
        authenticate!(req, self.access_token);
        let req = req.get_ref();
        if req.ip.is_some() && req.port.is_some() {
            let ip = if let Ok(ip) = IpAddr::from_str(&req.ip.as_ref().unwrap()) {
                ip
            } else {
                warn!("Invalid IP address in a PeerConnect request");
                return Err(Status::new(
                    Code::InvalidArgument,
                    "Invalid IP address",
                ))
            };
            let port = req.port.unwrap() as u16;
            let addr = SocketAddr::new(ip, port);
            let status = self.node.connect(PeerType::Node, addr, None).is_ok();
            Ok(Response::new(SuccessResponse { value: status }))
        } else {
            Ok(Response::new(SuccessResponse { value: false }))
        }
    }

    async fn peer_version(
        &self,
        req: Request<Empty>,
    ) -> Result<Response<StringResponse>, Status> {
        authenticate!(req, self.access_token);
        let resp = StringResponse { value: crate::VERSION.to_owned() };
        Ok(Response::new(resp))
    }

    async fn peer_uptime(
        &self,
        req: Request<Empty>,
    ) -> Result<tonic::Response<NumberResponse>, Status> {
        authenticate!(req, self.access_token);
        Ok(Response::new(NumberResponse { value: self.node.get_uptime() as u64 }))
    }

    async fn peer_total_received(
        &self,
        req: Request<Empty>,
    ) -> Result<Response<NumberResponse>, Status> {
        authenticate!(req, self.access_token);
        let value = self.node.total_received.load(Ordering::Relaxed);
        Ok(Response::new(NumberResponse { value }))
    }

    async fn peer_total_sent(
        &self,
        req: Request<Empty>,
    ) -> Result<Response<NumberResponse>, Status> {
        authenticate!(req, self.access_token);
        let value = self.node.total_sent.load(Ordering::Relaxed);
        Ok(Response::new(NumberResponse { value }))
    }

    async fn send_transaction(
        &self,
        req: Request<SendTransactionRequest>,
    ) -> Result<Response<SuccessResponse>, Status> {
        authenticate!(req, self.access_token);
        if let Some(ref consensus) = self.consensus {
            let req = req.get_ref();
            let transaction = &req.payload;
            let consensus_result = consensus.send_transaction(transaction);

            let result = if consensus_result == ConsensusFfiResponse::Success {
                let mut payload = Vec::with_capacity(2 + transaction.len());
                payload.write_u16::<BigEndian>(PacketType::Transaction as u16).unwrap(); // safe
                payload.write_all(&transaction).unwrap(); // also infallible

                CALLBACK_QUEUE.send_out_message(ConsensusMessage::new(
                    MessageType::Outbound(None),
                    PacketType::Transaction,
                    Arc::from(payload),
                    vec![],
                ))
            } else {
                Ok(())
            };
            match (result, consensus_result) {
                (Ok(_), ConsensusFfiResponse::Success) => {
                    Ok(Response::new(SuccessResponse { value: true }))
                }
                (Err(e), ConsensusFfiResponse::Success) => {
                    warn!("Couldn't put a transaction in the outbound queue due to {:?}", e);
                    Err(Status::new(
                        Code::Internal,
                        format!("Couldn't put a transaction in the outbound queue due to {:?}", e),
                    ))
                }
                (_, e) => {
                    warn!("Consensus didn't accept a transaction via RPC due to {:?}", e);
                    Err(Status::new(
                        Code::Internal,
                        format!("Consensus didn't accept a transaction via RPC due to {:?}", e),
                    ))
                }
            }
        } else {
            warn!("Can't respond to a SendTransaction request due to stopped Consensus");
            Err(Status::new(
                Code::Internal,
                "Consensus container is not initialized!",
            ))
        }
    }

    async fn join_network(
        &self,
        req: Request<NetworkChangeRequest>,
    ) -> Result<Response<SuccessResponse>, Status> {
        authenticate!(req, self.access_token);
        let req = req.get_ref();
        if req.network_id.is_some() && req.network_id.unwrap() > 0 && req.network_id.unwrap() < 100_000 {
            info!("Attempting to join network {}", req.network_id.unwrap());
            let network_id = NetworkId::from(req.network_id.unwrap() as u16);
            self.node.send_joinnetwork(network_id);
            Ok(Response::new(SuccessResponse { value: true }))
        } else {
            Ok(Response::new(SuccessResponse { value: false }))
        }
    }

    async fn leave_network(
        &self,
        req: Request<NetworkChangeRequest>,
    ) -> Result<Response<SuccessResponse>, Status> {
        authenticate!(req, self.access_token);
        let req = req.get_ref();
        if req.network_id.is_some() && req.network_id.unwrap() > 0 && req.network_id.unwrap() < 100_000 {
            info!("Attempting to leave network {}", req.network_id.unwrap());
            let network_id = NetworkId::from(req.network_id.unwrap() as u16);
            self.node.send_leavenetwork(network_id);
            Ok(Response::new(SuccessResponse { value: true }))
        } else {
            Ok(Response::new(SuccessResponse { value: false }))
        }
    }

    async fn peer_stats(
        &self,
        req: Request<PeersRequest>,
    ) -> Result<Response<PeerStatsResponse>, Status> {
        authenticate!(req, self.access_token);
        let peer_stats = self.node.get_peer_stats(None);
        let peerstats = peer_stats
            .into_iter()
            .filter(|peer| match peer.peer_type {
                PeerType::Node => true,
                PeerType::Bootstrapper => req.get_ref().include_bootstrappers,
            })
            .map(|peer|
                peer_stats_response::PeerStats {
                    node_id: format!("{:0>16x}", peer.id),
                    packets_sent: peer.sent,
                    packets_received: peer.received,
                    valid_latency: peer.valid_latency,
                    measured_latency: peer.measured_latency,
                }
            )
            .collect();

        Ok(Response::new(PeerStatsResponse {
            peerstats,
            avg_bps_in: self.node.stats.get_avg_bps_in(),
            avg_bps_out: self.node.stats.get_avg_bps_out(),
        }))
    }

    async fn peer_list(
        &self,
        req: Request<PeersRequest>,
    ) -> Result<Response<PeerListResponse>, Status> {
        authenticate!(req, self.access_token);
        let list = self
            .node
            .get_peer_stats(None)
            .iter()
            .filter(|peer| match peer.peer_type {
                PeerType::Node => true,
                PeerType::Bootstrapper => req.get_ref().include_bootstrappers,
            })
            .map(|peer| {
                PeerElement {
                    node_id: Some(format!("{:0>16x}", peer.id)),
                    ip: Some(peer.addr.ip().to_string()),
                    port: Some(peer.addr.port() as u32),
                }
            })
            .collect();

        Ok(Response::new(PeerListResponse {
            peer_type: self.node.peer_type().to_string(),
            peer: list,
        }))
    }

    async fn node_info(
        &self,
        req: Request<Empty>,
    ) -> Result<Response<NodeInfoResponse>, Status> {
        authenticate!(req, self.access_token);
        let node_id = Some(self.node.id().to_string());
        let peer_type = self.node.peer_type().to_string();
        let current_localtime = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("Time went backwards")
            .as_secs();
        let beta_username = {
            #[cfg(feature = "beta")] {
                Some(self.node.config.beta_username.clone())
            }
            #[cfg(not(feature = "beta"))] {
                None
            }
        };
        Ok(Response::new(match self.consensus {
            Some(ref consensus) => {
                NodeInfoResponse {
                    node_id,
                    current_localtime,
                    peer_type,
                    consensus_baker_running: consensus.is_baking(),
                    consensus_running: true,
                    consensus_type: consensus.consensus_type.to_string(),
                    consensus_baker_committee:
                        consensus.in_baking_committee()
                            == ConsensusIsInCommitteeResponse::ActiveInCommittee,
                    consensus_finalizer_committee: consensus.in_finalization_committee(),
                    beta_username,
                }
            }
            None => {
                NodeInfoResponse {
                    node_id,
                    current_localtime,
                    peer_type,
                    consensus_baker_running: false,
                    consensus_running: false,
                    consensus_type: "Inactive".to_owned(),
                    consensus_baker_committee: false,
                    consensus_finalizer_committee: false,
                    beta_username,
                }
            }
        }))
    }

    async fn ban_node(
        &self,
        req: Request<PeerElement>,
    ) -> Result<Response<SuccessResponse>, Status> {
        authenticate!(req, self.access_token);
        let req = req.get_ref();
        let banned_node = if req.node_id.is_some() && req.ip.is_none() {
            P2PNodeId::from_str(&req.node_id.as_ref().unwrap().to_string()).ok().map(BanId::NodeId)
        } else if req.ip.is_some() && req.node_id.is_none() {
            IpAddr::from_str(&req.ip.as_ref().unwrap().to_string()).ok().map(BanId::Ip)
        } else {
            None
        };

        if let Some(to_ban) = banned_node {
            match self.node.ban_node(to_ban) {
                Ok(_) => {
                    Ok(Response::new(SuccessResponse { value: true }))
                }
                Err(e) => {
                    warn!("couldn't fulfill a BanNode request: {}", e);
                    Err(Status::new(
                        Code::Aborted,
                        format!("couldn't fulfill a BanNode request: {}", e),
                    ))
                }
            }
        } else {
            Err(Status::new(
                Code::InvalidArgument,
                "Missing IP or address to ban",
            ))
        }
    }

    async fn unban_node(
        &self,
        req: Request<PeerElement>,
    ) -> Result<Response<SuccessResponse>, Status> {
        authenticate!(req, self.access_token);
        let req = req.get_ref();
        let banned_node = if req.node_id.is_some() && req.ip.is_none() {
            P2PNodeId::from_str(&req.node_id.as_ref().unwrap().to_string()).ok().map(BanId::NodeId)
        } else if req.ip.is_some() && req.node_id.is_none() {
            IpAddr::from_str(&req.ip.as_ref().unwrap().to_string()).ok().map(BanId::Ip)
        } else {
            None
        };

        if let Some(to_unban) = banned_node {
            match self.node.unban_node(to_unban) {
                Ok(_) => {
                    Ok(Response::new(SuccessResponse { value: true }))
                }
                Err(e) => {
                    warn!("couldn't fulfill an UnbanNode request: {}", e);
                    Err(Status::new(
                        Code::Aborted,
                        format!("couldn't fulfill a UnbanNode request: {}", e),
                    ))
                }
            }
        } else {
            Err(Status::new(
                Code::InvalidArgument,
                "Missing IP or address to unban",
            ))
        }
    }

    async fn get_consensus_status(
        &self,
        req: Request<Empty>,
    ) -> Result<Response<SuccessfulJsonPayloadResponse>, Status> {
        authenticate!(req, self.access_token);
        successful_json_response!(self, "GetConsensusStatus", |consensus: &ConsensusContainer| {
            consensus.get_consensus_status()
        })
    }

    async fn start_baker(
        &self,
        req: Request<Empty>,
    ) -> Result<Response<SuccessResponse>, Status> {
        authenticate!(req, self.access_token);
        successful_bool_response!(self, "StartBaker", |consensus: &ConsensusContainer| {
            consensus.start_baker()
        })
    }

    async fn stop_baker(
        &self,
        req: Request<Empty>,
    ) -> Result<Response<SuccessResponse>, Status> {
        authenticate!(req, self.access_token);
        successful_bool_response!(self, "StopBaker", |consensus: &ConsensusContainer| {
            consensus.stop_baker()
        })
    }

    async fn get_branches(
        &self,
        req: Request<Empty>,
    ) -> Result<Response<SuccessfulJsonPayloadResponse>, Status> {
        authenticate!(req, self.access_token);
        successful_json_response!(self, "GetBranches", |consensus: &ConsensusContainer| {
            consensus.get_branches()
        })
    }

    async fn get_block_info(
        &self,
        req: Request<BlockHash>,
    ) -> Result<Response<SuccessfulJsonPayloadResponse>, Status> {
        authenticate!(req, self.access_token);
        successful_json_response!(self, "GetBlockInfo", |consensus: &ConsensusContainer| {
            consensus.get_block_info(&req.get_ref().block_hash)
        })
    }

    async fn get_ancestors(
        &self,
        req: Request<BlockHashAndAmount>,
    ) -> Result<Response<SuccessfulJsonPayloadResponse>, Status> {
        authenticate!(req, self.access_token);
        successful_json_response!(self, "GetAncestors", |consensus: &ConsensusContainer| {
            consensus.get_ancestors(&req.get_ref().block_hash, req.get_ref().amount)
        })
    }

    async fn get_account_list(
        &self,
        req: Request<BlockHash>,
    ) -> Result<Response<SuccessfulJsonPayloadResponse>, Status> {
        authenticate!(req, self.access_token);
        successful_json_response!(self, "GetAccountList", |consensus: &ConsensusContainer| {
            consensus.get_account_list(&req.get_ref().block_hash)
        })
    }

    async fn get_instances(
        &self,
        req: Request<BlockHash>,
    ) -> Result<Response<SuccessfulJsonPayloadResponse>, Status> {
        authenticate!(req, self.access_token);
        successful_json_response!(self, "GetInstances", |consensus: &ConsensusContainer| {
            consensus.get_instances(&req.get_ref().block_hash)
        })
    }

    async fn get_account_info(
        &self,
        req: Request<GetAddressInfoRequest>,
    ) -> Result<Response<SuccessfulJsonPayloadResponse>, Status> {
        authenticate!(req, self.access_token);
        successful_json_response!(self, "GetAccountInfo", |consensus: &ConsensusContainer| {
            consensus.get_account_info(&req.get_ref().block_hash, &req.get_ref().address)
        })
    }

    async fn get_instance_info(
        &self,
        req: Request<GetAddressInfoRequest>,
    ) -> Result<Response<SuccessfulJsonPayloadResponse>, Status> {
        authenticate!(req, self.access_token);
        successful_json_response!(self, "GetInstanceInfo", |consensus: &ConsensusContainer| {
            consensus.get_instance_info(&req.get_ref().block_hash, &req.get_ref().address)
        })
    }

    async fn get_reward_status(
        &self,
        req: Request<BlockHash>,
    ) -> Result<Response<SuccessfulJsonPayloadResponse>, Status> {
        authenticate!(req, self.access_token);
        successful_json_response!(self, "GetRewardStatus", |consensus: &ConsensusContainer| {
            consensus.get_reward_status(&req.get_ref().block_hash)
        })
    }
/*
    fn get_baker_private_data(
        &self,
        req: Request<Empty>,
    ) -> Result<Response<SuccessfulJsonPayloadResponse>, Status> {
        use std::fs;
        authenticate!(req, self.access_token);
        let f = if let Some(file) = &self.baker_private_data_json_file {
            if let Ok(data) = fs::read_to_string(file) {
                let mut r: SuccessfulJsonPayloadResponse = SuccessfulJsonPayloadResponse::new();
                r.set_json_value(data);
                sink.success(r)
            } else {
                sink.fail(grpcio::RpcStatus::new(
                    Code::FailedPrecondition,
                    Some("Could not read baker private data file".to_string()),
                ))
            }
        } else {
            sink.fail(grpcio::RpcStatus::new(
                Code::FailedPrecondition,
                Some("Running in passive consensus mode".to_string()),
            ))
        };
        let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
        ctx.spawn(f);
    }

    fn get_birk_parameters(
        &self,
        req: Request<BlockHash,
    ) {
        authenticate!(req, self.access_token);
        successful_json_response!(self, "", |consensus: &ConsensusContainer| {
            consensus.get_birk_parameters(&req.get_ref().block_hash)
        })
    }

    fn get_module_list(
        &self,
        req: Request<BlockHash,
    ) -> Result<Response<SuccessfulJsonPayloadResponse>, Status> {
        authenticate!(req, self.access_token);
        successful_json_response!(self, "", |consensus: &ConsensusContainer| {
            consensus.get_module_list(&req.get_ref().block_hash)
        })
    }

    fn get_module_source(
        &self,
        req: Request<GetModuleSourceRequest,
    ) {
        authenticate!(req, self.access_token);
        successful_byte_response!(self, "", |consensus: &ConsensusContainer| {
            consensus.get_module_source(&req.get_ref().block_hash, &req.get_ref().module_ref)
        })
    }

    fn get_banned_peers(
        &self,
        req: Request<Empty>,
    ) {
        authenticate!(req, self.access_token);
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
                                BanId::NodeId(id) => id.to_string(),
                                _ => "*".to_owned(),
                            });
                            pe.set_node_id(node_id);
                            let mut ip = ::protobuf::well_known_types::StringValue::new();
                            ip.set_value(match banned_node {
                                BanId::Ip(addr) => addr.to_string(),
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
    }

    fn shutdown(
        &self,
        req: Request<Empty>,
    ) {
        authenticate!(req, self.access_token);
        let f = {
            let mut r: SuccessResponse = SuccessResponse::new();
            r.set_value(self.node.close());
            sink.success(r)
        };
        let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
        ctx.spawn(f);
    }

    #[cfg(feature = "benchmark")]
    fn tps_test(
        &self,
        req: Request<TpsRequest,
    ) {
        authenticate!(req, self.access_token);
        let f = {
            let (network_id, id, dir) =
                (NetworkId::from(req.network_id as u16), req.id.clone(), req.directory.clone());
            let _node_list = self.node.get_peer_stats(None);
            if !_node_list.into_iter().any(|s| P2PNodeId(s.id).to_string() == id) {
                sink.fail(grpcio::RpcStatus::new(
                    Code::FailedPrecondition,
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
                        self.node.self_peer.id,
                        to_send,
                        network_id,
                        Arc::from(message),
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
    }

    #[cfg(not(feature = "benchmark"))]
    fn tps_test(
        &self,
        req: Request<TpsRequest,
    ) {
        let f = sink
            .fail(grpcio::RpcStatus::new(
                Code::Unavailable,
                Some("Feature not activated".to_string()),
            ))
            .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
        ctx.spawn(f);
    }

    #[cfg(not(feature = "network_dump"))]
    fn dump_start(
        &self,
        req: Request<DumpRequest,
    ) {
        let f = sink
            .fail(grpcio::RpcStatus::new(
                Code::Unavailable,
                Some("Feature not activated".to_string()),
            ))
            .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
        ctx.spawn(f);
    }

    #[cfg(feature = "network_dump")]
    fn dump_start(
        &self,
        req: Request<DumpRequest,
    ) {
        authenticate!(req, self.access_token);
        let f = {
            let mut r = SuccessResponse::new();
            let file_path = req.get_ref().file().to_owned();
            if self
                .node
                .activate_dump(
                    if file_path.is_empty() {
                        "dump"
                    } else {
                        &file_path
                    },
                    req.get_ref().raw(),
                )
                .is_ok()
            {
                r.set_value(true);
            } else {
                r.set_value(false);
            }
            sink.success(r)
        };
        let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
        ctx.spawn(f);
    }

    #[cfg(not(feature = "network_dump"))]
    fn dump_stop(
        &self,
        req: Request<Empty>,
    ) {
        let f = sink
            .fail(grpcio::RpcStatus::new(
                Code::Unavailable,
                Some("Feature not activated".to_string()),
            ))
            .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
        ctx.spawn(f);
    }

    #[cfg(feature = "network_dump")]
    fn dump_stop(
        &self,
        req: Request<Empty>,
    ) {
        authenticate!(req, self.access_token);
        let f = {
            let mut r = SuccessResponse::new();
            r.set_value(self.node.stop_dump().is_ok());

            sink.success(r)
        };
        let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
        ctx.spawn(f);
    }

    fn hook_transaction(
        &self,
        req: Request<TransactionHash,
    ) -> Result<Response<SuccessfulJsonPayloadResponse>, Status> {
        authenticate!(req, self.access_token);
        successful_json_response!(self, "", |consensus: &ConsensusContainer| {
            consensus.hook_transaction(&req.get_ref().transaction_hash)
        })
    }*/
}

#[cfg(test)]
mod tests {
    use crate::{
        common::{P2PNodeId, PeerType},
        configuration,
        p2p::connectivity::send_broadcast_message,
        proto::concordium_p2p_rpc_grpc::P2PClient,
        rpc::RpcServerImpl,
        test_utils::{
            await_handshake, connect, get_test_config, make_node_and_sync,
            make_node_and_sync_with_rpc, next_available_port, setup_logger,
        },
    };
    use chrono::prelude::Utc;
    use failure::Fallible;
    use grpcio::{ChannelBuilder, EnvBuilder};
    use std::sync::Arc;

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
        let mut rpc_server = RpcServerImpl::new(node, None, &config.cli.rpc, rpc_rx, None);
        rpc_server.start_server().expect("rpc");

        let env = Arc::new(EnvBuilder::new().build());
        let ch = ChannelBuilder::new(env).connect(&format!("127.0.0.1:{}", rpc_port));

        let client = P2PClient::new(ch);

        let mut req_meta_builder = ::grpcio::MetadataBuilder::new();
        req_meta_builder.add_str("Authentication", "rpcadmin").unwrap();
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
                assert_eq!(x.status, Code::Unauthenticated)
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
        assert!(!client.peer_connect_opt(&pcr, callopts.clone()).unwrap().get_value());

        let port = next_available_port();

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
        let nt1 = client.peer_uptime_opt(&emp.clone(), callopts.clone())?.get_value();
        let t2 = Utc::now().timestamp_millis() as u64;
        let nt2 = client.peer_uptime_opt(&emp, callopts)?.get_value();
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
        let node2 = make_node_and_sync(port, vec![100], PeerType::Node)?;
        connect(&node2, &rpc_serv.node)?;
        await_handshake(&node2)?;
        await_handshake(&rpc_serv.node)?;
        let emp = crate::proto::Empty::new();
        let _rcv = client.peer_total_received_opt(&emp, callopts)?.get_value();
        Ok(())
    }

    #[test]
    fn test_peer_total_sent() -> Fallible<()> {
        let (client, rpc_serv, callopts) = create_node_rpc_call_option(PeerType::Node);
        let port = next_available_port();
        let node2 = make_node_and_sync(port, vec![100], PeerType::Node)?;
        connect(&node2, &rpc_serv.node)?;
        await_handshake(&node2)?;
        await_handshake(&rpc_serv.node)?;
        let emp = crate::proto::Empty::new();
        let _snt = client.peer_total_sent_opt(&emp, callopts)?.get_value();
        Ok(())
    }

    #[test]
    #[ignore]
    fn test_send_message() -> Fallible<()> {
        setup_logger();

        let (client, rpc_serv, callopts) = create_node_rpc_call_option(PeerType::Node);
        let port = next_available_port();
        let node2 = make_node_and_sync(port, vec![100], PeerType::Node)?;
        connect(&node2, &rpc_serv.node)?;
        await_handshake(&node2)?;
        await_handshake(&rpc_serv.node)?;
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
        // assert_eq!(
        // &*await_broadcast_message(&wt1).unwrap().remaining_bytes()?,
        // b"Hey"
        // );
        Ok(())
    }

    // test_send_transaction is not implemented as it is more of an integration test
    // rather that a unit test. The corresponding flow test is in
    // `tests/consensus-tests.rs`

    #[test]
    fn test_join_network() -> Fallible<()> {
        let (client, rpc_serv, callopts) = create_node_rpc_call_option(PeerType::Node);
        let port = next_available_port();
        let node2 = make_node_and_sync(port, vec![100], PeerType::Node)?;
        connect(&node2, &rpc_serv.node)?;
        await_handshake(&node2)?;
        await_handshake(&rpc_serv.node)?;
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
        let node2 = make_node_and_sync(port, vec![100], PeerType::Node)?;
        connect(&node2, &rpc_serv.node)?;
        await_handshake(&node2)?;
        await_handshake(&rpc_serv.node)?;
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
        let port = next_available_port();
        let node2 = make_node_and_sync(port, vec![100], PeerType::Node)?;
        connect(&node2, &rpc_serv.node)?;
        await_handshake(&node2)?;
        await_handshake(&rpc_serv.node)?;
        let req = crate::proto::PeersRequest::new();
        let rcv = client.peer_stats_opt(&req, callopts)?.get_peerstats().to_vec();
        assert_eq!(node2.get_peer_stats(None).len(), 1);
        assert_eq!(rcv.len(), 1);
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
        let node2 = make_node_and_sync(port, vec![100], PeerType::Node)?;
        connect(&node2, &rpc_serv.node)?;
        await_handshake(&node2)?;
        await_handshake(&rpc_serv.node)?;
        let req = crate::proto::PeersRequest::new();
        let rcv = client.peer_list_opt(&req, callopts)?.get_peer().to_vec();
        assert_eq!(rcv.len(), 1);
        let elem = rcv[0].clone();
        assert_eq!(
            P2PNodeId(u64::from_str_radix(elem.node_id.unwrap().get_value(), 16).unwrap())
                .to_string(),
            node2.id().to_string()
        );
        assert_eq!(elem.ip.unwrap().get_value(), node2.internal_addr().ip().to_string());
        Ok(())
    }

    #[test]
    pub fn test_grpc_peer_list_node_type() -> Fallible<()> {
        let types = [PeerType::Node, PeerType::Bootstrapper];
        types.iter().map(|m| grpc_peer_list_node_type_str(*m)).collect::<Fallible<Vec<()>>>()?;

        Ok(())
    }

    fn grpc_peer_list_node_type_str(peer_type: PeerType) -> Fallible<()> {
        let (client, _, callopts) = create_node_rpc_call_option(peer_type);
        let reply =
            client.peer_list_opt(&crate::proto::PeersRequest::new(), callopts).expect("rpc");
        assert_eq!(reply.peer_type, peer_type.to_string());
        Ok(())
    }

    #[test]
    fn test_node_info() -> Fallible<()> {
        let instant1 = (Utc::now().timestamp_millis() as u64) / 1000;
        let (client, rpc_serv, callopts) = create_node_rpc_call_option(PeerType::Node);
        let reply = client.node_info_opt(&crate::proto::Empty::new(), callopts).expect("rpc");
        let instant2 = (Utc::now().timestamp_millis() as u64) / 1000;
        assert!((reply.current_localtime >= instant1) && (reply.current_localtime <= instant2));
        assert_eq!(reply.peer_type, "Node");
        assert_eq!(reply.node_id.unwrap().get_value(), rpc_serv.node.id().to_string());
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
        client.subscription_start_opt(&crate::proto::Empty::new(), callopts.clone()).unwrap();
        assert!(client
            .subscription_stop_opt(&crate::proto::Empty::new(), callopts)
            .unwrap()
            .get_value());
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
        let node2 = make_node_and_sync(port, vec![100], PeerType::Node)?;
        connect(&node2, &rpc_serv.node)?;
        await_handshake(&node2)?;
        await_handshake(&rpc_serv.node)?;
        let mut req = crate::proto::TpsRequest::new();
        req.set_network_id(100);
        req.set_id(node2.id().to_string());
        req.set_directory("/tmp/blobs".to_string());
        client.tps_test_opt(&req, callopts)?;
        // assert_eq!(&await_direct_message(&wt2)?.remaining_bytes()?[..], b"Hey");
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
        let node2 = make_node_and_sync(port, vec![100], PeerType::Node)?;
        connect(&node2, &rpc_serv.node)?;
        await_handshake(&node2)?;
        await_handshake(&rpc_serv.node)?;
        let mut req = crate::proto::TpsRequest::new();
        req.set_network_id(100);
        req.set_id(node2.id().to_string());
        req.set_directory("/tmp/blobs".to_string());
        if let Err(grpcio::Error::RpcFailure(s)) = client.tps_test_opt(&req, callopts) {
            if Code::Unavailable == s.status
                && s.details.unwrap() == "Feature not activated"
            {
                return Ok(());
            }
        }
        bail!("grpc: TPS test should have been deactivated but doesn't answer with the propererror")
    }
}
