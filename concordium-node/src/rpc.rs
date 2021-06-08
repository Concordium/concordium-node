//! An implementation of an RPC server and functions handling all available gRPC
//! calls.

use crate::{
    common::{grpc_api::*, p2p_peer::RemotePeerId, PeerType},
    configuration,
    connection::ConnChange,
    consensus_ffi::{
        consensus::{ConsensusContainer, CALLBACK_QUEUE},
        helpers::{ConsensusFfiResponse, ConsensusIsInBakingCommitteeResponse, PacketType},
        messaging::{ConsensusMessage, MessageType},
    },
    failure::Fallible,
    network::NetworkId,
    p2p::{bans::PersistedBanId, P2PNode},
    read_or_die,
};
use byteorder::WriteBytesExt;
use p2p_server::*;
use std::{
    convert::TryInto,
    io::Write,
    net::{IpAddr, SocketAddr},
    str::FromStr,
    sync::{atomic::Ordering, Arc},
    time::{SystemTime, UNIX_EPOCH},
};
use tonic::{transport::Server, Code, Request, Response, Status};

/// The object used to initiate a gRPC server.
#[derive(Clone)]
pub struct RpcServerImpl {
    node:         Arc<P2PNode>,
    listen_addr:  SocketAddr,
    access_token: String,
    // this field is optional only for test purposes
    consensus: Option<ConsensusContainer>,
}

impl RpcServerImpl {
    /// Creates a new RPC server object.
    pub fn new(
        node: Arc<P2PNode>,
        consensus: Option<ConsensusContainer>,
        conf: &configuration::RpcCliConfig,
    ) -> Fallible<Self> {
        let listen_addr =
            SocketAddr::from((IpAddr::from_str(&conf.rpc_server_addr)?, conf.rpc_server_port));

        Ok(RpcServerImpl {
            node: Arc::clone(&node),
            listen_addr,
            access_token: conf.rpc_server_token.clone(),
            consensus,
        })
    }

    /// Starts the gRPC server.
    pub async fn start_server(&mut self) -> Fallible<()> {
        let self_clone = self.clone();
        let server = Server::builder().add_service(P2pServer::new(self_clone));

        server.serve(self.listen_addr).await.map_err(|e| e.into())
    }
}

macro_rules! authenticate {
    ($req:expr, $access_token:expr) => {
        if let Some(val) = $req.metadata().get("authentication") {
            match std::str::from_utf8(val.as_bytes()) {
                Ok(at) if at == $access_token => {}
                _ => {
                    error!("failed to reply to {:?}: invalid authentication token", $req);
                    return Err(Status::new(Code::Unauthenticated, "invalid authentication token"));
                }
            }
        } else {
            error!("failed to reply to {:?}: missing authentication token", $req);
            return Err(Status::new(Code::Unauthenticated, "missing authentication token"));
        }
    };
}

macro_rules! call_consensus {
    ($self:ident, $req_name:expr, $resp_type:ident, $consensus_call:expr) => {
        if let Some(ref container) = $self.consensus {
            if !container.consensus.load(Ordering::Relaxed).is_null() {
                Ok(Response::new($resp_type {
                    value: $consensus_call(container),
                }))
            } else {
                warn!("Can't respond to a {} request due to uninitialized Consensus", $req_name);
                Err(Status::new(Code::Internal, "The consensus layer has not been initialized!"))
            }
        } else {
            error!("Consensus container not supplied; is this a gRPC unit test?");
            Err(Status::new(Code::FailedPrecondition, "The consensus container is missing!"))
        }
    };
}

/// Enhances a request with an authentication token.
#[macro_export]
macro_rules! req_with_auth {
    ($req:expr, $token:expr) => {{
        let mut req = Request::new($req);
        req.metadata_mut().insert("authentication", MetadataValue::from_str($token).unwrap());
        req
    }};
}

#[tonic::async_trait]
impl P2p for RpcServerImpl {
    async fn peer_connect(
        &self,
        req: Request<PeerConnectRequest>,
    ) -> Result<Response<BoolResponse>, Status> {
        authenticate!(req, self.access_token);
        let req = req.get_ref();

        let ip = if let Some(ref ip) = req.ip {
            IpAddr::from_str(ip)
                .map_err(|_| Status::new(Code::InvalidArgument, "Invalid IP address"))
        } else {
            Err(Status::new(Code::InvalidArgument, "Missing IP address"))
        }?;
        let port = if let Some(port) = req.port {
            port as u16
        } else {
            return Err(Status::new(Code::InvalidArgument, "Missing port"));
        };
        let addr = SocketAddr::new(ip, port);
        self.node.register_conn_change(ConnChange::NewConn {
            addr,
            peer_type: PeerType::Node,
            given: true,
        });
        Ok(Response::new(BoolResponse {
            value: true,
        }))
    }

    async fn peer_disconnect(
        &self,
        req: Request<PeerConnectRequest>,
    ) -> Result<Response<BoolResponse>, Status> {
        authenticate!(req, self.access_token);
        let req = req.get_ref();

        let ip_addr = if let Some(ref ip) = req.ip {
            IpAddr::from_str(ip)
                .map_err(|_| Status::new(Code::InvalidArgument, "Invalid IP address"))
        } else {
            Err(Status::new(Code::InvalidArgument, "Missing IP address"))
        }?;
        if let Some(port) = req.port {
            match port.try_into() {
                Ok(x) => Ok(Response::new(BoolResponse {
                    value: self.node.drop_addr(SocketAddr::new(ip_addr, x)),
                })),
                Err(_) => Err(Status::new(Code::InvalidArgument, "Port out of range.")),
            }
        } else {
            Err(Status::new(Code::InvalidArgument, "Disconnect is only supported by address."))
        }
    }

    async fn peer_version(&self, req: Request<Empty>) -> Result<Response<StringResponse>, Status> {
        authenticate!(req, self.access_token);
        let resp = StringResponse {
            value: crate::VERSION.to_owned(),
        };
        Ok(Response::new(resp))
    }

    async fn peer_uptime(
        &self,
        req: Request<Empty>,
    ) -> Result<tonic::Response<NumberResponse>, Status> {
        authenticate!(req, self.access_token);
        Ok(Response::new(NumberResponse {
            value: self.node.get_uptime() as u64,
        }))
    }

    async fn peer_total_received(
        &self,
        req: Request<Empty>,
    ) -> Result<Response<NumberResponse>, Status> {
        authenticate!(req, self.access_token);
        let value = self.node.connection_handler.total_received.load(Ordering::Relaxed);
        Ok(Response::new(NumberResponse {
            value,
        }))
    }

    async fn peer_total_sent(
        &self,
        req: Request<Empty>,
    ) -> Result<Response<NumberResponse>, Status> {
        authenticate!(req, self.access_token);
        let value = self.node.connection_handler.total_sent.load(Ordering::Relaxed);
        Ok(Response::new(NumberResponse {
            value,
        }))
    }

    async fn send_transaction(
        &self,
        req: Request<SendTransactionRequest>,
    ) -> Result<Response<BoolResponse>, Status> {
        use ConsensusFfiResponse::*;

        authenticate!(req, self.access_token);
        if let Some(ref consensus) = self.consensus {
            let req = req.get_ref();
            let transaction = &req.payload;
            if transaction.len() > configuration::PROTOCOL_MAX_TRANSACTION_SIZE {
                warn!("Received a transaction that exceeds maximum transaction size.");
                return Err(Status::invalid_argument(
                    "Transaction size exceeds maximum allowed size.",
                ));
            }
            let consensus_result = consensus.send_transaction(transaction);

            let result = if consensus_result == Success {
                let mut payload = Vec::with_capacity(1 + transaction.len());
                payload.write_u8(PacketType::Transaction as u8)?;
                payload.write_all(&transaction)?;

                CALLBACK_QUEUE.send_out_message(ConsensusMessage::new(
                    MessageType::Outbound(None),
                    PacketType::Transaction,
                    Arc::from(payload),
                    vec![],
                    None,
                ))
            } else {
                Ok(())
            };
            // make the successful response. If the transaction was added
            // and retransmitted then the response is true, otherwise
            // we respond with false
            let mk_response = |value| {
                Response::new(BoolResponse {
                    value,
                })
            };
            match (result, consensus_result) {
                (Ok(_), Success) => Ok(mk_response(true)),
                (Ok(_), DuplicateEntry) => Ok(mk_response(false)),
                (Ok(_), DeserializationError) => Ok(mk_response(false)),
                (Ok(_), Stale) => Ok(mk_response(false)),
                (Ok(_), InvalidResult) => Ok(mk_response(false)),
                (Ok(_), TooLowEnergy) => Ok(mk_response(false)),
                (Ok(_), ExpiryTooLate) => Ok(mk_response(false)),
                (Ok(_), NonexistingSenderAccount) => Ok(mk_response(false)),
                (Err(e), Success) => {
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
            Err(Status::new(Code::Internal, "Consensus container is not initialized!"))
        }
    }

    async fn join_network(
        &self,
        req: Request<NetworkChangeRequest>,
    ) -> Result<Response<BoolResponse>, Status> {
        authenticate!(req, self.access_token);
        let req = req.get_ref();
        if let Some(id) = req.network_id {
            if id > 0 && id < 100_000 {
                info!("Attempting to join network {}", id);
                let network_id = NetworkId::from(id as u16);
                self.node.send_join_network(network_id);
                Ok(Response::new(BoolResponse {
                    value: true,
                }))
            } else {
                Err(Status::new(Code::InvalidArgument, "Invalid network id"))
            }
        } else {
            Err(Status::new(Code::InvalidArgument, "Missing network id"))
        }
    }

    async fn leave_network(
        &self,
        req: Request<NetworkChangeRequest>,
    ) -> Result<Response<BoolResponse>, Status> {
        authenticate!(req, self.access_token);
        let req = req.get_ref();
        if let Some(id) = req.network_id {
            if id > 0 && id < 100_000 {
                info!("Attempting to leave network {}", id);
                let network_id = NetworkId::from(id as u16);
                self.node.send_leave_network(network_id);
                Ok(Response::new(BoolResponse {
                    value: true,
                }))
            } else {
                Err(Status::new(Code::InvalidArgument, "Invalid network id"))
            }
        } else {
            Err(Status::new(Code::InvalidArgument, "Missing network id"))
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
            .map(|peer| peer_stats_response::PeerStats {
                node_id:          format!("{}", peer.self_id),
                packets_sent:     peer.msgs_sent,
                packets_received: peer.msgs_received,
                latency:          peer.latency,
            })
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
        let peer_catchup_stats = (*read_or_die!(self.node.peers)).peer_states.clone();
        let list = self
            .node
            .get_peer_stats(None)
            .iter()
            .filter(|peer| match peer.peer_type {
                PeerType::Node => true,
                PeerType::Bootstrapper => req.get_ref().include_bootstrappers,
            })
            .map(|peer| PeerElement {
                node_id: Some(format!("{}", peer.self_id)),
                ip:      Some(peer.addr.ip().to_string()),
                port:    Some(peer.addr.port() as u32),
                // We map the state, and if there's no state dwe default to pending
                catchup_status: match peer_catchup_stats.get(&peer.local_id) {
                    Some(&status) => status as i32,
                    _ => peer_element::CatchupStatus::Pending as i32,
                },
            })
            .collect();

        Ok(Response::new(PeerListResponse {
            peer_type: self.node.peer_type().to_string(),
            peers:     list,
        }))
    }

    async fn node_info(&self, req: Request<Empty>) -> Result<Response<NodeInfoResponse>, Status> {
        authenticate!(req, self.access_token);
        let node_id = Some(self.node.id().to_string());
        let peer_type = self.node.peer_type().to_string();
        let current_localtime =
            SystemTime::now().duration_since(UNIX_EPOCH).expect("Time went backwards").as_secs();
        Ok(Response::new(match self.consensus {
            Some(ref consensus) => {
                let consensus_baking_committee_status = consensus.in_baking_committee();
                NodeInfoResponse {
                    node_id,
                    current_localtime,
                    peer_type,
                    consensus_baker_running: consensus.is_baking(),
                    consensus_running: true,
                    consensus_type: consensus.consensus_type.to_string(),
                    consensus_baker_committee: match consensus_baking_committee_status {
                        ConsensusIsInBakingCommitteeResponse::ActiveInCommittee(_) => {
                            node_info_response::IsInBakingCommittee::ActiveInCommittee.into()
                        }
                        ConsensusIsInBakingCommitteeResponse::AddedButNotActiveInCommittee => {
                            node_info_response::IsInBakingCommittee::AddedButNotActiveInCommittee
                                .into()
                        }
                        ConsensusIsInBakingCommitteeResponse::AddedButWrongKeys => {
                            node_info_response::IsInBakingCommittee::AddedButWrongKeys.into()
                        }
                        ConsensusIsInBakingCommitteeResponse::NotInCommittee => {
                            node_info_response::IsInBakingCommittee::NotInCommittee.into()
                        }
                    },
                    consensus_finalizer_committee: consensus.in_finalization_committee(),
                    consensus_baker_id: match consensus_baking_committee_status {
                        ConsensusIsInBakingCommitteeResponse::ActiveInCommittee(baker_id) => {
                            Some(baker_id)
                        }
                        _ => None,
                    },
                    staging_net_username: None,
                }
            }
            None => NodeInfoResponse {
                node_id,
                current_localtime,
                peer_type,
                consensus_baker_running: false,
                consensus_running: false,
                consensus_type: "Inactive".to_owned(),
                consensus_baker_committee: node_info_response::IsInBakingCommittee::NotInCommittee
                    as i32,
                consensus_finalizer_committee: false,
                consensus_baker_id: None,
                staging_net_username: None,
            },
        }))
    }

    async fn ban_node(&self, req: Request<PeerElement>) -> Result<Response<BoolResponse>, Status> {
        authenticate!(req, self.access_token);
        let req = req.get_ref();
        let banned_node = match (&req.node_id, &req.ip) {
            (Some(node_id), None) => {
                if let Ok(id) = RemotePeerId::from_str(&node_id.to_string()) {
                    Ok(self.node.drop_by_id(id))
                } else {
                    return Err(Status::new(Code::InvalidArgument, "Malformed node ID."));
                }
            }
            (None, Some(ip)) => {
                if let Ok(ip) = IpAddr::from_str(&ip.to_string()) {
                    self.node.drop_by_ip_and_ban(ip)
                } else {
                    return Err(Status::new(Code::InvalidArgument, "Malformed IP address."));
                }
            }
            _ => {
                return Err(Status::new(
                    Code::InvalidArgument,
                    "Exactly one of IP or node ID must be provided.",
                ))
            }
        };

        match banned_node {
            Ok(value) => Ok(Response::new(BoolResponse {
                value,
            })),
            Err(e) => {
                warn!("couldn't fulfill a BanNode request: {}", e);
                Err(Status::new(
                    Code::Aborted,
                    format!("couldn't fulfill a BanNode request: {}", e),
                ))
            }
        }
    }

    async fn unban_node(
        &self,
        req: Request<PeerElement>,
    ) -> Result<Response<BoolResponse>, Status> {
        authenticate!(req, self.access_token);
        let req = req.get_ref();
        let banned_node = match req.ip {
            Some(ref ip) => IpAddr::from_str(&ip.to_string()).ok().map(PersistedBanId::Ip),
            _ => None,
        };

        if let Some(to_unban) = banned_node {
            match self.node.unban_node(to_unban) {
                Ok(_) => Ok(Response::new(BoolResponse {
                    value: true,
                })),
                Err(e) => {
                    warn!("couldn't fulfill an UnbanNode request: {}", e);
                    Err(Status::new(
                        Code::Aborted,
                        format!("couldn't fulfill a UnbanNode request: {}", e),
                    ))
                }
            }
        } else {
            Err(Status::new(Code::InvalidArgument, "Missing IP or address to unban"))
        }
    }

    async fn get_consensus_status(
        &self,
        req: Request<Empty>,
    ) -> Result<Response<JsonResponse>, Status> {
        authenticate!(req, self.access_token);
        call_consensus!(self, "GetConsensusStatus", JsonResponse, |cc: &ConsensusContainer| {
            cc.get_consensus_status()
        })
    }

    async fn start_baker(&self, req: Request<Empty>) -> Result<Response<BoolResponse>, Status> {
        authenticate!(req, self.access_token);
        call_consensus!(self, "StartBaker", BoolResponse, |cc: &ConsensusContainer| {
            cc.start_baker()
        })
    }

    async fn stop_baker(&self, req: Request<Empty>) -> Result<Response<BoolResponse>, Status> {
        authenticate!(req, self.access_token);
        call_consensus!(self, "StopBaker", BoolResponse, |cc: &ConsensusContainer| {
            cc.stop_baker()
        })
    }

    async fn get_branches(&self, req: Request<Empty>) -> Result<Response<JsonResponse>, Status> {
        authenticate!(req, self.access_token);
        call_consensus!(self, "GetBranches", JsonResponse, |cc: &ConsensusContainer| {
            cc.get_branches()
        })
    }

    async fn get_block_info(
        &self,
        req: Request<BlockHash>,
    ) -> Result<Response<JsonResponse>, Status> {
        authenticate!(req, self.access_token);
        call_consensus!(self, "GetBlockInfo", JsonResponse, |cc: &ConsensusContainer| {
            cc.get_block_info(&req.get_ref().block_hash)
        })
    }

    async fn get_ancestors(
        &self,
        req: Request<BlockHashAndAmount>,
    ) -> Result<Response<JsonResponse>, Status> {
        authenticate!(req, self.access_token);
        call_consensus!(self, "GetAncestors", JsonResponse, |cc: &ConsensusContainer| {
            cc.get_ancestors(&req.get_ref().block_hash, req.get_ref().amount)
        })
    }

    async fn get_blocks_at_height(
        &self,
        req: Request<BlockHeight>,
    ) -> Result<Response<JsonResponse>, Status> {
        authenticate!(req, self.access_token);
        call_consensus!(self, "GetBlocksAtHeight", JsonResponse, |cc: &ConsensusContainer| {
            cc.get_blocks_at_height(req.get_ref().block_height)
        })
    }

    async fn get_account_list(
        &self,
        req: Request<BlockHash>,
    ) -> Result<Response<JsonResponse>, Status> {
        authenticate!(req, self.access_token);
        call_consensus!(self, "GetAccountList", JsonResponse, |cc: &ConsensusContainer| {
            cc.get_account_list(&req.get_ref().block_hash)
        })
    }

    async fn get_instances(
        &self,
        req: Request<BlockHash>,
    ) -> Result<Response<JsonResponse>, Status> {
        authenticate!(req, self.access_token);
        call_consensus!(self, "GetInstances", JsonResponse, |cc: &ConsensusContainer| {
            cc.get_instances(&req.get_ref().block_hash)
        })
    }

    async fn get_account_info(
        &self,
        req: Request<GetAddressInfoRequest>,
    ) -> Result<Response<JsonResponse>, Status> {
        authenticate!(req, self.access_token);
        call_consensus!(self, "GetAccountInfo", JsonResponse, |cc: &ConsensusContainer| {
            cc.get_account_info(&req.get_ref().block_hash, &req.get_ref().address)
        })
    }

    async fn get_instance_info(
        &self,
        req: Request<GetAddressInfoRequest>,
    ) -> Result<Response<JsonResponse>, Status> {
        authenticate!(req, self.access_token);
        call_consensus!(self, "GetInstanceInfo", JsonResponse, |cc: &ConsensusContainer| {
            cc.get_instance_info(&req.get_ref().block_hash, &req.get_ref().address)
        })
    }

    async fn get_reward_status(
        &self,
        req: Request<BlockHash>,
    ) -> Result<Response<JsonResponse>, Status> {
        authenticate!(req, self.access_token);
        call_consensus!(self, "GetRewardStatus", JsonResponse, |cc: &ConsensusContainer| {
            cc.get_reward_status(&req.get_ref().block_hash)
        })
    }

    async fn get_birk_parameters(
        &self,
        req: Request<BlockHash>,
    ) -> Result<Response<JsonResponse>, Status> {
        authenticate!(req, self.access_token);
        call_consensus!(self, "GetBirkParameters", JsonResponse, |cc: &ConsensusContainer| {
            cc.get_birk_parameters(&req.get_ref().block_hash)
        })
    }

    async fn get_module_list(
        &self,
        req: Request<BlockHash>,
    ) -> Result<Response<JsonResponse>, Status> {
        authenticate!(req, self.access_token);
        call_consensus!(self, "GetModuleList", JsonResponse, |cc: &ConsensusContainer| {
            cc.get_module_list(&req.get_ref().block_hash)
        })
    }

    async fn get_transaction_status(
        &self,
        req: Request<TransactionHash>,
    ) -> Result<Response<JsonResponse>, Status> {
        authenticate!(req, self.access_token);
        call_consensus!(self, "GetTransactionStatus", JsonResponse, |cc: &ConsensusContainer| {
            cc.get_transaction_status(&req.get_ref().transaction_hash)
        })
    }

    async fn get_transaction_status_in_block(
        &self,
        req: Request<GetTransactionStatusInBlockRequest>,
    ) -> Result<Response<JsonResponse>, Status> {
        authenticate!(req, self.access_token);
        call_consensus!(
            self,
            "GetTransactionStatusInBlock",
            JsonResponse,
            |cc: &ConsensusContainer| {
                cc.get_transaction_status_in_block(
                    &req.get_ref().transaction_hash,
                    &req.get_ref().block_hash,
                )
            }
        )
    }

    async fn get_account_non_finalized_transactions(
        &self,
        req: Request<AccountAddress>,
    ) -> Result<Response<JsonResponse>, Status> {
        authenticate!(req, self.access_token);
        call_consensus!(
            self,
            "GetAccountNonFinalizedTransactions",
            JsonResponse,
            |cc: &ConsensusContainer| {
                cc.get_account_non_finalized_transactions(&req.get_ref().account_address)
            }
        )
    }

    async fn get_next_account_nonce(
        &self,
        req: Request<AccountAddress>,
    ) -> Result<Response<JsonResponse>, Status> {
        authenticate!(req, self.access_token);
        call_consensus!(self, "GetNextAccountNonce", JsonResponse, |cc: &ConsensusContainer| {
            cc.get_next_account_nonce(&req.get_ref().account_address)
        })
    }

    async fn get_identity_providers(
        &self,
        req: Request<BlockHash>,
    ) -> Result<Response<JsonResponse>, Status> {
        authenticate!(req, self.access_token);
        call_consensus!(self, "GetIdentityProviders", JsonResponse, |cc: &ConsensusContainer| {
            cc.get_identity_providers(&req.get_ref().block_hash)
        })
    }

    async fn get_anonymity_revokers(
        &self,
        req: Request<BlockHash>,
    ) -> Result<Response<JsonResponse>, Status> {
        authenticate!(req, self.access_token);
        call_consensus!(self, "GetAnonymityRevokers", JsonResponse, |cc: &ConsensusContainer| {
            cc.get_anonymity_revokers(&req.get_ref().block_hash)
        })
    }

    async fn get_cryptographic_parameters(
        &self,
        req: Request<BlockHash>,
    ) -> Result<Response<JsonResponse>, Status> {
        authenticate!(req, self.access_token);
        call_consensus!(
            self,
            "GetCryptographicParameters",
            JsonResponse,
            |cc: &ConsensusContainer| {
                cc.get_cryptographic_parameters(&req.get_ref().block_hash)
            }
        )
    }

    async fn get_block_summary(
        &self,
        req: Request<BlockHash>,
    ) -> Result<Response<JsonResponse>, Status> {
        authenticate!(req, self.access_token);
        call_consensus!(self, "GetBlockSummary", JsonResponse, |cc: &ConsensusContainer| {
            cc.get_block_summary(&req.get_ref().block_hash)
        })
    }

    async fn get_module_source(
        &self,
        req: Request<GetModuleSourceRequest>,
    ) -> Result<Response<BytesResponse>, Status> {
        authenticate!(req, self.access_token);
        call_consensus!(self, "GetModuleSource", BytesResponse, |cc: &ConsensusContainer| {
            cc.get_module_source(&req.get_ref().block_hash, &req.get_ref().module_ref)
        })
    }

    async fn get_banned_peers(
        &self,
        req: Request<Empty>,
    ) -> Result<Response<PeerListResponse>, Status> {
        authenticate!(req, self.access_token);
        let peers = if let Ok(banlist) = self.node.get_banlist() {
            banlist
                .into_iter()
                .map(|banned_node| {
                    let ip = match banned_node {
                        PersistedBanId::Ip(addr) => addr.to_string(),
                    };

                    PeerElement {
                        node_id: Some("*".to_owned()), // we do not record the id of banned peers.
                        ip: Some(ip),
                        port: None,
                        /// a banned peer is always in state pending for
                        /// catch-up
                        catchup_status: peer_element::CatchupStatus::Pending as i32,
                    }
                })
                .collect::<Vec<_>>()
        } else {
            warn!("Can't load the banlist in response to a GetBannedPeers request");
            Vec::new()
        };

        Ok(Response::new(PeerListResponse {
            peers,
            peer_type: "Node".to_owned(),
        }))
    }

    async fn shutdown(&self, req: Request<Empty>) -> Result<Response<BoolResponse>, Status> {
        authenticate!(req, self.access_token);
        Ok(Response::new(BoolResponse {
            value: self.node.close(),
        }))
    }

    #[cfg(not(feature = "network_dump"))]
    async fn dump_start(
        &self,
        req: Request<DumpRequest>,
    ) -> Result<Response<BoolResponse>, Status> {
        authenticate!(req, self.access_token);
        warn!("DumpStart RPC request received, but the \"network_dump\" feature is not active");
        Err(Status::new(Code::Unavailable, "Feature \"network_dump\" is not active"))
    }

    #[cfg(feature = "network_dump")]
    async fn dump_start(
        &self,
        req: Request<DumpRequest>,
    ) -> Result<Response<BoolResponse>, Status> {
        authenticate!(req, self.access_token);
        let file_path = req.get_ref().file.to_owned();
        let result = self
            .node
            .activate_dump(
                if file_path.is_empty() {
                    "dump"
                } else {
                    &file_path
                },
                req.get_ref().raw,
            )
            .is_ok();
        Ok(Response::new(BoolResponse {
            value: result,
        }))
    }

    #[cfg(not(feature = "network_dump"))]
    async fn dump_stop(&self, req: Request<Empty>) -> Result<Response<BoolResponse>, Status> {
        authenticate!(req, self.access_token);
        warn!("DumpStop RPC request received, but the \"network_dump\" feature is not active");
        Err(Status::new(Code::Unavailable, "Feature \"network_dump\" is not active"))
    }

    #[cfg(feature = "network_dump")]
    async fn dump_stop(&self, req: Request<Empty>) -> Result<Response<BoolResponse>, Status> {
        authenticate!(req, self.access_token);
        Ok(Response::new(BoolResponse {
            value: self.node.stop_dump().is_ok(),
        }))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        common::{grpc_api, P2PNodeId, PeerType},
        p2p::P2PNode,
        rpc::RpcServerImpl,
        test_utils::{
            await_handshakes, connect, dummy_regenesis_blocks, get_test_config, make_node_and_sync,
            next_available_port, stop_node_delete_dirs, wait_node_delete_dirs, DeletePermission,
        },
    };
    use chrono::prelude::Utc;
    use failure::Fallible;
    use tonic::{metadata::MetadataValue, transport::channel::Channel, Code, Request};

    use grpc_api::p2p_client::P2pClient;

    use std::sync::Arc;

    const TOKEN: &str = "rpcadmin";

    // The intended use is for spawning nodes for testing gRPC api.
    async fn create_test_rpc_node(
        nt: PeerType,
    ) -> Fallible<(P2pClient<Channel>, Arc<P2PNode>, DeletePermission)> {
        let (node, dp) =
            make_node_and_sync(next_available_port(), vec![100], nt, dummy_regenesis_blocks())
                .unwrap();

        let rpc_port = next_available_port();
        let mut config = get_test_config(8888, vec![100]);
        config.cli.rpc.rpc_server_port = rpc_port;
        config.cli.rpc.rpc_server_addr = "127.0.0.1".to_owned();
        config.cli.rpc.rpc_server_token = TOKEN.to_owned();
        let mut rpc_server = RpcServerImpl::new(node.clone(), None, &config.cli.rpc)?;
        tokio::spawn(async move { rpc_server.start_server().await });
        tokio::task::yield_now().await;

        let addr: &'static str =
            Box::leak(format!("http://127.0.0.1:{}", rpc_port).into_boxed_str());
        let channel = Channel::from_static(addr).connect().await?;
        let client = grpc_api::p2p_client::P2pClient::new(channel);

        Ok((client, node, dp))
    }

    #[tokio::test]
    async fn test_grpc_noauth() -> Fallible<()> {
        let (mut client, node, dp) = create_test_rpc_node(PeerType::Node).await.unwrap();

        match client.peer_version(req_with_auth!(grpc_api::Empty {}, "derp")).await {
            Err(status) => assert_eq!(status.code(), Code::Unauthenticated),
            _ => panic!("Wrong rejection"),
        };
        stop_node_delete_dirs(dp, node);
        Ok(())
    }

    #[tokio::test]
    async fn test_peer_version() -> Fallible<()> {
        let (mut client, node, dp) = create_test_rpc_node(PeerType::Node).await.unwrap();
        assert_eq!(
            client
                .peer_version(req_with_auth!(grpc_api::Empty {}, TOKEN))
                .await
                .unwrap()
                .get_ref()
                .value,
            crate::VERSION.to_owned()
        );
        stop_node_delete_dirs(dp, node);
        Ok(())
    }

    #[tokio::test]
    async fn test_peer_uptime() -> Fallible<()> {
        let t0 = Utc::now().timestamp_millis() as u64;
        let (mut client, node, dp) = create_test_rpc_node(PeerType::Node).await.unwrap();

        let req = || req_with_auth!(grpc_api::Empty {}, TOKEN);

        let t1 = Utc::now().timestamp_millis() as u64;
        let nt1 = client.peer_uptime(req()).await.unwrap().get_ref().value;
        let t2 = Utc::now().timestamp_millis() as u64;
        let nt2 = client.peer_uptime(req()).await.unwrap().get_ref().value;
        let t3 = Utc::now().timestamp_millis() as u64;
        assert!(nt1 <= (t2 - t0));
        assert!((nt2 - nt1) <= (t3 - t1));
        assert!(nt2 <= (t3 - t0));
        stop_node_delete_dirs(dp, node);
        Ok(())
    }

    #[tokio::test]
    async fn test_peer_total_received() -> Fallible<()> {
        let (mut client, node, dp) = create_test_rpc_node(PeerType::Node).await.unwrap();
        let port = next_available_port();
        let (node2, dp2) =
            make_node_and_sync(port, vec![100], PeerType::Node, dummy_regenesis_blocks())?;
        connect(&node2, &node);
        await_handshakes(&node);
        await_handshakes(&node2);
        let _rcv = client
            .peer_total_received(req_with_auth!(grpc_api::Empty {}, TOKEN))
            .await
            .unwrap()
            .get_ref()
            .value;
        stop_node_delete_dirs(dp, node);
        stop_node_delete_dirs(dp2, node2);
        Ok(())
    }

    #[tokio::test]
    async fn test_peer_total_sent() -> Fallible<()> {
        let (mut client, node, dp) = create_test_rpc_node(PeerType::Node).await.unwrap();
        let port = next_available_port();
        let (node2, dp2) =
            make_node_and_sync(port, vec![100], PeerType::Node, dummy_regenesis_blocks())?;
        connect(&node2, &node);
        await_handshakes(&node);
        await_handshakes(&node2);
        let _sent = client
            .peer_total_sent(req_with_auth!(grpc_api::Empty {}, TOKEN))
            .await
            .unwrap()
            .get_ref()
            .value;
        stop_node_delete_dirs(dp, node);
        stop_node_delete_dirs(dp2, node2);
        Ok(())
    }

    #[tokio::test]
    async fn test_peer_connect() -> Fallible<()> {
        let (mut client, node, dp) = create_test_rpc_node(PeerType::Node).await.unwrap();
        let port = next_available_port();
        let (node2, dp2) =
            make_node_and_sync(port, vec![100], PeerType::Node, dummy_regenesis_blocks())?;
        let _sent = client
            .peer_connect(req_with_auth!(
                grpc_api::PeerConnectRequest {
                    ip:   Some(node2.internal_addr().ip().to_string()),
                    port: Some(node2.internal_addr().port() as i32),
                },
                TOKEN
            ))
            .await
            .unwrap();
        await_handshakes(&node);
        await_handshakes(&node2);
        stop_node_delete_dirs(dp, node);
        stop_node_delete_dirs(dp2, node2);
        Ok(())
    }

    // test_send_transaction is not implemented as it is more of an integration test
    // rather that a unit test. The corresponding flow test is in
    // `tests/consensus-tests.rs`

    #[tokio::test]
    async fn test_join_network() -> Fallible<()> {
        let (mut client, node, dp) = create_test_rpc_node(PeerType::Node).await.unwrap();
        let port = next_available_port();
        let (node2, dp2) =
            make_node_and_sync(port, vec![100], PeerType::Node, dummy_regenesis_blocks())?;
        connect(&node2, &node);
        await_handshakes(&node);
        await_handshakes(&node2);
        let ncr = req_with_auth!(
            grpc_api::NetworkChangeRequest {
                network_id: Some(10),
            },
            TOKEN
        );
        assert!(client.join_network(ncr).await.unwrap().get_ref().value);
        stop_node_delete_dirs(dp, node);
        stop_node_delete_dirs(dp2, node2);
        Ok(())
    }

    #[tokio::test]
    async fn test_leave_network() -> Fallible<()> {
        let (mut client, node, dp) = create_test_rpc_node(PeerType::Node).await.unwrap();
        let port = next_available_port();
        let (node2, dp2) =
            make_node_and_sync(port, vec![100], PeerType::Node, dummy_regenesis_blocks())?;
        connect(&node2, &node);
        await_handshakes(&node);
        await_handshakes(&node2);
        let ncr = req_with_auth!(
            grpc_api::NetworkChangeRequest {
                network_id: Some(100),
            },
            TOKEN
        );
        assert!(client.leave_network(ncr).await.unwrap().get_ref().value);
        stop_node_delete_dirs(dp, node);
        stop_node_delete_dirs(dp2, node2);
        Ok(())
    }

    #[tokio::test]
    async fn test_peer_stats() -> Fallible<()> {
        let (mut client, node, dp) = create_test_rpc_node(PeerType::Node).await.unwrap();
        let port = next_available_port();
        let (node2, dp2) =
            make_node_and_sync(port, vec![100], PeerType::Node, dummy_regenesis_blocks())?;
        connect(&node2, &node);
        await_handshakes(&node);
        await_handshakes(&node2);
        let req = req_with_auth!(
            grpc_api::PeersRequest {
                include_bootstrappers: false,
            },
            TOKEN
        );
        let rcv = client.peer_stats(req).await.unwrap().get_ref().peerstats.clone();
        assert_eq!(node2.get_peer_stats(None).len(), 1);
        assert_eq!(rcv.len(), 1);
        assert_eq!(rcv[0].node_id, node2.id().to_string());
        stop_node_delete_dirs(dp, node);
        stop_node_delete_dirs(dp2, node2);
        Ok(())
    }

    #[tokio::test]
    async fn test_peer_list() -> Fallible<()> {
        let (mut client, node, dp) = create_test_rpc_node(PeerType::Node).await.unwrap();
        let port = next_available_port();
        let (node2, dp2) =
            make_node_and_sync(port, vec![100], PeerType::Node, dummy_regenesis_blocks())?;
        connect(&node2, &node);
        await_handshakes(&node);
        await_handshakes(&node2);
        let req = req_with_auth!(
            grpc_api::PeersRequest {
                include_bootstrappers: false,
            },
            TOKEN
        );
        let rcv = client.peer_list(req).await.unwrap().get_ref().peers.clone();
        assert_eq!(node2.get_peer_stats(None).len(), 1);
        assert_eq!(rcv.len(), 1);
        let elem = rcv[0].clone();
        assert_eq!(
            P2PNodeId(u64::from_str_radix(&elem.node_id.unwrap(), 16).unwrap()).to_string(),
            node2.id().to_string()
        );
        assert_eq!(elem.ip.unwrap(), node2.internal_addr().ip().to_string());
        stop_node_delete_dirs(dp, node);
        stop_node_delete_dirs(dp2, node2);
        Ok(())
    }

    #[cfg(flag = "bootstrapper")]
    #[tokio::test]
    async fn test_grpc_peer_list_bootstrapper() -> Fallible<()> {
        let (mut client, _, dp) = create_test_rpc_node(PeerType::Bootstrapper).await.unwrap();
        let req = req_with_auth!(
            grpc_api::PeersRequest {
                include_bootstrappers: true,
            },
            TOKEN
        );
        let reply = client.peer_list(req).await.unwrap();
        assert_eq!(reply.get_ref().peer_type, PeerType::Bootstrapper.to_string());
        stop_node_delete_dirs(dp, node);
    }

    #[tokio::test]
    async fn test_node_info() -> Fallible<()> {
        let instant1 = (Utc::now().timestamp_millis() as u64) / 1000;
        let (mut client, node, dp) = create_test_rpc_node(PeerType::Node).await.unwrap();
        let reply = client.node_info(req_with_auth!(grpc_api::Empty {}, TOKEN)).await.unwrap();
        let reply = reply.get_ref();
        let instant2 = (Utc::now().timestamp_millis() as u64) / 1000;
        assert!((reply.current_localtime >= instant1) && (reply.current_localtime <= instant2));
        assert_eq!(reply.peer_type, "Node");
        assert_eq!(reply.node_id.as_ref().unwrap(), &node.id().to_string());
        stop_node_delete_dirs(dp, node);
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

    #[tokio::test]
    async fn test_shutdown() -> Fallible<()> {
        let (mut client, node, dp) = create_test_rpc_node(PeerType::Node).await.unwrap();
        assert!(
            client
                .shutdown(req_with_auth!(grpc_api::Empty {}, TOKEN))
                .await
                .unwrap()
                .get_ref()
                .value
        );
        wait_node_delete_dirs(dp, node);
        Ok(())
    }
}
