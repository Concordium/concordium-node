use crate::proto::{
    concordium_p2p_rpc_grpc::{create_p2_p, P2P},
    AccountAddress, BlockHash, BlockHashAndAmount, ContractInstanceAddress, Empty,
    NetworkChangeRequest, NodeInfoResponse, NumberResponse, P2PNetworkMessage, PeerConnectRequest,
    PeerElement, PeerListResponse, PeerStatsResponse, SendMessageRequest, SendTransactionRequest,
    StringResponse, SuccessResponse, SuccessfulBytePayloadResponse, SuccessfulJsonPayloadResponse,
};
use std::sync::{Arc, RwLock};

#[derive(Clone)]
pub struct P2PServiceForwarder {
    pub targets: Arc<RwLock<Vec<Arc<Box<dyn P2P>>>>>,
    service:     Arc<Option<::grpcio::Service>>,
}

unsafe impl Send for P2PServiceForwarder {}
unsafe impl Sync for P2PServiceForwarder {}

impl P2PServiceForwarder {
    pub fn new() -> Self {
        let part_mself = P2PServiceForwarder {
            targets: Arc::new(RwLock::new(Vec::new())),
            service: Arc::new(None),
        };
        let service = create_p2_p(part_mself.clone());
        let mself = P2PServiceForwarder {
            targets: part_mself.targets.clone(),
            service: Arc::new(Some(service)),
        };
        mself
    }
}

lazy_static! {
    static ref P2P_SERVICE_FORWARDER: Arc<P2PServiceForwarder> =
        Arc::new(P2PServiceForwarder::new());
}

pub fn p2p_service_forwarder() -> Arc<P2PServiceForwarder> { P2P_SERVICE_FORWARDER.clone() }

macro_rules! forward_to_targets {
    ($target:expr, $func:ident, $ctx:ident, $req:ident, $sink:ident) => {
        let target_ref = &*($target.read().expect("Coudn't read target when forwarding"));

        match target_ref.len() {
            1 => {
                target_ref[0].$func($ctx, $req, $sink);
            }
            _ => {
                error!("Unsupported more that one forward target");
                // for p2p in target_ref {
                // p2p.$func( $ctx.clone(), $req.clone(), $sink.clone());
                // }
            }
        }
    };
}

impl P2P for P2PServiceForwarder {
    fn peer_connect(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: PeerConnectRequest,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        forward_to_targets!(self.targets, peer_connect, ctx, req, sink);
    }

    fn peer_uptime(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<NumberResponse>,
    ) {
        forward_to_targets!(self.targets, peer_uptime, ctx, req, sink);
    }

    fn peer_total_sent(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<NumberResponse>,
    ) {
        forward_to_targets!(self.targets, peer_total_sent, ctx, req, sink);
    }

    fn peer_total_received(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<NumberResponse>,
    ) {
        forward_to_targets!(self.targets, peer_total_received, ctx, req, sink);
    }

    fn peer_version(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<StringResponse>,
    ) {
        forward_to_targets!(self.targets, peer_version, ctx, req, sink);
    }

    fn send_message(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: SendMessageRequest,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        forward_to_targets!(self.targets, send_message, ctx, req, sink);
    }

    fn peer_stats(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<PeerStatsResponse>,
    ) {
        forward_to_targets!(self.targets, peer_stats, ctx, req, sink);
    }

    fn peer_list(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<PeerListResponse>,
    ) {
        forward_to_targets!(self.targets, peer_list, ctx, req, sink);
    }

    fn subscription_start(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        forward_to_targets!(self.targets, subscription_start, ctx, req, sink);
    }

    fn subscription_stop(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        forward_to_targets!(self.targets, subscription_stop, ctx, req, sink);
    }

    fn node_info(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<NodeInfoResponse>,
    ) {
        forward_to_targets!(self.targets, node_info, ctx, req, sink);
    }

    fn subscription_poll(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<P2PNetworkMessage>,
    ) {
        forward_to_targets!(self.targets, subscription_poll, ctx, req, sink);
    }

    fn ban_node(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: PeerElement,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        forward_to_targets!(self.targets, ban_node, ctx, req, sink);
    }

    fn unban_node(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: PeerElement,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        forward_to_targets!(self.targets, unban_node, ctx, req, sink);
    }

    fn join_network(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: NetworkChangeRequest,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        forward_to_targets!(self.targets, join_network, ctx, req, sink);
    }

    fn leave_network(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: NetworkChangeRequest,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        forward_to_targets!(self.targets, leave_network, ctx, req, sink);
    }

    fn get_consensus_status(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<SuccessfulJsonPayloadResponse>,
    ) {
        forward_to_targets!(self.targets, get_consensus_status, ctx, req, sink);
    }

    fn get_block_info(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: BlockHash,
        sink: ::grpcio::UnarySink<SuccessfulJsonPayloadResponse>,
    ) {
        forward_to_targets!(self.targets, get_block_info, ctx, req, sink);
    }

    fn get_ancestors(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: BlockHashAndAmount,
        sink: ::grpcio::UnarySink<SuccessfulJsonPayloadResponse>,
    ) {
        forward_to_targets!(self.targets, get_ancestors, ctx, req, sink);
    }

    fn get_branches(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<SuccessfulJsonPayloadResponse>,
    ) {
        forward_to_targets!(self.targets, get_branches, ctx, req, sink);
    }

    fn send_transaction(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: SendTransactionRequest,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        forward_to_targets!(self.targets, send_transaction, ctx, req, sink);
    }

    fn get_last_final_account_list(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<SuccessfulBytePayloadResponse>,
    ) {
        forward_to_targets!(self.targets, get_last_final_account_list, ctx, req, sink);
    }

    fn get_last_final_instances(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<SuccessfulBytePayloadResponse>,
    ) {
        forward_to_targets!(self.targets, get_last_final_instances, ctx, req, sink);
    }

    fn get_last_final_account_info(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: AccountAddress,
        sink: ::grpcio::UnarySink<SuccessfulBytePayloadResponse>,
    ) {
        forward_to_targets!(self.targets, get_last_final_account_info, ctx, req, sink);
    }

    fn get_last_final_instance_info(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: ContractInstanceAddress,
        sink: ::grpcio::UnarySink<SuccessfulBytePayloadResponse>,
    ) {
        forward_to_targets!(self.targets, get_last_final_instance_info, ctx, req, sink);
    }
}
