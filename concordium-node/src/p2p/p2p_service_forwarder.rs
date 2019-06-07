use crate::proto::{
    concordium_p2p_rpc_grpc::P2P, AccountAddress, BlockHash, BlockHashAndAmount,
    ContractInstanceAddress, DumpRequest, Empty, NetworkChangeRequest, NodeInfoResponse,
    NumberResponse, P2PNetworkMessage, PeerConnectRequest, PeerElement, PeerListResponse,
    PeerStatsResponse, RetransmitRequestMessage, SendMessageRequest, SendTransactionRequest,
    StringResponse, SuccessResponse, SuccessfulBytePayloadResponse, SuccessfulJsonPayloadResponse,
    TpsRequest,
};
use futures::future::Future;
use std::sync::Arc;

#[derive(Clone)]
pub struct P2PServiceForwarder {
    pub targets: Arc<Vec<Box<dyn P2P + Send + Sync>>>,
}

impl Default for P2PServiceForwarder {
    fn default() -> Self { P2PServiceForwarder::new() }
}

impl P2PServiceForwarder {
    pub fn new() -> Self {
        P2PServiceForwarder {
            targets: Arc::new(Vec::new()),
        }
    }
}

lazy_static! {
    static ref P2P_SERVICE_FORWARDER: Arc<P2PServiceForwarder> =
        Arc::new(P2PServiceForwarder::new());
}

pub fn p2p_service_forwarder() -> Arc<P2PServiceForwarder> { Arc::clone(&P2P_SERVICE_FORWARDER) }

macro_rules! forward_to_targets {
    ($target:expr, $func:ident, $ctx:ident, $req:ident, $sink:ident) => {
        let target_ref = &$target;

        match target_ref.len() {
            1 => {
                target_ref[0].$func($ctx, $req, $sink);
            }
            _ => {
                error!("Only one forward target is supported");
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

    fn get_banned_peers(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<PeerListResponse>,
    ) {
        forward_to_targets!(self.targets, get_banned_peers, ctx, req, sink);
    }

    fn shutdown(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        let mut r: SuccessResponse = SuccessResponse::new();
        r.set_value(false);
        let f = sink
            .success(r)
            .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
        ctx.spawn(f);
    }

    fn tps_test(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: TpsRequest,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        forward_to_targets!(self.targets, tps_test, ctx, req, sink);
    }

    fn dump_start(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: DumpRequest,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        forward_to_targets!(self.targets, dump_start, ctx, req, sink);
    }

    fn dump_stop(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: Empty,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        forward_to_targets!(self.targets, dump_stop, ctx, req, sink);
    }

    fn retransmit_request(
        &self,
        ctx: ::grpcio::RpcContext<'_>,
        req: RetransmitRequestMessage,
        sink: ::grpcio::UnarySink<SuccessResponse>,
    ) {
        forward_to_targets!(self.targets, retransmit_request, ctx, req, sink);
    }
}
