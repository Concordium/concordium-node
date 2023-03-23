//! Implementation of the health check service that is part of the GRPC2
//! interface.

use crate::{
    common::PeerType,
    consensus_ffi::{consensus::ConsensusContainer, helpers::ConsensusIsInBakingCommitteeResponse},
    p2p::P2PNode,
};
use std::sync::Arc;

include!(concat!(env!("OUT_DIR"), "/concordium.health.rs"));

pub(crate) static HEALTH_DESCRIPTOR: &[u8] =
    tonic::include_file_descriptor_set!("health_descriptor");

/// The type that implements the service that responds to queries.
pub(crate) struct HealthServiceImpl {
    pub(crate) consensus: ConsensusContainer,
    pub(crate) node: Arc<P2PNode>,
    pub(crate) health_max_finalization_delay: concordium_base::base::DurationSeconds,
    pub(crate) health_min_peers: Option<usize>,
}

#[tonic::async_trait]
impl health_server::Health for HealthServiceImpl {
    async fn check(
        &self,
        _request: tonic::Request<NodeHealthRequest>,
    ) -> Result<tonic::Response<NodeHealthResponse>, tonic::Status> {
        let consensus_running = self.consensus.is_consensus_running();

        if !consensus_running {
            return Err(tonic::Status::unavailable("Consensus is not running."));
        }

        let last_fin_slot_time = self.consensus.get_last_finalized_block_slot_time_v2();

        let current_time = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map_err(|_| tonic::Status::internal("Time went backwards."))?
            .as_millis() as u64;

        // If the slot time is in the future that is also good. We do accept blocks
        // a little bit in the future, but consensus ensures they are not too far.
        // That is why using saturating_sub is sensible.
        let delta = current_time.saturating_sub(last_fin_slot_time.millis);

        if delta > 1000 * u64::from(self.health_max_finalization_delay) {
            return Err(tonic::Status::unavailable("Last finalized block is too far behind."));
        }

        if let Some(min_allowed_peers) = self.health_min_peers {
            let num_peers = self.node.get_peer_stats(Some(PeerType::Node)).len();
            if num_peers < min_allowed_peers {
                return Err(tonic::Status::unavailable(format!(
                    "The node only has {} peers, but is required to have at least {}.",
                    num_peers, min_allowed_peers
                )));
            }
        }

        if self.consensus.is_active() {
            let (committee_status, _, _, _) = self.consensus.in_baking_committee();
            if committee_status != ConsensusIsInBakingCommitteeResponse::ActiveInCommittee {
                return Err(tonic::Status::unavailable(
                    "The node is configured with baker credentials, but is not in the baking \
                     committee.",
                ));
            }
        }

        Ok(tonic::Response::new(NodeHealthResponse {}))
    }
}
