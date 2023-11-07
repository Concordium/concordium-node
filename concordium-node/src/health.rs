//! Implementation of the health check service that is part of the GRPC2
//! interface.
//!
//! There are two "health" service implementations for the same underlying type.
//! The service under [`concordium`] is the original service, it takes no
//! arguments and returns no values, it communicates health via gRPC status
//! codes.
//!
//! The service under [`grpc_health_v1`] implements the
//! [GRPC health checking protocol](https://github.com/grpc/grpc/blob/master/doc/health-checking.md)
//! and responds with Ok and a message that contains the status.
//!
//! See also [GRPC Core health checking protocol](https://grpc.github.io/grpc/core/md_doc_health-checking.html)
//! for details about expectations of this service.

use crate::{
    common::PeerType,
    consensus_ffi::{consensus::ConsensusContainer, helpers::ConsensusIsInBakingCommitteeResponse},
    p2p::P2PNode,
};
use std::sync::Arc;

/// The type that implements the service that responds to health queries.
#[derive(Clone)]
pub(crate) struct HealthServiceImpl {
    pub(crate) consensus: ConsensusContainer,
    pub(crate) node: Arc<P2PNode>,
    pub(crate) health_max_finalization_delay: concordium_base::base::DurationSeconds,
    pub(crate) health_min_peers: Option<usize>,
}

#[derive(Debug, thiserror::Error)]
enum ServiceError<'a> {
    #[error("Consensus is not running.")]
    ConsensusNotRunning,
    #[error(
        "The node only has {num_peers} peers, but is required to have at least \
         {min_allowed_peers}."
    )]
    TooFewPeers {
        num_peers:         usize,
        min_allowed_peers: usize,
    },
    #[error("Last finalized block is too far behind.")]
    LastFinalFarBehind,
    #[error("The node is configured with baker credentials, but is not in the baking committee.")]
    NotInCommittee,
    #[error("Node local time is before Unix epoch: {0}.")]
    TimeInPast(#[from] std::time::SystemTimeError),
    #[error("Service not known: {service}")]
    ServiceNotFound {
        service: &'a str,
    },
}

impl<'a> ServiceError<'a> {
    pub fn is_not_found(&self) -> bool { matches!(self, Self::ServiceNotFound { .. }) }
}

impl<'a> From<ServiceError<'a>> for tonic::Status {
    fn from(value: ServiceError<'a>) -> Self {
        if value.is_not_found() {
            tonic::Status::not_found(value.to_string())
        } else {
            tonic::Status::unavailable(value.to_string())
        }
    }
}

impl HealthServiceImpl {
    /// Check whether the supplied service is healthy. The empty service `""` is
    /// interpreted as checking the overall health. This is to follow the grpc semantics [https://grpc.github.io/grpc/core/md_doc_health-checking.html].
    /// This notion is contrived in our case since we only have one service,
    /// `concordium.v2.Queries` but is implemented to follow the specification.
    async fn check_service<'a>(&self, service: &'a str) -> Result<(), ServiceError<'a>> {
        if !service.is_empty() && service != "concordium.v2.Queries" {
            return Err(ServiceError::ServiceNotFound {
                service,
            });
        }

        let consensus_running = self.consensus.is_consensus_running();

        if !consensus_running {
            return Err(ServiceError::ConsensusNotRunning);
        }

        let last_fin_slot_time = self.consensus.get_last_finalized_block_slot_time_v2();

        let current_time =
            std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH)?.as_millis() as u64;

        // If the slot time is in the future that is also good. We do accept blocks
        // a little bit in the future, but consensus ensures they are not too far.
        // That is why using saturating_sub is sensible.
        let delta = current_time.saturating_sub(last_fin_slot_time.millis);

        if delta > 1000 * u64::from(self.health_max_finalization_delay) {
            return Err(ServiceError::LastFinalFarBehind);
        }

        if let Some(min_allowed_peers) = self.health_min_peers {
            let num_peers = self.node.get_peer_stats(Some(PeerType::Node)).len();
            if num_peers < min_allowed_peers {
                return Err(ServiceError::TooFewPeers {
                    num_peers,
                    min_allowed_peers,
                });
            }
        }

        if self.consensus.is_active() {
            let (committee_status, _, _, _) = self.consensus.in_baking_committee();
            if committee_status != ConsensusIsInBakingCommitteeResponse::ActiveInCommittee {
                return Err(ServiceError::NotInCommittee);
            }
        }

        Ok(())
    }
}

pub mod grpc_health_v1 {
    include!(concat!(env!("OUT_DIR"), "/grpc.health.v1.rs"));

    pub(crate) static HEALTH_DESCRIPTOR: &[u8] =
        tonic::include_file_descriptor_set!("grpc_health_v1_descriptor");

    #[tonic::async_trait]
    impl health_server::Health for super::HealthServiceImpl {
        type WatchStream = futures::stream::Empty<tonic::Result<HealthCheckResponse>>;

        async fn check(
            &self,
            request: tonic::Request<HealthCheckRequest>,
        ) -> Result<tonic::Response<HealthCheckResponse>, tonic::Status> {
            match self.check_service(request.into_inner().service.as_str()).await {
                Ok(()) => Ok(tonic::Response::new(HealthCheckResponse {
                    status: health_check_response::ServingStatus::Serving.into(),
                })),
                Err(e) if e.is_not_found() => Err(e.into()),
                _ => Ok(tonic::Response::new(HealthCheckResponse {
                    status: health_check_response::ServingStatus::NotServing.into(),
                })),
            }
        }

        // According to the service description it is OK to respond with "Not
        // implemented" and the clients should not try to query this again.
        // We don't implement this since it'd require more extensive changes
        // to trigger sending subsequent messages if the service gets unhealthy.
        async fn watch(
            &self,
            _request: tonic::Request<HealthCheckRequest>,
        ) -> Result<tonic::Response<Self::WatchStream>, tonic::Status> {
            Err(tonic::Status::unimplemented("Watch is not implemented."))
        }
    }
}

pub mod concordium {
    include!(concat!(env!("OUT_DIR"), "/concordium.health.rs"));

    pub(crate) static HEALTH_DESCRIPTOR: &[u8] =
        tonic::include_file_descriptor_set!("health_descriptor");

    #[tonic::async_trait]
    impl health_server::Health for super::HealthServiceImpl {
        async fn check(
            &self,
            _request: tonic::Request<NodeHealthRequest>,
        ) -> Result<tonic::Response<NodeHealthResponse>, tonic::Status> {
            self.check_service("").await?;
            Ok(tonic::Response::new(NodeHealthResponse {}))
        }
    }
}
