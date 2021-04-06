use crate::{
    common::p2p_peer::RemotePeerId,
    consensus_ffi::{catch_up::PeerStatus, helpers::PacketType},
};
use std::{fmt, sync::Arc};

/// The type of messages passed between GlobalState and the consensus layer.
///
/// It contains an optional identifier of the source peer if it is not our own
/// consensus layer.
pub struct ConsensusMessage {
    pub direction:     MessageType,
    pub variant:       PacketType,
    pub payload:       Arc<[u8]>,
    pub dont_relay_to: Vec<RemotePeerId>,
    pub omit_status:   Option<PeerStatus>,
}

impl ConsensusMessage {
    pub fn new(
        direction: MessageType,
        variant: PacketType,
        payload: Arc<[u8]>,
        dont_relay_to: Vec<RemotePeerId>,
        omit_status: Option<PeerStatus>,
    ) -> Self {
        Self {
            direction,
            variant,
            payload,
            dont_relay_to,
            omit_status,
        }
    }

    pub fn distribution_mode(&self) -> DistributionMode {
        match self.direction {
            MessageType::Inbound(_, distribution_mode) => distribution_mode,
            MessageType::Outbound(Some(_)) => DistributionMode::Direct,
            MessageType::Outbound(None) => DistributionMode::Broadcast,
        }
    }

    pub fn target_peer(&self) -> Option<RemotePeerId> {
        if let MessageType::Outbound(target) = self.direction {
            target
        } else {
            panic!("An Inbound ConsensusMessage doesn't have a target peer!");
        }
    }

    pub fn source_peer(&self) -> RemotePeerId {
        if let MessageType::Inbound(source, _) = self.direction {
            source
        } else {
            panic!("An Outbound ConsensusMessage doesn't have a source peer!");
        }
    }

    pub fn dont_relay_to(&self) -> Vec<RemotePeerId> { self.dont_relay_to.clone() }
}

impl fmt::Display for ConsensusMessage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let party_name = match self.direction {
            MessageType::Inbound(peer_id, _) => format!("from peer {:?}", peer_id),
            MessageType::Outbound(Some(peer_id)) => format!("to peer {:?}", peer_id),
            _ => "from our consensus layer".to_owned(),
        };

        write!(f, "{} {}", self.variant, party_name)
    }
}

#[derive(PartialEq)]
/// The type indicating the source/target of a ConsensusMessage.
pub enum MessageType {
    /// Inbound messages come from other peers; they contain their PeerId and
    /// indicate whether is was a direct message or a broadcast.
    Inbound(RemotePeerId, DistributionMode),
    /// Outbound messages are produced by the consensus layer and either
    /// directed at a specific PeerId or None in case of broadcasts.
    Outbound(Option<RemotePeerId>),
}

#[derive(PartialEq, Clone, Copy)]
pub enum DistributionMode {
    Direct,
    Broadcast,
}
