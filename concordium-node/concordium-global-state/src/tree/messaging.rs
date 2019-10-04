use concordium_common::{hybrid_buf::HybridBuf, network_types::PeerId, PacketType};
use failure::Fail;

use std::fmt;

use crate::{common::HashBytes, finalization::*};

/// The type of messages passed between GlobalState and the consensus layer.
///
/// It contains an optional identifier of the source peer if it is not our own
/// consensus layer.
pub struct ConsensusMessage {
    pub direction:     MessageType,
    pub variant:       PacketType,
    pub payload:       HybridBuf,
    pub dont_relay_to: Vec<PeerId>,
}

impl ConsensusMessage {
    pub fn new(
        direction: MessageType,
        variant: PacketType,
        payload: HybridBuf,
        dont_relay_to: Vec<PeerId>,
    ) -> Self {
        Self {
            direction,
            variant,
            payload,
            dont_relay_to,
        }
    }

    pub fn distribution_mode(&self) -> DistributionMode {
        match self.direction {
            MessageType::Inbound(_, distribution_mode) => distribution_mode,
            MessageType::Outbound(Some(_)) => DistributionMode::Direct,
            MessageType::Outbound(None) => DistributionMode::Broadcast,
        }
    }

    pub fn target_peer(&self) -> Option<PeerId> {
        if let MessageType::Outbound(target) = self.direction {
            target
        } else {
            panic!("An Inbound ConsensusMessage doesn't have a target peer!");
        }
    }

    pub fn source_peer(&self) -> PeerId {
        if let MessageType::Inbound(source, _) = self.direction {
            source
        } else {
            panic!("An Outbound ConsensusMessage doesn't have a source peer!");
        }
    }

    pub fn dont_relay_to(&self) -> Vec<PeerId> { self.dont_relay_to.clone() }
}

impl fmt::Display for ConsensusMessage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let party_name = match self.direction {
            MessageType::Inbound(peer_id, _) => format!("from peer {:016x}", peer_id),
            MessageType::Outbound(Some(peer_id)) => format!("to peer {:016x}", peer_id),
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
    Inbound(PeerId, DistributionMode),
    /// Outbound messages are produced by the consensus layer and either
    /// directed at a specific PeerId or None in case of broadcasts.
    Outbound(Option<PeerId>),
}

#[derive(PartialEq, Clone, Copy)]
pub enum DistributionMode {
    Direct,
    Broadcast,
}

#[derive(Debug, PartialEq)]
/// Holds a response for a request to GlobalState.
///
/// Depending on the request, the result can either be just a status or contain
/// the requested data.
pub enum GlobalStateResult {
    SuccessfulEntry(PacketType),
    SuccessfulQuery(Box<[u8]>),
    DuplicateEntry,
    Error(GlobalStateError),
    Housekeeping,
    IgnoredEntry,
}

#[derive(Debug, PartialEq, Fail)]
#[rustfmt::skip]
/// Indicates an erroneous result of a request to GlobalState.
///
/// If there are two components, the first one is the target and the second is
/// the source
pub enum GlobalStateError {
    // a non-existent Block
    #[fail(display = "block {:?} does not exist in the store!", _0)]
    MissingBlock(HashBytes),
    // the parent block is not in the tree
    #[fail(display = "block {:?}'s parent ({:?}) is not in the tree!", _1, _0)]
    MissingParentBlock(HashBytes, HashBytes),
    // the target last finalized block is not in the tree
    #[fail(display = "block {:?}'s last finalized block ({:?}) is not in the tree!", _1, _0)]
    MissingLastFinalizedBlock(HashBytes, HashBytes),
    // the target last finalized block has not been finalized yet
    #[fail(display = "block {:?}'s last finalized block ({:?}) has not been finalized!", _1, _0)]
    LastFinalizedNotFinalized(HashBytes, HashBytes),
    // the target last finalized block is not the last finalized block in the tree
    #[fail(display = "block {:?}'s last finalized block ({:?}) is not the last one!", _1, _0)]
    InvalidLastFinalized(HashBytes, HashBytes),
    // the block pointed to by the finalization record is not in the tree
    #[fail(display = "finalization record references a non-existent block ({:?})!", _0)]
    MissingBlockToFinalize(HashBytes),
    // the finalization record's index is in the future
    #[fail(display = "finalization record's index ({}) is in the future (current: {})!", _0, _1)]
    FutureFinalizationRecord(FinalizationIndex, FinalizationIndex),
}
