use byteorder::{ByteOrder, NetworkEndian, WriteBytesExt};
use concordium_common::{PacketType, SHA256};
use failure::Fallible;

use std::{
    cmp::Ordering,
    convert::TryFrom,
    fmt,
    io::{Cursor, Read},
    mem::{self, size_of},
    sync::Arc,
};

use crate::{
    block::*,
    common::{create_serialization_cursor, HashBytes, SerializeToBytes},
    finalization::*,
};

use super::{PeerId, SkovState};

/// The type of messages passed between Skov and the consensus layer.
///
/// It contains an optional identifier of the source peer if it is not our own
/// consensus layer.
pub struct ConsensusMessage {
    pub direction: MessageType,
    pub variant:   PacketType,
    pub payload:   Arc<[u8]>,
}

impl ConsensusMessage {
    pub fn new(direction: MessageType, variant: PacketType, payload: Arc<[u8]>) -> Self {
        Self {
            direction,
            variant,
            payload,
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
}

impl fmt::Display for ConsensusMessage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        macro_rules! print_deserialized {
            ($object:ty) => {{
                if let Ok(object) = <$object>::deserialize(&self.payload) {
                    format!("{:?}", object)
                } else {
                    format!("corrupted bytes")
                }
            }};
        }

        let content = match self.variant {
            PacketType::Block => print_deserialized!(Block),
            PacketType::FinalizationRecord => print_deserialized!(FinalizationRecord),
            PacketType::FinalizationMessage => print_deserialized!(FinalizationMessage),
            PacketType::CatchupBlockByHash => {
                let hash = HashBytes::new(&self.payload[..SHA256 as usize]);
                let delta = NetworkEndian::read_u64(
                    &self.payload[SHA256 as usize..][..mem::size_of::<Delta>()],
                );
                let delta = if delta == 0 {
                    "".to_owned()
                } else {
                    format!(", delta {}", delta)
                };
                format!("catch-up request for block {:?}{}", hash, delta)
            }
            PacketType::CatchupFinalizationRecordByHash => {
                let hash = HashBytes::new(&self.payload[..SHA256 as usize]);
                format!(
                    "catch-up request for the finalization record for block {:?}",
                    hash
                )
            }
            PacketType::CatchupFinalizationRecordByIndex => {
                let idx = NetworkEndian::read_u64(
                    &self.payload[..mem::size_of::<FinalizationIndex>() as usize],
                );
                format!(
                    "catch-up request for the finalization record at index {}",
                    idx
                )
            }
            p => p.to_string(),
        };

        let party_name = match self.direction {
            MessageType::Inbound(peer_id, _) => format!("from peer {:016x}", peer_id),
            MessageType::Outbound(Some(peer_id)) => format!("to peer {:016x}", peer_id),
            _ => "from our consensus layer".to_owned(),
        };

        write!(f, "{} {}", content, party_name)
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
/// Holds a response for a request to Skov.
///
/// Depending on the request, the result can either be just a status or contain
/// the requested data.
pub enum SkovResult {
    SuccessfulEntry(PacketType),
    SuccessfulQuery(Box<[u8]>),
    DuplicateEntry,
    Error(SkovError),
    Housekeeping,
    IgnoredEntry,
    BestPeer((PeerId, SkovMetadata)),
}

#[derive(Debug, PartialEq)]
/// Indicates an erroneous result of a request to Skov.
///
/// If there are two components, the first one is the target and the second is
/// the source
pub enum SkovError {
    // the parent block is not in the tree
    MissingParentBlock(HashBytes, HashBytes),
    // the target last finalized block is not in the tree
    MissingLastFinalizedBlock(HashBytes, HashBytes),
    // the target last finalized block has not been finalized yet
    LastFinalizedNotFinalized(HashBytes, HashBytes),
    // the target last finalized block is not the last finalized block in the tree
    InvalidLastFinalized(HashBytes, HashBytes),
    // the block pointed to by the finalization record is not in the tree
    MissingBlockToFinalize(HashBytes),
    // the requested block is not available
    MissingBlock(HashBytes, Delta),
    // the requested finalization record for the given block hash is not available
    MissingFinalizationRecordByHash(HashBytes),
    // the requested finalization record with the given finalization index is not available
    MissingFinalizationRecordByIdx(FinalizationIndex),
    // the finalization record's index is in the future
    FutureFinalizationRecord(FinalizationIndex, FinalizationIndex),
}

impl fmt::Display for SkovError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let msg = match self {
            SkovError::MissingParentBlock(ref parent, ref pending) => format!(
                "block {:?} is pointing to a parent ({:?}) that is not in the tree",
                pending, parent
            ),
            SkovError::MissingLastFinalizedBlock(ref last_finalized, ref pending) => format!(
                "block {:?} is pointing to a last finalized block ({:?}) that is not in the tree",
                pending, last_finalized
            ),
            SkovError::LastFinalizedNotFinalized(ref last_finalized, ref pending) => format!(
                "block {:?} is pointing to a last finalized block ({:?}) that has not been \
                 finalized yet",
                pending, last_finalized
            ),
            SkovError::InvalidLastFinalized(ref last_finalized, ref pending) => format!(
                "block {:?} wrongly states that {:?} is the last finalized block",
                pending, last_finalized
            ),
            SkovError::MissingBlockToFinalize(ref target) => format!(
                "finalization record for block {:?} references a block that is not in the tree",
                target
            ),
            SkovError::MissingBlock(ref hash, delta) => format!(
                "requested block {:?} delta {} is not available",
                hash, delta
            ),
            SkovError::MissingFinalizationRecordByHash(ref hash) => format!(
                "requested finalization record for block {:?} is not available",
                hash
            ),
            SkovError::MissingFinalizationRecordByIdx(index) => format!(
                "requested finalization record for index {} is not available",
                index
            ),
            SkovError::FutureFinalizationRecord(future_idx, curr_idx) => format!(
                "the finalization record's index ({}) is in the future (current index: {})",
                future_idx, curr_idx
            ),
        };

        write!(f, "error: {}", msg)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SkovMetadata {
    pub finalized_height: BlockHeight,
    pub n_pending_blocks: u64,
    pub state:            SkovState,
}

impl SkovMetadata {
    pub fn is_usable(&self) -> bool {
        self.state == SkovState::Complete
            && !(self.finalized_height == 0 && self.n_pending_blocks == 0)
    }
}

impl PartialOrd for SkovMetadata {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let result = if self.finalized_height != other.finalized_height {
            self.finalized_height.cmp(&other.finalized_height)
        } else {
            self.n_pending_blocks.cmp(&other.n_pending_blocks)
        };

        Some(result)
    }
}

impl Ord for SkovMetadata {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap() // infallible
    }
}

impl<'a, 'b: 'a> SerializeToBytes<'a, 'b> for SkovMetadata {
    type Source = &'a [u8];

    fn deserialize(bytes: Self::Source) -> Fallible<Self> {
        let mut cursor = Cursor::new(bytes);

        let finalized_height = NetworkEndian::read_u64(&read_ty!(&mut cursor, BlockHeight));
        let n_pending_blocks = NetworkEndian::read_u64(&read_ty!(&mut cursor, u64));
        let state = SkovState::try_from(read_const_sized!(&mut cursor, 1)[0])?;

        Ok(SkovMetadata {
            finalized_height,
            n_pending_blocks,
            state,
        })
    }

    fn serialize(&self) -> Box<[u8]> {
        let mut cursor = create_serialization_cursor(size_of::<BlockHeight>() + 8 + 1);

        let _ = cursor.write_u64::<NetworkEndian>(self.finalized_height);
        let _ = cursor.write_u64::<NetworkEndian>(self.n_pending_blocks);
        let _ = cursor.write_u8(self.state as u8);

        cursor.into_inner()
    }
}
