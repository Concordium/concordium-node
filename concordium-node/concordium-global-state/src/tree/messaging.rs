use byteorder::{ByteOrder, NetworkEndian, WriteBytesExt};
use concordium_common::PacketType;
use failure::{Fail, Fallible};

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

use super::{PeerId, ProcessingState};

/// The type of messages passed between GlobalState and the consensus layer.
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
            PacketType::FullCatchupRequest => {
                let since = NetworkEndian::read_u64(
                    &self.payload[..mem::size_of::<BlockHeight>() as usize],
                );
                format!("catch-up request since height {}", since)
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
    BestPeer((PeerId, GlobalMetadata)),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GlobalMetadata {
    pub finalized_height: BlockHeight,
    pub n_pending_blocks: u64,
    pub state:            ProcessingState,
}

impl GlobalMetadata {
    pub fn is_usable(&self) -> bool {
        self.state == ProcessingState::Complete
            && !(self.finalized_height == 0 && self.n_pending_blocks == 0)
    }
}

impl PartialOrd for GlobalMetadata {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let result = if self.finalized_height != other.finalized_height {
            self.finalized_height.cmp(&other.finalized_height)
        } else {
            self.n_pending_blocks.cmp(&other.n_pending_blocks)
        };

        Some(result)
    }
}

impl Ord for GlobalMetadata {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap() // infallible
    }
}

impl<'a, 'b: 'a> SerializeToBytes<'a, 'b> for GlobalMetadata {
    type Source = &'a [u8];

    fn deserialize(bytes: Self::Source) -> Fallible<Self> {
        let mut cursor = Cursor::new(bytes);

        let finalized_height = NetworkEndian::read_u64(&read_ty!(&mut cursor, BlockHeight));
        let n_pending_blocks = NetworkEndian::read_u64(&read_ty!(&mut cursor, u64));
        let state = ProcessingState::try_from(read_const_sized!(&mut cursor, 1)[0])?;

        Ok(GlobalMetadata {
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
