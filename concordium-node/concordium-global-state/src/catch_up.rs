use byteorder::{ByteOrder, ReadBytesExt, WriteBytesExt};
use failure::Fallible;
use nohash_hasher::BuildNoHashHasher;
use priority_queue::PriorityQueue;

use crate::{block::BlockHeight, common::read_ty};

use concordium_common::{
    blockchain_types::BlockHash,
    network_types::PeerId,
    serial::{Endianness, NoParam, Serial},
};

use std::{cmp::Ordering, time::Instant};

#[derive(Debug)]
pub struct CatchUpStatus {
    is_request:              bool,
    last_finalized_block:    BlockHash,
    last_finalized_height:   BlockHeight,
    best_block:              BlockHash,
    finalization_justifiers: Box<[BlockHash]>,
}

impl Serial for CatchUpStatus {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        let is_request = read_ty!(source, bool)[0] != 0;
        let last_finalized_block = BlockHash::from(read_ty!(source, BlockHash));
        let last_finalized_height = BlockHeight::deserial(source)?;
        let best_block = BlockHash::from(read_ty!(source, BlockHash));

        let finalization_justifiers = read_multiple!(
            source,
            BlockHash::from(read_ty!(source, BlockHash)),
            4,
            1024
        );

        let status = CatchUpStatus {
            is_request,
            last_finalized_block,
            last_finalized_height,
            best_block,
            finalization_justifiers,
        };

        Ok(status)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        target.write_u8(self.is_request as u8)?;
        target.write_all(&self.last_finalized_block)?;
        self.last_finalized_height.serial(target)?;
        target.write_all(&self.best_block)?;

        target.write_u32::<Endianness>(self.finalization_justifiers.len() as u32)?;
        for fj in &*self.finalization_justifiers {
            target.write_all(fj)?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct PeerState {
    pub status: PeerStatus,
    timestamp:  Instant,
}

impl PeerState {
    pub fn new(status: PeerStatus) -> Self {
        Self {
            status,
            timestamp: Instant::now(),
        }
    }

    pub fn is_catching_up(&self) -> bool { PeerStatus::CatchingUp == self.status }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub enum PeerStatus {
    CatchingUp = 2,
    Pending    = 1,
    UpToDate   = 0,
}

impl Ord for PeerState {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.status != other.status {
            self.status.cmp(&other.status)
        } else {
            other.timestamp.cmp(&self.timestamp)
        }
    }
}

impl PartialOrd for PeerState {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { Some(self.cmp(other)) }
}

#[derive(Default)]
pub struct PeerList {
    pub peers:          PriorityQueue<PeerId, PeerState, BuildNoHashHasher<PeerId>>,
    pub catch_up_stamp: u64,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn peer_queue_logic() {
        let mut peers = PriorityQueue::new();

        peers.push(0, PeerState::new(PeerStatus::UpToDate));
        peers.push(1, PeerState::new(PeerStatus::Pending));
        peers.push(2, PeerState::new(PeerStatus::CatchingUp));
        peers.push(3, PeerState::new(PeerStatus::Pending));

        let sorted_ids = peers
            .into_sorted_iter()
            .map(|(id, _)| id)
            .collect::<Vec<_>>();

        assert_eq!(sorted_ids, vec![2, 1, 3, 0]);
    }
}
