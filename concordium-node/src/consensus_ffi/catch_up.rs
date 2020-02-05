use concordium_common::network_types::PeerId;
use nohash_hasher::BuildNoHashHasher;
use priority_queue::PriorityQueue;
use std::{cmp::Ordering, time::Instant};

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

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub enum PeerStatus {
    CatchingUp = 2,
    Pending    = 1,
    UpToDate   = 0,
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
