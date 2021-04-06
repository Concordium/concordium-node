use crate::common::{p2p_node_id::PeerId, p2p_peer::RemotePeerId};
use nohash_hasher::BuildNoHashHasher;
use std::{
    cmp::Ordering,
    collections::{HashMap, VecDeque},
    time::Instant,
};

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
    /// The state of each peer.
    pub peer_states: HashMap<RemotePeerId, PeerStatus, BuildNoHashHasher<PeerId>>,
    /// The timestamp at which we last tried to catch up with a peer.
    pub catch_up_stamp: u64,
    /// The peer that we are currently catching up with (if any).
    pub catch_up_peer: Option<RemotePeerId>,
    /// Queue of pending peers.
    pub pending_queue: VecDeque<RemotePeerId>,
}

impl PeerList {
    /// Pull the next pending peer from the queue and mark it as catching-up.
    /// This does not alter catch_up_stamp, but it does set catch_up_peer.
    /// pending_queue should only contain peers that are actually pending,
    /// (according to peer_states) but this is checked when they are dequeued
    /// and if a non-pending peer is encountered it is simply removed from
    /// the queue.
    pub fn next_pending(&mut self) -> Option<RemotePeerId> {
        let mut next = self.pending_queue.pop_front();
        while let Some(peer) = next {
            if let Some(state) = self.peer_states.get_mut(&peer) {
                if let PeerStatus::Pending = *state {
                    // The peer is actually pending.
                    *state = PeerStatus::CatchingUp;
                    break;
                }
            }
            // The peer is not actually pending.
            next = self.pending_queue.pop_front();
        }
        self.catch_up_peer = next;
        next
    }
}
