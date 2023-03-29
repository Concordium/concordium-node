//! Network bucket handling.

use rand::seq::IteratorRandom;
use std::{
    collections::HashSet,
    hash::{Hash, Hasher},
};

use crate::{
    common::{get_current_stamp, p2p_peer::RemotePeerId, PeerType, RemotePeer},
    network::Networks,
};

const BUCKET_COUNT: usize = 1;

/// A representation of a node in a bucket.
#[derive(Eq, Clone)]
pub struct Node {
    pub peer:      RemotePeer,
    pub networks:  Networks,
    /// The timestamp pointing to when the node was seen last.
    pub last_seen: u64,
}

impl PartialEq for Node {
    fn eq(&self, other: &Node) -> bool { self.peer == other.peer }
}

impl Hash for Node {
    fn hash<H: Hasher>(&self, state: &mut H) { self.peer.hash(state) }
}

/// A bucket of nodes.
pub type Bucket = HashSet<Node>;

/// The set of buckets.
pub struct Buckets {
    pub buckets: Vec<Bucket>,
}

impl Default for Buckets {
    fn default() -> Self {
        Buckets {
            buckets: vec![HashSet::new(); BUCKET_COUNT],
        }
    }
}

impl Buckets {
    /// Adds a peer to a bucket.
    pub fn insert_into_bucket(
        &mut self,
        peer: RemotePeer,
        networks: Networks,
        on_insert_success: impl Fn(u64),
    ) {
        let bucket = &mut self.buckets[0];

        if bucket.insert(Node {
            peer,
            networks,
            last_seen: get_current_stamp(),
        }) {
            on_insert_success(0);
        }
    }

    /// Update the networks of a node in the bucket.
    pub fn update_network_ids(&mut self, peer: RemotePeer, networks: Networks) {
        let bucket = &mut self.buckets[0];
        bucket.replace(Node {
            peer,
            networks,
            last_seen: get_current_stamp(),
        });
    }

    /// Returns all the nodes in buckets with the possible exception of the
    /// sender, if it is supplied.
    fn get_all_nodes(&self, sender: Option<RemotePeerId>, networks: &Networks) -> Vec<RemotePeer> {
        let mut nodes = Vec::new();
        let filter_criteria = |node: &&Node| {
            node.peer.peer_type == PeerType::Node
                && Some(node.peer.local_id) != sender
                && (networks.is_empty() || !node.networks.is_disjoint(networks))
        };

        for bucket in &self.buckets {
            nodes.extend(bucket.iter().filter(filter_criteria).map(|node| node.peer.to_owned()))
        }

        nodes
    }

    /// Returns the number of networks in the buckets.
    pub fn len(&self) -> usize {
        self.buckets.iter().flat_map(HashSet::iter).map(|node| node.networks.len()).sum()
    }

    /// Checks whether the buckets are empty.
    pub fn is_empty(&self) -> bool { self.len() == 0 }

    /// Returns the desired number of nodes from the buckets.
    pub fn get_random_nodes(
        &self,
        sender: RemotePeerId,
        number: usize,
        networks: &Networks,
    ) -> Vec<RemotePeer> {
        let mut rng = rand::thread_rng();
        self.get_all_nodes(Some(sender), networks).into_iter().choose_multiple(&mut rng, number)
    }

    /// Removes the bucket nodes older than then specified amount of time.
    pub fn clean_buckets(&mut self, timeout_bucket_entry_period: u64, on_removal: impl Fn(u64)) {
        let clean_before = get_current_stamp() - timeout_bucket_entry_period;
        self.buckets[0].retain(|entry| {
            if entry.last_seen < clean_before {
                on_removal(0);
            }
            entry.last_seen >= clean_before
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::P2PNodeId;
    use rand::Rng;
    use std::net::{IpAddr, Ipv4Addr, SocketAddr};

    #[test]
    pub fn test_buckets_insert_duplicate_peer_id() {
        let mut buckets = Buckets::default();

        let local_id: RemotePeerId = rand::thread_rng().gen();

        // Create two peers with the same local id but different everything else.
        let p2p_peer = RemotePeer {
            self_id: Some(rand::thread_rng().gen::<P2PNodeId>()),
            addr: SocketAddr::new(IpAddr::V4(Ipv4Addr::LOCALHOST), 8888),
            local_id,
            external_port: 8888,
            peer_type: PeerType::Node,
        };

        let p2p_duplicate_peer = RemotePeer {
            self_id: Some(rand::thread_rng().gen::<P2PNodeId>()),
            addr: SocketAddr::new(IpAddr::V4(Ipv4Addr::LOCALHOST), 8889),
            local_id,
            external_port: 8889,
            peer_type: PeerType::Node,
        };

        // and check that only one is inserted
        buckets.insert_into_bucket(p2p_peer, Default::default(), |_| {});
        buckets.insert_into_bucket(p2p_duplicate_peer, Default::default(), |_| {});
        assert_eq!(buckets.buckets.len(), 1);
    }
}
