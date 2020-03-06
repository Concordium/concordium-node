//! Network bucket handling.

use rand::seq::IteratorRandom;
use std::{
    collections::HashSet,
    hash::{Hash, Hasher},
};

use crate::{
    common::{get_current_stamp, P2PPeer, PeerType},
    network::NetworkId,
};

const BUCKET_COUNT: usize = 1;

/// A representation of a node in a bucket.
#[derive(Eq, Clone)]
pub struct Node {
    pub peer: P2PPeer,
    pub networks: HashSet<NetworkId>,
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
    pub fn insert_into_bucket(&mut self, peer: P2PPeer, networks: HashSet<NetworkId>) {
        let bucket = &mut self.buckets[0];

        bucket.insert(Node {
            peer,
            networks,
            last_seen: get_current_stamp(),
        });
    }

    /// Update the networks of a node in the bucket.
    pub fn update_network_ids(&mut self, peer: P2PPeer, networks: HashSet<NetworkId>) {
        let bucket = &mut self.buckets[0];

        bucket.replace(Node {
            peer,
            networks,
            last_seen: get_current_stamp(),
        });
    }

    /// Returns all the nodes in buckets.
    pub fn get_all_nodes(
        &self,
        sender: Option<&P2PPeer>,
        networks: &HashSet<NetworkId>,
    ) -> Vec<P2PPeer> {
        let mut nodes = Vec::new();
        let filter_criteria = |node: &&Node| {
            node.peer.peer_type == PeerType::Node
                && if let Some(sender) = sender {
                    node.peer != *sender
                } else {
                    true
                }
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
        sender: &P2PPeer,
        number: usize,
        networks: &HashSet<NetworkId>,
        partition: bool,
    ) -> Vec<P2PPeer> {
        let mut rng = rand::thread_rng();
        if partition {
            self.get_all_nodes(Some(sender), networks)
                .into_iter()
                .filter(|peer| {
                    if sender.id.0 % 2 == 0 {
                        peer.id.0 % 2 == 0
                    } else {
                        peer.id.0 % 2 != 0
                    }
                })
                .choose_multiple(&mut rng, number)
        } else {
            self.get_all_nodes(Some(sender), networks).into_iter().choose_multiple(&mut rng, number)
        }
    }

    /// Removes the bucket nodes older than then specified amount of time.
    pub fn clean_buckets(&mut self, timeout_bucket_entry_period: u64) {
        let clean_before = get_current_stamp() - timeout_bucket_entry_period;
        self.buckets[0].retain(|entry| entry.last_seen >= clean_before);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::P2PNodeId;
    use std::{
        collections::HashSet,
        net::{IpAddr, Ipv4Addr, SocketAddr},
    };

    #[test]
    pub fn test_buckets_insert_duplicate_peer_id() {
        let mut buckets = Buckets::default();

        let p2p_node_id = P2PNodeId::default();

        let p2p_peer = P2PPeer::from((
            PeerType::Node,
            p2p_node_id,
            SocketAddr::new(IpAddr::V4(Ipv4Addr::LOCALHOST), 8888),
        ));
        let p2p_duplicate_peer = P2PPeer::from((
            PeerType::Node,
            p2p_node_id,
            SocketAddr::new(IpAddr::V4(Ipv4Addr::LOCALHOST), 8889),
        ));
        buckets.insert_into_bucket(p2p_peer, HashSet::new());
        buckets.insert_into_bucket(p2p_duplicate_peer, HashSet::new());
        assert_eq!(buckets.buckets.len(), 1);
    }
}
