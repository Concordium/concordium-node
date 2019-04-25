use rand::{rngs::OsRng, seq::IteratorRandom};
use std::{
    collections::HashSet,
    hash::{Hash, Hasher},
    sync::RwLock,
};

use crate::{
    common::{P2PPeer, PeerType},
    network::NetworkId,
};

const BUCKET_COUNT: usize = 1;

#[derive(Eq, Clone)]
pub struct Node {
    pub peer:     P2PPeer,
    pub networks: HashSet<NetworkId>,
}

impl PartialEq for Node {
    fn eq(&self, other: &Node) -> bool { self.peer == other.peer }
}

impl Hash for Node {
    fn hash<H: Hasher>(&self, state: &mut H) { self.peer.hash(state) }
}

pub type Bucket = HashSet<Node>;
pub struct Buckets {
    pub buckets: Vec<Bucket>,
}

lazy_static! {
    static ref RNG: RwLock<OsRng> = { RwLock::new(OsRng::new().unwrap()) };
}

impl Default for Buckets {
    fn default() -> Self { Buckets::new() }
}

impl Buckets {
    pub fn new() -> Buckets {
        Buckets {
            buckets: vec![HashSet::new(); BUCKET_COUNT],
        }
    }

    pub fn insert_into_bucket(&mut self, peer: &P2PPeer, networks: HashSet<NetworkId>) {
        let bucket = &mut self.buckets[0];

        bucket.insert(Node {
            peer: peer.to_owned(),
            networks,
        });
    }

    pub fn update_network_ids(&mut self, peer: &P2PPeer, networks: HashSet<NetworkId>) {
        let bucket = &mut self.buckets[0];

        bucket.replace(Node {
            peer: peer.to_owned(),
            networks,
        });
    }

    pub fn get_all_nodes(
        &self,
        sender: Option<&P2PPeer>,
        networks: &HashSet<NetworkId>,
    ) -> Vec<P2PPeer> {
        let mut nodes = Vec::new();
        let filter_criteria = |node: &&Node| {
            node.peer.peer_type() == PeerType::Node
                && if let Some(sender) = sender {
                    node.peer != *sender
                } else {
                    true
                }
                && (networks.is_empty() || !node.networks.is_disjoint(networks))
        };

        for bucket in &self.buckets {
            nodes.extend(
                bucket
                    .iter()
                    .filter(filter_criteria)
                    .map(|node| node.peer.to_owned()),
            )
        }

        nodes
    }

    pub fn len(&self) -> usize {
        self.buckets
            .iter()
            .flat_map(HashSet::iter)
            .map(|node| node.networks.len())
            .sum()
    }

    pub fn is_empty(&self) -> bool { self.len() == 0 }

    pub fn get_random_nodes(
        &self,
        sender: &P2PPeer,
        amount: usize,
        networks: &HashSet<NetworkId>,
    ) -> Vec<P2PPeer> {
        if let Ok(ref mut rng) = safe_write!(RNG) {
            self.get_all_nodes(Some(sender), networks)
                .into_iter()
                .choose_multiple(&mut **rng, amount)
        } else {
            vec![]
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::P2PNodeId;
    use std::{
        collections::HashSet,
        net::{IpAddr, SocketAddr},
        str::FromStr,
    };

    #[test]
    pub fn test_buckets_insert_duplicate_peer_id() {
        let mut buckets = Buckets::new();

        let p2p_node_id = P2PNodeId::default();

        let p2p_peer = P2PPeer::from(
            PeerType::Node,
            p2p_node_id,
            SocketAddr::new(IpAddr::from_str("127.0.0.1").unwrap(), 8888),
        );
        let p2p_duplicate_peer = P2PPeer::from(
            PeerType::Node,
            p2p_node_id,
            SocketAddr::new(IpAddr::from_str("127.0.0.1").unwrap(), 8889),
        );
        buckets.insert_into_bucket(&p2p_peer, HashSet::new());
        buckets.insert_into_bucket(&p2p_duplicate_peer, HashSet::new());
        assert_eq!(buckets.buckets.len(), 1);
    }
}
