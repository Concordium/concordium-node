use rand::{rngs::OsRng, seq::IteratorRandom};
use std::{collections::HashSet, sync::RwLock};

use crate::common::{ConnectionType, P2PPeer};

const BUCKET_COUNT: usize = 1;

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct Node {
    pub peer:     P2PPeer,
    pub networks: Vec<u16>,
}

pub type Bucket = HashSet<Node>;

pub struct Buckets(pub Vec<Bucket>);

lazy_static! {
    static ref RNG: RwLock<OsRng> = { RwLock::new(OsRng::new().unwrap()) };
}

impl Buckets {
    pub fn new() -> Buckets { Buckets(vec![HashSet::new(); BUCKET_COUNT]) }

    pub fn insert_into_bucket(&mut self, peer: &P2PPeer, networks: Vec<u16>) {
        let bucket = &mut self.0[0];

        bucket.insert(Node {
            peer: peer.to_owned(),
            networks,
        });
    }

    pub fn update_network_ids(&mut self, peer: &P2PPeer, networks: Vec<u16>) {
        let bucket = &mut self.0[0];

        bucket.replace(Node {
            peer: peer.to_owned(),
            networks,
        });
    }

    pub fn get_all_nodes(&self, sender: Option<&P2PPeer>, networks: &[u16]) -> Vec<P2PPeer> {
        let mut nodes = Vec::new();
        let filter_criteria = |node: &&Node| {
            node.peer.connection_type() == ConnectionType::Node
                && if let Some(sender) = sender {
                    node.peer != *sender
                } else {
                    true
                }
                && (networks.is_empty() || node.networks.iter().any(|net| networks.contains(net)))
        };

        for bucket in &self.0 {
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
        self.0
            .iter()
            .flat_map(|bucket| bucket.iter())
            .map(|node| node.networks.len())
            .sum()
    }

    pub fn get_random_nodes(
        &self,
        sender: &P2PPeer,
        amount: usize,
        networks: &[u16],
    ) -> Vec<P2PPeer> {
        match safe_write!(RNG) {
            Ok(ref mut rng) => self
                .get_all_nodes(Some(sender), networks)
                .into_iter()
                .choose_multiple(&mut **rng, amount),
            _ => vec![],
        }
    }
}
