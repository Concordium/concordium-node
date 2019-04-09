use num_bigint::{BigUint };
use num_traits::{
    cast::ToPrimitive,
};
use rand::{rngs::OsRng, seq::SliceRandom};
use std::{
    collections::{HashMap, HashSet},
    cmp::Ordering,
    ops::Range,
    sync::RwLock,
};

use crate::common::{ConnectionType, P2PNodeId, P2PPeer};

const KEY_SIZE: usize = 256;
const BUCKET_SIZE: u8 = 20;

pub struct Bucket {
    pub peer:     P2PPeer,
    pub networks: HashSet<u16>,
}

pub struct Buckets {
    buckets: HashMap<u16, Vec<Bucket>>,
}

fn make_distance_table() -> Vec<Range<u64>> {
    let mut dist_table = Vec::with_capacity( KEY_SIZE as usize);
    for i in 0..(KEY_SIZE as usize) {
       dist_table.push( Range {
            start: 2u64.pow(i as u32),
            end:   2u64.pow(i as u32 + 1)
        });
    }

    dist_table
}

lazy_static! {
    static ref RNG: RwLock<OsRng> = { RwLock::new(OsRng::new().unwrap()) };
    static ref DISTANCE_TABLE : Vec<Range<u64>> = make_distance_table();
}

impl Buckets {
    pub fn new() -> Buckets {
        let mut buckets = HashMap::with_capacity(KEY_SIZE);
        for i in 0..KEY_SIZE {
            buckets.insert(i as u16, Vec::new());
        }

        Buckets { buckets }
    }

    pub fn distance(&self, from: &P2PNodeId, to: &P2PNodeId) -> BigUint {
        from.get_id() ^ to.get_id()
    }

    pub fn insert_into_bucket(&mut self, node: &P2PPeer, own_id: P2PNodeId, nids: HashSet<u16>) {
        let index_opt = self.find_bucket_id( own_id, node.id());
        for i in 0..KEY_SIZE as u16 {
            if let Some(bucket_list) = self.buckets.get_mut(&i) {
                bucket_list.retain(|ref ele| ele.peer != *node);

                if let Some(index) = index_opt {
                    if bucket_list.len() >= BUCKET_SIZE as usize {
                        bucket_list.remove(0);
                    }
                    bucket_list.push(Bucket {
                        peer:     node.to_owned(),
                        networks: nids,
                    });
                    break;
                }
            }
        }
    }

    pub fn update_network_ids(&mut self, node: &P2PPeer, network_ids: &HashSet<u16>) {
        for i in 0..KEY_SIZE as u16 {
            match self.buckets.get_mut(&i) {
                Some(x) => {
                    x.retain(|ref ele| ele.peer != *node);
                    x.push( Bucket{ peer: node.to_owned(), networks: network_ids.to_owned()});
                    break;
                }
                None => {
                    error!("Couldn't get buck as mutable");
                }
            }
        }
    }

    fn find_bucket_id(&self, own_id: P2PNodeId, id: P2PNodeId) -> Option<usize> {
        // TODO
        let dist = self.distance(&own_id, &id).to_u64().unwrap_or( 0u64);

        DISTANCE_TABLE.binary_search_by(
            |ref range|{
                if range.contains( &dist) {
                    Ordering::Equal
                } else if range.end < dist {
                    Ordering::Less
                } else {
                    Ordering::Greater
                }
        }).ok()
    }

    pub fn closest_nodes(&self, _id: &P2PNodeId) -> Vec<P2PPeer> {
        let mut ret: Vec<P2PPeer> = Vec::with_capacity(KEY_SIZE as usize);
        let mut count = 0;
        for (_, bucket_list) in &self.buckets {
            // Fix later to do correctly
            if count < KEY_SIZE {
                for bucket in bucket_list {
                    if count < KEY_SIZE {
                        ret.push(bucket.peer.to_owned());
                        count += 1;
                    } else {
                        break;
                    }
                }
            } else {
                break;
            }
        }
        ret
    }

    pub fn clean_peers(&mut self, retain_minimum: usize) {
        let self_len = self.len();
        for i in 0..KEY_SIZE as u16 {
            match self.buckets.get_mut(&i) {
                Some(bucket_list) => {
                    if retain_minimum < bucket_list.len() {
                        debug!("Cleaning buckets currently at {}", self_len);
                        bucket_list.sort_by(|a, b| {
                            use std::cmp::Ordering;
                            if a.peer > b.peer {
                                return Ordering::Less;
                            } else if a.peer < b.peer {
                                return Ordering::Greater;
                            } else {
                                return Ordering::Equal;
                            }
                        });
                        bucket_list.drain(retain_minimum..);
                    }
                }
                None => {
                    error!("Couldn't get bucket as mutable");
                }
            }
        }
    }

    pub fn get_all_nodes(&self, sender: Option<&P2PPeer>, networks: &[u16]) -> Vec<P2PPeer> {
        let mut ret: Vec<P2PPeer> = Vec::new();

        match sender {
            Some(sender_peer) => {
                for (_, bucket_list) in &self.buckets {
                    for bucket in bucket_list {
                        if sender_peer != &bucket.peer
                            && bucket.peer.connection_type() == ConnectionType::Node
                            && (networks.is_empty() || networks.iter().any( |net_id| bucket.networks.contains(net_id)))
                        {
                            ret.push(bucket.peer.to_owned());
                        }
                    }
                }
            }
            None => {
                for (_, bucket_list) in &self.buckets {
                    for bucket in bucket_list {
                        if bucket.peer.connection_type() == ConnectionType::Node
                            && (networks.is_empty() || networks.iter().any( |net_id| bucket.networks.contains(net_id)))
                        {
                            ret.push(bucket.peer.to_owned());
                        }
                    }
                }
            }
        }

        ret
    }

    pub fn len(&self) -> usize { self.buckets.iter().map(|(_, y)| y.len()).sum() }

    pub fn get_random_nodes(&self, sender: &P2PPeer, amount: usize, nids: &[u16]) -> Vec<P2PPeer> {
        match safe_write!(RNG) {
            Ok(ref mut rng) => self
                .get_all_nodes(Some(sender), nids)
                .choose_multiple(&mut **rng, amount)
                .cloned()
                .collect(),
            _ => vec![],
        }
    }
}
