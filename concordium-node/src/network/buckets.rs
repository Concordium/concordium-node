use rand::{ Rng, thread_rng };
use num_traits::{ pow };
use num_bigint::{ BigUint, ToBigUint };
use std::collections::{ HashMap };

use common::{ P2PPeer, ConnectionType, P2PNodeId };

const KEY_SIZE: u16 = 256;
const BUCKET_SIZE: u8 = 20;

pub struct Buckets {
    buckets: HashMap<u16, Vec<(P2PPeer, Vec<u16>)>>,
}

impl Buckets {
    pub fn new() -> Buckets {
        let mut buckets = HashMap::new();
        for i in 0..KEY_SIZE {
            buckets.insert(i, Vec::new());
        }

        Buckets { buckets }
    }

    pub fn distance(&self, from: &P2PNodeId, to: &P2PNodeId) -> BigUint {
        from.get_id().clone() ^ to.get_id().clone()
    }

    pub fn insert_into_bucket(&mut self, node: &P2PPeer, own_id: &P2PNodeId, nids: Vec<u16>) {
        let dist = self.distance(&own_id, &node.id());
        for i in 0..KEY_SIZE {
            match self.buckets.get_mut(&i) {
                Some(x) => {
                    x.retain(|ref ele| ele.0 != *node);
                }
                _ => {}
            }
            if dist >= pow(2_i8.to_biguint().unwrap(), i as usize)
               && dist < pow(2_i8.to_biguint().unwrap(), (i as usize) + 1)
            {
                match self.buckets.get_mut(&i) {
                    Some(x) => {
                        if x.len() >= BUCKET_SIZE as usize {
                            x.remove(0);
                        }
                        x.push((node.clone(), nids.clone()));
                        break;
                    }
                    None => {
                        error!("Couldn't get bucket as mutable");
                    }
                }
            }
        }
    }

    pub fn update_network_ids(&mut self, node: &P2PPeer, nids: Vec<u16>) {
        for i in 0..KEY_SIZE {
            match self.buckets.get_mut(&i) {
                Some(x) => {
                    x.retain(|ref ele| ele.0 != *node);
                    x.push((node.clone(), nids.clone()));
                    break;
                }
                None => {
                    error!("Couldn't get buck as mutable");
                }
            }
        }
    }

    fn _find_bucket_id(&mut self, own_id: P2PNodeId, id: P2PNodeId) -> Option<u16> {
        let dist = self.distance(&own_id, &id);
        let mut ret: i32 = -1;
        for i in 0..KEY_SIZE {
            if dist >= pow(2_i8.to_biguint().unwrap(), i as usize)
               && dist < pow(2_i8.to_biguint().unwrap(), (i as usize) + 1)
            {
                ret = i as i32;
            }
        }

        if ret == -1 {
            None
        } else {
            Some(ret as u16)
        }
    }

    pub fn closest_nodes(&self, _id: &P2PNodeId) -> Vec<P2PPeer> {
        let mut ret: Vec<P2PPeer> = Vec::with_capacity(KEY_SIZE as usize);
        let mut count = 0;
        for (_, bucket) in &self.buckets {
            //Fix later to do correctly
            if count < KEY_SIZE {
                for peer in bucket {
                    if count < KEY_SIZE {
                        ret.push(peer.0.clone());
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
        for i in 0..KEY_SIZE {
            match self.buckets.get_mut(&i) {
                Some(x) => {
                    if retain_minimum < x.len() {
                        debug!("Cleaning buckets currently at {}", self_len);
                        x.sort_by(|a, b| {
                                      use std::cmp::Ordering;
                                      if a > b {
                                          return Ordering::Less;
                                      } else if a < b {
                                          return Ordering::Greater;
                                      } else {
                                          return Ordering::Equal;
                                      }
                                  });
                        x.drain(retain_minimum..);
                    }
                }
                None => {
                    error!("Couldn't get bucket as mutable");
                }
            }
        }
    }

    pub fn get_all_nodes(&self, sender: Option<&P2PPeer>, networks: &Vec<u16>) -> Vec<P2PPeer> {
        let mut ret: Vec<P2PPeer> = Vec::new();
        match sender {
            Some(sender_peer) => {
                for (_, bucket) in &self.buckets {
                    for peer in bucket {
                        if sender_peer != &peer.0
                           && peer.0.connection_type() == ConnectionType::Node
                           && (networks.len() == 0 || peer.1.iter().any(|x| networks.contains(x)))
                        {
                            ret.push(peer.0.clone());
                        }
                    }
                }
            }
            None => {
                for (_, bucket) in &self.buckets {
                    for peer in bucket {
                        if peer.0.connection_type() == ConnectionType::Node
                           && (networks.len() == 0 || peer.1.iter().any(|x| networks.contains(x)))
                        {
                            ret.push(peer.0.clone());
                        }
                    }
                }
            }
        }

        ret
    }

    pub fn len(&self) -> usize {
        self.buckets.iter().map(|(_, y)| y.len() as usize).sum()
    }

    pub fn get_random_nodes(&self,
                            sender: &P2PPeer,
                            amount: usize,
                            nids: &Vec<u16>)
                            -> Vec<P2PPeer> {
        let mut ret: Vec<P2PPeer> = self.get_all_nodes(Some(sender), nids);
        thread_rng().shuffle(&mut ret);
        ret.truncate(amount);
        ret
    }
}
