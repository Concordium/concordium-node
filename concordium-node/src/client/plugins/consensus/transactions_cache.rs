use concordium_global_state::{
    common::{HashBytes, SerializeToBytes},
    transaction::Transaction,
};
use std::{
    collections::{HashMap, VecDeque},
    time::{SystemTime, UNIX_EPOCH},
};

/// Maximum number of transactions that are going to be stored in the cache
static MAX_CACHED_TXS: usize = 100;

struct QueueEntry {
    pub hash:  HashBytes,
    pub since: u64,
}

#[derive(Default)]
/// Cache of seen transactions
pub struct TransactionsCache<'a> {
    hash_map: HashMap<HashBytes, &'a Transaction>,
    queue:    VecDeque<QueueEntry>,
}

impl<'a, 'b: 'a> TransactionsCache<'a> {
    pub fn new() -> Self {
        TransactionsCache {
            hash_map: HashMap::new(),
            queue:    VecDeque::new(),
        }
    }

    /// Inserts a transaction in the cache
    ///
    /// If the cache has reached the limit of size, it will pop the oldest
    /// element out of the cache
    pub fn insert(&mut self, elem: &'b Transaction) -> bool {
        let hash = HashBytes::new(&elem.serialize());
        if self.hash_map.insert(hash.clone(), elem).is_some() {
            false
        } else {
            let start = SystemTime::now();
            let since_the_epoch = start
                .duration_since(UNIX_EPOCH)
                .expect("Time went backwards");
            self.queue.push_back(QueueEntry {
                hash,
                since: since_the_epoch.as_secs(),
            });
            if self.queue.len() >= MAX_CACHED_TXS {
                self.queue
                    .pop_front()
                    .as_ref()
                    .and_then(|elem| self.hash_map.remove(&elem.hash));
            }
            true
        }
    }

    /// Get all the transactions since a specific point in time
    pub fn get_since(&self, queried_since: u64) -> Vec<&Transaction> {
        self.queue
            .iter()
            .skip_while(|elem| elem.since < queried_since)
            .map(|elem| self.hash_map.get(&elem.hash))
            .flat_map(|elem| elem.into_iter())
            .cloned()
            .collect()
    }
}
