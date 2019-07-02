use circular_queue::CircularQueue;
use std::{
    collections::HashMap,
    time::{SystemTime, UNIX_EPOCH},
};
use crate::HashBytes;

const DEFAULT_CACHE_SIZE: usize = 128;

struct QueueEntry {
    pub hash:  HashBytes,
    pub since: u64,
}

/// Cache of seen transactions
pub struct Cache<'a, T> {
    hash_map: HashMap<HashBytes, &'a T>,
    queue:    CircularQueue<QueueEntry>,
}

impl<T> Default for Cache<'_, T> {
    fn default() -> Self {
        Self {
            hash_map: HashMap::new(),
            queue:    CircularQueue::with_capacity(DEFAULT_CACHE_SIZE),
        }
    }
}

impl<'a, T> Cache<'a, T> {
    /// Inserts a transaction in the cache
    ///
    /// If the cache has reached the limit of size, it will pop the oldest
    /// element out of the cache
    pub fn insert(&mut self, hash: HashBytes, elem: &'a T) -> bool {
        if self.hash_map.insert(hash.clone(), elem).is_some() {
            false // do nothing when a duplicate is encountered
        } else {
            let timestamp = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .expect("Time went backwards")
                .as_secs();

            if self.queue.len() == self.queue.capacity() {
                if let Some(entry) = self.queue.iter().next_back() {
                    self.hash_map.remove(&entry.hash);
                };
            }

            self.queue.push(QueueEntry {
                hash,
                since: timestamp,
            });

            true
        }
    }

    /// Get all the transactions since a specific point in time
    pub fn get_since(&self, queried_since: u64) -> Vec<&T> {
        self.queue
            .iter()
            .skip_while(|elem| elem.since < queried_since)
            .filter_map(|elem| self.hash_map.get(&elem.hash))
            .cloned()
            .collect()
    }
}
