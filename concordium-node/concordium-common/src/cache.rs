use circular_queue::CircularQueue;
use failure::Fallible;
use rkv::{Rkv, SingleStore, Value};

use std::{
    collections::HashMap,
    marker::PhantomData,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::{HashBytes, SerializeToBytes};

const DEFAULT_CACHE_SIZE: usize = 128;

struct QueueEntry {
    pub hash:  HashBytes,
    pub since: u64,
}

/// A temporary cache
pub struct Cache<T> {
    hash_map: HashMap<HashBytes, T>,
    queue:    CircularQueue<QueueEntry>,
}

impl<T> Default for Cache<T> {
    fn default() -> Self {
        Self {
            hash_map: HashMap::new(),
            queue:    CircularQueue::with_capacity(DEFAULT_CACHE_SIZE),
        }
    }
}

impl<T> Cache<T> {
    /// Inserts an entry in the cache
    ///
    /// If the cache has reached the limit of size, it will pop the oldest
    /// element out of the cache and return it
    pub fn insert(&mut self, hash: HashBytes, elem: T) -> Option<T> {
        if self.hash_map.insert(hash.clone(), elem).is_some() {
            None // do nothing when a duplicate is encountered
        } else {
            let timestamp = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .expect("Time went backwards")
                .as_secs();

            let mut ret = None;

            if self.queue.len() == self.queue.capacity() {
                if let Some(entry) = self.queue.iter().next_back() {
                    ret = self.hash_map.remove(&entry.hash);
                };
            }

            self.queue.push(QueueEntry {
                hash,
                since: timestamp,
            });

            ret
        }
    }

    /// Get all the entries since a specific point in time
    pub fn get_since(&self, queried_since: u64) -> Vec<&T> {
        self.queue
            .iter()
            .skip_while(|elem| elem.since < queried_since)
            .filter_map(|elem| self.hash_map.get(&elem.hash))
            .collect()
    }
}

/// A cache dumping overflowing entries as blobs to a key-value store.
pub struct DiskCache<'a, 'b, T: SerializeToBytes<'a, 'b>> {
    store:   SingleStore,
    cache:   Cache<T>,
    phantom: PhantomData<(&'a T, &'b T)>,
}

impl<'a, 'b, T: SerializeToBytes<'a, 'b>> DiskCache<'a, 'b, T> {
    pub fn insert(&mut self, store_handle: &Rkv, hash: HashBytes, elem: T) -> Fallible<()> {
        if let Some(entry) = self.cache.insert(hash.clone(), elem) {
            let mut store_writer = store_handle.write().expect("Can't write to the store!");

            Ok(self
                .store
                .put(&mut store_writer, hash, &Value::Blob(&entry.serialize()))?)
        } else {
            Ok(())
        }
    }
}
