use super::fails;
use concordium_consensus::common::SerializeToBytes;
use failure::{bail, Fallible};
use std::{
    cmp::Eq,
    collections::HashSet,
    fmt::Debug,
    hash::Hash,
    sync::{Arc, RwLock},
};

#[derive(Debug, Clone)]
pub struct SeenTransmissionsList<T>
where
    T: Eq + Debug + Hash + SerializeToBytes, {
    seen_transmissions:         Arc<RwLock<HashSet<T>>>,
    max_elements:               u64,
    max_size_bytes_per_element: u64,
}

impl<T> SeenTransmissionsList<T>
where
    T: Eq + Debug + Hash + SerializeToBytes,
{
    pub fn new(max_elements: u64, max_size_bytes_per_element: u64) -> Self {
        Self {
            seen_transmissions: Arc::new(RwLock::new(HashSet::new())),
            max_elements,
            max_size_bytes_per_element,
        }
    }

    fn append(&self, payload: T) -> Fallible<bool> {
        let mut list = safe_write!(self.seen_transmissions)?;
        if !list.contains(&payload) {
            if self.max_elements != 0 && list.len() >= self.max_elements as usize {
                // TODO: Manage cache length
                Ok(list.insert(payload))
            } else {
                Ok(list.insert(payload))
            }
        } else {
            bail!(fails::DuplicateSeenTransmissionElementAttempted::new(
                format!("{:?}", payload)
            ))
        }
    }

    pub fn add_transmission(
        &self,
        // TODO : ignore this field for now
        _seen_at: u64,
        payload: T,
    ) -> Fallible<bool> {
        self.append(payload)
    }

    pub fn get_transmissions_since(
        &self,
        // TODO : ignore this for now, but implemented later
        _since_timestamp: u64,
    ) -> Fallible<Vec<Box<[u8]>>> {
        if _since_timestamp == 0 {
            Ok(safe_read!(self.seen_transmissions)?
                .iter()
                .map(|element| element.serialize())
                .collect())
        } else {
            Ok(safe_read!(self.seen_transmissions)?
                .iter()
                .map(|element| element.serialize())
                .collect())
        }
    }
}
