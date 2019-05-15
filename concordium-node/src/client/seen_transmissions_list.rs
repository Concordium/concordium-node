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
    T: Eq + Debug + Hash + SerializeToBytes + Clone, {
    seen_transmissions:         Arc<RwLock<HashSet<T>>>,
    max_elements:               u64,
    max_size_bytes_per_element: u64,
}

impl<T> SeenTransmissionsList<T>
where
    T: Eq + Debug + Hash + SerializeToBytes + Clone,
{
    pub fn new(max_elements: u64, max_size_bytes_per_element: u64) -> Self {
        Self {
            seen_transmissions: Arc::new(RwLock::new(HashSet::new())),
            max_elements,
            max_size_bytes_per_element,
        }
    }

    pub fn add_transmission(
        &self,
        // TODO : ignore this field for now
        _seen_at: u64,
        payload: &T,
    ) -> Fallible<bool> {
        let mut list = safe_write!(self.seen_transmissions)?;
        // TODO: Manage cache length
        if !list.insert(payload.to_owned()) {
            bail!(fails::DuplicateSeenTransmissionElementAttempted::new(
                format!("{:?}", payload)
            ))
        }
        Ok(true)
    }

    pub fn get_transmissions_since(
        &self,
        // TODO : ignore this for now, but implemented later
        _since_timestamp: u64,
    ) -> Fallible<Vec<Box<[u8]>>> {
        Ok(safe_read!(self.seen_transmissions)?
            .iter()
            .map(SerializeToBytes::serialize)
            .collect())
    }
}
