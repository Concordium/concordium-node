use crate::failure::{BlockStateFailure, BlockStateResult};
use crate::persistent::blob_store::{BlobStoreLoad, BlobStoreStore, Loadable, Storable};
use crate::persistent::cacheable::Cacheable;
use crate::persistent::hash;
use crate::persistent::hash::Hashable;
use concordium_base::common::{Buffer, Get, Put};
use concordium_base::hashes::Hash;
use std::io::Read;

/// Node-owned persistent external chain parameters for P11.
///
/// This mirrors the parts of the public chain-parameter view whose authoritative
/// state is managed outside the ordinary Haskell chain-parameter record.
#[derive(Debug, Clone, Default, Eq, PartialEq)]
pub struct PersistentChainParametersP11 {
    /// Maximum relative duration for protocol-level token locks, in milliseconds.
    pub max_lock_duration: u64,
}

impl Loadable for PersistentChainParametersP11 {
    fn load_from_buffer(
        mut buffer: impl Read,
        _loader: &impl BlobStoreLoad,
    ) -> Result<Self, BlockStateFailure> {
        let max_lock_duration = buffer.get().map_err(|err| {
            BlockStateFailure::BlobStoreDecode(format!(
                "Error parsing P11 chain-parameters max_lock_duration: {err}"
            ))
        })?;
        Ok(Self { max_lock_duration })
    }
}

impl Storable for PersistentChainParametersP11 {
    fn store_to_buffer(&self, mut buffer: impl Buffer, _storer: &mut impl BlobStoreStore) {
        buffer.put(self.max_lock_duration);
    }
}

impl Cacheable for PersistentChainParametersP11 {
    fn cache_reference_values(&self, _loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        Ok(())
    }
}

impl Hashable for PersistentChainParametersP11 {
    fn hash(&self, _loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        Ok(hash::hash_of_serialization(self.max_lock_duration))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::persistent::blob_store;
    use crate::persistent::blob_store::test_stub::BlobStoreStub;

    #[test]
    fn store_load_roundtrip() {
        let mut store = BlobStoreStub::default();
        let params = PersistentChainParametersP11 {
            max_lock_duration: 42,
        };
        let location = blob_store::store_to_store(&mut store, &params);
        let loaded: PersistentChainParametersP11 = blob_store::load_from_store(&store, location)
            .expect("P11 chain parameters should load");
        assert_eq!(params, loaded);
    }

    #[test]
    fn hash_changes_with_max_lock_duration() {
        let store = BlobStoreStub::default();
        let zero = PersistentChainParametersP11::default();
        let non_zero = PersistentChainParametersP11 {
            max_lock_duration: 42,
        };
        assert_ne!(
            zero.hash(&store).expect("zero hash"),
            non_zero.hash(&store).expect("non-zero hash")
        );
    }
}
