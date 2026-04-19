//! Representation of an immutable value that is stored in the blob store.
//!
//! See [`HashedCacheableRef`].

/// Representation of an immutable, cacheable and lazily hashed value of type `V`.
///
/// See [`StoredRef`].
use crate::block_state::blob_store;
use crate::block_state::blob_store::{
    BlobStoreLoad, BlobStoreLocation, BlobStoreStore, Loadable, ParseResultExt, Storable,
};

use crate::block_state_interface::BlockStateResult;
use concordium_base::common::{Buffer, Get, Put};
use std::io::Read;
use std::marker::PhantomData;

/// Representation of an immutable value of type `V`, that is stored in the blob
/// store and not represented in memory.
#[derive(Debug)]
pub struct StoredRef<V> {
    /// Location in the blob store the value is stored at.
    blob_location: BlobStoreLocation,
    _phantom_data: PhantomData<V>,
}

impl<V> StoredRef<V> {
    /// Access the referenced value. If the value is already in memory, the value
    /// is returned as borrowed. If it is not in memory, it is loaded from the
    /// blob store, and passed owned to the closure as owned.
    ///
    /// Loading from the blob store will not make the value cached in the reference.
    ///
    /// # Errors
    ///
    /// Returns [`BlockStateError`] if decoding data from the blob store fails.
    pub fn value(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<V>
    where
        V: Loadable,
    {
        blob_store::load_from_store(loader, self.blob_location)
    }
}

/// Implement [`Clone`] explicitly, such that clonability does not depend on
/// if `V` is clonable.
impl<V> Clone for StoredRef<V> {
    fn clone(&self) -> Self {
        Self {
            blob_location: self.blob_location,
            _phantom_data: PhantomData,
        }
    }
}

impl<V> Loadable for StoredRef<V> {
    fn load_from_buffer(
        mut buffer: impl Read,
        _loader: &impl BlobStoreLoad,
    ) -> BlockStateResult<Self> {
        let blob_location: BlobStoreLocation = buffer.get().map_parse_err_to_block_state_err()?;

        Ok(Self {
            blob_location,
            _phantom_data: PhantomData,
        })
    }
}

impl<V> Storable for StoredRef<V> {
    fn store_to_buffer(&self, mut buffer: impl Buffer, _storer: &mut impl BlobStoreStore) {
        buffer.put(self.blob_location);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_state::blob_reference::hashed_cacheable_reference::HashedCacheableRef;
    use crate::block_state::blob_store;
    use crate::block_state::blob_store::StoreSerialized;
    use crate::block_state::blob_store::test_stub::BlobStoreStub;

    type TestHashedCacheableRef = HashedCacheableRef<StoreSerialized<u64>>;
    type TestRef = StoredRef<StoreSerialized<u64>>;

    /// Test load and get value.
    #[test]
    fn test_load_and_get_value() {
        let mut store = BlobStoreStub::default();

        // Save value to store
        let val1 = TestHashedCacheableRef::new(StoreSerialized(1u64));
        let blob_ref = blob_store::store_to_store(&mut store, &val1);
        drop(val1);

        // Load the value ref
        let val2: TestRef = blob_store::load_from_store(&store, blob_ref).unwrap();

        // Get value
        assert_eq!(val2.value(&store).unwrap(), StoreSerialized(1u64));
    }

    /// Test load and store.
    #[test]
    fn test_load_and_store() {
        let mut store = BlobStoreStub::default();

        // Save value to store
        let val1 = TestHashedCacheableRef::new(StoreSerialized(1u64));
        let blob_ref = blob_store::store_to_store(&mut store, &val1);
        drop(val1);

        // Load the value ref
        let val2: TestRef = blob_store::load_from_store(&store, blob_ref).unwrap();

        // Store and load again
        let blob_ref2 = blob_store::store_to_store(&mut store, &val2);
        let val3: TestRef = blob_store::load_from_store(&store, blob_ref2).unwrap();
        assert_eq!(val3.value(&store).unwrap(), StoreSerialized(1u64));
    }
}
