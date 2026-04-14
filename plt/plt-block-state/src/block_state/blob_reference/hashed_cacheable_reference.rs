/// Representation of an immutable, cacheable and lazily hashed value of type `V`.
///
/// See [`HashedCacheableRef`].
use crate::block_state::blob_store;
use crate::block_state::blob_store::{
    BlobStoreLoad, BlobStoreLocation, BlobStoreStore, Loadable, ParseResultExt, Storable,
};
use crate::block_state::cacheable::Cacheable;
use crate::block_state::hash::Hashable;
use crate::block_state::utils::OwnedOrBorrowed;
use crate::block_state_interface::BlockStateResult;
use concordium_base::common::{Buffer, Get, Put};
use concordium_base::hashes::Hash;
use std::io::Read;
use std::sync::{Arc, OnceLock};

/// Representation of an immutable, cachable and lazily hashed value of type `V`.
/// The represented value is immutable in the sense that the value itself does not change,
/// once the [`HashedCacheableRef`] has been created. The value representation can be in
///
/// * memory: initial representation for a new value created with [`HashedCacheableRef::new`]
/// * blob store: initial representation for a value loaded
///   from the blob store with [`Loadable::load_from_buffer`]
/// * cached: representation where the value is both in the blob store and in memory
///
/// The cached representation is the result of either storing a value represented in
/// memory with [`Storable::store_to_buffer`] or caching a value in the blob store
/// with [`Cacheable::cache_reference_values`].
///
/// ## Interior mutability
///
/// The representation change during the lifetime of [`HashedCacheableRef`] is implemented
/// via interior mutability, but the represented value itself never changes during the lifetime.
///
/// ## Hashing
///
/// The hash of the represented value is calculated lazily when needed, and cached
/// via interior mutability.
#[derive(Debug)]
pub struct HashedCacheableRef<V> {
    /// The representation is wrapped in a [`Arc`] to allow cheap cloning and
    /// interior mutability of a shared instance.
    inner: Arc<HashedBufferedRefInner<V>>,
}

impl<V: Default> Default for HashedCacheableRef<V> {
    fn default() -> Self {
        Self::new(Default::default())
    }
}

impl<V> HashedCacheableRef<V> {
    /// Create a new value represented in memory.
    pub fn new(value: V) -> Self {
        let inner = HashedBufferedRefInner {
            hash: OnceLock::new(),
            blob_location: OnceLock::new(),
            value: OnceLock::from(value),
        };

        Self {
            inner: Arc::new(inner),
        }
    }

    /// Access the referenced value. If the value is already in memory, the value
    /// is returned as borrowed. If it is not in memory, it is loaded from the
    /// blob store, and passed owned to the closure as owned.
    ///
    /// Loading from the blob store will not make the value cached in the reference.
    ///
    /// # Errors
    ///
    /// Returns [`BlockStateError`] if decoding data from the blob store fails.
    pub fn value(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<OwnedOrBorrowed<'_, V>>
    where
        V: Loadable,
    {
        self.inner.get_or_load_value(loader)
    }
}

/// Implement [`Clone`] directly, such that clonability does not depend on
/// if `V` is clonable.
impl<V> Clone for HashedCacheableRef<V> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

/// The [`HashedCacheableRef`] behind the `Arc`.
///
/// Invariant: At least one of `value` and `blob_location` are always set. Depending on the "state":
/// * memory: only `value` set
/// * blob store: only `blob_location` set
/// * cached: `value` and `blob_location` both set
#[derive(Debug)]
struct HashedBufferedRefInner<V> {
    /// In-memory value. Is set if the value is currently represented in memory.
    value: OnceLock<V>,
    /// Location of the value in the blob store. Is set if the value is stored in the blob store.
    blob_location: OnceLock<BlobStoreLocation>,
    /// Lazily calculated hash. If set, it is the hash of `value`.
    hash: OnceLock<Hash>,
}

impl<V> HashedBufferedRefInner<V> {
    /// Load the referenced value and return it. If the value is already in memory, a reference
    /// to it is simply returned. If it is not in memory, it is loaded from the blob store,
    /// and returned as owned.
    ///
    /// Loading from the blob store will not make the value cached in the reference.
    fn get_or_load_value(
        &self,
        loader: &impl BlobStoreLoad,
    ) -> BlockStateResult<OwnedOrBorrowed<'_, V>>
    where
        V: Loadable,
    {
        Ok(match self.value.get() {
            Some(value) => OwnedOrBorrowed::Borrowed(value),
            None => {
                let blob_location = self
                    .blob_location
                    .get()
                    .expect("Neither value nor blob_location set in HashedBufferedRefInner");
                let value: V = blob_store::load_from_store(loader, *blob_location)?;
                OwnedOrBorrowed::Owned(value)
            }
        })
    }

    /// Cache the referenced value and return it. If the value is already in memory, a reference
    /// to it is simply returned. If it is not in memory, it is loaded from the blob store and
    /// cached in [`HashedCacheableRefRepr::Cache`] first.
    fn get_or_cache_value(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<&V>
    where
        V: Loadable,
    {
        Ok(match self.value.get() {
            Some(value) => value,
            None => {
                let blob_location = self
                    .blob_location
                    .get()
                    .expect("Neither value nor blob_location set in HashedBufferedRefInner");
                let value: V = blob_store::load_from_store(loader, *blob_location)?;
                // todo ar get_or_try_init
                self.value.get_or_init(|| value)
            }
        })
    }

    /// Store the value and return its [`BlobStoreLocation`]. If the value is already stored in
    /// the blob store, the [`BlobStoreLocation`] for it is simply returned. If it is not stored,
    /// it is stored into the blob store, and the resulting [`BlobStoreLocation`] is saved in
    /// [`HashedCacheableRefRepr::Cache`] and returned.
    fn get_reference_or_store(&self, storer: &mut impl BlobStoreStore) -> BlobStoreLocation
    where
        V: Storable,
    {
        match self.blob_location.get() {
            Some(blob_location) => *blob_location,
            None => {
                let value = self
                    .value
                    .get()
                    .expect("Neither value nor blob_location set in HashedBufferedRefInner");
                // todo ar get_or_try_init
                *self
                    .blob_location
                    .get_or_init(|| blob_store::store_to_store(storer, value))
            }
        }
    }
}

impl<V: Loadable> Loadable for HashedCacheableRef<V> {
    fn load_from_buffer(
        mut buffer: impl Read,
        _loader: &impl BlobStoreLoad,
    ) -> BlockStateResult<Self> {
        let blob_location: BlobStoreLocation = buffer.get().map_parse_err_to_block_state_err()?;
        let inner = HashedBufferedRefInner {
            hash: OnceLock::new(),
            blob_location: OnceLock::from(blob_location),
            value: OnceLock::new(),
        };

        Ok(Self {
            inner: Arc::new(inner),
        })
    }
}

impl<V: Storable> Storable for HashedCacheableRef<V> {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        let reference = self.inner.get_reference_or_store(storer);
        buffer.put(reference);
    }
}

impl<V: Cacheable + Loadable> Cacheable for HashedCacheableRef<V> {
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        let value = self.inner.get_or_cache_value(loader)?;
        value.cache_reference_values(loader)
    }
}

impl<V: Hashable + Loadable> Hashable for HashedCacheableRef<V> {
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        match self.inner.hash.get() {
            Some(hash) => Ok(*hash),
            None => {
                let value = self.inner.get_or_load_value(loader)?;
                let hash = value.hash(loader)?;
                // todo ar get_or_try_init
                self.inner.hash.get_or_init(|| hash);
                Ok(hash)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_state::blob_store;
    use crate::block_state::blob_store::StoreSerialized;
    use crate::block_state::blob_store::test_stub::{BlobStoreStub, UnreachableBlobStore};

    type TestRef = HashedCacheableRef<StoreSerialized<u64>>;

    fn assert_in_memory_repr<V>(hcr: &HashedCacheableRef<V>) -> &V {
        assert!(hcr.inner.blob_location.get().is_none(), "in blob store");
        hcr.inner.value.get().expect("not in memory")
    }

    fn assert_stored_repr<V>(hcr: &HashedCacheableRef<V>) -> BlobStoreLocation {
        assert!(hcr.inner.value.get().is_none(), "in memory");
        *hcr.inner.blob_location.get().expect("not in blob store")
    }

    fn assert_cached_repr<V>(hcr: &HashedCacheableRef<V>) -> (&V, BlobStoreLocation) {
        (
            hcr.inner.value.get().expect("not in memory"),
            *hcr.inner.blob_location.get().expect("not in blob store"),
        )
    }

    /// Test full lifecycle of a value:
    ///
    /// * create as new in memory
    /// * store the in-memory value to blob store
    /// * load the value from the blob store
    /// * cache the stored value
    ///
    /// There are further test cases of store and cache applied on representations
    /// not covered in this test.
    #[test]
    fn test_store_load_and_cache() {
        let mut store = BlobStoreStub::default();

        // Create new value an assert representation is memory
        let val1 = TestRef::new(StoreSerialized(1u64));
        assert_eq!(*assert_in_memory_repr(&val1), StoreSerialized(1));

        // Store value to blob store and assert representation is now cache.
        let blob_ref = blob_store::store_to_store(&mut store, &val1);
        let (val_tmp, val_ref) = assert_cached_repr(&val1);
        assert_eq!(*val_tmp, StoreSerialized(1));

        drop(val1);

        // Load value from blob store and assert representation is store.
        let val2: TestRef = blob_store::load_from_store(&store, blob_ref).unwrap();
        assert_eq!(assert_stored_repr(&val2), val_ref);

        // Cache the value and assert representation is now cache
        val2.cache_reference_values(&store).expect("cache");
        let (val_tmp, val_ref_tmp) = assert_cached_repr(&val2);
        assert_eq!(*val_tmp, StoreSerialized(1));
        assert_eq!(val_ref_tmp, val_ref);
    }

    /// Test storing cached value.
    #[test]
    fn test_store_cached_value() {
        let mut store = BlobStoreStub::default();
        let val1 = TestRef::new(StoreSerialized(1u64));

        // Store value to make it cached
        blob_store::store_to_store(&mut store, &val1);
        let (_, val_ref) = assert_cached_repr(&val1);

        // Store value again and assert this does not change the reference to the value.
        blob_store::store_to_store(&mut store, &val1);
        let (val_tmp, val_ref_tmp) = assert_cached_repr(&val1);
        assert_eq!(*val_tmp, StoreSerialized(1));
        assert_eq!(val_ref_tmp, val_ref);
    }

    /// Test storing stored value.
    #[test]
    fn test_store_stored_value() {
        let mut store = BlobStoreStub::default();
        let val1 = TestRef::new(StoreSerialized(1u64));
        let blob_ref = blob_store::store_to_store(&mut store, &val1);
        drop(val1);

        // Load value to make it stored
        let val2: TestRef = blob_store::load_from_store(&store, blob_ref).unwrap();
        let val_ref = assert_stored_repr(&val2);

        // Store value and assert this does not change the reference to the value.
        blob_store::store_to_store(&mut store, &val2);
        assert_eq!(assert_stored_repr(&val2), val_ref);
    }

    /// Test caching cached value.
    #[test]
    fn test_cache_cached_value() {
        let mut store = BlobStoreStub::default();
        let val1 = TestRef::new(StoreSerialized(1u64));

        // Store value to make it cached
        blob_store::store_to_store(&mut store, &val1);
        let (val_tmp, val_ref) = assert_cached_repr(&val1);
        assert_eq!(*val_tmp, StoreSerialized(1));

        // Cache value and assert it does not change representation
        // Assert that we don't need to read from the blob store by using UnreachableBlobStore
        val1.cache_reference_values(&UnreachableBlobStore)
            .expect("cache");
        let (val_tmp, val_ref_tmp) = assert_cached_repr(&val1);
        assert_eq!(*val_tmp, StoreSerialized(1));
        assert_eq!(val_ref_tmp, val_ref);
    }

    /// Test caching in-memory value.
    #[test]
    fn test_cache_in_memory_value() {
        let val1 = TestRef::new(StoreSerialized(1u64));

        // Cache value and assert it does not change representation
        // Assert that we don't need to read from the blob store by using UnreachableBlobStore
        val1.cache_reference_values(&UnreachableBlobStore)
            .expect("cache");
        assert_eq!(*assert_in_memory_repr(&val1), StoreSerialized(1));
    }

    /// Test [`HashedCacheableRef::value`]
    #[test]
    fn value() {
        let mut store = BlobStoreStub::default();

        // Test in-memory value. Assert in-memory representation does not change.
        // Assert that we don't need to read from the blob store by using UnreachableBlobStore
        let val1 = TestRef::new(StoreSerialized(1u64));
        assert_eq!(
            *val1.value(&UnreachableBlobStore).unwrap(),
            StoreSerialized(1u64)
        );
        assert_in_memory_repr(&val1);

        // Store value to make it cached
        let blob_ref = blob_store::store_to_store(&mut store, &val1);
        assert_cached_repr(&val1);

        // Test cached value. Assert cached representation does not change.
        // Assert that we don't need to read from the blob store by using UnreachableBlobStore
        assert_eq!(
            *val1.value(&UnreachableBlobStore).unwrap(),
            StoreSerialized(1u64)
        );
        assert_cached_repr(&val1);

        // Load value to make it stored.
        drop(val1);
        let val2: TestRef = blob_store::load_from_store(&store, blob_ref).unwrap();
        assert_stored_repr(&val2);

        // Test stored value. Assert stored representation does not change.
        assert_eq!(*val2.value(&store).unwrap(), StoreSerialized(1u64));
        assert_stored_repr(&val2);
    }

    /// Test hash in-memory value.
    #[test]
    fn test_hash_in_memory_value() {
        let store = BlobStoreStub::default();

        // Test hash in-memory value.
        // Assert that we don't need to read from the blob store by using UnreachableBlobStore
        let val1 = TestRef::new(StoreSerialized(1u64));
        assert_eq!(val1.inner.hash.get(), None);
        assert_eq!(
            val1.hash(&UnreachableBlobStore).unwrap(),
            StoreSerialized(1u64).hash(&store).unwrap()
        );
        assert_eq!(
            val1.inner.hash.get().copied(),
            Some(StoreSerialized(1u64).hash(&store).unwrap())
        );
    }

    /// Test hash cached value.
    #[test]
    fn test_hash_cached_value() {
        let mut store = BlobStoreStub::default();

        // Store value to make it cached
        let val1 = TestRef::new(StoreSerialized(1u64));
        blob_store::store_to_store(&mut store, &val1);
        assert_cached_repr(&val1);

        // Test hash cached value.
        // Assert that we don't need to read from the blob store by using UnreachableBlobStore
        assert_eq!(val1.inner.hash.get(), None);
        assert_eq!(
            val1.hash(&UnreachableBlobStore).unwrap(),
            StoreSerialized(1u64).hash(&store).unwrap()
        );
        assert_eq!(
            val1.inner.hash.get().copied(),
            Some(StoreSerialized(1u64).hash(&store).unwrap())
        );
    }

    /// Test hash stored value.
    #[test]
    fn test_hash_stored_value() {
        let mut store = BlobStoreStub::default();

        // Load value to make it stored
        let val1 = TestRef::new(StoreSerialized(1u64));
        let blob_ref = blob_store::store_to_store(&mut store, &val1);
        drop(val1);
        let val2: TestRef = blob_store::load_from_store(&store, blob_ref).unwrap();
        assert_stored_repr(&val2);

        // Test hash stored value. Assert stored representation does not change.
        assert_eq!(val2.inner.hash.get(), None);
        assert_eq!(
            val2.hash(&store).unwrap(),
            StoreSerialized(1u64).hash(&store).unwrap()
        );
        assert_eq!(
            val2.inner.hash.get().copied(),
            Some(StoreSerialized(1u64).hash(&store).unwrap())
        );
        assert_stored_repr(&val2);

        // Test hash again. This time we don't need to read from blob store, since we cache the hash.
        assert_eq!(
            val2.hash(&UnreachableBlobStore).unwrap(),
            StoreSerialized(1u64).hash(&store).unwrap()
        );
    }

    /// Test store, load and cache a reference with a nested reference.
    #[test]
    fn test_nested_reference_store_load_and_cache() {
        type NestedTestRef = HashedCacheableRef<HashedCacheableRef<StoreSerialized<u64>>>;

        let mut store = BlobStoreStub::default();
        let val1 = HashedCacheableRef::new(HashedCacheableRef::new(StoreSerialized(1u64)));

        // Store value to blob store and assert representation is now cache.
        let blob_ref = blob_store::store_to_store(&mut store, &val1);
        let (val_nested1, val_blob_ref) = assert_cached_repr(&val1);
        let (_, val_nested_blob_ref) = assert_cached_repr(val_nested1);

        drop(val1);

        // Load value from blob store and assert representation is store.
        let val2: NestedTestRef = blob_store::load_from_store(&store, blob_ref).unwrap();
        assert_eq!(assert_stored_repr(&val2), val_blob_ref);

        // Cache the value and assert representation is now cache
        val2.cache_reference_values(&store).expect("cache");
        let (val_nested2, val_ref_tmp) = assert_cached_repr(&val2);
        assert_eq!(val_ref_tmp, val_blob_ref);
        let (val_tmp, val_ref_tmp) = assert_cached_repr(val_nested2);
        assert_eq!(*val_tmp, StoreSerialized(1));
        assert_eq!(val_ref_tmp, val_nested_blob_ref);
    }
}
