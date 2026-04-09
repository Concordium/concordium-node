/// Representation of an immutable, cachable and lazily hashed value of type `V`.
///
/// See [`HashedCacheableRef`].
use crate::block_state::blob_store;
use crate::block_state::blob_store::{
    BlobStoreLoad, BlobStoreLocation, BlobStoreStore, Loadable, ParseResultExt, Storable,
};
use crate::block_state::cacheable::Cacheable;
use crate::block_state::hash::Hashable;
use crate::block_state::utils::{LockRef, OwnedOrBorrowed};
use crate::block_state_interface::BlockStateResult;
use concordium_base::common::{Buffer, Get, Put};
use concordium_base::hashes::Hash;
use std::io::Read;
use std::mem;

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
    /// The representation is wrapped in a [`LockRef`] to allow cheap cloning and
    /// interior mutability. The interior mutability must not be used to change which value
    /// is actually represented, only to update internal structure, such that where the value
    /// is represented.
    inner: LockRef<HashedBufferedRefInner<V>>,
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
            hash: None,
            repr: HashedCacheableRefRepr::Memory { value },
        };

        Self::from_inner(inner)
    }

    fn from_inner(inner: HashedBufferedRefInner<V>) -> Self {
        Self {
            inner: LockRef::new(inner),
        }
    }

    /// Access the referenced value via the given `read` closure. The value of type `T` returned by `read`
    /// is the value returned by `with_value`. If the value is already in memory, the value
    /// is passed to the closure as borrowed. If it is not in memory, it is loaded from the
    /// blob store, and passed owned to the closure as owned.
    ///
    /// Loading from the blob store will not make the value cached in the reference.
    ///
    /// # Errors
    ///
    /// Returns [`BlockStateError`] if returned by `read` or if
    /// decoding data from the blob store fails.
    pub fn with_value<T>(
        &self,
        loader: &impl BlobStoreLoad,
        read: impl FnOnce(OwnedOrBorrowed<V>) -> BlockStateResult<T>,
    ) -> BlockStateResult<T>
    where
        V: Loadable,
    {
        let inner = self.inner.read();
        inner.repr.get_or_load_value(loader).and_then(read)
    }

    /// Load the referenced value and return it. If the value is already in memory, it is cloned
    /// and returned. If it is not in memory, it is loaded from the blob store, and returned.
    ///
    /// Loading from the blob store will not make the value cached in the reference.
    ///
    /// # Errors
    ///
    /// Returns [`BlockStateError`] if decoding data from the blob store fails.
    pub fn clone_or_load_value(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<V>
    where
        V: Loadable + Clone,
    {
        let inner = self.inner.read();
        Ok(inner.repr.get_or_load_value(loader)?.into_owned())
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

/// The [`HashedCacheableRef`] behind the [`LockRef`].
#[derive(Debug)]
struct HashedBufferedRefInner<V> {
    /// Lazily calculated hash.
    hash: Option<Hash>,
    /// The potentially buffered value.
    repr: HashedCacheableRefRepr<V>,
}

impl<V> HashedCacheableRefRepr<V> {
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
        Ok(match self {
            HashedCacheableRefRepr::Store { reference } => {
                let value: V = blob_store::load_from_store(loader, *reference)?;
                OwnedOrBorrowed::Owned(value)
            }
            HashedCacheableRefRepr::Memory { value } => OwnedOrBorrowed::Borrowed(value),
            HashedCacheableRefRepr::Cache { value, .. } => OwnedOrBorrowed::Borrowed(value),
        })
    }

    /// Cache the referenced value and return it. If the value is already in memory, a reference
    /// to it is simply returned. If it is not in memory, it is loaded from the blob store and
    /// cached in [`HashedCacheableRefRepr::Cache`] first.
    fn get_or_cache_value(&mut self, loader: &impl BlobStoreLoad) -> BlockStateResult<&V>
    where
        V: Loadable,
    {
        Ok(match self {
            HashedCacheableRefRepr::Store { reference } => {
                let value: V = blob_store::load_from_store(loader, *reference)?;
                *self = HashedCacheableRefRepr::Cache {
                    reference: *reference,
                    value,
                };
                // We just wrote the Cache variant, so we can safely assert this variant,
                // in order to borrow the value just written.
                let HashedCacheableRefRepr::Cache { value, .. } = self else {
                    unreachable!("not HashedBufferedRefRepr::Cache though it was just written")
                };
                value
            }
            HashedCacheableRefRepr::Memory { value } => value,
            HashedCacheableRefRepr::Cache { value, .. } => value,
        })
    }

    /// Store the value and return its [`BlobStoreLocation`]. If the value is already stored in
    /// the blob store, the [`BlobStoreLocation`] for it is simply returned. If it is not stored,
    /// it is stored into the blob store, and the resulting [`BlobStoreLocation`] is saved in
    /// [`HashedCacheableRefRepr::Cache`] and returned.
    fn get_reference_or_store(&mut self, storer: &mut impl BlobStoreStore) -> BlobStoreLocation
    where
        V: Storable,
    {
        match self {
            HashedCacheableRefRepr::Store { reference } => *reference,
            HashedCacheableRefRepr::Memory { value } => {
                let reference = blob_store::store_to_store(storer, &*value);

                // We need the Memory representation owned, in order to move the value out of
                // it and into the Cache representation, without cloning it.
                // In order to achieve this, we swap the intermediate representation Store to self.
                let mut repr_tmp = HashedCacheableRefRepr::Store { reference };
                mem::swap(&mut repr_tmp, self);
                let HashedCacheableRefRepr::Memory { value } = repr_tmp else {
                    unreachable!("not HashedBufferedRefRepr::Memory though it was just matched")
                };
                *self = HashedCacheableRefRepr::Cache { value, reference };

                reference
            }
            HashedCacheableRefRepr::Cache { reference, .. } => *reference,
        }
    }
}

/// The possible representations of the referenced value.
#[derive(Debug)]
enum HashedCacheableRefRepr<V> {
    /// The value is in the blob store.
    Store { reference: BlobStoreLocation },
    /// The value is in memory and not written to blob store.
    Memory { value: V },
    /// The value is in the blob store, and also cached in memory.
    Cache {
        reference: BlobStoreLocation,
        value: V,
    },
}

impl<V: Loadable> Loadable for HashedCacheableRef<V> {
    fn load_from_buffer(mut buffer: impl Read) -> BlockStateResult<Self> {
        let reference = buffer.get().map_parse_err_to_block_state_err()?;
        let inner = HashedBufferedRefInner {
            hash: None,
            repr: HashedCacheableRefRepr::Store { reference },
        };

        Ok(Self::from_inner(inner))
    }
}

impl<V: Storable> Storable for HashedCacheableRef<V> {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        let mut inner = self.inner.write();
        let reference = inner.repr.get_reference_or_store(storer);
        buffer.put(reference);
    }
}

impl<V: Cacheable + Loadable> Cacheable for HashedCacheableRef<V> {
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        let mut inner = self.inner.write();
        let value = inner.repr.get_or_cache_value(loader)?;
        value.cache_reference_values(loader)
    }
}

impl<V: Hashable + Loadable> Hashable for HashedCacheableRef<V> {
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        let mut inner = self.inner.write();
        Ok(if let Some(hash) = inner.hash {
            hash
        } else {
            let value = inner.repr.get_or_load_value(loader)?;
            let hash = value.hash(loader)?;
            inner.hash = Some(hash);
            hash
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_state::blob_store;
    use crate::block_state::blob_store::StoreSerialized;
    use crate::block_state::blob_store::test_stub::{BlobStoreStub, UnreachableBlobStore};
    use assert_matches::assert_matches;

    type TestRef = HashedCacheableRef<StoreSerialized<u64>>;

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
        assert_matches!(&val1.inner.read().repr, HashedCacheableRefRepr::Memory{value} => {
            assert_eq!(*value, StoreSerialized(1));
        });

        // Store value to blob store and assert representation is now cache.
        let blob_ref = blob_store::store_to_store(&mut store, &val1);
        let val_ref = assert_matches!(&val1.inner.read().repr, HashedCacheableRefRepr::Cache {reference,value} => {
            assert_eq!(*value, StoreSerialized(1));
            *reference
        });

        drop(val1);

        // Load value from blob store and assert representation is store.
        let val2: TestRef = blob_store::load_from_store(&store, blob_ref).unwrap();
        assert_matches!(&val2.inner.read().repr, HashedCacheableRefRepr::Store {reference} => {
            assert_eq!(*reference, val_ref);
        });

        // Cache the value and assert representation is now cache
        val2.cache_reference_values(&store).expect("cache");
        assert_matches!(&val2.inner.read().repr, HashedCacheableRefRepr::Cache {reference,value} => {
            assert_eq!(*value, StoreSerialized(1));
            assert_eq!(*reference, val_ref);
        });
    }

    /// Test storing cached value.
    #[test]
    fn test_store_cached_value() {
        let mut store = BlobStoreStub::default();
        let val1 = TestRef::new(StoreSerialized(1u64));

        // Store value to make it cached
        blob_store::store_to_store(&mut store, &val1);
        let val_ref = assert_matches!(&val1.inner.read().repr, HashedCacheableRefRepr::Cache {reference,..} => {
            *reference
        });

        // Store value again and assert this does not change the reference to the value.
        blob_store::store_to_store(&mut store, &val1);
        assert_matches!(&val1.inner.read().repr, HashedCacheableRefRepr::Cache {reference,value} => {
            assert_eq!(*value, StoreSerialized(1));
            assert_eq!(*reference, val_ref);
        });
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
        let val_ref = assert_matches!(&val2.inner.read().repr, HashedCacheableRefRepr::Store {reference} => {
            *reference
        });

        // Store value and assert this does not change the reference to the value.
        blob_store::store_to_store(&mut store, &val2);
        assert_matches!(&val2.inner.read().repr, HashedCacheableRefRepr::Store {reference} => {
            assert_eq!(*reference, val_ref);
        });
    }

    /// Test caching cached value.
    #[test]
    fn test_cache_cached_value() {
        let mut store = BlobStoreStub::default();
        let val1 = TestRef::new(StoreSerialized(1u64));

        // Store value to make it cached
        blob_store::store_to_store(&mut store, &val1);
        let val_ref = assert_matches!(&val1.inner.read().repr, HashedCacheableRefRepr::Cache {reference,value} => {
            assert_eq!(*value, StoreSerialized(1));
            *reference
        });

        // Cache value and assert it does not change representation
        // Assert that we don't need to read from the blob store by using UnreachableBlobStore
        val1.cache_reference_values(&UnreachableBlobStore)
            .expect("cache");
        assert_matches!(&val1.inner.read().repr, HashedCacheableRefRepr::Cache {reference,value} => {
            assert_eq!(*value, StoreSerialized(1));
            assert_eq!(*reference, val_ref);
        });
    }

    /// Test caching in-memory value.
    #[test]
    fn test_cache_in_memory_value() {
        let val1 = TestRef::new(StoreSerialized(1u64));

        // Cache value and assert it does not change representation
        // Assert that we don't need to read from the blob store by using UnreachableBlobStore
        val1.cache_reference_values(&UnreachableBlobStore)
            .expect("cache");
        assert_matches!(&val1.inner.read().repr, HashedCacheableRefRepr::Memory {value} => {
            assert_eq!(*value, StoreSerialized(1));
        });
    }

    /// Test [`HashedCacheableRef::clone_or_load_value`]
    #[test]
    fn test_clone_or_load_value() {
        let mut store = BlobStoreStub::default();

        // Test in-memory value. Assert in-memory representation does not change.
        // Assert that we don't need to read from the blob store by using UnreachableBlobStore
        let val1 = TestRef::new(StoreSerialized(1u64));
        assert_eq!(
            val1.clone_or_load_value(&UnreachableBlobStore).unwrap(),
            StoreSerialized(1u64)
        );
        assert_matches!(
            &val1.inner.read().repr,
            HashedCacheableRefRepr::Memory { .. }
        );

        // Store value to make it cached
        let blob_ref = blob_store::store_to_store(&mut store, &val1);
        assert_matches!(
            &val1.inner.read().repr,
            HashedCacheableRefRepr::Cache { .. }
        );

        // Test cached value. Assert cached representation does not change.
        // Assert that we don't need to read from the blob store by using UnreachableBlobStore
        assert_eq!(
            val1.clone_or_load_value(&UnreachableBlobStore).unwrap(),
            StoreSerialized(1u64)
        );
        assert_matches!(
            &val1.inner.read().repr,
            HashedCacheableRefRepr::Cache { .. }
        );

        // Load value to make it stored.
        drop(val1);
        let val2: TestRef = blob_store::load_from_store(&store, blob_ref).unwrap();
        assert_matches!(
            &val2.inner.read().repr,
            HashedCacheableRefRepr::Store { .. }
        );

        // Test stored value. Assert stored representation does not change.
        assert_eq!(
            val2.clone_or_load_value(&store).unwrap(),
            StoreSerialized(1u64)
        );
        assert_matches!(
            &val2.inner.read().repr,
            HashedCacheableRefRepr::Store { .. }
        );
    }

    /// Test [`HashedCacheableRef::with_value`]
    #[test]
    fn test_with_value() {
        let mut store = BlobStoreStub::default();

        // Test in-memory value. Assert in-memory representation does not change.
        // Assert that we don't need to read from the blob store by using UnreachableBlobStore
        let val1 = TestRef::new(StoreSerialized(1u64));
        assert_eq!(
            val1.with_value(&UnreachableBlobStore, |val| Ok(*val))
                .unwrap(),
            StoreSerialized(1u64)
        );
        assert_matches!(
            &val1.inner.read().repr,
            HashedCacheableRefRepr::Memory { .. }
        );

        // Store value to make it cached
        let blob_ref = blob_store::store_to_store(&mut store, &val1);
        assert_matches!(
            &val1.inner.read().repr,
            HashedCacheableRefRepr::Cache { .. }
        );

        // Test cached value. Assert cached representation does not change.
        // Assert that we don't need to read from the blob store by using UnreachableBlobStore
        assert_eq!(
            val1.with_value(&UnreachableBlobStore, |val| Ok(*val))
                .unwrap(),
            StoreSerialized(1u64)
        );
        assert_matches!(
            &val1.inner.read().repr,
            HashedCacheableRefRepr::Cache { .. }
        );

        // Load value to make it stored.
        drop(val1);
        let val2: TestRef = blob_store::load_from_store(&store, blob_ref).unwrap();
        assert_matches!(
            &val2.inner.read().repr,
            HashedCacheableRefRepr::Store { .. }
        );

        // Test stored value. Assert stored representation does not change.
        assert_eq!(
            val2.with_value(&store, |val| Ok(*val)).unwrap(),
            StoreSerialized(1u64)
        );
        assert_matches!(
            &val2.inner.read().repr,
            HashedCacheableRefRepr::Store { .. }
        );
    }

    /// Test hash in-memory value.
    #[test]
    fn test_hash_in_memory_value() {
        let store = BlobStoreStub::default();

        // Test hash in-memory value.
        // Assert that we don't need to read from the blob store by using UnreachableBlobStore
        let val1 = TestRef::new(StoreSerialized(1u64));
        assert_eq!(val1.inner.read().hash, None);
        assert_eq!(
            val1.hash(&UnreachableBlobStore).unwrap(),
            StoreSerialized(1u64).hash(&store).unwrap()
        );
        assert_eq!(
            val1.inner.read().hash,
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
        assert_matches!(
            &val1.inner.read().repr,
            HashedCacheableRefRepr::Cache { .. }
        );

        // Test hash cached value.
        // Assert that we don't need to read from the blob store by using UnreachableBlobStore
        assert_eq!(val1.inner.read().hash, None);
        assert_eq!(
            val1.hash(&UnreachableBlobStore).unwrap(),
            StoreSerialized(1u64).hash(&store).unwrap()
        );
        assert_eq!(
            val1.inner.read().hash,
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
        assert_matches!(
            &val2.inner.read().repr,
            HashedCacheableRefRepr::Store { .. }
        );

        // Test hash stored value. Assert stored representation does not change.
        assert_eq!(val2.inner.read().hash, None);
        assert_eq!(
            val2.hash(&store).unwrap(),
            StoreSerialized(1u64).hash(&store).unwrap()
        );
        assert_eq!(
            val2.inner.read().hash,
            Some(StoreSerialized(1u64).hash(&store).unwrap())
        );
        assert_matches!(
            &val2.inner.read().repr,
            HashedCacheableRefRepr::Store { .. }
        );

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
        let (val_blob_ref, val_nested1) = assert_matches!(&val1.inner.read().repr, HashedCacheableRefRepr::Cache {reference,value} => {
            (*reference, value.clone())
        });
        let val_nested_blob_ref = assert_matches!(&val_nested1.inner.read().repr, HashedCacheableRefRepr::Cache {reference,..} => {
            *reference
        });

        drop(val1);

        // Load value from blob store and assert representation is store.
        let val2: NestedTestRef = blob_store::load_from_store(&store, blob_ref).unwrap();
        assert_matches!(&val2.inner.read().repr, HashedCacheableRefRepr::Store {reference} => {
            assert_eq!(*reference, val_blob_ref);
        });

        // Cache the value and assert representation is now cache
        val2.cache_reference_values(&store).expect("cache");
        let val_nested2 = assert_matches!(&val2.inner.read().repr, HashedCacheableRefRepr::Cache {reference,value} => {
            assert_eq!(*reference, val_blob_ref);
            value.clone()
        });
        assert_matches!(&val_nested2.inner.read().repr, HashedCacheableRefRepr::Cache {reference,value} => {
            assert_eq!(*value, StoreSerialized(1));
            assert_eq!(*reference, val_nested_blob_ref);
        });
    }
}
