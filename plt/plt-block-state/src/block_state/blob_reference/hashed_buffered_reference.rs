use crate::block_state::blob_reference::{BlobReference, Link};
use crate::block_state::blob_store;
use crate::block_state::blob_store::{
    BackingStoreLoad, BackingStoreStore, DecodeError, Loadable, ParseResultExt, Storable,
};
use crate::block_state::cacheable::Cacheable;
use crate::block_state::hash::{FromPureHash, Hashable, IntoPureHash};
use concordium_base::common::{Buffer, Deserial, Serial};
use concordium_base::hashes::Hash;
use std::io::Read;
use std::sync::{Arc, RwLockReadGuard, RwLockWriteGuard};

/// Representation of an immutable, cachable and lazily hashed value of type `V`.
/// The represented value is immutable in the sense that the value itself does not change,
/// once the [`HashedCacheableRef`] has been created. The value representation can be in
///
/// * memory: initial representation for a new value created with [`HashedCacheableRef::new`]
/// * backing store: initial representation for a value loaded
///   from the backing store with [`Loadable::load_from_buffer`]
/// * cached: representation where the value is both in the backing store and in memory
///
/// The cached representation is the result of either storing a value represented in
/// memory with [`Storable::store_to_buffer`] or caching a value in the backing store
/// with [`Cacheable::cache_reference_values`].
///
/// The representation change during the lifetime of [`HashedCacheableRef`] is implemented
/// via interior mutability, but the value itself never changes during the lifetime.
///
/// The hash of the represented is calculated lazily when needed, and cached
/// via interior mutability.
#[derive(Debug)]
pub struct HashedCacheableRef<V> {
    inner: Link<HashedBufferedRefImpl<V>>,
}

impl<V> HashedCacheableRef<V> {
    /// Create a new value represented in memory.
    pub fn new(value: V) -> Self {
        let inner = HashedBufferedRefImpl {
            hash: None,
            repr: HashedBufferedRefRepr::Memory {
                value: Arc::new(value),
            },
        };

        Self::from_inner(inner)
    }

    fn from_inner(inner: HashedBufferedRefImpl<V>) -> Self {
        Self {
            inner: Link::new(inner),
        }
    }

    fn inner(&self) -> RwLockReadGuard<'_, HashedBufferedRefImpl<V>> {
        self.inner
            .link
            .read()
            .expect("HashedCacheableRef inner poisoned")
    }

    fn inner_mut(&self) -> RwLockWriteGuard<'_, HashedBufferedRefImpl<V>> {
        self.inner
            .link
            .write()
            .expect("HashedCacheableRef inner poisoned")
    }

    /// Load the referenced value. If the value is already in memory, it is simply
    /// returned. If it is not in memory, it is loaded from the backing storage.
    /// Loading from the backing storage will not make the value cached in the reference.
    fn get_or_load_value(&self, loader: impl BackingStoreLoad) -> Result<Arc<V>, DecodeError>
    where
        V: Loadable,
    {
        let inner = self.inner();
        Ok(match &inner.repr {
            HashedBufferedRefRepr::Store { reference } => {
                let value: V = blob_store::load_from_store(loader, *reference)?;
                Arc::new(value)
            }
            HashedBufferedRefRepr::Memory { value } => Arc::clone(value),
            HashedBufferedRefRepr::Cache { value, .. } => Arc::clone(value),
        })
    }
}

impl<V> Clone for HashedCacheableRef<V> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

#[derive(Debug)]
struct HashedBufferedRefImpl<V> {
    /// Lazily calculated hash.
    hash: Option<Hash>,
    /// The potentially buffered value.
    repr: HashedBufferedRefRepr<V>,
}

#[derive(Debug)]
enum HashedBufferedRefRepr<V> {
    /// The value is in the backing storage.
    Store { reference: BlobReference },
    /// The value is in memory and not written to backing storage.
    Memory { value: Arc<V> },
    /// The value is in the backing storage, and also cached in memory.
    Cache {
        reference: BlobReference,
        value: Arc<V>,
    },
}

impl<V: Loadable> Loadable for HashedCacheableRef<V> {
    fn load_from_buffer(mut buffer: impl Read) -> Result<Self, DecodeError> {
        let reference = BlobReference::deserial(&mut buffer).into_decode_result()?;
        let inner = HashedBufferedRefImpl {
            hash: None,
            repr: HashedBufferedRefRepr::Store { reference },
        };

        Ok(Self::from_inner(inner))
    }
}

impl<V: Storable> Storable for HashedCacheableRef<V> {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: impl BackingStoreStore) {
        let mut inner = self.inner_mut();
        let reference = match &inner.repr {
            HashedBufferedRefRepr::Store { reference } => *reference,
            HashedBufferedRefRepr::Memory { value } => {
                let reference = blob_store::store_to_store(storer, &**value);
                inner.repr = HashedBufferedRefRepr::Cache {
                    reference,
                    value: Arc::clone(value),
                };
                reference
            }
            HashedBufferedRefRepr::Cache { reference, .. } => *reference,
        };
        reference.serial(&mut buffer);
    }
}

impl<V: Cacheable + Loadable> Cacheable for HashedCacheableRef<V> {
    fn cache_reference_values(&self, mut loader: impl BackingStoreLoad) -> Result<(), DecodeError> {
        let mut inner = self.inner_mut();
        let value = match &inner.repr {
            HashedBufferedRefRepr::Store { reference } => {
                let value: V = blob_store::load_from_store(&mut loader, *reference)?;
                let value = Arc::new(value);
                inner.repr = HashedBufferedRefRepr::Cache {
                    reference: *reference,
                    value: Arc::clone(&value),
                };
                value
            }
            HashedBufferedRefRepr::Memory { value } => Arc::clone(value),
            HashedBufferedRefRepr::Cache { value, .. } => Arc::clone(value),
        };
        value.cache_reference_values(loader)?;
        Ok(())
    }
}

impl<V: Hashable + Loadable> Hashable for HashedCacheableRef<V> {
    type Hash = <V as Hashable>::Hash;

    fn hash(&self, mut loader: impl BackingStoreLoad) -> Result<Self::Hash, DecodeError> {
        let mut inner = self.inner_mut();
        Ok(if let Some(hash) = inner.hash {
            Self::Hash::from_pure(hash)
        } else {
            let value = self.get_or_load_value(&mut loader)?;
            let hash = value.hash(loader)?;
            inner.hash = Some(hash.into_pure());
            hash
        })
    }
}
