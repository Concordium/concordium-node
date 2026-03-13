use crate::block_state::blob_reference::BlobReference;
use crate::block_state::blob_store;
use crate::block_state::blob_store::{
    BackingStoreLoad, BackingStoreStore, DecodeError, Loadable, ParseResultExt, Storable,
};
use crate::block_state::cacheable::Cacheable;
use crate::block_state::hash::{FromPureHash, Hashable, IntoPureHash};
use crate::block_state::utils::{Link, OwnedOrBorrowed};
use concordium_base::common::{Buffer, Get, Put};
use concordium_base::hashes::Hash;
use std::io::Read;
use std::mem;

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
            repr: HashedBufferedRefRepr::Memory { value },
        };

        Self::from_inner(inner)
    }

    fn from_inner(inner: HashedBufferedRefImpl<V>) -> Self {
        Self {
            inner: Link::new(inner),
        }
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

impl<V> HashedBufferedRefRepr<V> {
    /// Load the referenced value and return it. If the value is already in memory, a reference to it is simply
    /// returned. If it is not in memory, it is loaded from the backing storage, and returned as owned.
    /// Loading from the backing storage will not make the value cached in the reference.
    fn get_or_load_value(
        &self,
        loader: impl BackingStoreLoad,
    ) -> Result<OwnedOrBorrowed<'_, V>, DecodeError>
    where
        V: Loadable,
    {
        Ok(match self {
            HashedBufferedRefRepr::Store { reference } => {
                let value: V = blob_store::load_from_store(loader, *reference)?;
                OwnedOrBorrowed::Owned(value)
            }
            HashedBufferedRefRepr::Memory { value } => OwnedOrBorrowed::Borrowed(value),
            HashedBufferedRefRepr::Cache { value, .. } => OwnedOrBorrowed::Borrowed(value),
        })
    }

    /// Cache the referenced value and return it. If the value is already in memory, a reference to it is simply
    /// returned. If it is not in memory, it is loaded from the backing storage and cached in [`HashedBufferedRefRepr::Cache`] first.
    fn get_or_cache_value(&mut self, loader: impl BackingStoreLoad) -> Result<&V, DecodeError>
    where
        V: Loadable,
    {
        Ok(match self {
            HashedBufferedRefRepr::Store { reference } => {
                let value: V = blob_store::load_from_store(loader, *reference)?;
                *self = HashedBufferedRefRepr::Cache {
                    reference: *reference,
                    value,
                };
                // We just wrote the Cache variant, so we can safely assert this variant,
                // in order to borrow the value just written.
                let HashedBufferedRefRepr::Cache { value, .. } = self else {
                    unreachable!("not HashedBufferedRefRepr::Cache though it was just written")
                };
                value
            }
            HashedBufferedRefRepr::Memory { value } => value,
            HashedBufferedRefRepr::Cache { value, .. } => value,
        })
    }

    /// Store the value and return its [`BlobReference`]. If the value is already stored in the backing store,
    /// the [`BlobReference`] to it is simply returned. If it is not stored, it is stored into the
    /// backing storage, and the resulting [`BlobReference`] is saved in [`HashedBufferedRefRepr::Cache`]
    /// before it is returned.
    fn get_reference_or_store(&mut self, storer: impl BackingStoreStore) -> BlobReference
    where
        V: Storable,
    {
        match self {
            HashedBufferedRefRepr::Store { reference } => *reference,
            HashedBufferedRefRepr::Memory { value } => {
                let reference = blob_store::store_to_store(storer, &*value);

                // We need the Memory representation owned, in order to move the value out of
                // it and into the Cache representation. In order to achieve this,
                // we write an intermediate representation Store to self.
                // It is done this way to avoid cloning the value.
                let mut old_repr = HashedBufferedRefRepr::Store { reference };
                mem::swap(&mut old_repr, self);
                let HashedBufferedRefRepr::Memory { value } = old_repr else {
                    unreachable!("not HashedBufferedRefRepr::Memory though it was just written")
                };
                *self = HashedBufferedRefRepr::Cache { value, reference };

                reference
            }
            HashedBufferedRefRepr::Cache { reference, .. } => *reference,
        }
    }
}

#[derive(Debug)]
enum HashedBufferedRefRepr<V> {
    /// The value is in the backing storage.
    Store { reference: BlobReference },
    /// The value is in memory and not written to backing storage.
    Memory { value: V },
    /// The value is in the backing storage, and also cached in memory.
    Cache { reference: BlobReference, value: V },
}

impl<V: Loadable> Loadable for HashedCacheableRef<V> {
    fn load_from_buffer(mut buffer: impl Read) -> Result<Self, DecodeError> {
        let reference = buffer.get().into_decode_result()?;
        let inner = HashedBufferedRefImpl {
            hash: None,
            repr: HashedBufferedRefRepr::Store { reference },
        };

        Ok(Self::from_inner(inner))
    }
}

impl<V: Storable> Storable for HashedCacheableRef<V> {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: impl BackingStoreStore) {
        let mut inner = self.inner.write();
        let reference = inner.repr.get_reference_or_store(storer);
        buffer.put(reference);
    }
}

impl<V: Cacheable + Loadable> Cacheable for HashedCacheableRef<V> {
    fn cache_reference_values(&self, mut loader: impl BackingStoreLoad) -> Result<(), DecodeError> {
        let mut inner = self.inner.write();
        let value = inner.repr.get_or_cache_value(&mut loader)?;
        value.cache_reference_values(loader)
    }
}

impl<V: Hashable + Loadable> Hashable for HashedCacheableRef<V> {
    type Hash = <V as Hashable>::Hash;

    fn hash(&self, mut loader: impl BackingStoreLoad) -> Result<Self::Hash, DecodeError> {
        let mut inner = self.inner.write();
        Ok(if let Some(hash) = inner.hash {
            Self::Hash::from_pure(hash)
        } else {
            let value = inner.repr.get_or_load_value(&mut loader)?;
            let hash = value.hash(loader)?;
            inner.hash = Some(hash.into_pure());
            hash
        })
    }
}
