use crate::block_state::blob_reference::{BlobReference, Link};
use crate::block_state::blob_store::{
    BackingStoreLoad, BackingStoreStore, DecodeError, Loadable, ParseResultExt, Storable,
};
use crate::block_state::bufferable::Bufferable;
use crate::block_state::hash::Hashable;
use concordium_base::common::{Buffer, Deserial};
use concordium_base::hashes::Hash;
use std::io::Read;

/// Representation of an immutable, buffered and lazily hashed value of type `V`.
/// The represented value is immutable in the sense that the value itself does not change,
/// once the [`HashedBufferedRef`] has been created. The value representation can be in
///
/// * memory: initial representation for a new value created with [`HashedBufferedRef::new`]
/// * backing store: initial representation for a value loaded
///   from the backing store with [`Loadable::load`]
/// * backing store and buffered in memory: representation after either storing a value
///   represented in memory with [`Storable::store`] or buffering a value in the backing store
///   with [`Bufferable::buffer_blob_references`].
///
/// The representation change during the lifetime of [`HashedBufferedRef`] is implemented
/// via interior mutability, but the value itself never changes during the lifetime.
///
/// The hash of the represented is calculated lazily when needed, and cached
/// via interior mutability.
#[derive(Debug)]
pub struct HashedBufferedRef<V> {
    inner: Link<HashedBufferedRefImpl<V>>,
}

impl<V> HashedBufferedRef<V> {
    /// Create a new value represented in memory.
    pub fn new(value: V) -> Self {
        todo!()
    }
}

impl<V> Clone for HashedBufferedRef<V> {
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
    variant: HashedBufferedRefVariant<V>,
}

#[derive(Debug)]
enum HashedBufferedRefVariant<V> {
    /// The value is in the backing storage.
    Store { reference: BlobReference },
    /// The value is in memory and not written to backing storage.
    Memory { value: V },
    /// The value is in the backing storage, and also buffered in memory.
    Buffered { reference: BlobReference, value: V },
}

impl<V: Loadable> Loadable for HashedBufferedRef<V> {
    fn load(mut source: impl Read) -> Result<Self, DecodeError> {
        let reference = BlobReference::deserial(&mut source).into_decode_result()?;
        todo!()
    }
}

impl<V: Storable> Storable for HashedBufferedRef<V> {
    fn store(&self, buffer: impl Buffer, storer: impl BackingStoreStore) {
        todo!()
    }
}

impl<V: Bufferable> Bufferable for HashedBufferedRef<V> {
    fn buffer_blob_references(&self, loader: impl BackingStoreLoad) {
        todo!()
    }
}

impl<V: Hashable> Hashable for HashedBufferedRef<V> {
    type Hash = Hash;

    fn hash(&self, loader: impl BackingStoreLoad) -> Self::Hash {
        todo!()
    }
}
