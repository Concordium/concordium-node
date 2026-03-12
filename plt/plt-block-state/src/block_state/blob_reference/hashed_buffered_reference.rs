use crate::block_state::blob_reference::{BlobReference, Link};
use crate::block_state::blob_store::{
    BackingStoreLoad, BackingStoreStore, DecodeError, Loadable, Storable,
};
use crate::block_state::bufferable::Bufferable;
use crate::block_state::hash::Hashable;
use concordium_base::common::{Buffer, Deserial};
use concordium_base::hashes::Hash;
use std::io::Read;

/// A potentially buffered and lazily hashed value V. This is a value that can either be purely in
/// memory, purely in backing storage, or both in memory and in backing storage.
/// The hash is calculated lazily when needed. The implementation of
/// [`Loadable`] will only load the blob reference from the blob store.
#[derive(Debug)]
pub struct HashedBufferedRef<V> {
    inner: Link<HashedBufferedRefImpl<V>>,
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
    Disk { reference: BlobReference },
    /// The value is in memory and not written to backing storage.
    Memory { value: V },
    /// The value is in the backing storage, and also buffered in memory.
    Buffered { reference: BlobReference, value: V },
}

impl<V: Loadable> Loadable for HashedBufferedRef<V> {
    fn load(source: impl Read) -> Result<Self, DecodeError> {
        let reference = BlobReference::deserial(source)?;
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
