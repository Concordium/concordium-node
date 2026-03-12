use crate::block_state::blob_store::{
    BackingStoreLoad, BackingStoreStore, DecodeError, Loadable, Storable,
};
use crate::block_state::types::blob_reference::{BlobReference, Link};
use concordium_base::common::Buffer;
use concordium_base::hashes::Hash;
use std::io::{Read, Write};

/// A potentially buffered and lazily hashed value V. This is a value that can either be purely in
/// memory, purely in backing storage, or both in memory and in backing storage.
/// The hash is calculated lazily when needed. The implementation of
/// [`Loadable`] will only load the blob reference from the blob store.
///
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
        todo!()
    }
}

impl<V: Storable> Storable for HashedBufferedRef<V> {
    fn store(&self, buffer: impl Buffer, storer: impl BackingStoreStore) {
        todo!()
    }
}
