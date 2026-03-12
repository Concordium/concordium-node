use crate::block_state::blob_store::BackingStoreLoad;
use concordium_base::hashes::{Hash, HashBytes};

/// Trait implemented by hashable values, that can be hashed without
/// loading blob store references.
pub trait SimpleHashable {
    /// Calculate hash of value.
    fn simple_hash(&self) -> Hash;
}

pub trait IntoPureHash {
    fn into_pure(self) -> Hash;
}

impl<Purpose> IntoPureHash for HashBytes<Purpose> {
    fn into_pure(self) -> Hash {
        Hash::from(self.bytes)
    }
}

/// Trait implemented by hashable values, that potentially needs
/// to read from the backing store to calculate the hash.
pub trait Hashable {
    /// Type of the hash. Must be convertible into a "pure" hash.
    type Hash: IntoPureHash;

    /// Calculate hash of value. The given backing store `loader` can be used
    /// to load nested values pointed to by blob references from the backing store.
    /// The loaded values should generally not be buffered as a side effect. But the
    /// hash calculated from loaded values should generally be cached and reused for the
    /// next hash calculation, such that reading from the blob store is not necessary
    /// if the hash needs to be calculated again.
    /// As such, `store` is mixture of a "shallow" and "deep" operation.
    fn hash(&self, loader: impl BackingStoreLoad) -> Self::Hash;
}
