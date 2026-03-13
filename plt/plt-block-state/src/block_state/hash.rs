use crate::block_state::blob_store::{BackingStoreLoad, DecodeError};
use concordium_base::hashes::{Hash, HashBytes};
use std::fmt::Debug;

/// Trait implemented by hashable values, that can be hashed without
/// loading blob store references.
pub trait SimpleHashable {
    /// Calculate hash of value.
    fn simple_hash(&self) -> Hash;
}

/// Implemented by types that are marked hashes `HashBytes<Purpose>`,
/// but can be converted into a "pure", unmarked [hash](Hash)
pub trait IntoPureHash {
    fn into_pure(self) -> Hash;
}

impl<Purpose> IntoPureHash for HashBytes<Purpose> {
    fn into_pure(self) -> Hash {
        Hash::from(self.bytes)
    }
}

/// Trait implemented by hashable values, that potentially needs
/// to load values from the backing store to calculate the hash.
pub trait Hashable {
    /// Type of the hash. Must be convertible into a "pure" hash.
    type Hash: IntoPureHash + Debug + Copy;

    /// Calculate hash of value. The given backing store `loader` can be used
    /// to load values pointed to by [`BlobReference`](super::blob_reference::BlobReference)s
    /// from the backing store.
    /// The loaded values should generally not be buffered as a side effect. But the
    /// hash calculated from loaded values should generally be cached and reused for the
    /// next hash calculation, such that loading from the blob store is not necessary
    /// if the hash needs to be calculated again.
    /// As such, the [`Self::hash`] is mixture of a "shallow" and "deep" operation.
    fn hash(&self, loader: impl BackingStoreLoad) -> Result<Self::Hash, DecodeError>;
}
