use crate::block_state::blob_store::{BackingStoreLoad, DecodeError, StoreSerialized};
use concordium_base::common::{Put, Serial};
use concordium_base::hashes::Hash;
use sha2::Digest;

/// Trait implemented by hashable values, that potentially needs
/// to load values from the backing store to calculate the hash.
pub trait Hashable {
    /// Calculate hash of value. The given backing store `loader` can be used
    /// to load values pointed to by [`BlobReference`](super::blob_reference::BlobReference)s
    /// from the backing store.
    /// The loaded values should generally not be buffered as a side effect. But the
    /// hash calculated from loaded values should generally be cached and reused for the
    /// next hash calculation, such that loading from the blob store is not necessary
    /// if the hash needs to be calculated again.
    /// As such, the [`Self::hash`] is mixture of a "shallow" and "deep" operation.
    fn hash(&self, loader: impl BackingStoreLoad) -> Result<Hash, DecodeError>;
}

impl<T: Serial> Hashable for StoreSerialized<T> {
    fn hash(&self, _loader: impl BackingStoreLoad) -> Result<Hash, DecodeError> {
        Ok(hash_of_serialization(&self.0))
    }
}

/// Calculate has by digesting the bytes of two hashes.
pub fn hash_of_hashes(hash1: Hash, hash2: Hash) -> Hash {
    let mut hasher = sha2::Sha256::new();
    hasher.update(hash1);
    hasher.update(hash2);
    Hash::new(hasher.finalize().into())
}

/// Calculate hash by digesting the serialized bytes of a value.
pub fn hash_of_serialization(value: impl Serial) -> Hash {
    let mut hasher = sha2::Sha256::new();
    hasher.put(value);
    Hash::new(hasher.finalize().into())
}
