use crate::block_state::blob_store::{BackingStoreLoad, DecodeError, StoreSerialized};
use concordium_base::common::Deserial;

/// Trait implemented by types that are stored in the backing store and may
/// be composed of further [`BlobReference`](super::blob_reference::BlobReference)s
/// that represents values that can be cached into memory on demand.
pub trait Cacheable {
    /// Load any values pointed to by composed [`BlobReference`](super::blob_reference::BlobReference)s
    /// into memory in a cached representation.
    /// This operation should recursively apply the cache operation as values are cached into memory,
    /// and values that are already in memory.
    /// As such, `store` is a "deep" operation.
    fn cache_reference_values(&self, loader: &impl BackingStoreLoad) -> Result<(), DecodeError>;
}

impl<T: Deserial> Cacheable for StoreSerialized<T> {
    fn cache_reference_values(&self, _loader: &impl BackingStoreLoad) -> Result<(), DecodeError> {
        // nothing to cache, if a value is directly deserializable
        Ok(())
    }
}
