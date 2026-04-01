//! Definition of the [`Cacheable`] trait that allows caching block state components wrapped in
//! [blob references](super::blob_reference) into memory.

use crate::block_state::blob_store::{BlobStoreLoad, StoreSerialized};
use crate::block_state_interface::BlockStateResult;
use concordium_base::common::Deserial;

/// Trait implemented by types that are stored in the blob store and may
/// be composed of [blob references](super::blob_reference)
/// that represents values that can be cached into memory on demand.
pub trait Cacheable {
    /// Load any values pointed to by [blob references](super::blob_reference)
    /// into memory in a cached representation.
    /// This operation should recursively apply the cache operation as values are cached into memory,
    /// and values that are already in memory.
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()>;
}

impl<T: Deserial> Cacheable for StoreSerialized<T> {
    fn cache_reference_values(&self, _loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        // nothing to cache, if a value is directly deserializable
        Ok(())
    }
}
