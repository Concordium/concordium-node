use crate::block_state::blob_store::BackingStoreLoad;

/// Trait implemented by types that are stored in the backing store and may
/// be composed of further [`BlobReference`](super::blob_reference::BlobReference)s
/// that represents values that can be buffered into memory.
pub trait Bufferable {
    /// Load any values pointed to by composed [`BlobReference`](super::blob_reference::BlobReference)s
    /// into memory in a buffered representation.
    /// This operation should recursively apply the buffer operation as values are buffered into memory.
    /// As such, `store` is a "deep" operation.
    fn buffer_blob_references(&self, loader: impl BackingStoreLoad);
}
