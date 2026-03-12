use crate::block_state::blob_store::BackingStoreLoad;

/// Trait implemented by types that are stored in the backing store and have
/// nested blob store references to values that can be buffered into memory.
pub trait Bufferable {
    /// Load any values pointed to by nested blob references into memory. This operation
    /// should recursively apply the buffer operation to values buffered into memory.
    /// As such, `store` is a "deep" operation.//
    fn buffer_blob_references(&self, loader: impl BackingStoreLoad);
}
