use crate::block_state::blob_store::BackingStoreLoad;
use crate::block_state::types::reference::Link;
use concordium_base::hashes::Hash;

/// Compute SHA256 of data.
pub trait ToSHA256 {
    fn hash(&self) -> Hash;
}

/// Compute SHA256 of data, allowing access to read
/// from the backing store.
pub trait ToSHA256WithBackingStoreLoader {
    fn hash(&self, loader: impl BackingStoreLoad) -> Hash;
}

