use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};
use crate::block_state::blob_store::{BackingStoreLoad, DecodeError};

/// Trait implemented by types that are stored in the backing store and may
/// be composed of further [`BlobReference`](super::blob_reference::BlobReference)s
/// that represents values that can be cached into memory on demand.
pub trait Cacheable {
    /// Load any values pointed to by composed [`BlobReference`](super::blob_reference::BlobReference)s
    /// into memory in a cached representation.
    /// This operation should recursively apply the cache operation as values are cached into memory,
    /// and values that are already in memory.
    /// As such, `store` is a "deep" operation.
    fn cache_reference_values(&self, loader: impl BackingStoreLoad) -> Result<(), DecodeError>;
}

/// A link to a shared occurrence of a value `V`.
/// This is used in this module to construct trees, allowing for sharing of
/// values in trees and subtrees in case of the persistent tree.
///
/// This [Link] achieves the following properties
/// - it is cheap to clone
/// - it allows for inner mutability
/// - it is safe to use in a concurrent context.
#[derive(Debug)]
pub struct Link<V> {
    link: Arc<RwLock<V>>,
}

impl<V> Clone for Link<V> {
    fn clone(&self) -> Self {
        Self {
            link: self.link.clone(),
        }
    }
}



impl<V> Link<V> {
    /// Create new [`Link`] with given value.
    pub fn new(value: V) -> Self {
        Self {
            link: Arc::new(RwLock::new(value)),
        }
    }

    /// Get read access guard for the linked value.
    pub fn read(&self) -> RwLockReadGuard<'_, V> {
        self.link.read().expect("Link poisoned")
    }

    /// Get  write access guard for the linked value.
    pub fn write(&self) -> RwLockWriteGuard<'_, V> {
        self.link.write().expect("Link poisoned")
    }
}
