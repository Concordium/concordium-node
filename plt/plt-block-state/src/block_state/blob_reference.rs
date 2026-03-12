use concordium_base::common::Serialize;
use std::sync::{Arc, RwLock};

pub mod hashed_buffered_reference;

/// Reference to a storage location in the backing store where a value is located.
#[derive(Default, Debug, Clone, Copy, Eq, PartialEq, Serialize)]
#[repr(transparent)]
pub struct BlobReference(pub u64);

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
    #[inline(always)]
    fn clone(&self) -> Self {
        Self {
            link: self.link.clone(),
        }
    }
}

impl<V> Link<V> {
    pub fn new(value: V) -> Self {
        Self {
            link: Arc::new(RwLock::new(value)),
        }
    }
}
