//! Blob store reference types. A blob store reference represents the value of a block state component,
//! that may be in the blob store, in memory, or both. A blob store reference to a block state component
//! is stored in the blob store as a reference to another location in the blob store where the block
//! state component is stored. This allows for
//!
//! * sharing block state components pointed to by blob references between different block states
//! * deciding when to load the referenced block state component,
//!   e.g. on demand as needed or via [`Cacheable`](super::cacheable::Cacheable)
//!
//! Blob store reference types are also cheaply clonable, such that they can be shared in the
//! in-memory representation of the block state.

pub mod hashed_cacheable_reference;
pub mod stored_reference;

use hashed_cacheable_reference::HashedCacheableRef;

/// Trait for abstracting over blob reference type families.
/// A blob reference type family provides a generic reference type [`BlobRefTypeFamily::Ref<T>`]
/// for any value type `T`.
pub trait BlobRefTypeFamily {
    /// The reference type for a value of type `T`.
    type Ref<T>: Clone;
}

/// Type family for [`HashedCacheableRef`].
#[derive(Debug, Clone)]
pub struct HashedCacheableRefFamily;

impl BlobRefTypeFamily for HashedCacheableRefFamily {
    type Ref<T> = HashedCacheableRef<T>;
}

