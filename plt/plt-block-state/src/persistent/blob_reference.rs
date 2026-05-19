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
