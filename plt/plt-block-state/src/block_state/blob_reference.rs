//! Definition of blob store reference types. A blob store reference may largely be though of as
//! a pointer to a value in the blob store, but the representation of the value may vary during
//! the lifetime of the reference. The value may be represented in,
//!
//! * the blob store only (the reference is a pure blob store pointer)
//! * memory (the value will eventually be written to the blob store),
//! * both the blob store and memory (the reference is a pointer
//!   that also caches the value in memory).
//!
//! A blob store reference to a block state component
//! is stored in the blob store as a [blob location](super::blob_store::BlobStoreLocation)
//! This allows for
//!
//! * sharing block state components pointed to by blob references between different block states
//! * deciding when to load the referenced block state component,
//!   e.g. on demand as needed or via [`Cacheable`](super::cacheable::Cacheable)
//!
//! Blob store reference types are also cheaply clonable, such that they can be shared in the
//! in-memory representation of the block state.

pub mod hashed_cacheable_reference;
