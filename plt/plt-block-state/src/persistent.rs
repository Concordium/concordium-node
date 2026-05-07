//! Persistent (immutable) model for the block state. The types in this module are the types
//! closes to the actual storage (blob store).
//! The model generally allows representing block state components and values
//! in memory, in the [blob store](super::blob_store), or both (cached), and the representation
//! may change during the lifetime of components and values (via interior mutability).

pub mod block_state;
pub mod protocol_level_tokens;
