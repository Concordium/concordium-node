//! Persistent (immutable) model for the block state. The types in this module are the types
//! closes to the actual storage (blob store).
//! The model generally allows representing block state components and values
//! in memory, in the [blob store](super::blob_store), or both (cached), and the representation
//! may change during the lifetime of components and values (via interior mutability).

use crate::block_state::blob_store::{BlobStoreLoad, BlobStoreStore, Loadable, Storable};
use crate::block_state::cacheable::Cacheable;
use crate::block_state::hash::Hashable;
use concordium_base::common::Buffer;
use std::io::Read;

pub mod block_state;
pub mod protocol_level_tokens;
