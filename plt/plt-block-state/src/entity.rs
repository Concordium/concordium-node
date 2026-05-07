//! Stateful entity model for block state

use crate::block_state::blob_store::BlobStoreLoad;
use crate::block_state::external::ExternalBlockStateOperations;
use std::fmt::Debug;

pub mod accounts;
pub mod block_state;
pub mod protocol_level_tokens;

/// Types needed to define the [`EntityContext`]
pub trait EntityContextTypes: Debug {
    /// Type for externally managed block state interactions.
    type ExternalBlockState: ExternalBlockStateOperations + Debug;
    /// Type for blob store loader.
    type Loader: BlobStoreLoad + Debug;
}

/// Context needed to call functions on the block state and entities
/// in the block state.
#[derive(Debug)]
pub struct EntityContext<C: EntityContextTypes> {
    /// Externally managed block state
    pub(crate) external: C::ExternalBlockState,
    /// Blob store loader.
    pub(crate) loader: C::Loader,
}
