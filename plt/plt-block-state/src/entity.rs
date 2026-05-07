//! Stateful entity model for block state

use crate::block_state::blob_store::BlobStoreLoad;
use crate::block_state::external::ExternalBlockStateOperations;

pub mod accounts;
pub mod block_state;
pub mod protocol_level_tokens;

pub trait EntityContextTypes {
    type ExternalBlockState: ExternalBlockStateOperations;
    type Loader: BlobStoreLoad;
}

pub struct EntityContext<C: EntityContextTypes> {
    /// Externally managed block state
    pub(crate) external: C::ExternalBlockState,
    /// Blob store loader.
    pub(crate) loader: C::Loader,
}
