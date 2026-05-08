//! Entity model for block state. This defines the block state interface to
//! the scheduler and generally exposes a statically types model.

use crate::block_state::blob_store::BlobStoreLoad;
use crate::external::ExternalBlockStateOperations;
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


#[cfg(test)]
pub mod entity_test_stub {
    use crate::block_state::blob_store::test_stub::BlobStoreStub;
    use crate::entity::{EntityContext, EntityContextTypes};
    use crate::external::test_stub::NoExternalBlockStateStub;

    /// Context with no external block state (will panic if accessed).
    #[derive(Debug)]
    pub struct NoExternalBlockStateTypes;

    impl EntityContextTypes for NoExternalBlockStateTypes {
        type ExternalBlockState = NoExternalBlockStateStub;
        type Loader = BlobStoreStub;
    }

    /// Create context with no external block state (will panic if accessed).
    pub fn new_context_no_external() -> EntityContext<NoExternalBlockStateTypes> {
        let blob_store = BlobStoreStub::default();
        EntityContext {
            external: NoExternalBlockStateStub,
            loader: blob_store,
        }

    }
}
