//! Entity model for block state. This defines the block state interface to
//! the scheduler and generally exposes a statically types model.

use crate::external::ExternalBlockStateOperations;
use crate::persistent::blob_store::BlobStoreLoad;
use std::fmt::Debug;

pub mod accounts;
pub mod block_state;
pub mod protocol_level_locks;
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
    pub external: C::ExternalBlockState,
    /// Blob store loader.
    pub loader: C::Loader,
}

pub mod entity_test_stub {
    use crate::entity::block_state::p9::BlockStateP9;
    use crate::entity::block_state::p11::BlockStateP11;
    use crate::entity::{EntityContext, EntityContextTypes};
    use crate::external::test_stub::NoExternalBlockStateStub;
    use crate::persistent::blob_store;
    use crate::persistent::blob_store::BlobStoreLocation;
    use crate::persistent::blob_store::test_stub::BlobStoreStub;
    use crate::persistent::block_state::p9::PersistentBlockStateP9;
    use crate::persistent::block_state::p11::PersistentBlockStateP11;

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

    pub fn load_block_state_p9<C: EntityContextTypes>(
        context: &EntityContext<C>,
        blob_ref: BlobStoreLocation,
    ) -> BlockStateP9 {
        let persistent_block_state: PersistentBlockStateP9 =
            blob_store::load_from_store(&context.loader, blob_ref).expect("load block state");
        BlockStateP9 {
            persistent: persistent_block_state,
        }
    }

    pub fn load_block_state_p11<C: EntityContextTypes>(
        context: &EntityContext<C>,
        blob_ref: BlobStoreLocation,
    ) -> BlockStateP11 {
        let persistent_block_state: PersistentBlockStateP11 =
            blob_store::load_from_store(&context.loader, blob_ref).expect("load block state");
        BlockStateP11 {
            persistent: persistent_block_state,
        }
    }
}
