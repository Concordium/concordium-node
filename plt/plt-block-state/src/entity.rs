//! Entity model for block state. This defines the block state interface to
//! the scheduler and generally exposes a statically types model.

use crate::external::ExternalBlockStateOperations;
use crate::persistent::blob_store::BlobStoreLoad;
use std::fmt::Debug;
use std::marker::PhantomData;

pub mod accounts;
pub mod block_state;
pub mod protocol_level_locks;
pub mod protocol_level_tokens;

/// Types needed to define the [`EntityContext`]
pub trait EntityContextTypes {
    /// Type for externally managed block state interactions.
    type ExternalBlockState: ExternalBlockStateOperations;
    /// Type for blob store.
    type Store: BlobStoreLoad;
}

#[derive(Debug, Default, Clone)]
pub struct EntityContextTypesWitness<ExternalBlockState, Store>(
    PhantomData<(ExternalBlockState, Store)>,
);

impl<ExternalBlockState: ExternalBlockStateOperations, Store: BlobStoreLoad> EntityContextTypes
    for EntityContextTypesWitness<ExternalBlockState, Store>
{
    type ExternalBlockState = ExternalBlockState;
    type Store = Store;
}

// todo ar try another construction, where you can specify with bounds what you want implemented for entity context types

/// Context needed to call functions on the block state and entities
/// in the block state.
#[derive(Debug, Default, Clone)]
pub struct EntityContext<C: EntityContextTypes> {
    /// Externally managed block state
    pub external: C::ExternalBlockState,
    /// Blob store loader.
    pub store: C::Store,
}

pub mod entity_test_stub {
    use crate::entity::block_state::p9::BlockStateP9;
    use crate::entity::block_state::p11::BlockStateP11;
    use crate::entity::{EntityContext, EntityContextTypes, EntityContextTypesWitness};
    use crate::external::test_stub::{ExternalBlockStateStub, UnreachableExternalBlockState};
    use crate::persistent::blob_store;
    use crate::persistent::blob_store::BlobStoreLocation;
    use crate::persistent::blob_store::test_stub::BlobStoreStub;
    use crate::persistent::block_state::p9::PersistentBlockStateP9;
    use crate::persistent::block_state::p11::PersistentBlockStateP11;

    type NoExternalBlockStateTypes =
        EntityContextTypesWitness<UnreachableExternalBlockState, BlobStoreStub>;

    /// Stubbed context with no external block state (will panic if accessed).
    pub type StubbedNoExternalEntityContext = EntityContext<NoExternalBlockStateTypes>;

    /// Create stubbed context with no external block state (will panic if accessed).
    pub fn new_no_external_context() -> StubbedNoExternalEntityContext {
        let blob_store = BlobStoreStub::default();
        EntityContext {
            external: UnreachableExternalBlockState,
            store: blob_store,
        }
    }

    type StubbedExternalBlockStateTypes =
        EntityContextTypesWitness<ExternalBlockStateStub, BlobStoreStub>;

    /// Stubbed context with stubbed external block state.
    pub type StubbedEntityContext = EntityContext<StubbedExternalBlockStateTypes>;

    /// Create stubbed context with stubbed external block state.
    pub fn new_stubbed_context() -> StubbedEntityContext {
        let blob_store = BlobStoreStub::default();
        EntityContext {
            external: ExternalBlockStateStub::default(),
            store: blob_store,
        }
    }

    pub fn load_block_state_p9<C: EntityContextTypes>(
        context: &EntityContext<C>,
        blob_ref: BlobStoreLocation,
    ) -> BlockStateP9 {
        let persistent_block_state: PersistentBlockStateP9 =
            blob_store::load_from_store(&context.store, blob_ref).expect("load block state");
        BlockStateP9 {
            persistent: persistent_block_state,
        }
    }

    pub fn load_block_state_p11<C: EntityContextTypes>(
        context: &EntityContext<C>,
        blob_ref: BlobStoreLocation,
    ) -> BlockStateP11 {
        let persistent_block_state: PersistentBlockStateP11 =
            blob_store::load_from_store(&context.store, blob_ref).expect("load block state");
        BlockStateP11 {
            persistent: persistent_block_state,
        }
    }
}
