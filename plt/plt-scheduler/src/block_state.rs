//! This module contains the [`BlockState`] which provides an implementation of [`BlockStateOperations`].
//!

pub mod blob_store;
#[cfg(feature = "ffi")]
pub mod ffi;

pub enum PltBlockStateHashMarker {}
pub type PltBlockStateHash = concordium_base::hashes::HashBytes<PltBlockStateHashMarker>;

/// Immutable block state save-point.
///
/// This is a safe wrapper around a [`BlockState`] ensuring further mutations can only be done by
/// unwrapping using [`BlockStateSavepoint::new_generation`] which creates a new generation.
#[derive(Debug)]
pub struct BlockStateSavepoint {
    /// The inner block state, which will not be mutated further for this generation.
    block_state: BlockState,
}

impl BlockStateSavepoint {
    /// Initialize a new block state.
    pub fn empty() -> Self {
        Self {
            block_state: BlockState::empty(),
        }
    }

    /// Compute the hash.
    pub fn hash(&self, _loader: impl blob_store::BackingStoreLoad) -> PltBlockStateHash {
        todo!()
    }

    /// Store a PLT block state in a blob store.
    pub fn store_update(
        &mut self,
        _storer: &mut impl blob_store::BackingStoreStore,
    ) -> blob_store::StoreResult<blob_store::Reference> {
        todo!()
    }

    /// Migrate the PLT block state from one blob store to another.
    pub fn migrate(
        &mut self,
        _loader: &impl blob_store::BackingStoreLoad,
        _storer: &mut impl blob_store::BackingStoreStore,
    ) -> blob_store::LoadStoreResult<Self> {
        todo!()
    }

    /// Cache the block state in memory.
    pub fn cache(&mut self, _loader: &impl blob_store::BackingStoreLoad) {
        todo!()
    }

    /// Construct a new generation block state which can be mutated without affecting this
    /// save-point.
    pub fn new_generation(&self) -> BlockState {
        let mut block_state = self.block_state.clone();
        block_state.generation += 1;
        block_state
    }
}

impl blob_store::Loadable for BlockStateSavepoint {
    fn load<S: std::io::Read, F: blob_store::BackingStoreLoad>(
        _loader: &mut F,
        _source: &mut S,
    ) -> blob_store::LoadResult<Self> {
        todo!()
    }
}

/// Block state providing the various block state operations.
#[derive(Debug, Clone)]
pub struct BlockState {
    /// The generation counter for the block state.
    generation: u64,
}

impl BlockState {
    /// Construct an empty block state.
    fn empty() -> Self {
        BlockState { generation: 0 }
    }

    /// Consume the mutable block state and create a immutable save-point.
    pub fn savepoint(self) -> BlockStateSavepoint {
        BlockStateSavepoint { block_state: self }
    }
}
