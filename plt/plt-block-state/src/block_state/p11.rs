use super::{blob_store, p10, BlockStateOperations};

/// Block state providing the various block state operations.
#[derive(Debug, Clone)]
pub struct PltBlockStateP11 {
    // todo implement real block state as part of https://linear.app/concordium/issue/PSR-11/port-the-plt-block-state-to-rust
    /// Simplistic state that is used as a temporary implementation of the block state
    pub tokens: p10::Tokens,
    pub locks: Locks,
}

impl PltBlockStateP11 {
    pub fn migrate_from_p10(
        _loader: &mut impl blob_store::BackingStoreLoad,
        _storer: &mut impl blob_store::BackingStoreStore,
        _old: &p10::PltBlockStateP10,
    ) -> Self {
        todo!()
    }
}

impl blob_store::Loadable for PltBlockStateP11 {
    fn load(
        _loader: &mut impl blob_store::BackingStoreLoad,
        _source: impl AsRef<[u8]>,
    ) -> Result<Self, blob_store::DecodeError> {
        todo!()
    }
}

impl BlockStateOperations for PltBlockStateP11 {
    fn empty() -> Self {
        Self {
            tokens: Default::default(),
            locks: (),
        }
    }

    fn hash(&self, _loader: &mut impl blob_store::BackingStoreLoad) -> super::PltBlockStateHash {
        todo!()
    }

    fn store_update(
        &self,
        _storer: &mut impl blob_store::BackingStoreStore,
    ) -> super::blob_store::Reference {
        todo!()
    }

    fn cache(&mut self, _loader: &mut impl blob_store::BackingStoreLoad) {
        todo!()
    }
}

type Locks = ();
