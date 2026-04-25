use crate::block_state::blob_store::{BlobStoreLoad, BlobStoreStore, Loadable, Storable};
use crate::block_state::cacheable::Cacheable;
use crate::block_state::hash::Hashable;
use crate::block_state_interface::{BlockStateFailure, BlockStateResult};
use crate::persistent::block_state::p9::PersistentBlockStateP9;
use concordium_base::common::Buffer;
use concordium_base::hashes::Hash;
use std::io::Read;

/// P10 block state.
#[derive(Debug, Clone)]
pub struct PersistentBlockStateP10 {
    /// P9 block state
    pub p9_block_state: PersistentBlockStateP9,
}

impl Loadable for PersistentBlockStateP10 {
    fn load_from_buffer(
        buffer: impl Read,
        loader: &impl BlobStoreLoad,
    ) -> Result<Self, BlockStateFailure> {
        let p9_block_state = Loadable::load_from_buffer(buffer, loader)?;

        Ok(Self { p9_block_state })
    }
}

impl Storable for PersistentBlockStateP10 {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        self.p9_block_state.store_to_buffer(&mut buffer, storer);
    }
}

impl Cacheable for PersistentBlockStateP10 {
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        self.p9_block_state.cache_reference_values(loader)?;
        Ok(())
    }
}

impl Hashable for PersistentBlockStateP10 {
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        self.p9_block_state.hash(loader)
    }
}
