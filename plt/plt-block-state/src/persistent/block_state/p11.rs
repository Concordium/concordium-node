use std::io::Read;
use concordium_base::common::Buffer;
use concordium_base::hashes::Hash;
use crate::block_state::blob_store::{BlobStoreLoad, BlobStoreStore, Loadable, Storable};
use crate::block_state::cacheable::Cacheable;
use crate::block_state::hash::Hashable;
use crate::block_state::persistent::protocol_level_tokens::PersistentPlTokens;
use crate::block_state_interface::{BlockStateFailure, BlockStateResult};

/// P11 block state.
#[derive(Debug, Clone)]
pub struct PersistentBlockStateP11 {
    /// Protocol-level tokens
    tokens: PersistentPlTokens,
}

impl Loadable for PersistentBlockStateP11 {
    fn load_from_buffer(
        buffer: impl Read,
        loader: &impl BlobStoreLoad,
    ) -> Result<Self, BlockStateFailure> {
        let tokens = Loadable::load_from_buffer(buffer, loader)?;

        Ok(Self {
            tokens,
        })
    }
}

impl Storable for PersistentBlockStateP11 {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        self.tokens.store_to_buffer(&mut buffer, storer);
    }
}

impl Cacheable for PersistentBlockStateP11 {
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        self.tokens.cache_reference_values(loader)?;
        Ok(())
    }
}

impl Hashable for PersistentBlockStateP11 {
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        self.tokens.hash(loader)
    }
}
