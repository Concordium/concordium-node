use crate::block_state::blob_store::{BlobStoreLoad, BlobStoreStore, Loadable, Storable};
use crate::block_state::cacheable::Cacheable;
use crate::block_state::hash::Hashable;
use crate::block_state::persistent::protocol_level_tokens::PersistentPlTokens;
use crate::block_state_interface::{BlockStateFailure, BlockStateResult};
use concordium_base::common::Buffer;
use concordium_base::hashes::Hash;
use std::io::Read;

/// P11 block state.
#[derive(Debug, Clone)]
pub struct PersistentBlockStateP10 {
    /// Protocol-level tokens
    tokens: PersistentPlTokens,
}

impl Loadable for PersistentBlockStateP10 {
    fn load_from_buffer(
        buffer: impl Read,
        loader: &impl BlobStoreLoad,
    ) -> Result<Self, BlockStateFailure> {
        let tokens = Loadable::load_from_buffer(buffer, loader)?;

        Ok(Self { tokens })
    }
}

impl Storable for PersistentBlockStateP10 {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        self.tokens.store_to_buffer(&mut buffer, storer);
    }
}

impl Cacheable for PersistentBlockStateP10 {
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        self.tokens.cache_reference_values(loader)?;
        Ok(())
    }
}

impl Hashable for PersistentBlockStateP10 {
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        self.tokens.hash(loader)
    }
}
