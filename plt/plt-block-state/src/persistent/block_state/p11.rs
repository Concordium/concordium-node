use crate::block_state_interface::{BlockStateFailure, BlockStateResult};
use crate::persistent::blob_reference::hashed_cacheable_reference::HashedCacheableRef;
use crate::persistent::blob_store::{BlobStoreLoad, BlobStoreStore, Loadable, Storable};
use crate::persistent::cacheable::Cacheable;
use crate::persistent::hash;
use crate::persistent::hash::Hashable;
use crate::persistent::protocol_level_locks::p11::PersistentLocksP11;
use crate::persistent::protocol_level_tokens::p9::PersistentTokensP9;
use concordium_base::common::Buffer;
use concordium_base::hashes::Hash;
use std::io::Read;

/// P11 block state.
#[derive(Debug, Clone, Default)]
pub struct PersistentBlockStateP11 {
    /// Protocol-level tokens
    pub(crate) tokens: HashedCacheableRef<PersistentTokensP9>,
    /// Protocol-level locks
    pub(crate) locks: HashedCacheableRef<PersistentLocksP11>,
}

impl Loadable for PersistentBlockStateP11 {
    fn load_from_buffer(
        mut buffer: impl Read,
        loader: &impl BlobStoreLoad,
    ) -> Result<Self, BlockStateFailure> {
        let tokens = Loadable::load_from_buffer(&mut buffer, loader)?;
        let locks = Loadable::load_from_buffer(&mut buffer, loader)?;

        Ok(Self { tokens, locks })
    }
}

impl Storable for PersistentBlockStateP11 {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        self.tokens.store_to_buffer(&mut buffer, storer);
        self.locks.store_to_buffer(&mut buffer, storer);
    }
}

impl Cacheable for PersistentBlockStateP11 {
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        self.tokens.cache_reference_values(loader)?;
        self.locks.cache_reference_values(loader)?;
        Ok(())
    }
}

impl Hashable for PersistentBlockStateP11 {
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        let tokens = self.tokens.hash(loader)?;
        let locks = self.locks.hash(loader)?;

        Ok(hash::hash_of_hashes(tokens, locks))
    }
}
