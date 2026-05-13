//! Persistent model for protocol-level locks in the block state.

use crate::block_state::blob_store::{
    BlobStoreLoad, BlobStoreStore, Loadable, Storable, StoreSerialized,
};
use crate::block_state::cacheable::Cacheable;
use crate::block_state::hash::Hashable;
use crate::block_state::types::protocol_level_locks::LockConfiguration;
use crate::block_state::types::protocol_level_tokens::TokenIndex;
use crate::block_state_interface::{BlockStateFailure, BlockStateResult};
use concordium_base::base::AccountIndex;
use concordium_base::common::{Buffer, Serialize};
use concordium_base::hashes::Hash;
use concordium_base::protocol_level_locks::LockId;
use std::collections::{BTreeMap, BTreeSet};
use std::io::Read;

/// Block state for protocol level tokens
#[derive(Debug, Clone, Default)]
pub struct ProtocolLevelLocks {
    pub locks: StoreSerialized<BTreeMap<LockId, Lock>>,
}

impl Loadable for ProtocolLevelLocks {
    fn load_from_buffer(
        buffer: impl Read,
        loader: &impl BlobStoreLoad,
    ) -> Result<Self, BlockStateFailure> {
        let locks = Loadable::load_from_buffer(buffer, loader)?;

        Ok(Self { locks })
    }
}

impl Storable for ProtocolLevelLocks {
    fn store_to_buffer(&self, buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        self.locks.store_to_buffer(buffer, storer)
    }
}

impl Cacheable for ProtocolLevelLocks {
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        self.locks.cache_reference_values(loader)
    }
}

impl Hashable for ProtocolLevelLocks {
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        self.locks.hash(loader)
    }
}

impl ProtocolLevelLocks {
    pub fn empty() -> Self {
        Self {
            locks: Default::default(),
        }
    }
}

/// The block state for a single protocol-level lock.
#[derive(Debug, Clone, Serialize)]
pub struct Lock {
    /// Contains references to the tokens with balances locked within this lock
    pub locked_balances: BTreeSet<(AccountIndex, TokenIndex)>,
    /// The configuration parameters for the lock.
    pub configuration: LockConfiguration,
}
