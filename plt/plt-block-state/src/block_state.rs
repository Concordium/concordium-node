//! This module contains the [`PltBlockState`] which provides an implementation of [`BlockStateOperations`].

use crate::block_state::blob_store::{BackingStoreLoad, DecodeError};

use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;

pub mod blob_store;
pub mod external;
pub mod p10;
pub mod p11;
pub mod types;

/// Account with given address does not exist
#[derive(Debug, thiserror::Error)]
#[error("Account with address {0} does not exist")]
pub struct AccountNotFoundByAddressError(pub AccountAddress);

/// Account with given index does not exist
#[derive(Debug, thiserror::Error)]
#[error("Account with index {0} does not exist")]
pub struct AccountNotFoundByIndexError(pub AccountIndex);

/// Marker for PLT block state hash type.
pub enum PltBlockStateHashMarker {}
/// Hash of PLT block state
pub type PltBlockStateHash = concordium_base::hashes::HashBytes<PltBlockStateHashMarker>;

/// Top level operations on a block state implementation.
pub trait BlockStateOperations {
    /// Construct an empty block state.
    fn empty() -> Self;

    /// Compute the hash of the block state.
    fn hash(&self, loader: &mut impl blob_store::BackingStoreLoad) -> PltBlockStateHash;

    /// Store the block state into a blob store.
    fn store_update(
        &self,
        storer: &mut impl blob_store::BackingStoreStore,
    ) -> blob_store::Reference;

    /// Cache the block state into memory.
    fn cache(&mut self, loader: &mut impl blob_store::BackingStoreLoad);
}

/// Immutable block state save-point.
///
/// This is a wrapper around a [`PltBlockState`] ensuring further mutations can only be done by
/// unwrapping using [`PltBlockStateSavepoint::mutable_state`].
#[derive(Debug)]
pub struct BlockStateSavepoint<BlockStateVersion> {
    /// The inner block state, which will not be mutated.
    block_state: BlockStateVersion,
}

impl<BlockStateVersion> BlockStateSavepoint<BlockStateVersion> {
    /// Consume the mutable block state and create an immutable save-point.
    pub fn save(block_state: BlockStateVersion) -> Self {
        Self { block_state }
    }

    /// Construct a mutable block state which can be mutated without affecting this
    /// save-point.
    pub fn mutable_state(&self) -> BlockStateVersion
    where
        BlockStateVersion: Clone,
    {
        self.block_state.clone()
    }

    /// Get reference to the inner block state.
    pub fn state(&self) -> &BlockStateVersion {
        &self.block_state
    }
}

impl<BlockStateVersion> BlockStateOperations for BlockStateSavepoint<BlockStateVersion>
where
    BlockStateVersion: BlockStateOperations,
{
    fn empty() -> Self {
        Self::save(BlockStateVersion::empty())
    }

    fn hash(&self, loader: &mut impl blob_store::BackingStoreLoad) -> PltBlockStateHash {
        self.block_state.hash(loader)
    }

    fn store_update(
        &self,
        storer: &mut impl blob_store::BackingStoreStore,
    ) -> blob_store::Reference {
        self.block_state.store_update(storer)
    }

    fn cache(&mut self, loader: &mut impl blob_store::BackingStoreLoad) {
        self.block_state.cache(loader);
    }
}

impl<BlockStateVersion> blob_store::Loadable for BlockStateSavepoint<BlockStateVersion>
where
    BlockStateVersion: blob_store::Loadable,
{
    fn load(
        loader: &mut impl BackingStoreLoad,
        source: impl AsRef<[u8]>,
    ) -> Result<Self, DecodeError> {
        // todo do real implementation as part of https://linear.app/concordium/issue/PSR-11/port-the-plt-block-state-to-rust
        BlockStateVersion::load(loader, source).map(|block_state| Self { block_state })
    }
}
