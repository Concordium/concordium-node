//! This module contains the [`BlockState`] which provides an implementation of [`BlockStateOperations`].

use crate::block_state::blob_store::{BlobStoreLoad, BlobStoreStore, Loadable, Storable};
use crate::block_state::cacheable::Cacheable;
use crate::block_state::external::{ExternalBlockStateOperations, ExternalBlockStateQuery};
use crate::block_state::hash::Hashable;
use crate::block_state::state::protocol_level_tokens::ProtocolLevelTokens;
use crate::block_state::types::AccountWithCanonicalAddress;
use crate::block_state::types::protocol_level_tokens::{
    TokenAccountState, TokenConfiguration, TokenIndex, TokenStateKey, TokenStateValue,
};
use crate::block_state_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, BlockStateOperations,
    BlockStateQuery, BlockStateResult, OverflowError, RawTokenAmountDelta, TokenNotFoundByIdError,
};
use concordium_base::base::{AccountIndex, ProtocolVersion};
use concordium_base::common::Buffer;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::hashes::Hash;
use concordium_base::protocol_level_tokens::TokenId;
use plt_scheduler_types::types::tokens::RawTokenAmount;
use std::io::Read;
use std::mem;

pub mod blob_reference;
pub mod blob_store;
pub mod cacheable;
pub mod external;
pub mod hash;
pub mod lfmb_tree;
mod smart_contract_trie;
mod state;
pub mod types;
mod utils;

/// Immutable block state. The block state is immutable in the sense,
/// that the state it represents never changes during the lifetime of values of type [`BlockState`].
/// In order to perform mutating operations on the block state, a new [`BlockState`]
/// must be created.
///
/// The internal representation in [`BlockState`] may change during the lifetime via interior mutability.
/// This happens if state are cached, stored or hashes are lazily calculated.
#[derive(Debug, Clone)]
pub struct BlockState {
    /// The protocol version of the block state.
    pub protocol_version: ProtocolVersion,
    /// Data in the block state
    pub data: BlockStateData,
}

#[derive(Debug, Clone, Default)]
pub struct BlockStateData {
    /// Protocol-level tokens
    tokens: ProtocolLevelTokens,
}

impl BlockStateData {
    pub fn empty() -> Self {
        BlockStateData {
            tokens: ProtocolLevelTokens::empty(),
        }
    }
}

impl BlockState {
    /// Construct an empty block state.
    pub fn empty(protocol_version: ProtocolVersion) -> Self {
        BlockState {
            protocol_version,
            data: BlockStateData::empty(),
        }
    }

    /// Consume the immutable block state and create a mutable block state.
    pub fn into_mutable(self) -> MutableBlockState {
        MutableBlockState::new(self)
    }

    /// Migrate the PLT block state from one blob store to another.
    ///
    /// # Arguments
    ///
    /// - `from_loader` Blob store loader for the blob store we are migrating from.
    /// - `to_storer` Blob store storer for the blob store we are migrating to.
    /// - `to_protocol_version` Protocol version for the block state to migrate to.
    pub fn migrate(
        &self,
        _from_loader: impl BlobStoreLoad,
        _to_storer: impl BlobStoreStore,
        _to_protocol_version: ProtocolVersion,
    ) -> Self {
        // todo ar
        todo!()
    }
}

impl Loadable for BlockStateData {
    fn load_from_buffer(
        mut buffer: impl Read,
        loader: &impl BlobStoreLoad,
    ) -> BlockStateResult<Self> {
        let tokens = Loadable::load_from_buffer(&mut buffer, loader)?;

        Ok(Self { tokens })
    }
}

impl Storable for BlockState {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        self.data.tokens.store_to_buffer(&mut buffer, storer);
    }
}

impl Cacheable for BlockState {
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        self.data.tokens.cache_reference_values(loader)?;
        Ok(())
    }
}

impl Hashable for BlockState {
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        self.data.tokens.hash(loader)
    }
}

/// Mutable block state. In contrast to the immutable block state [`BlockState`],
/// operations on the mutable block state changes the state that
/// the value represents.
#[derive(Debug, Clone)]
pub struct MutableBlockState {
    /// Immutable block state value. The block state represented by [`MutableBlockState`] is
    /// mutated simply by setting a new value for the immutable block state [`BlockState`].
    immutable_state: BlockState,
}

impl MutableBlockState {
    /// Create mutable block state from immutable block state.
    fn new(mutable_state: BlockState) -> Self {
        Self {
            immutable_state: mutable_state,
        }
    }

    /// Consume the mutable block state and create an immutable block state.
    pub fn into_immutable(self) -> BlockState {
        self.immutable_state
    }

    /// Update the block state using `update` closure and return
    /// the additional value of type `T` returned by the closure.
    fn update_block_state<T>(
        &mut self,
        update: impl FnOnce(BlockStateData) -> BlockStateResult<(T, BlockStateData)>,
    ) -> BlockStateResult<T> {
        let ret;
        (ret, self.immutable_state.data) = update(mem::take(&mut self.immutable_state.data))?;
        Ok(ret)
    }

    /// Update the block state using `update` closure.
    fn update_block_state_(
        &mut self,
        update: impl FnOnce(BlockStateData) -> BlockStateResult<BlockStateData>,
    ) -> BlockStateResult<()> {
        self.immutable_state.data = update(mem::take(&mut self.immutable_state.data))?;
        Ok(())
    }
}

/// Runtime/execution state relevant for providing an implementation of
/// [`BlockStateQuery`] and [`BlockStateOperations`].
///
/// In addition to the PLT block state, this type contains callbacks
/// for the parts of the state that is managed on the Haskell side.
#[derive(Debug)]
pub struct ExecutionTimeBlockState<IntState, Load, ExtState> {
    /// The library block state implementation.
    pub internal_block_state: IntState,
    /// External function for reading from the blob store.
    pub blob_store_load: Load,
    /// Part of block state that is managed externally.
    pub external_block_state: ExtState,
}

/// Provides access needed for querying block state (but not to do operations on the block state).
trait HasBlockState {
    fn block_state(&self) -> &BlockStateData;

    fn protocol_version(&self) -> ProtocolVersion;
}

impl HasBlockState for &BlockState {
    fn block_state(&self) -> &BlockStateData {
        &self.data
    }

    fn protocol_version(&self) -> ProtocolVersion {
        self.protocol_version
    }
}

impl HasBlockState for BlockState {
    fn block_state(&self) -> &BlockStateData {
        &self.data
    }

    fn protocol_version(&self) -> ProtocolVersion {
        self.protocol_version
    }
}

impl HasBlockState for MutableBlockState {
    fn block_state(&self) -> &BlockStateData {
        &self.immutable_state.data
    }

    fn protocol_version(&self) -> ProtocolVersion {
        self.immutable_state.protocol_version
    }
}

impl<IntState: HasBlockState, Load: BlobStoreLoad, ExtState: ExternalBlockStateQuery>
    BlockStateQuery for ExecutionTimeBlockState<IntState, Load, ExtState>
{
    type MutableTokenKeyValueState = smart_contract_trie::MutableState;
    type Account = AccountIndex;
    type Token = TokenIndex;

    fn plt_list(&self) -> impl ExactSizeIterator<Item = TokenId> {
        // todo propagate block state error as part of https://linear.app/concordium/issue/COR-2346/push-blockstateerror-to-scheduler-code
        self.internal_block_state
            .block_state()
            .tokens
            .plt_list(&self.blob_store_load)
            .map(|item| item.unwrap())
    }

    fn token_by_id(&self, token_id: &TokenId) -> Result<Self::Token, TokenNotFoundByIdError> {
        self.internal_block_state
            .block_state()
            .tokens
            .token_by_id(token_id)
            .ok_or_else(|| TokenNotFoundByIdError(token_id.clone()))
    }

    fn mutable_token_key_value_state(
        &self,
        token: &Self::Token,
    ) -> Self::MutableTokenKeyValueState {
        // todo propagate block state error as part of https://linear.app/concordium/issue/COR-2346/push-blockstateerror-to-scheduler-code
        self.internal_block_state
            .block_state()
            .tokens
            .mutable_token_key_value_state(&self.blob_store_load, *token)
            .unwrap()
    }

    fn token_configuration(&self, token: &Self::Token) -> TokenConfiguration {
        // todo propagate block state error as part of https://linear.app/concordium/issue/COR-2346/push-blockstateerror-to-scheduler-code
        self.internal_block_state
            .block_state()
            .tokens
            .token_configuration(&self.blob_store_load, *token)
            .unwrap()
    }

    fn token_circulating_supply(&self, token: &Self::Token) -> RawTokenAmount {
        // todo propagate block state error as part of https://linear.app/concordium/issue/COR-2346/push-blockstateerror-to-scheduler-code
        self.internal_block_state
            .block_state()
            .tokens
            .token_circulating_supply(&self.blob_store_load, *token)
            .unwrap()
    }

    fn lookup_token_state_value(
        &self,
        token_key_value: &Self::MutableTokenKeyValueState,
        key: &TokenStateKey,
    ) -> Option<TokenStateValue> {
        token_key_value
            .lookup_value(&self.blob_store_load, &key.0)
            .map(TokenStateValue)
    }

    fn iter_token_state_prefix<'a>(
        &'a self,
        token_key_value: &Self::MutableTokenKeyValueState,
        prefix: &TokenStateKey,
    ) -> impl Iterator<Item = (TokenStateKey, TokenStateValue)> + use<'a, IntState, Load, ExtState>
    {
        // todo propagate block state error as part of https://linear.app/concordium/issue/COR-2346/push-blockstateerror-to-scheduler-code
        token_key_value
            .iter_prefix(&self.blob_store_load, &prefix.0)
            .unwrap()
            .map(|entry| (TokenStateKey(entry.0), TokenStateValue(entry.1)))
    }

    fn update_token_state_value(
        &self,
        token_key_value_state: &mut Self::MutableTokenKeyValueState,
        key: &TokenStateKey,
        value: Option<TokenStateValue>,
    ) {
        // todo propagate block state error as part of https://linear.app/concordium/issue/COR-2346/push-blockstateerror-to-scheduler-code
        if let Some(value) = value {
            token_key_value_state
                .insert_value(&self.blob_store_load, &key.0, value.0)
                .unwrap();
        } else {
            token_key_value_state
                .delete_value(&self.blob_store_load, &key.0)
                .unwrap();
        }
    }

    fn account_by_address(
        &self,
        address: &AccountAddress,
    ) -> Result<Self::Account, AccountNotFoundByAddressError> {
        let index = self
            .external_block_state
            .account_index_by_account_address(address)?;

        Ok(index)
    }

    fn account_by_index(
        &self,
        index: AccountIndex,
    ) -> Result<AccountWithCanonicalAddress<Self::Account>, AccountNotFoundByIndexError> {
        let canonical_account_address = self
            .external_block_state
            .account_canonical_address_by_account_index(index)?;

        Ok(AccountWithCanonicalAddress {
            account: index,
            canonical_account_address,
        })
    }

    fn account_index(&self, account: &Self::Account) -> AccountIndex {
        *account
    }

    fn account_token_balance(
        &self,
        account: &Self::Account,
        token: &Self::Token,
    ) -> RawTokenAmount {
        self.external_block_state
            .read_token_account_balance(*account, *token)
    }

    fn token_account_states(
        &self,
        account: &Self::Account,
    ) -> impl Iterator<Item = (Self::Token, TokenAccountState)> {
        self.external_block_state
            .token_account_states(*account)
            .into_iter()
    }

    fn protocol_version(&self) -> ProtocolVersion {
        self.internal_block_state.protocol_version()
    }
}

impl<Load: BlobStoreLoad, ExtState: ExternalBlockStateOperations> BlockStateOperations
    for ExecutionTimeBlockState<MutableBlockState, Load, ExtState>
{
    fn set_token_circulating_supply(
        &mut self,
        token: &Self::Token,
        circulating_supply: RawTokenAmount,
    ) {
        // todo propagate block state error as part of https://linear.app/concordium/issue/COR-2346/push-blockstateerror-to-scheduler-code
        self.internal_block_state
            .update_block_state_(|state| {
                Ok(BlockStateData {
                    tokens: state.tokens.set_token_circulating_supply(
                        &self.blob_store_load,
                        *token,
                        circulating_supply,
                    )?,
                })
            })
            .unwrap();
    }

    fn create_token(&mut self, configuration: TokenConfiguration) -> Self::Token {
        // todo propagate block state error as part of https://linear.app/concordium/issue/COR-2346/push-blockstateerror-to-scheduler-code
        self.internal_block_state
            .update_block_state(|state| {
                let (token_index, tokens) = state
                    .tokens
                    .create_token(&self.blob_store_load, configuration)?;
                Ok((token_index, BlockStateData { tokens }))
            })
            .unwrap()
    }

    fn update_token_account_balance(
        &mut self,
        token: &Self::Token,
        account: &Self::Account,
        amount_delta: RawTokenAmountDelta,
    ) -> Result<(), OverflowError> {
        self.external_block_state
            .update_token_account_balance(*account, *token, amount_delta)
    }

    fn touch_token_account(&mut self, token: &Self::Token, account: &Self::Account) {
        self.external_block_state
            .touch_token_account(*account, *token);
    }

    fn increment_plt_update_instruction_sequence_number(&mut self) {
        self.external_block_state
            .increment_plt_update_sequence_number();
    }

    fn set_token_key_value_state(
        &mut self,
        token: &Self::Token,
        token_key_value_state: Self::MutableTokenKeyValueState,
    ) {
        // todo propagate block state error as part of https://linear.app/concordium/issue/COR-2346/push-blockstateerror-to-scheduler-code
        self.internal_block_state
            .update_block_state_(|state| {
                Ok(BlockStateData {
                    tokens: state.tokens.set_token_key_value_state(
                        &self.blob_store_load,
                        *token,
                        token_key_value_state,
                    )?,
                })
            })
            .unwrap();
    }
}
