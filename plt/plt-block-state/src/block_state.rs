//! This module contains the [`BlockState`] which provides an implementation of [`BlockStateOperations`].

use crate::block_state::blob_store::{
    BackingStoreLoad, BackingStoreStore, DecodeError, Loadable, Storable,
};
use crate::block_state::cacheable::Cacheable;
use crate::block_state::external::{ExternalBlockStateOperations, ExternalBlockStateQuery};
use crate::block_state::hash::Hashable;
use crate::block_state::state::protocol_level_tokens::{
    ProtocolLevelTokens, SimplisticTokenKeyValueState,
};
use crate::block_state::types::AccountWithCanonicalAddress;
use crate::block_state::types::protocol_level_tokens::{
    TokenAccountState, TokenConfiguration, TokenIndex, TokenStateKey, TokenStateValue,
};
use crate::block_state_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, BlockStateOperations,
    BlockStateQuery, OverflowError, RawTokenAmountDelta, TokenNotFoundByIdError,
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
mod lfmb_tree;
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
#[derive(Debug, Clone, Default)]
pub struct BlockState {
    /// Simplistic state that is used as a temporary implementation of the block state
    tokens: ProtocolLevelTokens,
}

impl BlockState {
    /// Construct an empty block state.
    pub fn empty() -> Self {
        BlockState {
            tokens: ProtocolLevelTokens::empty(),
        }
    }

    /// Consume the immutable block state and create a mutable block state.
    pub fn into_mutable(self) -> MutableBlockState {
        MutableBlockState::new(self)
    }

    /// Migrate the PLT block state from one blob store to another.
    pub fn migrate(&self, _loader: impl BackingStoreLoad, _storer: impl BackingStoreStore) -> Self {
        // todo implement as part of https://linear.app/concordium/issue/PSR-11/port-the-plt-block-state-to-rust
        todo!()
    }
}

impl Loadable for BlockState {
    fn load_from_buffer(mut buffer: impl Read) -> Result<Self, DecodeError> {
        let tokens = Loadable::load_from_buffer(&mut buffer)?;

        Ok(Self { tokens })
    }
}

impl Storable for BlockState {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BackingStoreStore) {
        self.tokens.store_to_buffer(&mut buffer, storer);
    }
}

impl Cacheable for BlockState {
    fn cache_reference_values(&self, loader: &impl BackingStoreLoad) -> Result<(), DecodeError> {
        self.tokens.cache_reference_values(loader)?;
        Ok(())
    }
}

impl Hashable for BlockState {
    fn hash(&self, loader: &impl BackingStoreLoad) -> Result<Hash, DecodeError> {
        self.tokens.hash(loader)
    }
}

/// Mutable block state. In contracts to the immutable block state [`BlockState`],
/// operations on the mutable block state changes the state that
/// the value represents.
#[derive(Debug, Clone)]
pub struct MutableBlockState {
    /// Immutable block state value. The block state represented by [`MutableBlockState`] is
    /// mutated simply by setting a new value for the immutable block state [`BlockState`].
    immutable_state: BlockState,
}

impl MutableBlockState {
    fn new(mutable_state: BlockState) -> Self {
        Self {
            immutable_state: mutable_state,
        }
    }

    pub fn into_immutable(self) -> BlockState {
        self.immutable_state
    }

    fn update_block_state_(&mut self, update: impl FnOnce(BlockState) -> BlockState) {
        self.immutable_state = update(mem::take(&mut self.immutable_state));
    }

    fn update_block_state<T>(&mut self, update: impl FnOnce(BlockState) -> (T, BlockState)) -> T {
        let ret;
        (ret, self.immutable_state) = update(mem::take(&mut self.immutable_state));
        ret
    }
}

/// Runtime/execution state relevant for providing an implementation of
/// [`BlockStateQuery`] and [`BlockStateOperations`].
///
/// In addition to the PLT block state, this type contains callbacks
/// for the parts of the state that is managed on the Haskell side.
#[derive(Debug)]
pub struct ExecutionTimeBlockState<IntState, Load, ExtState> {
    /// The protocol version of the block state.
    pub protocol_version: ProtocolVersion,
    /// The library block state implementation.
    pub internal_block_state: IntState,
    /// External function for reading from the blob store.
    pub backing_store_load: Load,
    /// Part of block state that is managed externally.
    pub external_block_state: ExtState,
}

/// Provides access needed for querying block state (but not to do operations on the block state).
trait HasBlockState {
    fn block_state(&self) -> &BlockState;
}

impl HasBlockState for &BlockState {
    fn block_state(&self) -> &BlockState {
        self
    }
}

impl HasBlockState for MutableBlockState {
    fn block_state(&self) -> &BlockState {
        &self.immutable_state
    }
}

impl<IntState: HasBlockState, Load: BackingStoreLoad, ExtState: ExternalBlockStateQuery>
    BlockStateQuery for ExecutionTimeBlockState<IntState, Load, ExtState>
{
    type TokenKeyValueState = SimplisticTokenKeyValueState;
    type Account = AccountIndex;
    type Token = TokenIndex;

    fn plt_list(&self) -> impl Iterator<Item = TokenId> {
        self.internal_block_state
            .block_state()
            .tokens
            .plt_list(&self.backing_store_load)
    }

    fn token_by_id(&self, token_id: &TokenId) -> Result<Self::Token, TokenNotFoundByIdError> {
        // todo ar look up via normalized token id map
        // self.internal_block_state
        //     .block_state()
        //     .tokens
        //     .iter()
        //     .enumerate()
        //     .find_map(|(i, token)| {
        //         if token
        //             .configuration
        //             .token_id
        //             .as_ref()
        //             .eq_ignore_ascii_case(token_id.as_ref())
        //         {
        //             Some(TokenIndex(i as u64))
        //         } else {
        //             None
        //         }
        //     })
        //     .ok_or(TokenNotFoundByIdError(token_id.clone()))
        todo!()
    }

    fn mutable_token_key_value_state(&self, token: &TokenIndex) -> Self::TokenKeyValueState {
        self.internal_block_state
            .block_state()
            .tokens
            .mutable_token_key_value_state(&self.backing_store_load, *token)
    }

    fn token_configuration(&self, token: &Self::Token) -> TokenConfiguration {
        self.internal_block_state
            .block_state()
            .tokens
            .token_configuration(&self.backing_store_load, *token)
    }

    fn token_circulating_supply(&self, token: &Self::Token) -> RawTokenAmount {
        self.internal_block_state
            .block_state()
            .tokens
            .token_circulating_supply(&self.backing_store_load, *token)
    }

    fn lookup_token_state_value(
        &self,
        token_key_value_state: &Self::TokenKeyValueState,
        key: &TokenStateKey,
    ) -> Option<TokenStateValue> {
        token_key_value_state.lookup_value(key)
    }

    fn update_token_state_value(
        &self,
        token_key_value_state: &mut Self::TokenKeyValueState,
        key: &TokenStateKey,
        value: Option<TokenStateValue>,
    ) {
        token_key_value_state.update_value(key, value)
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
        self.protocol_version
    }
}

impl<Load: BackingStoreLoad, ExtState: ExternalBlockStateOperations> BlockStateOperations
    for ExecutionTimeBlockState<MutableBlockState, Load, ExtState>
{
    fn set_token_circulating_supply(
        &mut self,
        token: &Self::Token,
        circulating_supply: RawTokenAmount,
    ) {
        self.internal_block_state
            .update_block_state_(|state| BlockState {
                tokens: state.tokens.set_token_circulating_supply(
                    &self.backing_store_load,
                    *token,
                    circulating_supply,
                ),
            });
    }

    fn create_token(&mut self, configuration: TokenConfiguration) -> Self::Token {
        self.internal_block_state.update_block_state(|state| {
            let (token_index, tokens) = state
                .tokens
                .create_token(&self.backing_store_load, configuration);
            (token_index, BlockState { tokens })
        })
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
        token_key_value_state: Self::TokenKeyValueState,
    ) {
        self.internal_block_state
            .update_block_state_(|state| BlockState {
                tokens: state.tokens.set_token_key_value_state(
                    &self.backing_store_load,
                    *token,
                    token_key_value_state,
                ),
            });
    }
}
