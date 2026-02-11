//! This module contains the [`PltBlockState`] which provides an implementation of [`BlockStateOperations`].
//!

use crate::block_state::blob_store::{BackingStoreLoad, BackingStoreStore, DecodeError};
use crate::block_state::external::{
    GetAccountIndexByAddress, GetCanonicalAddressByAccountIndex, GetTokenAccountStates,
    IncrementPltUpdateSequenceNumber, ReadTokenAccountBalance, UpdateTokenAccountBalance,
};
use crate::block_state::types::{TokenAccountState, TokenConfiguration, TokenIndex};
use crate::block_state_interface::{
    BlockStateOperations, BlockStateQuery, OverflowError, RawTokenAmountDelta,
    TokenNotFoundByIdError,
};
use concordium_base::base::AccountIndex;
use concordium_base::common;
use concordium_base::common::Serialize;
use concordium_base::constants::SHA256;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::TokenId;
use plt_scheduler_interface::error::{AccountNotFoundByAddressError, AccountNotFoundByIndexError};
use plt_scheduler_interface::token_kernel_interface::{
    AccountWithCanonicalAddress, TokenStateKey, TokenStateValue,
};
use plt_types::types::tokens::RawTokenAmount;
use sha2::Digest;
use std::collections::BTreeMap;

pub mod blob_store;
pub mod external;
pub mod types;

/// Marker for PLT block state hash type.
pub enum PltBlockStateHashMarker {}
/// Hash of PLT block state
pub type PltBlockStateHash = concordium_base::hashes::HashBytes<PltBlockStateHashMarker>;

/// Immutable block state save-point.
///
/// This is a wrapper around a [`PltBlockState`] ensuring further mutations can only be done by
/// unwrapping using [`PltBlockStateSavepoint::mutable_state`] which creates a new generation.
#[derive(Debug)]
pub struct PltBlockStateSavepoint {
    /// The inner block state, which will not be mutated further for this generation.
    block_state: PltBlockState,
}

impl PltBlockStateSavepoint {
    /// Initialize a new block state.
    pub fn empty() -> Self {
        Self {
            block_state: PltBlockState::empty(),
        }
    }

    /// Compute the hash.
    pub fn hash(&self, _loader: &mut impl BackingStoreLoad) -> PltBlockStateHash {
        // todo do real implementation as part of https://linear.app/concordium/issue/PSR-11/port-the-plt-block-state-to-rust
        let block_state_bytes = common::to_bytes(&self.block_state.state);
        PltBlockStateHash::from(<[u8; SHA256]>::from(sha2::Sha256::digest(
            &block_state_bytes,
        )))
    }

    /// Store a PLT block state in a blob store.
    pub fn store_update(&mut self, storer: &mut impl BackingStoreStore) -> blob_store::Reference {
        // todo do real implementation as part of https://linear.app/concordium/issue/PSR-11/port-the-plt-block-state-to-rust
        let block_state_bytes = common::to_bytes(&self.block_state.state);
        storer.store_raw(&block_state_bytes)
    }

    /// Migrate the PLT block state from one blob store to another.
    pub fn migrate(
        &mut self,
        _loader: &mut impl BackingStoreLoad,
        _storer: &mut impl BackingStoreStore,
    ) -> Self {
        // todo implement as part of https://linear.app/concordium/issue/PSR-11/port-the-plt-block-state-to-rust
        todo!()
    }

    /// Cache the block state in memory.
    pub fn cache(&mut self, _loader: &mut impl BackingStoreLoad) {
        // todo implement as part of https://linear.app/concordium/issue/PSR-11/port-the-plt-block-state-to-rust
    }

    /// Construct a mutable block state which can be mutated without affecting this
    /// save-point.
    pub fn mutable_state(&self) -> PltBlockState {
        self.block_state.clone()
    }
}

impl blob_store::Loadable for PltBlockStateSavepoint {
    fn load(
        _loader: &mut impl BackingStoreLoad,
        source: impl AsRef<[u8]>,
    ) -> Result<Self, DecodeError> {
        // todo do real implementation as part of https://linear.app/concordium/issue/PSR-11/port-the-plt-block-state-to-rust
        let state: SimplisticPltBlockState = common::from_bytes_complete(source)
            .map_err(|err| DecodeError::Decode(err.to_string()))?;

        Ok(Self {
            block_state: PltBlockState { state },
        })
    }
}

/// Block state providing the various block state operations.
#[derive(Debug, Clone)]
pub struct PltBlockState {
    // todo implement real block state as part of https://linear.app/concordium/issue/PSR-11/port-the-plt-block-state-to-rust
    /// Simplistic state that is used as a temporary implementation of the block state
    state: SimplisticPltBlockState,
}

impl PltBlockState {
    /// Construct an empty block state.
    fn empty() -> Self {
        PltBlockState {
            state: Default::default(),
        }
    }

    /// Consume the mutable block state and create an immutable save-point.
    pub fn savepoint(self) -> PltBlockStateSavepoint {
        PltBlockStateSavepoint { block_state: self }
    }
}

/// Runtime/execution state relevant for providing an implementation of
/// [`BlockStateOperations`].
///
/// In addition to the PLT block state, this type contains callbacks
/// for the parts of the state that is managed on the Haskell side.
#[derive(Debug)]
pub struct ExecutionTimePltBlockState<L, T> {
    /// The library block state implementation.
    pub inner_block_state: PltBlockState,
    /// External function for reading from the blob store.
    pub backing_store_load: L,
    /// Part of block state that is managed externally.
    pub external_block_state: T,
}

/// Type definition for calls to externally managed parts of the block state.
pub trait ExternalBlockState:
    ReadTokenAccountBalance
    + UpdateTokenAccountBalance
    + IncrementPltUpdateSequenceNumber
    + GetCanonicalAddressByAccountIndex
    + GetAccountIndexByAddress
    + GetTokenAccountStates
{
}

impl<L: BackingStoreLoad, T: ExternalBlockState> BlockStateQuery
    for ExecutionTimePltBlockState<L, T>
{
    type TokenKeyValueState = SimplisticTokenKeyValueState;
    type Account = AccountIndex;
    type Token = TokenIndex;

    fn plt_list(&self) -> impl Iterator<Item = TokenId> {
        self.inner_block_state
            .state
            .tokens
            .iter()
            .map(|token| token.configuration.token_id.clone())
    }

    fn token_by_id(&self, token_id: &TokenId) -> Result<Self::Token, TokenNotFoundByIdError> {
        self.inner_block_state
            .state
            .tokens
            .iter()
            .enumerate()
            .find_map(|(i, token)| {
                if token
                    .configuration
                    .token_id
                    .as_ref()
                    .eq_ignore_ascii_case(token_id.as_ref())
                {
                    Some(TokenIndex(i as u64))
                } else {
                    None
                }
            })
            .ok_or(TokenNotFoundByIdError(token_id.clone()))
    }

    fn mutable_token_key_value_state(&self, token: &TokenIndex) -> Self::TokenKeyValueState {
        self.inner_block_state.state.tokens[token.0 as usize]
            .key_value_state
            .clone()
    }

    fn token_configuration(&self, token: &Self::Token) -> TokenConfiguration {
        let configuration = self.inner_block_state.state.tokens[token.0 as usize]
            .configuration
            .clone();

        TokenConfiguration {
            token_id: configuration.token_id,
            module_ref: configuration.module_ref,
            decimals: configuration.decimals,
        }
    }

    fn token_circulating_supply(&self, token: &Self::Token) -> RawTokenAmount {
        self.inner_block_state.state.tokens[token.0 as usize].circulating_supply
    }

    fn lookup_token_state_value(
        &self,
        token_key_value_state: &Self::TokenKeyValueState,
        key: &TokenStateKey,
    ) -> Option<TokenStateValue> {
        token_key_value_state.state.get(key).cloned()
    }

    fn update_token_state_value(
        &self,
        token_key_value_state: &mut Self::TokenKeyValueState,
        key: &TokenStateKey,
        value: Option<TokenStateValue>,
    ) {
        if let Some(value) = value {
            token_key_value_state.state.insert(key.clone(), value);
        } else {
            token_key_value_state.state.remove(key);
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
}

impl<L: BackingStoreLoad, T: ExternalBlockState> BlockStateOperations
    for ExecutionTimePltBlockState<L, T>
{
    fn set_token_circulating_supply(
        &mut self,
        token: &Self::Token,
        circulating_supply: RawTokenAmount,
    ) {
        self.inner_block_state.state.tokens[token.0 as usize].circulating_supply =
            circulating_supply;
    }

    fn create_token(&mut self, configuration: TokenConfiguration) -> Self::Token {
        let token_index = TokenIndex(self.inner_block_state.state.tokens.len() as u64);
        let token = Token {
            key_value_state: Default::default(),
            configuration,
            circulating_supply: Default::default(),
        };
        self.inner_block_state.state.tokens.push(token);
        token_index
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

    fn increment_plt_update_instruction_sequence_number(&mut self) {
        self.external_block_state
            .increment_plt_update_sequence_number();
    }

    fn set_token_key_value_state(
        &mut self,
        token: &Self::Token,
        token_key_value_state: Self::TokenKeyValueState,
    ) {
        self.inner_block_state.state.tokens[token.0 as usize].key_value_state =
            token_key_value_state;
    }
}

/// Simplistic implementation of the block state that serializes as a flat sequence of bytes.
// todo do real implementation as part of https://linear.app/concordium/issue/PSR-11/port-the-plt-block-state-to-rust
#[derive(Debug, Default, Clone, Serialize)]
struct SimplisticPltBlockState {
    tokens: Vec<Token>,
}

#[derive(Debug, Clone, Serialize)]
struct Token {
    key_value_state: SimplisticTokenKeyValueState,
    configuration: TokenConfiguration,
    circulating_supply: RawTokenAmount,
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct SimplisticTokenKeyValueState {
    state: BTreeMap<TokenStateKey, TokenStateValue>,
}
