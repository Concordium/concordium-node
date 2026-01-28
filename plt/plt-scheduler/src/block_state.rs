//! This module contains the [`PltBlockState`] which provides an implementation of [`BlockStateOperations`].
//!

use crate::block_state::blob_store::{BackingStoreLoad, BackingStoreStore, DecodeError};
use crate::block_state::external::{
    GetAccountIndexByAddress, GetCanonicalAddressByAccountIndex, IncrementPltUpdateSequenceNumber,
    ReadTokenAccountBalance, UpdateTokenAccountBalance,
};
use crate::block_state::types::{TokenAccountState, TokenConfiguration, TokenIndex};
use crate::block_state_interface::{
    BlockStateOperations, BlockStateQuery, OverflowError, RawTokenAmountDelta,
    TokenNotFoundByIdError,
};
use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::TokenId;
use plt_scheduler_interface::error::{AccountNotFoundByAddressError, AccountNotFoundByIndexError};
use plt_scheduler_interface::token_kernel_interface::{
    AccountWithCanonicalAddress, TokenStateKey, TokenStateValue,
};

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
/// unwrapping using [`PltBlockStateSavepoint::new_generation`] which creates a new generation.
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
        todo!()
    }

    /// Store a PLT block state in a blob store.
    pub fn store_update(&mut self, storer: &mut impl BackingStoreStore) -> blob_store::Reference {
        // todo
        storer.store_raw(&[])
    }

    /// Migrate the PLT block state from one blob store to another.
    pub fn migrate(
        &mut self,
        _loader: &mut impl BackingStoreLoad,
        _storer: &mut impl BackingStoreStore,
    ) -> Self {
        todo!()
    }

    /// Cache the block state in memory.
    pub fn cache(&mut self, _loader: &mut impl BackingStoreLoad) {
        todo!()
    }

    /// Construct a new generation block state which can be mutated without affecting this
    /// save-point.
    pub fn new_generation(&self) -> PltBlockState {
        let mut block_state = self.block_state.clone();
        block_state.generation += 1;
        block_state
    }
}

impl blob_store::Loadable for PltBlockStateSavepoint {
    fn load(
        _loader: &mut impl BackingStoreLoad,
        _source: &mut impl std::io::Read,
    ) -> Result<Self, DecodeError> {
        todo!()
    }
}

/// Block state providing the various block state operations.
#[derive(Debug, Clone)]
pub struct PltBlockState {
    /// The generation counter for the block state.
    generation: u64,
}

impl PltBlockState {
    /// Construct an empty block state.
    fn empty() -> Self {
        PltBlockState { generation: 0 }
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
pub struct ExecutionTimePltBlockState<L: BackingStoreLoad, T: BlockStateExternal> {
    /// The library block state implementation.
    pub inner_block_state: PltBlockState,
    /// External function for reading from the blob store.
    pub backing_store_load: L,
    /// External function for reading the token balance for an account.
    pub read_token_account_balance: T::ReadTokenAccountBalance,
    /// External function for updating the token balance for an account.
    pub update_token_account_balance: T::UpdateTokenAccountBalance,
    /// External function for incrementing the PLT update sequence number.
    pub increment_plt_update_sequence_number: T::IncrementPltUpdateSequenceNumber,
    /// External function for fetching account address by index.
    pub get_account_address_by_index: T::GetCanonicalAddressByAccountIndex,
    /// External function for fetching account index by address.
    pub get_account_index_by_address: T::GetAccountIndexByAddress,
}

/// Calls to externally managed parts of the block state.
pub trait BlockStateExternal {
    type ReadTokenAccountBalance: ReadTokenAccountBalance;
    type UpdateTokenAccountBalance: UpdateTokenAccountBalance;
    type IncrementPltUpdateSequenceNumber: IncrementPltUpdateSequenceNumber;
    type GetCanonicalAddressByAccountIndex: GetCanonicalAddressByAccountIndex;
    type GetAccountIndexByAddress: GetAccountIndexByAddress;
}

impl<L: BackingStoreLoad, T: BlockStateExternal> BlockStateQuery
    for ExecutionTimePltBlockState<L, T>
{
    type TokenKeyValueState = ();
    type Account = AccountIndex;
    type Token = TokenIndex;

    fn plt_list(&self) -> impl Iterator<Item = TokenId> {
        // TODO implement this. The implementation below is just to help the type checker infer
        // enough for this to compile.
        Vec::new().into_iter()
    }

    fn token_by_id(&self, _token_id: &TokenId) -> Result<Self::Token, TokenNotFoundByIdError> {
        todo!()
    }

    fn mutable_token_key_value_state(&self, _token_index: &TokenIndex) -> Self::TokenKeyValueState {
        todo!()
    }

    fn token_configuration(&self, _token: &Self::Token) -> TokenConfiguration {
        todo!()
    }

    fn token_circulating_supply(&self, _token: &Self::Token) -> RawTokenAmount {
        todo!()
    }

    fn lookup_token_state_value(
        &self,
        _token_key_value_state: &Self::TokenKeyValueState,
        _key: &TokenStateKey,
    ) -> Option<TokenStateValue> {
        todo!()
    }

    fn update_token_state_value(
        &self,
        _token_key_value_state: &mut Self::TokenKeyValueState,
        _key: &TokenStateKey,
        _value: Option<TokenStateValue>,
    ) {
        todo!()
    }

    fn account_by_address(
        &self,
        address: &AccountAddress,
    ) -> Result<Self::Account, AccountNotFoundByAddressError> {
        let index = self
            .get_account_index_by_address
            .account_index_by_account_address(address)?;

        Ok(index)
    }

    fn account_by_index(
        &self,
        index: AccountIndex,
    ) -> Result<AccountWithCanonicalAddress<Self::Account>, AccountNotFoundByIndexError> {
        let canonical_account_address = self
            .get_account_address_by_index
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
        self.read_token_account_balance
            .read_token_account_balance(*account, *token)
    }

    fn token_account_states(
        &self,
        _account: &Self::Account,
    ) -> impl Iterator<Item = (Self::Token, TokenAccountState)> {
        // TODO implement this. The implementation below is just to help the type checker infer
        // enough for this to compile.
        Vec::new().into_iter()
    }
}

impl<L: BackingStoreLoad, T: BlockStateExternal> BlockStateOperations
    for ExecutionTimePltBlockState<L, T>
{
    fn set_token_circulating_supply(
        &mut self,
        _token: &Self::Token,
        _circulating_supply: RawTokenAmount,
    ) {
        todo!()
    }

    fn create_token(&mut self, _configuration: TokenConfiguration) -> Self::Token {
        todo!()
    }

    fn update_token_account_balance(
        &mut self,
        token: &Self::Token,
        account: &Self::Account,
        amount_delta: RawTokenAmountDelta,
    ) -> Result<(), OverflowError> {
        self.update_token_account_balance
            .update_token_account_balance(*account, *token, amount_delta)
    }

    fn increment_plt_update_instruction_sequence_number(&mut self) {
        self.increment_plt_update_sequence_number
            .increment_plt_update_sequence_number();
    }

    fn set_token_key_value_state(
        &mut self,
        _token: &Self::Token,
        _token_key_value_state: Self::TokenKeyValueState,
    ) {
        todo!()
    }
}
