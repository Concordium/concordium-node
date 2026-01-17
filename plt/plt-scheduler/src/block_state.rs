//! This module contains the [`BlockState`] which provides an implementation of [`BlockStateOperations`].
//!

use crate::block_state::blob_store::{BackingStoreLoad, BackingStoreStore, DecodeError};
use crate::block_state_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, BlockStateOperations,
    BlockStateQuery, OverflowError, RawTokenAmountDelta, TokenConfiguration,
    TokenNotFoundByIdError,
};
use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::TokenId;
use plt_token_module::token_kernel_interface::{ModuleStateKey, ModuleStateValue, RawTokenAmount};

pub mod blob_store;

/// Index of the protocol-level token in the block state map of tokens.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TokenIndex(pub u64);

pub enum PltBlockStateHashMarker {}
pub type PltBlockStateHash = concordium_base::hashes::HashBytes<PltBlockStateHashMarker>;

/// Immutable block state save-point.
///
/// This is a safe wrapper around a [`BlockState`] ensuring further mutations can only be done by
/// unwrapping using [`BlockStateSavepoint::new_generation`] which creates a new generation.
#[derive(Debug)]
pub struct BlockStateSavepoint {
    /// The inner block state, which will not be mutated further for this generation.
    block_state: BlockState,
}

impl BlockStateSavepoint {
    /// Initialize a new block state.
    pub fn empty() -> Self {
        Self {
            block_state: BlockState::empty(),
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
    pub fn new_generation(&self) -> BlockState {
        let mut block_state = self.block_state.clone();
        block_state.generation += 1;
        block_state
    }
}

impl blob_store::Loadable for BlockStateSavepoint {
    fn load(
        _loader: &mut impl BackingStoreLoad,
        _source: &mut impl std::io::Read,
    ) -> Result<Self, DecodeError> {
        todo!()
    }
}

/// Block state providing the various block state operations.
#[derive(Debug, Clone)]
pub struct BlockState {
    /// The generation counter for the block state.
    generation: u64,
}

impl BlockState {
    /// Construct an empty block state.
    fn empty() -> Self {
        BlockState { generation: 0 }
    }

    /// Consume the mutable block state and create an immutable save-point.
    pub fn savepoint(self) -> BlockStateSavepoint {
        BlockStateSavepoint { block_state: self }
    }
}

/// Trait allowing reading the account token balance from the block state.
/// The account token balance block state is currently managed in Haskell.
///
/// # Arguments
///
/// - `account_index` The index of the account to update a token balance for. Must be a valid account index of an existing account.
/// - `token_index` The index of the token. Must be a valid token index of an existing token.
pub trait ReadTokenAccountBalance {
    /// Change the account.
    fn read_token_account_balance(
        &self,
        account: AccountIndex,
        token: TokenIndex,
    ) -> RawTokenAmount;
}

/// Trait allowing updating the account token balance in the block state.
/// The account token balance block state is currently managed in Haskell.
///
/// # Arguments
///
/// - `account_index` The index of the account to update a token balance for. Must be a valid account index of an existing account.
/// - `token_index` The index of the token. Must be a valid token index of an existing token.
/// - `amount_delta` The amount to add to or subtract from the balance.
pub trait UpdateTokenAccountBalance {
    /// Change the account.
    fn update_token_account_balance(
        &mut self,
        account: AccountIndex,
        token: TokenIndex,
        amount_delta: RawTokenAmountDelta,
    ) -> Result<(), OverflowError>;
}

/// Runtime/execution state relevant for providing an implementation of
/// [`BlockStateOperations`].
///
/// This is needed since callbacks are only available during the execution time.
#[derive(Debug)]
pub struct ExecutionTimeBlockState<T: BlockStateExternal> {
    /// The library block state implementation.
    pub inner_block_state: BlockState,
    // Temporary disable warning until we have the implementation below started.
    #[expect(dead_code)]
    /// External function for reading from the blob store.
    pub load_callback: T::BackingStoreLoad,
    /// External function for reading the token balance for an account.
    pub read_token_account_balance_callback: T::ReadTokenAccountBalance,
    /// External function for updating the token balance for an account.
    pub update_token_account_balance_callback: T::UpdateTokenAccountBalance,
}

pub trait BlockStateExternal {
    type BackingStoreLoad: BackingStoreLoad;
    type ReadTokenAccountBalance: ReadTokenAccountBalance;
    type UpdateTokenAccountBalance: UpdateTokenAccountBalance;
}

impl<T: BlockStateExternal> BlockStateQuery for ExecutionTimeBlockState<T> {
    type MutableTokenModuleState = ();
    type Account = (AccountIndex, AccountAddress);
    type Token = TokenIndex;

    fn plt_list(&self) -> impl Iterator<Item = TokenId> {
        // TODO implement this. The implementation below is just to help the type checker infer
        // enough for this to compile.
        Vec::new().into_iter()
    }

    fn token_by_id(&self, _token_id: &TokenId) -> Result<Self::Token, TokenNotFoundByIdError> {
        todo!()
    }

    fn mutable_token_module_state(&self, _token: &Self::Token) -> Self::MutableTokenModuleState {
        todo!()
    }

    fn token_configuration(&self, _token: &Self::Token) -> TokenConfiguration {
        todo!()
    }

    fn token_circulating_supply(&self, _token: &Self::Token) -> RawTokenAmount {
        todo!()
    }

    fn lookup_token_module_state_value(
        &self,
        _token_module_state: &Self::MutableTokenModuleState,
        _key: &ModuleStateKey,
    ) -> Option<ModuleStateValue> {
        todo!()
    }

    fn update_token_module_state_value(
        &self,
        _token_module_state: &mut Self::MutableTokenModuleState,
        _key: &ModuleStateKey,
        _value: Option<ModuleStateValue>,
    ) {
        todo!()
    }

    fn account_by_address(
        &self,
        _address: &AccountAddress,
    ) -> Result<Self::Account, AccountNotFoundByAddressError> {
        todo!()
    }

    fn account_by_index(
        &self,
        _index: AccountIndex,
    ) -> Result<Self::Account, AccountNotFoundByIndexError> {
        todo!()
    }

    fn account_index(&self, account: &Self::Account) -> AccountIndex {
        account.0
    }

    fn account_canonical_address(&self, account: &Self::Account) -> AccountAddress {
        account.1
    }

    fn account_token_balance(
        &self,
        account: &Self::Account,
        token: &Self::Token,
    ) -> RawTokenAmount {
        self.read_token_account_balance_callback
            .read_token_account_balance(account.0, *token)
    }
}

impl<T: BlockStateExternal> BlockStateOperations for ExecutionTimeBlockState<T> {
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
        self.update_token_account_balance_callback
            .update_token_account_balance(account.0, *token, amount_delta)
    }

    fn touch_token_account(&mut self, _token: &Self::Token, _account: &Self::Account) {
        todo!()
    }

    fn increment_plt_update_instruction_sequence_number(&mut self) {
        todo!()
    }

    fn set_token_module_state(
        &mut self,
        _token: &Self::Token,
        _mutable_token_module_state: Self::MutableTokenModuleState,
    ) {
        todo!()
    }
}
