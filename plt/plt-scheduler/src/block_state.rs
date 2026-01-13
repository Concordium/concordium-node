//! This module contains the [`BlockState`] which provides an implementation of [`BlockStateOperations`].
//!

use crate::block_state::blob_store::BackingStoreLoad;
use crate::block_state_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, BlockStateOperations,
    BlockStateQuery, RawTokenAmountDelta, TokenConfiguration, TokenNotFoundByIdError,
    UnderOrOverflowError,
};
use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::TokenId;
use plt_token_module::token_kernel_interface::{ModuleStateKey, ModuleStateValue, RawTokenAmount};

pub mod blob_store;
#[cfg(feature = "ffi")]
pub mod ffi;

/// Index of the protocol-level token in the block state map of tokens.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TokenIndex(u64);

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
    pub fn hash(&self, _loader: impl blob_store::BackingStoreLoad) -> PltBlockStateHash {
        todo!()
    }

    /// Store a PLT block state in a blob store.
    pub fn store_update(
        &mut self,
        _storer: &mut impl blob_store::BackingStoreStore,
    ) -> blob_store::StoreResult<blob_store::Reference> {
        todo!()
    }

    /// Migrate the PLT block state from one blob store to another.
    pub fn migrate(
        &mut self,
        _loader: &impl blob_store::BackingStoreLoad,
        _storer: &mut impl blob_store::BackingStoreStore,
    ) -> blob_store::LoadStoreResult<Self> {
        todo!()
    }

    /// Cache the block state in memory.
    pub fn cache(&mut self, _loader: &impl blob_store::BackingStoreLoad) {
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
    fn load<S: std::io::Read, F: blob_store::BackingStoreLoad>(
        _loader: &mut F,
        _source: &mut S,
    ) -> blob_store::LoadResult<Self> {
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
pub trait ReadTokenAccountBalanceFromBlockState {
    /// Change the account.
    fn read_token_account_balance(
        &self,
        account: AccountIndex,
        token: TokenIndex,
    ) -> RawTokenAmount;
}

/// Trait allowing updating the account token balance in the block state.
/// The account token balance block state is currently managed in Haskell.
pub trait UpdateTokenAccountBalanceInBlockState {
    /// Change the account.
    fn update_token_account_balance(
        &mut self,
        account: AccountIndex,
        token: TokenIndex,
        amount_delta: RawTokenAmountDelta,
    ) -> Result<(), UnderOrOverflowError>;
}

/// Runtime/execution state relevant for providing an implementation of
/// [`BlockStateOperations`].
///
/// This is needed since callbacks are only available during the execution time.
#[derive(Debug)]
pub struct ExecutionTimeBlockState<L, R, U> {
    /// The library block state implementation.
    pub inner_block_state: BlockState,
    // Temporary disable warning until we have the implementation below started.
    #[expect(dead_code)]
    /// External function for reading from the blob store.
    pub load_callback: L,
    /// External function for reading the token balance for an account.
    pub read_token_account_balance_callback: R,
    /// External function for updating the token balance for an account.
    pub update_token_account_balance_callback: U,
}

impl<
    L: BackingStoreLoad,
    R: ReadTokenAccountBalanceFromBlockState,
    U: UpdateTokenAccountBalanceInBlockState,
> BlockStateQuery for ExecutionTimeBlockState<L, R, U>
{
    type MutableTokenModuleState = ();
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

    fn account_index(&self, _account: &Self::Account) -> AccountIndex {
        todo!()
    }

    fn account_canonical_address(&self, _account: &Self::Account) -> AccountAddress {
        todo!()
    }

    fn account_token_balance(
        &self,
        account: &Self::Account,
        token: &Self::Token,
    ) -> RawTokenAmount {
        self.read_token_account_balance_callback
            .read_token_account_balance(*account, *token)
    }
}

impl<
    L: BackingStoreLoad,
    R: ReadTokenAccountBalanceFromBlockState,
    U: UpdateTokenAccountBalanceInBlockState,
> BlockStateOperations for ExecutionTimeBlockState<L, R, U>
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
    ) -> Result<(), UnderOrOverflowError> {
        self.update_token_account_balance_callback
            .update_token_account_balance(*account, *token, amount_delta)
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
