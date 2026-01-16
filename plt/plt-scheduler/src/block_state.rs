//! This module contains the [`BlockState`] which provides an implementation of [`BlockStateOperations`].
//!

use crate::block_state::blob_store::{BackingStoreLoad, BackingStoreStore, LoadError};
use crate::block_state_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, BlockStateOperations,
    BlockStateQuery, RawTokenAmountDelta, TokenConfiguration, TokenNotFoundByIdError,
    UnderOrOverflowError,
};
use concordium_base::base::AccountIndex;
use concordium_base::common;
use concordium_base::common::__serialize_private::anyhow::Context;
use concordium_base::common::{Buffer, Deserial, ParseResult, ReadBytesExt, Serial, Serialize};
use concordium_base::constants::SHA256;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{TokenId, TokenModuleRef};
use plt_token_module::token_kernel_interface::{ModuleStateKey, ModuleStateValue, RawTokenAmount};
use sha2::Digest;
use std::collections::BTreeMap;

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
    pub fn hash(&self, _loader: impl BackingStoreLoad) -> PltBlockStateHash {
        // todo do real implementation as part of https://linear.app/concordium/issue/PSR-11/port-the-plt-block-state-to-rust
        let block_state_bytes = common::to_bytes(&self.block_state.state);
        PltBlockStateHash::from(<[u8; SHA256]>::from(sha2::Sha256::digest(
            &block_state_bytes,
        )))
    }

    /// Store a PLT block state in a blob store.
    pub fn store_update(
        &mut self,
        storer: &mut impl BackingStoreStore,
    ) -> blob_store::StoreResult<blob_store::Reference> {
        // todo do real implementation as part of https://linear.app/concordium/issue/PSR-11/port-the-plt-block-state-to-rust
        let block_state_bytes = common::to_bytes(&self.block_state.state);
        storer.store_raw(&block_state_bytes)
    }

    /// Migrate the PLT block state from one blob store to another.
    pub fn migrate(
        &mut self,
        _loader: &impl blob_store::BackingStoreLoad,
        _storer: &mut impl blob_store::BackingStoreStore,
    ) -> blob_store::LoadStoreResult<Self> {
        // todo implement as part of https://linear.app/concordium/issue/PSR-11/port-the-plt-block-state-to-rust
        todo!()
    }

    /// Cache the block state in memory.
    pub fn cache(&mut self, _loader: &impl BackingStoreLoad) {
        // todo implement as part of https://linear.app/concordium/issue/PSR-11/port-the-plt-block-state-to-rust
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
    fn load<S: std::io::Read, F: BackingStoreLoad>(
        _loader: &mut F,
        mut source: &mut S,
    ) -> blob_store::LoadResult<Self> {
        // todo do real implementation as part of https://linear.app/concordium/issue/PSR-11/port-the-plt-block-state-to-rust
        let state: SimplisticBlockState =
            common::from_bytes(&mut source).map_err(|err| LoadError::Decode(err.to_string()))?;

        Ok(Self {
            block_state: BlockState {
                generation: 0,
                state,
            },
        })
    }
}

/// Block state providing the various block state operations.
#[derive(Debug, Clone)]
pub struct BlockState {
    /// The generation counter for the block state.
    generation: u64,
    /// Simplistic state that is used as a temporary implementation of the block state
    state: SimplisticBlockState,
}

impl BlockState {
    /// Construct an empty block state.
    fn empty() -> Self {
        BlockState {
            generation: 0,
            state: Default::default(),
        }
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
    type MutableTokenModuleState = SimplisticTokenModuleState;
    type Account = (AccountIndex, AccountAddress);
    type Token = TokenIndex;

    fn plt_list(&self) -> impl Iterator<Item = TokenId> {
        self.inner_block_state
            .state
            .tokens
            .iter()
            .map(|token| token.configuration.token_id.0.clone())
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
                    .0
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

    fn mutable_token_module_state(&self, token: &Self::Token) -> Self::MutableTokenModuleState {
        self.inner_block_state.state.tokens[token.0 as usize]
            .module_state
            .clone()
    }

    fn token_configuration(&self, token: &Self::Token) -> TokenConfiguration {
        let configuration = self.inner_block_state.state.tokens[token.0 as usize]
            .configuration
            .clone();

        TokenConfiguration {
            token_id: configuration.token_id.0,
            module_ref: configuration.module_ref,
            decimals: configuration.decimals,
        }
    }

    fn token_circulating_supply(&self, token: &Self::Token) -> RawTokenAmount {
        self.inner_block_state.state.tokens[token.0 as usize]
            .circulating_supply
            .0
    }

    fn lookup_token_module_state_value(
        &self,
        token_module_state: &Self::MutableTokenModuleState,
        key: &ModuleStateKey,
    ) -> Option<ModuleStateValue> {
        token_module_state.state.get(key).cloned()
    }

    fn update_token_module_state_value(
        &self,
        token_module_state: &mut Self::MutableTokenModuleState,
        key: &ModuleStateKey,
        value: Option<ModuleStateValue>,
    ) {
        if let Some(value) = value {
            token_module_state.state.insert(key.clone(), value);
        } else {
            token_module_state.state.remove(key);
        }
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

impl<
    L: BackingStoreLoad,
    R: ReadTokenAccountBalanceFromBlockState,
    U: UpdateTokenAccountBalanceInBlockState,
> BlockStateOperations for ExecutionTimeBlockState<L, R, U>
{
    fn set_token_circulating_supply(
        &mut self,
        token: &Self::Token,
        circulating_supply: RawTokenAmount,
    ) {
        self.inner_block_state.state.tokens[token.0 as usize]
            .circulating_supply
            .0 = circulating_supply;
    }

    fn create_token(&mut self, configuration: TokenConfiguration) -> Self::Token {
        let token_index = TokenIndex(self.inner_block_state.state.tokens.len() as u64);
        let configuration = TokenConfigurationSerialize {
            token_id: TokenIdSerialize(configuration.token_id),
            module_ref: configuration.module_ref,
            decimals: configuration.decimals,
        };
        let token = Token {
            module_state: Default::default(),
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
    ) -> Result<(), UnderOrOverflowError> {
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
        token: &Self::Token,
        mutable_token_module_state: Self::MutableTokenModuleState,
    ) {
        self.inner_block_state.state.tokens[token.0 as usize].module_state =
            mutable_token_module_state;
    }
}

#[derive(Debug, Default, Clone, Serialize)]
struct SimplisticBlockState {
    tokens: Vec<Token>,
}

#[derive(Debug, Clone, Serialize)]
struct Token {
    module_state: SimplisticTokenModuleState,
    configuration: TokenConfigurationSerialize,
    circulating_supply: RawTokenAmountSerialize,
}

#[derive(Debug, Clone, Default)]
struct RawTokenAmountSerialize(RawTokenAmount);

impl Serial for RawTokenAmountSerialize {
    fn serial<B: Buffer>(&self, out: &mut B) {
        self.0.0.serial(out)
    }
}

impl Deserial for RawTokenAmountSerialize {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> ParseResult<Self> {
        Ok(Self(RawTokenAmount(u64::deserial(source)?)))
    }
}

#[derive(Debug, Clone)]
struct TokenIdSerialize(TokenId);

impl Serial for TokenIdSerialize {
    fn serial<B: Buffer>(&self, out: &mut B) {
        self.0.as_ref().to_string().serial(out)
    }
}

impl Deserial for TokenIdSerialize {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> ParseResult<Self> {
        Ok(Self(
            TokenId::try_from(String::deserial(source)?).context("string not valid token id")?,
        ))
    }
}

#[derive(Debug, Clone, Serialize)]
struct TokenConfigurationSerialize {
    pub token_id: TokenIdSerialize,
    pub module_ref: TokenModuleRef,
    pub decimals: u8,
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct SimplisticTokenModuleState {
    state: BTreeMap<ModuleStateKey, ModuleStateValue>,
}
