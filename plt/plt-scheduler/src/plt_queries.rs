use crate::block_state_interface::{BlockStateQuery, TokenNotFoundByIdError};
use crate::{block_state_interface, TOKEN_MODULE_REF};
use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{RawCbor, TokenAmount, TokenId, TokenModuleRef};
use plt_token_module::token_kernel_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, ModuleStateKey, ModuleStateValue,
    RawTokenAmount, TokenKernelQueries,
};
use plt_token_module::token_module::QueryTokenModuleError;
use plt_token_module::token_module;

/// Get the [`TokenId`]s of all protocol-level tokens registered on the chain.
pub fn plt_list(block_state: &impl BlockStateQuery) -> Vec<TokenId> {
    block_state.plt_list().collect()
}

/// Token state at the block level
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TokenState {
    /// The reference of the module implementing this token.
    pub token_module_ref: TokenModuleRef,
    /// Number of decimals in the decimal number representation of amounts.
    pub decimals: u8,
    /// The total available token supply.
    pub total_supply: TokenAmount,
    /// Token module specific state, such as token name, feature flags, meta
    /// data.
    pub module_state: RawCbor,
}

/// Represents the reasons why a query of token state may fail
#[derive(Debug, thiserror::Error)]
pub enum QueryTokenStateError {
    #[error("Error returned when querying the token module: {0}")]
    QueryTokenModule(#[from] QueryTokenModuleError),
    #[error("The token does not exist: {0}")]
    TokenDoesNotExist(#[from] TokenNotFoundByIdError),
}

/// Get the token state associated with the given token id.
pub fn token_state(
    block_state: &impl BlockStateQuery,
    token_id: &TokenId,
) -> Result<TokenState, QueryTokenStateError> {
    let token = block_state.token_by_id(token_id)?;

    let token_configuration = block_state.token_configuration(&token);
    let circulating_supply = block_state.token_circulating_supply(&token);

    let total_supply = TokenAmount::from_raw(circulating_supply.0, token_configuration.decimals);

    let token_module_state = block_state.mutable_token_module_state(&token);

    let kernel = TokenKernelQueriesImpl {
        block_state,
        token: &token,
        token_module_state: &token_module_state,
    };

    let module_state = token_module::query_token_module_state(&kernel)?;

    let token_state = TokenState {
        token_module_ref: TOKEN_MODULE_REF,
        decimals: token_configuration.decimals,
        total_supply,
        module_state,
    };

    Ok(token_state)
}

/// State of a protocol level token associated with some account.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TokenAccountState {
    /// The token balance of the account.
    pub balance: TokenAmount,
    /// The token-module defined state of the account.
    pub module_state: RawCbor,
}

/// Represents the reasons why a query of token state may fail
#[derive(Debug, thiserror::Error)]
pub enum QueryTokenAccountStateError {
    #[error("Error returned when querying the token module: {0}")]
    QueryTokenModule(#[from] QueryTokenModuleError),
}

/// Get the list of tokens on an account
pub fn token_account_states(
    _block_state: &impl BlockStateQuery,
    _account: AccountIndex,
) -> Result<Vec<TokenAccountState>, QueryTokenAccountStateError> {
    todo!()
}

struct TokenKernelQueriesImpl<'a, BSQ: BlockStateQuery> {
    block_state: &'a BSQ,
    token: &'a BSQ::Token,
    token_module_state: &'a BSQ::MutableTokenModuleState,
}

impl<BSQ: BlockStateQuery> TokenKernelQueries for TokenKernelQueriesImpl<'_, BSQ> {
    type Account = BSQ::Account;

    fn account_by_address(
        &self,
        address: &AccountAddress,
    ) -> Result<Self::Account, AccountNotFoundByAddressError> {
        self.block_state.account_by_address(address).map_err(
            |block_state_interface::AccountNotFoundByAddressError(account_address)| {
                AccountNotFoundByAddressError(account_address)
            },
        )
    }

    fn account_by_index(
        &self,
        index: AccountIndex,
    ) -> Result<Self::Account, AccountNotFoundByIndexError> {
        self.block_state.account_by_index(index).map_err(
            |block_state_interface::AccountNotFoundByIndexError(index)| {
                AccountNotFoundByIndexError(index)
            },
        )
    }

    fn account_index(&self, account: &Self::Account) -> AccountIndex {
        self.block_state.account_index(account)
    }

    fn account_canonical_address(&self, account: &Self::Account) -> AccountAddress {
        self.block_state.account_canonical_address(account)
    }

    fn account_token_balance(&self, account: &Self::Account) -> RawTokenAmount {
        self.block_state.account_token_balance(account, self.token)
    }

    fn circulating_supply(&self) -> RawTokenAmount {
        self.block_state.token_circulating_supply(self.token)
    }

    fn decimals(&self) -> u8 {
        self.block_state.token_configuration(self.token).decimals
    }

    fn lookup_token_module_state_value(&self, key: ModuleStateKey) -> Option<ModuleStateValue> {
        self.block_state
            .lookup_token_module_state_value(self.token_module_state, &key)
    }
}
