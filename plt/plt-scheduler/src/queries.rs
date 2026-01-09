//! Implementation of queries related to protocol-level tokens.

use crate::TOKEN_MODULE_REF;
use crate::block_state_interface::{BlockStateQuery, TokenNotFoundByIdError};
use crate::types::state::{TokenAccountState, TokenState};
use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{TokenAmount, TokenId};
use plt_scheduler_interface::{AccountNotFoundByAddressError, AccountNotFoundByIndexError};
use plt_token_module::token_kernel_interface::{
    ModuleStateKey, ModuleStateValue, RawTokenAmount, TokenKernelQueries,
};
use plt_token_module::token_module;
use plt_token_module::token_module::QueryTokenModuleError;

/// Get the [`TokenId`]s of all protocol-level tokens registered on the chain.
pub fn plt_list(block_state: &impl BlockStateQuery) -> Vec<TokenId> {
    block_state.plt_list().collect()
}

/// The token state at the block level.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TokenInfo {
    /// The canonical identifier/symbol for the protocol level token.
    pub token_id: TokenId,
    /// The associated block level state.
    pub state: TokenState,
}

/// Represents the reasons why a query of token state may fail
#[derive(Debug, thiserror::Error)]
pub enum QueryTokenInfoError {
    #[error("Error returned when querying the token module: {0}")]
    QueryTokenModule(#[from] QueryTokenModuleError),
    #[error("The token does not exist: {0}")]
    TokenDoesNotExist(#[from] TokenNotFoundByIdError),
}

/// Get the token state associated with the given token id.
pub fn token_info(
    block_state: &impl BlockStateQuery,
    token_id: &TokenId,
) -> Result<TokenInfo, QueryTokenInfoError> {
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

    let token_info = TokenInfo {
        // The token configuration contains the canonical token id specified in the original casing
        token_id: token_configuration.token_id,
        state: token_state,
    };

    Ok(token_info)
}

/// State of a protocol level token associated with some account.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TokenAccountInfo {
    /// The canonical identifier/symbol for the protocol level token.
    pub token_id: TokenId,
    /// The state of the token associated with the account.
    pub account_state: TokenAccountState,
}

/// Represents the reasons why a query of token state may fail
#[derive(Debug, thiserror::Error)]
pub enum QueryTokenAccountStateError {
    #[error("Error returned when querying the token module: {0}")]
    QueryTokenModule(#[from] QueryTokenModuleError),
    #[error("{0}")]
    AccountNotFoundByIndex(#[from] AccountNotFoundByIndexError),
}

/// Get the list of tokens on an account
pub fn token_account_infos(
    block_state: &impl BlockStateQuery,
    account_index: AccountIndex,
) -> Result<Vec<TokenAccountInfo>, QueryTokenAccountStateError> {
    let account = block_state.account_by_index(account_index)?;

    block_state
        .token_account_states(&account)
        .map(|(token, state)| {
            let token_configuration = block_state.token_configuration(&token);

            let token_module_state = block_state.mutable_token_module_state(&token);

            let kernel = TokenKernelQueriesImpl {
                block_state,
                token: &token,
                token_module_state: &token_module_state,
            };

            let module_state = token_module::query_account_token_module_state(&kernel, &account)?;

            let balance = TokenAmount::from_raw(state.balance.0, token_configuration.decimals);

            let account_state = TokenAccountState {
                balance,
                module_state,
            };

            let token_account_info = TokenAccountInfo {
                token_id: token_configuration.token_id,
                account_state,
            };

            Ok(token_account_info)
        })
        .collect()
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
        self.block_state.account_by_address(address)
    }

    fn account_by_index(
        &self,
        index: AccountIndex,
    ) -> Result<Self::Account, AccountNotFoundByIndexError> {
        self.block_state.account_by_index(index)
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

    fn decimals(&self) -> u8 {
        self.block_state.token_configuration(self.token).decimals
    }

    fn lookup_token_module_state_value(&self, key: ModuleStateKey) -> Option<ModuleStateValue> {
        self.block_state
            .lookup_token_module_state_value(self.token_module_state, &key)
    }
}
