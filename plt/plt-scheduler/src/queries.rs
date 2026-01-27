//! Implementation of queries related to protocol-level tokens.

use crate::block_state_interface::{BlockStateQuery, TokenNotFoundByIdError};
use crate::token_kernel::TokenKernelQueriesImpl;
use concordium_base::protocol_level_tokens::TokenId;
use plt_token_module::token_module;
use plt_token_module::token_module::QueryTokenModuleError;
use plt_types::types::queries::{TokenAccountInfo, TokenAccountState, TokenInfo, TokenState};
use plt_types::types::tokens::TokenAmount;

/// Get the [`TokenId`]s of all protocol-level tokens registered on the chain.
pub fn plt_list(block_state: &impl BlockStateQuery) -> Vec<TokenId> {
    block_state.plt_list().collect()
}

/// Represents the reasons why a query of token state may fail
#[derive(Debug, thiserror::Error)]
pub enum QueryTokenInfoError {
    #[error("Error returned when querying the token module: {0}")]
    QueryTokenModule(#[from] QueryTokenModuleError),
    #[error("{0}")]
    TokenDoesNotExist(#[from] TokenNotFoundByIdError),
}

/// Get the token state associated with the given token id.
pub fn query_token_info(
    block_state: &impl BlockStateQuery,
    token_id: &TokenId,
) -> Result<TokenInfo, QueryTokenInfoError> {
    let token = block_state.token_by_id(token_id)?;

    let token_configuration = block_state.token_configuration(&token);
    let circulating_supply = block_state.token_circulating_supply(&token);

    let total_supply = TokenAmount {
        amount: circulating_supply,
        decimals: token_configuration.decimals,
    };

    let token_module_state = block_state.mutable_token_key_value_state(&token);

    let kernel = TokenKernelQueriesImpl {
        block_state,
        token: &token,
        token_module_state: &token_module_state,
    };

    let module_state = token_module::query_token_module_state(&kernel)?;

    let token_state = TokenState {
        token_module_ref: token_configuration.module_ref,
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

/// Get the list of tokens on an account
pub fn query_token_account_infos<BSQ: BlockStateQuery>(
    block_state: &BSQ,
    account: BSQ::Account,
) -> Vec<TokenAccountInfo> {
    block_state
        .token_account_states(&account)
        .map(|(token, state)| {
            let token_configuration = block_state.token_configuration(&token);

            let token_module_state = block_state.mutable_token_key_value_state(&token);

            let kernel = TokenKernelQueriesImpl {
                block_state,
                token: &token,
                token_module_state: &token_module_state,
            };

            let module_state = token_module::query_token_module_account_state(&kernel, &account);

            let balance = TokenAmount {
                amount: state.balance,
                decimals: token_configuration.decimals,
            };

            let account_state = TokenAccountState {
                balance,
                module_state,
            };

            TokenAccountInfo {
                token_id: token_configuration.token_id,
                account_state,
            }
        })
        .collect()
}
