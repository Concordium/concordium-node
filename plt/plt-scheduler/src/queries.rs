//! Implementation of queries related to protocol-level tokens.

use crate::locks;
use crate::token_context::TokenQueryContext;
use crate::token_module;
use concordium_base::common::cbor::cbor_encode;
use concordium_base::protocol_level_locks::LockId;
use concordium_base::protocol_level_tokens::TokenId;
use plt_block_state::block_state_interface::{
    AccountNotFoundByIndexError, BlockStateQuery, LockNotFoundByIdError, TokenNotFoundByIdError,
};
use plt_scheduler_types::types::queries::{
    TokenAccountInfo, TokenAccountState, TokenAuthorizations, TokenInfo, TokenState,
};
use plt_scheduler_types::types::tokens::TokenAmount;
/// Get the [`TokenId`]s of all protocol-level tokens registered on the chain.
pub fn query_plt_list(block_state: &impl BlockStateQuery) -> Vec<TokenId> {
    block_state.plt_list().collect()
}

/// Represents the reasons why a query of token state may fail
#[derive(Debug, thiserror::Error)]
pub enum QueryTokenInfoError {
    #[error("Error returned when querying the token module: {0}")]
    QueryTokenModule(#[from] token_module::QueryTokenModuleError),
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

    let context = TokenQueryContext {
        block_state,
        token_module_state: &token_module_state,
    };

    let module_state = token_module::query_token_module_state(&context)?;

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
pub fn query_token_account_infos<BSQ>(
    block_state: &BSQ,
    account: BSQ::Account,
) -> Result<Vec<TokenAccountInfo>, QueryTokenInfoError>
where
    BSQ: BlockStateQuery,
{
    block_state
        .token_account_states(&account)
        .map(|(token, state)| {
            let token_configuration = block_state.token_configuration(&token);
            let token_module_state = block_state.mutable_token_key_value_state(&token);
            let context = TokenQueryContext {
                block_state,
                token_module_state: &token_module_state,
            };
            let balance = TokenAmount {
                amount: state.balance,
                decimals: token_configuration.decimals,
            };
            let module_state = token_module::query_token_module_account_state(
                &context,
                block_state.account_index(&account),
                state.balance,
                token_configuration.decimals,
            )?;
            let account_state = TokenAccountState {
                balance,
                module_state: Some(module_state),
            };
            Ok(TokenAccountInfo {
                token_id: token_configuration.token_id,
                account_state,
            })
        })
        .collect()
}

/// Get the authorizations of a token.
pub fn query_token_authorizations(
    block_state: &impl BlockStateQuery,
    token_id: &TokenId,
) -> Result<TokenAuthorizations, QueryTokenInfoError> {
    let token = block_state.token_by_id(token_id)?;
    let token_module_state = block_state.mutable_token_key_value_state(&token);
    let context = TokenQueryContext {
        block_state,
        token_module_state: &token_module_state,
    };
    let details = token_module::query_token_authorizations(&context)?;
    let token_configuration = block_state.token_configuration(&token);
    Ok(TokenAuthorizations {
        token_id: token_configuration.token_id,
        details,
    })
}

/// Get the [`LockId`]s of all protocol-level locks registered on the chain at the
/// end of the block.
///
/// NOTE: this is a naive implementation. We might need to optimize with a streaming solution
/// instead, to not load all locks in existance into memory all at once.
pub fn query_lock_list(block_state: &impl BlockStateQuery) -> Vec<LockId> {
    block_state.lock_list().collect()
}

/// Reasons why a lock query (e.g. [`query_lock_info`]) may fail.
#[derive(Debug, thiserror::Error)]
pub enum QueryLockError {
    /// The requested lock does not exist in the block state.
    #[error("Lock does not exist")]
    LockDoesNotExist,
    /// On-chain block state violates an invariant assumed by the
    /// query assembly path. These are unrecoverable for clients and surface as `Panic`
    /// over FFI.
    #[error("State invariant violation while assembling lock info: {0}")]
    StateInvariantViolation(String),
}

impl From<token_module::QueryTokenModuleError> for QueryLockError {
    fn from(err: token_module::QueryTokenModuleError) -> Self {
        QueryLockError::StateInvariantViolation(err.to_string())
    }
}

impl From<LockNotFoundByIdError> for QueryLockError {
    fn from(_: LockNotFoundByIdError) -> Self {
        QueryLockError::LockDoesNotExist
    }
}

impl From<AccountNotFoundByIndexError> for QueryLockError {
    fn from(err: AccountNotFoundByIndexError) -> Self {
        QueryLockError::StateInvariantViolation(err.to_string())
    }
}

/// Assemble the [`LockInfo`] CBOR payload for a lock.
///
/// Thin orchestrator: resolves `lock_id` to a [`LockConfiguration`] via the block state
/// and delegates payload assembly to
/// [`crate::locks::lock_configuration::LockInfoQuery::query_info`].
///
/// [`LockInfo`]: concordium_base::protocol_level_locks::LockInfo
/// [`LockConfiguration`]: plt_block_state::block_state::types::protocol_level_locks::LockConfiguration
pub fn query_lock_info<BSQ: BlockStateQuery>(
    block_state: &BSQ,
    lock_id: &LockId,
) -> Result<Vec<u8>, QueryLockError> {
    let lock = block_state.lock_by_id(lock_id)?;
    let configuration = block_state.lock_configuration(&lock);
    let lock_info = locks::get_lock_info(block_state, lock_id, &configuration)?;
    Ok(cbor_encode(&lock_info))
}
