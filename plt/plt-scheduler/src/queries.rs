//! Implementation of queries related to protocol-level tokens.

use crate::token_context::TokenQueryContext;
use crate::token_module;
use concordium_base::base::AccountIndex;
use concordium_base::common::cbor::cbor_encode;
use concordium_base::protocol_level_locks::{
    LockAccountFunds, LockController, LockControllerSimpleV0, LockControllerSimpleV0Grant, LockId,
    LockInfo, LockedTokenAmount,
};
use concordium_base::protocol_level_tokens::{CborHolderAccount, TokenId};
use plt_block_state::block_state_interface::{
    BlockStateQuery, LockNotFoundByIdError, TokenNotFoundByIdError,
};
use plt_scheduler_types::types::locks::LockControllerConfig;
use plt_scheduler_types::types::queries::{
    TokenAccountInfo, TokenAccountState, TokenAuthorizations, TokenInfo, TokenState,
};
use plt_scheduler_types::types::tokens::TokenAmount;
use std::collections::BTreeMap;
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
) -> Vec<TokenAccountInfo>
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
            let module_state = token_module::query_token_module_account_state(
                &context,
                block_state.account_index(&account),
            );

            let balance = TokenAmount {
                amount: state.balance,
                decimals: token_configuration.decimals,
            };

            let account_state = TokenAccountState {
                balance,
                module_state: Some(module_state),
            };

            TokenAccountInfo {
                token_id: token_configuration.token_id,
                account_state,
            }
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
pub fn query_lock_list(block_state: &impl BlockStateQuery) -> Vec<LockId> {
    block_state.lock_list().collect()
}

/// Reasons why a lock query (e.g. [`query_lock_info`]) may fail.
#[derive(Debug, thiserror::Error)]
pub enum QueryLockError {
    /// The requested lock does not exist in the block state.
    #[error("Lock does not exist")]
    LockDoesNotExist,
    /// On-chain block / token-module state violates an invariant assumed by the
    /// `lock-info` assembly path — e.g. an `AccountIndex` recorded in the lock
    /// configuration or returned by `lock_balances` does not resolve, or a tracked
    /// `(account, token)` pair has an undecodable `quanta` entry in the token-module
    /// key-value state. These are unrecoverable for clients and surface as `Panic`
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

/// Assemble the canonical [`LockInfo`] CBOR payload for a lock.
///
/// The function resolves the lock via [`BlockStateQuery::lock_by_id`], collects its
/// [`BlockStateQuery::lock_configuration`] and per-`(account, token)`
/// [`BlockStateQuery::account_token_balance`] entries, builds a
/// [`concordium_base::protocol_level_locks::queries::LockInfo`] value, and CBOR-encodes
/// it via the existing `CborSerialize` derive. The returned `Vec<u8>` is the canonical
/// CBOR `lock-info` payload — callers MUST treat it as opaque.
pub fn query_lock_info<BSQ: BlockStateQuery>(
    block_state: &BSQ,
    lock_id: &LockId,
) -> Result<Vec<u8>, QueryLockError> {
    let lock = block_state.lock_by_id(lock_id)?;
    let configuration = block_state.lock_configuration(&lock);

    // Resolve recipients (block-state `AccountIndex`es) into `CborHolderAccount` values
    // by looking up each account's canonical address.
    let recipients: Vec<CborHolderAccount> = configuration
        .recipients
        .iter()
        .map(|account_index| {
            let with_addr = block_state.account_by_index(*account_index).map_err(|_| {
                QueryLockError::StateInvariantViolation(format!(
                    "recipient account index {} recorded in lock configuration does not exist",
                    account_index
                ))
            })?;
            Ok(CborHolderAccount::from(with_addr.canonical_account_address))
        })
        .collect::<Result<_, QueryLockError>>()?;

    // Convert the lock controller configuration into the CBOR `LockController` shape used
    // by the `lock-info` payload.
    let controller = match configuration.controller {
        LockControllerConfig::SimpleV0(simple) => {
            let grants = simple
                .grants
                .into_iter()
                .map(|grant| {
                    let with_addr = block_state.account_by_index(grant.account).map_err(|_| {
                        QueryLockError::StateInvariantViolation(format!(
                            "controller grant account index {} recorded in lock configuration \
                             does not exist",
                            grant.account
                        ))
                    })?;
                    Ok(LockControllerSimpleV0Grant {
                        account: CborHolderAccount::from(with_addr.canonical_account_address),
                        roles: grant.roles,
                    })
                })
                .collect::<Result<_, QueryLockError>>()?;
            LockController::SimpleV0(LockControllerSimpleV0 {
                grants,
                tokens: simple.tokens,
                keep_alive: simple.keep_alive,
                memo: simple.memo,
            })
        }
    };

    // Group the tracked `(account, token)` balances by account so we emit a single
    // `LockAccountFunds` entry per account, matching the CBOR `lock-info` shape. We key
    // the intermediate map by `AccountIndex` (which is `Ord`) since the opaque
    // `BSQ::Account` associated type is not required by the trait to support ordering.
    //
    // The locked amount for each `(account, token)` pair is read from the token-module
    // key-value state via `token_module::query_locked_balance` (see
    // `plt-scheduler/src/token_module/key_value_state.rs::get_locked_balance_for`),
    // which is the source of truth for per-`(account, token, lock)` locked amounts.
    // Reading the account's full token balance here would over-report by including the
    // unlocked portion.
    let mut funds_by_account: BTreeMap<AccountIndex, Vec<LockedTokenAmount>> = BTreeMap::new();
    for (account, token) in block_state.lock_balances(lock_id) {
        let account_index = block_state.account_index(&account);
        let token_configuration = block_state.token_configuration(&token);
        let token_module_state = block_state.mutable_token_key_value_state(&token);
        let context = TokenQueryContext {
            block_state,
            token_module_state: &token_module_state,
        };
        let raw_balance =
            token_module::query_locked_balance(&context, account_index, lock_id.clone())?;
        let amount = concordium_base::protocol_level_tokens::TokenAmount::from_raw(
            raw_balance.0,
            token_configuration.decimals,
        );
        funds_by_account
            .entry(account_index)
            .or_default()
            .push(LockedTokenAmount {
                token: token_configuration.token_id,
                amount,
            });
    }

    let funds: Vec<LockAccountFunds> = funds_by_account
        .into_iter()
        .map(|(account_index, amounts)| {
            let with_addr = block_state.account_by_index(account_index).map_err(|_| {
                QueryLockError::StateInvariantViolation(format!(
                    "account index {} returned by `lock_balances` does not exist",
                    account_index
                ))
            })?;
            Ok(LockAccountFunds {
                account: CborHolderAccount::from(with_addr.canonical_account_address),
                amounts,
            })
        })
        .collect::<Result<_, QueryLockError>>()?;

    let lock_info = LockInfo {
        lock,
        recipients,
        expiry: configuration.expiry,
        controller,
        funds,
    };

    Ok(cbor_encode(&lock_info))
}
