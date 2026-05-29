use crate::token_context::TokenQueryContext;
use crate::token_module::errors::TokenStateInvariantError;
use crate::token_module::key_value_state;
use concordium_base::base::AccountIndex;
use concordium_base::common::cbor;
use concordium_base::protocol_level_locks::LockId;
use concordium_base::protocol_level_tokens::{
    AccountLockAmount, CborHolderAccount, RawCbor, TokenAmount, TokenModuleAccountState,
    TokenModuleState,
};
use plt_block_state::block_state_interface::BlockStateQuery;
use plt_scheduler_types::types::tokens::RawTokenAmount;

/// Represents the reasons why a query to the token module can fail.
#[derive(Debug, thiserror::Error)]
pub enum QueryTokenModuleError {
    #[error("{0}")]
    StateInvariantViolation(#[from] TokenStateInvariantError),
}

/// Get the CBOR-encoded representation of the token module state.
pub fn query_token_module_state<BSQ: BlockStateQuery>(
    context: &TokenQueryContext<'_, BSQ>,
) -> Result<RawCbor, QueryTokenModuleError> {
    let state = query_token_module_state_impl(context)?;

    Ok(RawCbor::from(cbor::cbor_encode(&state)))
}

fn query_token_module_state_impl<BSQ: BlockStateQuery>(
    context: &TokenQueryContext<'_, BSQ>,
) -> Result<TokenModuleState, QueryTokenModuleError> {
    let name = key_value_state::get_token_name(context)?;
    let metadata = key_value_state::get_metadata(context)?;
    let allow_list = key_value_state::has_allow_list(context);
    let deny_list = key_value_state::has_deny_list(context);
    let mintable = key_value_state::is_mintable(context);
    let burnable = key_value_state::is_burnable(context);
    let paused = key_value_state::is_paused(context);

    let governance_account_index = key_value_state::get_governance_account_index(context)?;
    let governance_account = context
        .block_state
        .account_by_index(governance_account_index)
        .map_err(|_| {
            TokenStateInvariantError(format!(
                "Stored governance account with index {} does not exist",
                governance_account_index
            ))
        })?;

    let state = TokenModuleState {
        name: Some(name),
        metadata: Some(metadata),
        governance_account: Some(CborHolderAccount::from(
            governance_account.canonical_account_address,
        )),
        allow_list: Some(allow_list),
        deny_list: Some(deny_list),
        mintable: Some(mintable),
        burnable: Some(burnable),
        paused: Some(paused),
    };

    Ok(state)
}

/// Get the CBOR-encoded representation of the token module account state.
///
/// # Arguments
///
/// - `context`: Query access to the token module state and surrounding block state.
/// - `account`: Account whose token-module account state should be queried.
/// - `balance`: Total balance of the token held by the account before subtracting locked funds.
/// - `decimals`: Number of decimals used when encoding token amounts in the result.
///
/// # Errors
///
/// Returns an error if stored token-module state is inconsistent, such as an
/// undecodable lock entry, overflow while summing locked balances, or locked
/// balance exceeding the account balance.
pub fn query_token_module_account_state<BSQ: BlockStateQuery>(
    context: &TokenQueryContext<'_, BSQ>,
    account: AccountIndex,
    balance: RawTokenAmount,
    decimals: u8,
) -> Result<RawCbor, QueryTokenModuleError> {
    let state = query_token_module_account_state_impl(context, account, balance, decimals)?;
    Ok(RawCbor::from(cbor::cbor_encode(&state)))
}

fn query_token_module_account_state_impl<BSQ: BlockStateQuery>(
    context: &TokenQueryContext<'_, BSQ>,
    account: AccountIndex,
    total_token_balance: RawTokenAmount,
    token_decimals: u8,
) -> Result<TokenModuleAccountState, QueryTokenModuleError> {
    let has_allow_list = key_value_state::has_allow_list(context);
    let allow_list = if has_allow_list {
        key_value_state::get_allow_list_for(context, account).into()
    } else {
        None
    };
    let has_deny_list = key_value_state::has_deny_list(context);
    let deny_list = if has_deny_list {
        key_value_state::get_deny_list_for(context, account).into()
    } else {
        None
    };

    let mut total_locked = 0u64;
    let mut locks = Vec::new();
    for (lock, locked_balance) in
        key_value_state::get_locked_balances_for_account(context, account)?
    {
        if locked_balance == RawTokenAmount(0) {
            continue;
        }
        total_locked = total_locked.checked_add(locked_balance.0).ok_or_else(|| {
            TokenStateInvariantError("total locked token balance overflow".to_string())
        })?;
        locks.push(AccountLockAmount {
            lock,
            amount: TokenAmount::from_raw(locked_balance.0, token_decimals),
        });
    }

    let available = if total_locked == 0 {
        None
    } else {
        let available = total_token_balance
            .0
            .checked_sub(total_locked)
            .ok_or_else(|| {
                TokenStateInvariantError(
                    "total locked token balance exceeds account token balance".to_string(),
                )
            })?;
        Some(TokenAmount::from_raw(available, token_decimals))
    };

    Ok(TokenModuleAccountState {
        allow_list,
        deny_list,
        locks,
        available,
    })
}

/// Get authorization roles and assigned accounts for the token.
pub fn query_token_authorizations<BSQ: BlockStateQuery>(
    context: &TokenQueryContext<'_, BSQ>,
) -> Result<RawCbor, QueryTokenModuleError> {
    Ok(RawCbor::from(cbor::cbor_encode(
        &key_value_state::get_token_authorizations(context)?,
    )))
}

/// Get the locked balance of `account` under `lock` for the token in context.
pub fn query_locked_balance<BSQ: BlockStateQuery>(
    context: &TokenQueryContext<'_, BSQ>,
    account: AccountIndex,
    lock: &LockId,
) -> Result<RawTokenAmount, QueryTokenModuleError> {
    Ok(key_value_state::get_locked_balance_for(
        context, account, lock,
    )?)
}
