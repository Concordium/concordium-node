use crate::token_context::TokenQueryContext;
use crate::token_module::errors::TokenStateInvariantError;
use crate::token_module::key_value_state;
use concordium_base::base::AccountIndex;
use concordium_base::common::cbor;
use concordium_base::protocol_level_locks::LockId;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, RawCbor, TokenModuleAccountState, TokenModuleState,
};
use plt_block_state::block_state_interface::{
    AccountNotFoundByIndexError, BlockStateFailure, BlockStateQuery, BlockStateResult,
};
use plt_block_state::entity::block_state::Accounts;
use plt_block_state::entity::protocol_level_tokens::p9::TokenP9;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_scheduler_types::types::tokens::RawTokenAmount;

// todo ar delete

/// Represents the reasons why a query to the token module can fail.
#[derive(Debug, thiserror::Error)]
pub enum QueryTokenModuleError {
    #[error("{0}")]
    StateInvariantViolation(#[from] TokenStateInvariantError),
}

/// Get the CBOR-encoded representation of the token module state.
pub fn query_token_module_state<C: EntityContextTypes>(
    context: &EntityContext<C>,
    accounts: &impl Accounts,
    token: &TokenP9,
) -> BlockStateResult<RawCbor> {
    let state = query_token_module_state_impl(context, accounts, token)?;

    Ok(RawCbor::from(cbor::cbor_encode(&state)))
}

fn query_token_module_state_impl<C: EntityContextTypes>(
    context: &EntityContext<C>,
    accounts: &impl Accounts,
    token: &TokenP9,
) -> BlockStateResult<TokenModuleState> {
    let governance_account_index = token.get_governance_account_index(context)?;
    let governance_account = accounts
        .account_by_index(context, governance_account_index)
        .map_err(|_: AccountNotFoundByIndexError| {
            BlockStateFailure::Invariant(format!(
                "Stored governance account with index {} does not exist",
                governance_account_index
            ))
        })?;

    let state = TokenModuleState {
        name: Some(token.get_token_name(context)?),
        metadata: Some(token.get_metadata(context)?),
        governance_account: Some(CborHolderAccount::from(
            governance_account.canonical_account_address,
        )),
        allow_list: Some(token.has_allow_list(context)),
        deny_list: Some(token.has_deny_list(context)),
        mintable: Some(token.is_mintable(context)),
        burnable: Some(token.is_burnable(context)),
        paused: Some(token.is_paused(context)),
    };

    Ok(state)
}

/// Get the CBOR-encoded representation of the token module account state.
pub fn query_token_module_account_state<C: EntityContextTypes>(
    context: &EntityContext<C>,
    token: &TokenP9,
    account: AccountIndex,
) -> BlockStateResult<RawCbor> {
    let state = query_token_module_account_state_impl(context, token, account)?;
    Ok(RawCbor::from(cbor::cbor_encode(&state)))
}

fn query_token_module_account_state_impl<C: EntityContextTypes>(
    context: &EntityContext<C>,
    token: &TokenP9,
    account: AccountIndex,
) -> BlockStateResult<TokenModuleAccountState> {
    let has_allow_list = token.has_allow_list(context);
    let allow_list = if has_allow_list {
        token.get_allow_list_for(context, account).into()
    } else {
        None
    };
    let has_deny_list = token.has_deny_list(context);
    let deny_list = if has_deny_list {
        token.get_deny_list_for(context, account).into()
    } else {
        None
    };

    Ok(TokenModuleAccountState {
        allow_list,
        deny_list,
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
    lock: LockId,
) -> Result<RawTokenAmount, QueryTokenModuleError> {
    Ok(key_value_state::get_locked_balance_for(
        context, account, lock,
    )?)
}
