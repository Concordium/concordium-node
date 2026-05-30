use crate::block_state_polymorph::token::TokenPXRef;
use concordium_base::base::AccountIndex;
use concordium_base::protocol_level_locks::LockId;
use concordium_base::protocol_level_tokens::{
    AccountLockAmount, CborHolderAccount, TokenAdminRole, TokenAmount, TokenAuthorizations,
    TokenModuleAccountState, TokenModuleState, TokenRoleAuthorizations,
};
use plt_block_state::entity::accounts::Accounts;
use plt_block_state::entity::protocol_level_tokens::p9::TokenP9Base;
use plt_block_state::entity::protocol_level_tokens::p11::TokenP11;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::external::AccountNotFoundByIndexError;
use plt_block_state::failure::{BlockStateFailure, BlockStateResult};
use plt_scheduler_types::types::tokens::RawTokenAmount;

/// Get the CBOR-encoded representation of the token module state.
pub fn query_token_module_state<C: EntityContextTypes>(
    context: &EntityContext<C>,
    token: &TokenP9Base,
) -> BlockStateResult<TokenModuleState> {
    let governance_account_index = token.get_governance_account_index(context)?;
    let governance_account = context.account_by_index(governance_account_index).map_err(
        |_: AccountNotFoundByIndexError| {
            BlockStateFailure::Invariant(format!(
                "Stored governance account with index {} does not exist",
                governance_account_index
            ))
        },
    )?;

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
    token: TokenPXRef<'_>,
    account: AccountIndex,
    total_token_balance: RawTokenAmount,
) -> BlockStateResult<TokenModuleAccountState> {
    let token_base = token.token_p9_base();
    let has_allow_list = token_base.has_allow_list(context);
    let allow_list = if has_allow_list {
        token_base.get_allow_list_for(context, account).into()
    } else {
        None
    };
    let has_deny_list = token_base.has_deny_list(context);
    let deny_list = if has_deny_list {
        token_base.get_deny_list_for(context, account).into()
    } else {
        None
    };

    let token_configuration = token_base.token_configuration(context)?;

    let mut total_locked = RawTokenAmount(0);
    let mut locks = Vec::new();

    if let TokenPXRef::TokenP11(token_p11) = token {
        for (lock, locked_balance) in token_p11
            .get_locked_balances_for_account(context, account)?
            .into_iter()
        {
            if locked_balance == RawTokenAmount(0) {
                continue;
            }
            total_locked.0 = total_locked
                .0
                .checked_add(locked_balance.0)
                .ok_or_else(|| {
                    BlockStateFailure::Invariant("Total locked token balance overflow".to_string())
                })?;
            locks.push(AccountLockAmount {
                lock,
                amount: TokenAmount::from_raw(locked_balance.0, token_configuration.decimals),
            });
        }
    }

    let available = if total_locked == RawTokenAmount(0) {
        None
    } else {
        let available = total_token_balance
            .0
            .checked_sub(total_locked.0)
            .ok_or_else(|| {
                BlockStateFailure::Invariant(
                    "Total locked token balance exceeds account token balance".to_string(),
                )
            })?;
        Some(TokenAmount::from_raw(
            available,
            token_configuration.decimals,
        ))
    };

    Ok(TokenModuleAccountState {
        allow_list,
        deny_list,
        locks,
        available,
    })
}

/// Get authorization roles and assigned accounts for the token.
pub fn query_token_authorizations<C: EntityContextTypes>(
    context: &EntityContext<C>,
    token: &TokenP11,
) -> BlockStateResult<TokenAuthorizations> {
    let mut update_admin_roles = TokenRoleAuthorizations::default();
    let mut mint = TokenRoleAuthorizations::default();
    let mut burn = TokenRoleAuthorizations::default();
    let mut update_allow_list = TokenRoleAuthorizations::default();
    let mut update_deny_list = TokenRoleAuthorizations::default();
    let mut pause = TokenRoleAuthorizations::default();
    let mut update_metadata = TokenRoleAuthorizations::default();

    for (account_index, roles) in token.all_roles(context)?.into_iter() {
        let account = context
            .account_by_index(account_index)
            .map_err(|err| {
                BlockStateFailure::Invariant(format!(
                    "Stored account index in authorizations cannot be found: {}",
                    err
                ))
            })?
            .canonical_account_address;

        for role in roles.iter_assigned() {
            match role {
                TokenAdminRole::UpdateAdminRoles => {
                    update_admin_roles.accounts.push(account.into())
                }
                TokenAdminRole::Mint => mint.accounts.push(account.into()),
                TokenAdminRole::Burn => burn.accounts.push(account.into()),
                TokenAdminRole::UpdateAllowList => update_allow_list.accounts.push(account.into()),
                TokenAdminRole::UpdateDenyList => update_deny_list.accounts.push(account.into()),
                TokenAdminRole::Pause => pause.accounts.push(account.into()),
                TokenAdminRole::UpdateMetadata => update_metadata.accounts.push(account.into()),
            }
        }
    }
    Ok(TokenAuthorizations {
        update_admin_roles: Some(update_admin_roles),
        mint: token.token_p9_base.is_mintable(context).then_some(mint),
        burn: token.token_p9_base.is_burnable(context).then_some(burn),
        update_allow_list: token
            .token_p9_base
            .has_allow_list(context)
            .then_some(update_allow_list),
        update_deny_list: token
            .token_p9_base
            .has_deny_list(context)
            .then_some(update_deny_list),
        pause: Some(pause),
        update_metadata: Some(update_metadata),
    })
}

/// Get the locked balance of `account` under `lock` for the token in context.
pub fn query_locked_balance<C: EntityContextTypes>(
    context: &EntityContext<C>,
    token: &TokenP11,
    account: AccountIndex,
    lock_id: &LockId,
) -> BlockStateResult<RawTokenAmount> {
    token.get_locked_balance_for_account(context, account, lock_id)
}
