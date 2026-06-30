use std::collections::BTreeMap;

use crate::{locks::lock_controller::LockController, protocol_level_tokens::token_module};
use concordium_base::{
    base::AccountIndex,
    protocol_level_locks::{
        LockAccountFunds, LockConfig, LockInfo, LockRecipients, LockedTokenAmount,
    },
    protocol_level_tokens::{CborHolderAccount, TokenAmount},
};
use plt_block_state::entity::accounts::Accounts;
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::protocol_level_locks::p11::LockP11;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::external::AccountNotFoundByIndexError;
use plt_block_state::failure::{BlockStateFailure, BlockStateResult};
use plt_block_state::persistent::protocol_level_locks::p11::{
    LockConfiguration, LockRecipients as BlockStateLockRecipients,
};

/// Get the recipients for a lock configuration, resolving [`AccountIndex`]es
/// to [`CborHolderAccount`]s unless the block-state sentinel represents an
/// any-recipient lock.
fn get_recipients<C: EntityContextTypes>(
    context: &EntityContext<C>,
    configuration: &LockConfiguration,
) -> BlockStateResult<LockRecipients> {
    match &configuration.recipients {
        BlockStateLockRecipients::Any => Ok(LockRecipients::Any),
        BlockStateLockRecipients::Limited(recipients) => {
            let recipients = recipients
                .iter()
                .map(|account_index| {
                    let with_addr = context.account_by_index(*account_index).map_err(
                        |_err: AccountNotFoundByIndexError| {
                            BlockStateFailure::Invariant(format!(
                                "account index {} in lock recipients does not exist",
                                account_index
                            ))
                        },
                    )?;
                    Ok(CborHolderAccount::from(with_addr.canonical_account_address))
                })
                .collect::<Result<Vec<_>, _>>()?;

            Ok(LockRecipients::Limited(recipients))
        }
    }
}

/// Get the lock configuration as a CBOR-representable [`LockConfig`] with
/// resolved account addresses.
pub fn get_lock_config<C: EntityContextTypes>(
    context: &EntityContext<C>,
    configuration: &LockConfiguration,
) -> BlockStateResult<LockConfig> {
    let recipients = get_recipients(context, configuration)?;
    let controller = configuration.controller.to_cbor_controller(context)?;

    Ok(LockConfig {
        recipients,
        expiry: configuration.expiry,
        controller,
        metadata: configuration.metadata.clone(),
    })
}

/// Build the [`LockInfo`] for a lock from its [`LockConfiguration`] and the live
/// per-`(account, token)` balances held by the lock.
pub fn get_lock_info<C: EntityContextTypes>(
    context: &EntityContext<C>,
    block_state: &BlockStateP11,
    lock: &LockP11,
    lock_configuration: &LockConfiguration,
) -> BlockStateResult<LockInfo> {
    // Resolve recipients (block-state `AccountIndex`es) into `CborHolderAccount` values
    // by looking up each account's canonical address.
    let recipients = get_recipients(context, lock_configuration)?;

    // Convert the lock controller configuration into the CBOR `LockController` shape used
    // by the `lock-info` payload. Variant-specific resolution (e.g. expanding grant
    // `AccountIndex`es to `CborHolderAccount`) lives on the per-variant
    // `crate::locks::lock_controller::LockController` impl.
    let controller = lock_configuration.controller.to_cbor_controller(context)?;

    // Group the tracked `(account, token)` balances by account so we emit a single
    // `LockAccountFunds` entry per account.
    let mut funds_by_account: BTreeMap<AccountIndex, Vec<LockedTokenAmount>> = BTreeMap::new();
    for (account_index, token_index) in lock.lock_balance_refs() {
        let token = block_state.token_by_index(context, token_index)?;
        let token_configuration = token.token_p9_base.token_configuration(context)?;

        // for each locked balance record for the lock, get the locked token amount recorded in the
        // account state of the token.
        let raw_balance = token_module::query_locked_balance(
            context,
            &token,
            account_index,
            &lock_configuration.lock_id,
        )?;
        let amount = TokenAmount::from_raw(raw_balance.0, token_configuration.decimals);
        funds_by_account
            .entry(account_index)
            .or_default()
            .push(LockedTokenAmount {
                token: token_configuration.token_id,
                amount,
            });
    }

    // Resolve the account addresses for the accounts holding locked funds
    let funds: Vec<LockAccountFunds> = funds_by_account
        .into_iter()
        .map(|(account_index, amounts)| {
            let with_addr = context.account_by_index(account_index).map_err(
                |_err: AccountNotFoundByIndexError| {
                    BlockStateFailure::Invariant(format!(
                        "account index {} returned by `lock_balances` does not exist",
                        account_index
                    ))
                },
            )?;
            Ok(LockAccountFunds {
                account: CborHolderAccount::from(with_addr.canonical_account_address),
                amounts,
            })
        })
        .collect::<Result<_, BlockStateFailure>>()?;

    Ok(LockInfo {
        lock: lock_configuration.lock_id.clone(),
        recipients,
        expiry: lock_configuration.expiry,
        controller,
        metadata: lock_configuration.metadata.clone(),
        funds,
    })
}
