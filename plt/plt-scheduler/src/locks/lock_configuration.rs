use std::collections::BTreeMap;

use crate::{
    locks::lock_controller::LockController, protocol_level_tokens::token_module,
    queries::QueryLockError,
};
use concordium_base::{
    base::AccountIndex,
    protocol_level_locks::{
        LockAccountFunds, LockConfig, LockInfo, LockRecipients, LockedTokenAmount,
    },
    protocol_level_tokens::{CborHolderAccount, TokenAmount},
};
use plt_block_state::block_state_interface::BlockStateQuery;
use plt_block_state::entity::protocol_level_locks::p11::LockP11;
use plt_block_state::external::AccountNotFoundByIndexError;
use plt_block_state::persistent::protocol_level_locks::p11::{
    LockConfiguration, LockRecipients as BlockStateLockRecipients,
};

/// Get the recipients for a lock configuration, resolving [`AccountIndex`]es
/// to [`CborHolderAccount`]s unless the block-state sentinel represents an
/// any-recipient lock.
fn get_recipients<BSQ: BlockStateQuery>(
    bsq: &BSQ,
    configuration: &LockConfiguration,
) -> Result<LockRecipients, AccountNotFoundByIndexError> {
    match &configuration.recipients {
        BlockStateLockRecipients::Any => Ok(LockRecipients::Any),
        BlockStateLockRecipients::Limited(recipients) => {
            let recipients = recipients
                .iter()
                .map(|account_index| {
                    let with_addr = bsq.account_by_index(*account_index)?;
                    Ok(CborHolderAccount::from(with_addr.canonical_account_address))
                })
                .collect::<Result<Vec<_>, _>>()?;

            Ok(LockRecipients::Limited(recipients))
        }
    }
}

/// Get the lock configuration as a CBOR-representable [`LockConfig`] with
/// resolved account addresses.
pub fn get_lock_config<BSQ: BlockStateQuery>(
    bsq: &BSQ,
    configuration: &LockConfiguration,
) -> Result<LockConfig, AccountNotFoundByIndexError> {
    let recipients = get_recipients(bsq, configuration)?;
    let controller = configuration.controller.to_cbor_controller(bsq)?;

    Ok(LockConfig {
        recipients,
        expiry: configuration.expiry,
        controller,
    })
}

/// Build the [`LockInfo`] for a lock from its [`LockConfiguration`] and the live
/// per-`(account, token)` balances held by the lock.
pub fn get_lock_info<BSQ: BlockStateQuery>(
    bsq: &BSQ,
    lock: &LockP11,
    lock_configuration: &LockConfiguration,
) -> Result<LockInfo, QueryLockError> {
    // Resolve recipients (block-state `AccountIndex`es) into `CborHolderAccount` values
    // by looking up each account's canonical address.
    let recipients = get_recipients(bsq, lock_configuration)?;

    // Convert the lock controller configuration into the CBOR `LockController` shape used
    // by the `lock-info` payload. Variant-specific resolution (e.g. expanding grant
    // `AccountIndex`es to `CborHolderAccount`) lives on the per-variant
    // `crate::locks::lock_controller::LockController` impl.
    let controller = lock_configuration.controller.to_cbor_controller(bsq)?;

    // Group the tracked `(account, token)` balances by account so we emit a single
    // `LockAccountFunds` entry per account.
    let mut funds_by_account: BTreeMap<AccountIndex, Vec<LockedTokenAmount>> = BTreeMap::new();
    for (account_index, token) in bsq.lock_balances(lock) {
        let token_configuration = bsq.token_configuration(&token);

        // for each locked balance record for the lock, get the locked token amount recorded in the
        // account state of the token.
        let raw_balance = token_module::query_locked_balance(
            bsq.context(),
            &bsq.token_p11(&token),
            account_index,
            &lock_configuration.lock_id,
        )
        .map_err(|err| QueryLockError::StateInvariantViolation(err.to_string()))?;
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
            let with_addr = bsq.account_by_index(account_index).map_err(|_| {
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

    Ok(LockInfo {
        lock: lock_configuration.lock_id.clone(),
        recipients,
        expiry: lock_configuration.expiry,
        controller,
        funds,
    })
}
