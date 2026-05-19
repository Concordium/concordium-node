use std::collections::BTreeMap;

use crate::{
    locks::lock_controller::LockController, queries::QueryLockError,
    token_context::TokenQueryContext, token_module,
};
use concordium_base::{
    base::AccountIndex,
    protocol_level_locks::{LockAccountFunds, LockConfig, LockInfo, LockedTokenAmount},
    protocol_level_tokens::{CborHolderAccount, TokenAmount},
};
use plt_block_state::block_state_interface::{
    AccountNotFoundByIndexError, BlockStateQuery, HasLockId,
};
use plt_block_state::persistent::protocol_level_locks::p11::LockConfiguration;

/// Get the list of recipient accounts for a lock configuration, resolving
/// [`AccountIndex`]es to [`CborHolderAccount`]s.
fn get_recipients<BSQ: BlockStateQuery>(
    bsq: &BSQ,
    configuration: &LockConfiguration,
) -> Result<Vec<CborHolderAccount>, AccountNotFoundByIndexError> {
    configuration
        .recipients_iter()
        .map(|account_index| {
            let with_addr = bsq.account_by_index(*account_index)?;
            Ok(CborHolderAccount::from(with_addr.canonical_account_address))
        })
        .collect()
}

/// Get the lock configuration as a CBOR-representable [`LockConfig`] with
/// resolved account addresses.
pub fn get_lock_config<BSQ: BlockStateQuery>(
    bsq: &BSQ,
    configuration: &LockConfiguration,
) -> Result<LockConfig, AccountNotFoundByIndexError> {
    let recipients = get_recipients(bsq, configuration)?;
    let controller = configuration.controller().to_cbor_controller(bsq)?;

    Ok(LockConfig {
        recipients,
        expiry: configuration.expiry(),
        controller,
    })
}

/// Build the [`LockInfo`] for a lock from its [`LockConfiguration`] and the live
/// per-`(account, token)` balances held by the lock.
pub fn get_lock_info<BSQ: BlockStateQuery>(
    bsq: &BSQ,
    lock: &BSQ::Lock,
    configuration: &LockConfiguration,
) -> Result<LockInfo, QueryLockError> {
    // Resolve recipients (block-state `AccountIndex`es) into `CborHolderAccount` values
    // by looking up each account's canonical address.
    let recipients = get_recipients(bsq, configuration)?;

    // Convert the lock controller configuration into the CBOR `LockController` shape used
    // by the `lock-info` payload. Variant-specific resolution (e.g. expanding grant
    // `AccountIndex`es to `CborHolderAccount`) lives on the per-variant
    // `crate::locks::lock_controller::LockController` impl.
    let controller = configuration.controller().to_cbor_controller(bsq)?;

    // Group the tracked `(account, token)` balances by account so we emit a single
    // `LockAccountFunds` entry per account.
    let mut funds_by_account: BTreeMap<AccountIndex, Vec<LockedTokenAmount>> = BTreeMap::new();
    for (account_index, token) in bsq.lock_balances(lock) {
        let token_configuration = bsq.token_configuration(&token);
        let token_module_state = bsq.mutable_token_key_value_state(&token);
        let context = TokenQueryContext {
            block_state: bsq,
            token_module_state: &token_module_state,
        };

        // for each locked balance record for the lock, get the locked token amount recorded in the
        // account state of the token.
        let raw_balance =
            token_module::query_locked_balance(&context, account_index, lock.lock_id())?;
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
        lock: lock.lock_id().clone(),
        recipients,
        expiry: configuration.expiry(),
        controller,
        funds,
    })
}
