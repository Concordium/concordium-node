use crate::block_state_polymorph::token::{TokenPXRef, TokenPXRefMut};
use crate::failure::{HigherLevelProtocolError, ResultWithBlockStateFailure};
use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_locks::LockId;
use concordium_base::transactions::Memo;
use plt_block_state::entity::accounts::{Account, Accounts};
use plt_block_state::entity::protocol_level_tokens::p9::TokenP9Base;
use plt_block_state::entity::protocol_level_tokens::p11::TokenP11;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::external::{OverflowError, RawTokenAmountDelta};
use plt_block_state::failure::{BlockStateFailure, BlockStateResult};
use plt_scheduler_types::types::events::{
    BlockItemEvent, TokenBurnEvent, TokenMintEvent, TokenTransferEvent,
};
use plt_scheduler_types::types::tokens::{RawTokenAmount, TokenAmount, TokenHolder};

/// The account has insufficient balance.
#[derive(Debug, thiserror::Error)]
#[error("Insufficient balance on account")]
pub struct InsufficientBalanceError {
    /// Balance available on account
    pub available: RawTokenAmount,
    /// Balance required on account
    pub required: RawTokenAmount,
}

/// Mint exceed the representable amount.
#[derive(Debug, thiserror::Error)]
#[error("Minting the requested amount would overflow the circulating supply amount")]
pub struct MintWouldOverflowError {
    /// Amount requested to be minted
    pub requested_amount: RawTokenAmount,
    /// Current circulating supply of the token
    pub current_supply: RawTokenAmount,
    /// Maximum representable token amount
    pub max_representable_amount: RawTokenAmount,
}

impl HigherLevelProtocolError for InsufficientBalanceError {}
impl HigherLevelProtocolError for MintWouldOverflowError {}

/// Get the available balance for an account.
///
/// For protocol versions without locks this is the total account balance.
/// For protocol versions with locks this is the total account balance minus the
/// sum of all locked balances.
///
/// # Errors
///
/// Returns a [`BlockStateFailure::Invariant`] if the sum of locked balances
/// overflows or exceeds the total account balance.
pub fn available_balance<C: EntityContextTypes>(
    context: &EntityContext<C>,
    token: TokenPXRef<'_>,
    account: &Account,
) -> BlockStateResult<RawTokenAmount> {
    let total = account.account_token_balance(context, token.token_p9_base().token_index());

    let TokenPXRef::TokenP11(token) = token else {
        return Ok(total);
    };

    let mut total_locked = RawTokenAmount(0);
    for (_, locked_balance) in token
        .get_locked_balances_for_account(context, account.account_index())?
        .into_iter()
    {
        total_locked = total_locked.checked_add(locked_balance).ok_or_else(|| {
            BlockStateFailure::Invariant("Total locked token balance overflow".to_string())
        })?;
    }

    total.checked_sub(total_locked).ok_or_else(|| {
        BlockStateFailure::Invariant(
            "Total locked token balance exceeds account token balance".to_string(),
        )
    })
}

/// Mint a specified amount and deposit it in the account.
///
/// # Events
///
/// This will produce a `TokenMintEvent` in the logs.
///
/// # Errors
///
/// - [`MintWouldOverflowError`] The total supply would exceed the representable amount.
pub fn mint<C: EntityContextTypes>(
    context: &mut EntityContext<C>,
    events: &mut impl Extend<BlockItemEvent>,
    token: &mut TokenP9Base,
    account: &Account,
    account_address: AccountAddress,
    amount: RawTokenAmount,
) -> ResultWithBlockStateFailure<(), MintWouldOverflowError> {
    let token_configuration = token.token_configuration(context)?;

    // Update total supply
    let new_circulating_supply = token
        .token_circulating_supply()
        .0
        .checked_add(amount.0)
        .map(RawTokenAmount)
        .ok_or(MintWouldOverflowError {
            requested_amount: amount,
            current_supply: token.token_circulating_supply(),
            max_representable_amount: RawTokenAmount::MAX,
        })?;

    token.set_token_circulating_supply(new_circulating_supply);

    // Update balance of the account
    account
        .update_token_account_balance(
            context,
            token.token_index(),
            RawTokenAmountDelta::Add(amount),
        )
        .map_err(|_err: OverflowError| {
            // We should never overflow account balance at mint, since the total circulating supply of the token
            // is always less that what is representable as a token amount.
            BlockStateFailure::Invariant("Mint destination account amount overflow".to_string())
        })?;

    // Issue event
    let event = BlockItemEvent::TokenMint(TokenMintEvent {
        token_id: token_configuration.token_id.clone(),
        target: TokenHolder::Account(account_address),
        amount: TokenAmount {
            amount,
            decimals: token_configuration.decimals,
        },
    });

    events.extend(Some(event));

    Ok(())
}

/// Burn a specified amount from the account.
///
/// # Events
///
/// This will produce a `TokenBurnEvent` in the logs.
///
/// # Errors
///
/// - [`InsufficientBalanceError`] The sender has insufficient balance.
pub fn burn<C: EntityContextTypes>(
    context: &mut EntityContext<C>,
    events: &mut impl Extend<BlockItemEvent>,
    mut token: TokenPXRefMut<'_>,
    account: &Account,
    account_address: AccountAddress,
    amount: RawTokenAmount,
) -> ResultWithBlockStateFailure<(), InsufficientBalanceError> {
    let token_configuration = token.token_p9_base().token_configuration(context)?;

    let available = available_balance(context, token.as_ref(), account)?;
    if amount > available {
        return Err(InsufficientBalanceError {
            available,
            required: amount,
        }
        .into());
    }

    // Update balance of the account
    account
        .update_token_account_balance(
            context,
            token.token_p9_base().token_index(),
            RawTokenAmountDelta::Subtract(amount),
        )
        .map_err(|_err: OverflowError| {
            BlockStateFailure::Invariant(
                "Available token balance check passed, but burn underflowed".to_string(),
            )
        })?;

    // Update total supply
    let new_circulating_supply = token
        .token_p9_base()
        .token_circulating_supply()
        .0
        .checked_sub(amount.0)
        .map(RawTokenAmount)
        .ok_or_else(||
        // We should never overflow total supply at burn, since the total circulating supply of the token
        // is always more than any account balance.
        BlockStateFailure::Invariant(
            "Circulating supply amount overflow at burn".to_string(),
        ))?;
    token
        .token_p9_base_mut()
        .set_token_circulating_supply(new_circulating_supply);

    // Issue event
    let event = BlockItemEvent::TokenBurn(TokenBurnEvent {
        token_id: token_configuration.token_id.clone(),
        target: TokenHolder::Account(account_address),
        amount: TokenAmount {
            amount,
            decimals: token_configuration.decimals,
        },
    });

    events.extend(Some(event));

    Ok(())
}

/// Transfer a token amount from one account to another, with an optional memo.
///
/// # Events
///
/// This will produce a `TokenTransferEvent` in the logs.
///
/// # Errors
///
/// - [`InsufficientBalanceError`] The sender has insufficient balance.
#[allow(clippy::too_many_arguments)]
pub fn transfer<C: EntityContextTypes>(
    context: &mut EntityContext<C>,
    events: &mut impl Extend<BlockItemEvent>,
    token: TokenPXRefMut<'_>,
    from: &Account,
    from_address: AccountAddress,
    to: &Account,
    to_address: AccountAddress,
    amount: RawTokenAmount,
    memo: Option<Memo>,
) -> ResultWithBlockStateFailure<(), InsufficientBalanceError> {
    let token_configuration = token.token_p9_base().token_configuration(context)?;

    let available = available_balance(context, token.as_ref(), from)?;
    if amount > available {
        return Err(InsufficientBalanceError {
            available,
            required: amount,
        }
        .into());
    }

    // Update sender balance
    from.update_token_account_balance(
        context,
        token.token_p9_base().token_index(),
        RawTokenAmountDelta::Subtract(amount),
    )
    .map_err(|_err: OverflowError| {
        BlockStateFailure::Invariant(
            "Available token balance check passed, but transfer underflowed".to_string(),
        )
    })?;

    // Update receiver balance
    to.update_token_account_balance(
        context,
        token.token_p9_base().token_index(),
        RawTokenAmountDelta::Add(amount),
    )
    .map_err(|_err: OverflowError| {
        // We should never overflow at transfer, since the total circulating supply of the token
        // is always less that what is representable as a token amount.
        BlockStateFailure::Invariant("Transfer destination token amount overflow".to_string())
    })?;

    // Issue event
    let event = BlockItemEvent::TokenTransfer(TokenTransferEvent {
        token_id: token_configuration.token_id.clone(),
        from: TokenHolder::Account(from_address),
        to: TokenHolder::Account(to_address),
        amount: TokenAmount {
            amount,
            decimals: token_configuration.decimals,
        },
        memo,
        from_lock: None,
        to_lock: None,
    });

    events.extend(Some(event));

    Ok(())
}

/// Move `amount` of tokens from an account's available balance into the control of a lock.
/// The tokens remain on the account but become locked.
///
/// Returns `true` if the account had no locked balance for this lock before — i.e. a new
/// `(account, lock)` balance relationship was created — and `false` if the account already
/// held a non-zero locked balance for the lock.
///
/// # Events
///
/// Produces a [`TokenTransferEvent`] with `to_lock` set to the lock id.
///
/// # Errors
///
/// - [`InsufficientBalanceError`] The account has insufficient available balance.
#[allow(clippy::too_many_arguments)]
pub fn lock_amount<C: EntityContextTypes>(
    context: &mut EntityContext<C>,
    events: &mut impl Extend<BlockItemEvent>,
    token: &mut TokenP11,
    account: &Account,
    account_address: AccountAddress,
    lock_id: &LockId,
    amount: RawTokenAmount,
    memo: Option<Memo>,
) -> ResultWithBlockStateFailure<bool, InsufficientBalanceError> {
    let available = available_balance(context, TokenPXRef::TokenP11(token), account)?;
    if amount > available {
        return Err(InsufficientBalanceError {
            available,
            required: amount,
        }
        .into());
    }

    let old_locked =
        token.get_locked_balance_for_account(context, account.account_index(), lock_id)?;
    let new_locked = old_locked.checked_add(amount).ok_or_else(|| {
        BlockStateFailure::Invariant("Locked balance overflow at fund".to_string())
    })?;
    token.set_locked_balance_for_account(context, account.account_index(), lock_id, new_locked)?;

    let token_configuration = token.token_p9_base.token_configuration(context)?;
    events.extend(Some(BlockItemEvent::TokenTransfer(TokenTransferEvent {
        token_id: token_configuration.token_id,
        from: TokenHolder::Account(account_address),
        to: TokenHolder::Account(account_address),
        amount: TokenAmount {
            amount,
            decimals: token_configuration.decimals,
        },
        memo,
        from_lock: None,
        to_lock: Some(lock_id.clone()),
    })));

    Ok(old_locked == RawTokenAmount(0) && new_locked > RawTokenAmount(0))
}

/// Move `amount` of tokens from a lock's control on `source` to `recipient`'s available balance.
///
/// Returns the remaining locked balance for `source` after the operation. A return value of
/// zero indicates the `(source, lock)` balance relationship should be removed by the caller.
///
/// # Events
///
/// Produces a [`TokenTransferEvent`] with `from_lock` set to the lock id.
///
/// # Errors
///
/// - [`InsufficientBalanceError`] The source has insufficient locked balance.
#[allow(clippy::too_many_arguments)]
pub fn send_locked_amount<C: EntityContextTypes>(
    context: &mut EntityContext<C>,
    events: &mut impl Extend<BlockItemEvent>,
    token: &mut TokenP11,
    source: &Account,
    source_address: AccountAddress,
    recipient: &Account,
    recipient_address: AccountAddress,
    lock_id: &LockId,
    amount: RawTokenAmount,
    memo: Option<Memo>,
) -> ResultWithBlockStateFailure<RawTokenAmount, InsufficientBalanceError> {
    let old_locked =
        token.get_locked_balance_for_account(context, source.account_index(), lock_id)?;
    let new_locked = old_locked
        .checked_sub(amount)
        .ok_or(InsufficientBalanceError {
            available: old_locked,
            required: amount,
        })?;
    token.set_locked_balance_for_account(context, source.account_index(), lock_id, new_locked)?;

    source
        .update_token_account_balance(
            context,
            token.token_p9_base.token_index(),
            RawTokenAmountDelta::Subtract(amount),
        )
        .map_err(|_err: OverflowError| {
            BlockStateFailure::Invariant("Transfer source token amount overflow".to_string())
        })?;
    recipient
        .update_token_account_balance(
            context,
            token.token_p9_base.token_index(),
            RawTokenAmountDelta::Add(amount),
        )
        .map_err(|_err: OverflowError| {
            BlockStateFailure::Invariant("Transfer destination token amount overflow".to_string())
        })?;

    let token_configuration = token.token_p9_base.token_configuration(context)?;
    events.extend(Some(BlockItemEvent::TokenTransfer(TokenTransferEvent {
        token_id: token_configuration.token_id,
        from: TokenHolder::Account(source_address),
        to: TokenHolder::Account(recipient_address),
        amount: TokenAmount {
            amount,
            decimals: token_configuration.decimals,
        },
        memo,
        from_lock: Some(lock_id.clone()),
        to_lock: None,
    })));

    Ok(new_locked)
}

/// Release `amount` from a lock's control back to the owner account's available balance.
/// The tokens remain on the account but are freed from lock control.
///
/// Returns the remaining locked balance for `account` after the operation. A return value of
/// zero indicates the `(account, lock)` balance relationship should be removed by the caller.
///
/// # Events
///
/// Produces a [`TokenTransferEvent`] with `from_lock` set to the lock id.
///
/// # Errors
///
/// - [`InsufficientBalanceError`] The account has insufficient locked balance.
#[allow(clippy::too_many_arguments)]
pub fn return_locked_amount<C: EntityContextTypes>(
    context: &mut EntityContext<C>,
    events: &mut impl Extend<BlockItemEvent>,
    token: &mut TokenP11,
    account_index: AccountIndex,
    account_address: AccountAddress,
    lock_id: &LockId,
    amount: RawTokenAmount,
    memo: Option<Memo>,
) -> ResultWithBlockStateFailure<RawTokenAmount, InsufficientBalanceError> {
    let old_locked = token.get_locked_balance_for_account(context, account_index, lock_id)?;
    let new_locked = old_locked
        .checked_sub(amount)
        .ok_or(InsufficientBalanceError {
            available: old_locked,
            required: amount,
        })?;
    token.set_locked_balance_for_account(context, account_index, lock_id, new_locked)?;

    let token_configuration = token.token_p9_base.token_configuration(context)?;
    events.extend(Some(BlockItemEvent::TokenTransfer(TokenTransferEvent {
        token_id: token_configuration.token_id,
        from: TokenHolder::Account(account_address),
        to: TokenHolder::Account(account_address),
        amount: TokenAmount {
            amount,
            decimals: token_configuration.decimals,
        },
        memo,
        from_lock: Some(lock_id.clone()),
        to_lock: None,
    })));

    Ok(new_locked)
}

/// Unlock the balance of an account associated with a particular lock for
/// this particular token. This generates a `TokenTransferEvent` to reflect
/// the change in the locked balance.
pub fn unlock_balance<C: EntityContextTypes>(
    context: &EntityContext<C>,
    events: &mut impl Extend<BlockItemEvent>,
    token: &mut TokenP11,
    account_index: AccountIndex,
    lock_id: &LockId,
    memo: &Option<Memo>,
) -> BlockStateResult<()> {
    let old_balance = token.get_locked_balance_for_account(context, account_index, lock_id)?;
    if old_balance == RawTokenAmount(0) {
        // No locked balance, nothing to do.
        return Ok(());
    }
    token.set_locked_balance_for_account(context, account_index, lock_id, RawTokenAmount(0))?;

    let token_configuration = token.token_p9_base.token_configuration(context)?;
    let account_address = context
        .account_by_index(account_index)
        .map_err(|err| {
            BlockStateFailure::Invariant(format!("Account not found by index: {}", err))
        })?
        .canonical_account_address;
    events.extend(Some(BlockItemEvent::TokenTransfer(TokenTransferEvent {
        token_id: token_configuration.token_id,
        from: TokenHolder::Account(account_address),
        to: TokenHolder::Account(account_address),
        amount: TokenAmount {
            amount: old_balance,
            decimals: token_configuration.decimals,
        },
        memo: memo.clone(),
        from_lock: Some(lock_id.clone()),
        to_lock: None,
    })));

    Ok(())
}
