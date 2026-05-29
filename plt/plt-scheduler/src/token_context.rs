use crate::token_module::errors::{InsufficientBalanceError, MintWouldOverflowError};
use concordium_base::contracts_common::AccountAddress;
use concordium_base::transactions::Memo;
use plt_block_state::entity::accounts::Account;
use plt_block_state::entity::protocol_level_tokens::p9::TokenP9Base;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::external::{OverflowError, RawTokenAmountDelta};
use plt_block_state::failure::{BlockStateFailure, BlockStateResult};
use plt_scheduler_types::types::events::{
    BlockItemEvent, TokenBurnEvent, TokenMintEvent, TokenTransferEvent,
};
use plt_scheduler_types::types::tokens::{RawTokenAmount, TokenAmount, TokenHolder};

// todo ar move to token module

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
) -> BlockStateResult<Result<(), MintWouldOverflowError>> {
    let token_configuration = token.token_configuration(context)?;

    // Update total supply
    let new_circulating_supply = match token
        .token_circulating_supply()
        .0
        .checked_add(amount.0)
        .map(RawTokenAmount)
    {
        Some(circulating_supply) => circulating_supply,
        None => {
            return Ok(Err(MintWouldOverflowError {
                requested_amount: amount,
                current_supply: token.token_circulating_supply(),
                max_representable_amount: RawTokenAmount::MAX,
            }));
        }
    };

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

    Ok(Ok(()))
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
    token: &mut TokenP9Base,
    account: &Account,
    account_address: AccountAddress,
    amount: RawTokenAmount,
) -> BlockStateResult<Result<(), InsufficientBalanceError>> {
    let token_configuration = token.token_configuration(context)?;

    // Update balance of the account
    match account.update_token_account_balance(
        context,
        token.token_index(),
        RawTokenAmountDelta::Subtract(amount),
    ) {
        Ok(()) => (),
        Err(OverflowError) => {
            return Ok(Err(InsufficientBalanceError {
                available: account.account_token_balance(context, token.token_index()),
                required: amount,
            }));
        }
    };

    // Update total supply
    let new_circulating_supply = token
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
    token.set_token_circulating_supply(new_circulating_supply);

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

    Ok(Ok(()))
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
    token: &mut TokenP9Base,
    from: &Account,
    from_address: AccountAddress,
    to: &Account,
    to_address: AccountAddress,
    amount: RawTokenAmount,
    memo: Option<Memo>,
) -> BlockStateResult<Result<(), InsufficientBalanceError>> {
    let token_configuration = token.token_configuration(context)?;

    // Update sender balance
    match from.update_token_account_balance(
        context,
        token.token_index(),
        RawTokenAmountDelta::Subtract(amount),
    ) {
        Ok(()) => (),
        Err(OverflowError) => {
            return Ok(Err(InsufficientBalanceError {
                available: from.account_token_balance(context, token.token_index()),
                required: amount,
            }));
        }
    };

    // Update receiver balance
    to.update_token_account_balance(
        context,
        token.token_index(),
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

    Ok(Ok(()))
}
