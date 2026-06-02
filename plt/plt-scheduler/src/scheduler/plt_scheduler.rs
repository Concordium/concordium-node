//! Scheduler implementation for protocol-level lock operations.

use crate::locks::lock_controller::LockController;
use crate::locks::{get_lock_config, lock_controller};
use crate::protocol_level_tokens::balance_operations;
use crate::protocol_level_tokens::token_module::errors::InsufficientBalanceError;
use crate::scheduler::TransactionFailure;
use crate::transaction_execution::TransactionExecution;
use concordium_base::common::cbor::{self};
use concordium_base::protocol_level_locks::LockId;
use concordium_base::protocol_level_tokens::meta_operations::LockOperation;
use concordium_base::protocol_level_tokens::{
    DeserializationFailureRejectReason, RawCbor, TokenAmount as BaseTokenAmount,
    TokenBalanceInsufficientRejectReason, TokenModuleRejectReason,
};
use concordium_base::transactions;
use plt_block_state::block_state::ExecutionTimeBlockStateP11;
use plt_block_state::entity::accounts::{Account, Accounts};
use plt_block_state::entity::block_state::TokenNotFoundByIdError;
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::protocol_level_tokens::p11::TokenP11;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::external::{OverflowError, RawTokenAmountDelta};
use plt_block_state::failure::{BlockStateFailure, BlockStateResult};
use plt_block_state::persistent::protocol_level_locks::p11::{
    LockConfiguration, LockControllerConfig,
};
use plt_block_state::persistent::protocol_level_tokens::p9::TokenConfiguration;
use plt_scheduler_types::types::events::{self, BlockItemEvent, TokenTransferEvent};
use plt_scheduler_types::types::reject_reasons::{
    EncodedTokenModuleRejectReason, TransactionRejectReason,
};
use plt_scheduler_types::types::tokens::{RawTokenAmount, TokenAmount, TokenHolder};

/// Execute [`LockOperation`].
pub fn execute_lock_operation<C: EntityContextTypes>(
    context: &mut EntityContext<C>,
    transaction_execution: &mut TransactionExecution,
    block_state: &mut BlockStateP11,
    operation_index: usize,
    lock_operation: LockOperation,
    events: &mut Vec<BlockItemEvent>,
) -> Result<(), TransactionFailure>
where
    EntityContext<C>: Clone,
{
    match lock_operation {
        LockOperation::Fund(meta_lock_fund_details) => {
            // TODO: (COR-2306) charge.
            let bsq = ExecutionTimeBlockStateP11 {
                block_state: block_state.clone(),
                context: context.clone(),
            };
            let mut lock = block_state
                .lock_by_id(context, &meta_lock_fund_details.lock)?
                .map_err(|err| TransactionRejectReason::NonExistentLockId(err.0))?;

            let lock_configuration = lock.lock_configuration(context);
            if lock_configuration
                .expiry()
                .is_expired(transaction_execution.timestamp())
            {
                return Err(TransactionRejectReason::LockExpired(lock.lock_id().clone()).into());
            }

            lock_configuration.controller().validate_operation(
                &bsq,
                transaction_execution.sender_account_address(),
                transaction_execution.sender_account(),
                &lock_controller::LockOperation::Fund(meta_lock_fund_details.clone()),
            )?;

            let mut token = block_state
                .token_by_id(context, &meta_lock_fund_details.token)?
                .map_err(|TokenNotFoundByIdError(token_id)| {
                    TransactionRejectReason::NonExistentTokenId(token_id)
                })?;
            let token_configuration = token.token_p9_base.token_configuration(context)?;
            let raw_amount = parse_raw_amount(
                &token_configuration,
                meta_lock_fund_details.amount,
                operation_index,
            )?;

            let available =
                get_available_balance(context, &token, transaction_execution.sender_account())?;
            if raw_amount > available {
                return Err(token_balance_insufficient_reject_reason(
                    operation_index,
                    &token_configuration,
                    InsufficientBalanceError {
                        available,
                        required: raw_amount,
                    },
                )
                .into());
            }

            let sender_index = transaction_execution.sender_account().account_index();
            let old_locked =
                token.get_locked_balance_for_account(context, sender_index, lock.lock_id())?;
            let new_locked = old_locked.checked_add(raw_amount).ok_or_else(|| {
                BlockStateFailure::Invariant("Locked balance overflow at fund".to_string())
            })?;
            token.set_locked_balance_for_account(
                context,
                sender_index,
                lock.lock_id(),
                new_locked,
            )?;
            let token_index = token.token_p9_base.token_index();
            block_state.update_token(context, token)?;

            if old_locked == RawTokenAmount(0) && new_locked > RawTokenAmount(0) {
                lock.add_lock_balance_ref(sender_index, token_index);
                block_state.update_lock(context, lock)?;
            }

            let memo = meta_lock_fund_details.memo.map(transactions::Memo::from);
            events.push(BlockItemEvent::TokenTransfer(TokenTransferEvent {
                token_id: token_configuration.token_id,
                from: TokenHolder::Account(transaction_execution.sender_account_address()),
                to: TokenHolder::Account(transaction_execution.sender_account_address()),
                amount: TokenAmount::from_raw(raw_amount.0, token_configuration.decimals),
                memo,
                from_lock: None,
                to_lock: Some(meta_lock_fund_details.lock),
            }));
        }
        LockOperation::Send(meta_lock_send_details) => {
            // TODO: (COR-2306) charge.
            let bsq = ExecutionTimeBlockStateP11 {
                block_state: block_state.clone(),
                context: context.clone(),
            };
            let mut lock = block_state
                .lock_by_id(context, &meta_lock_send_details.lock)?
                .map_err(|err| TransactionRejectReason::NonExistentLockId(err.0))?;

            let lock_configuration = lock.lock_configuration(context);
            if lock_configuration
                .expiry()
                .is_expired(transaction_execution.timestamp())
            {
                return Err(TransactionRejectReason::LockExpired(lock.lock_id().clone()).into());
            }

            let source_address = meta_lock_send_details.source.address;
            let source = context
                .account_by_address(&source_address)
                .map_err(|_| TransactionRejectReason::InvalidAccountReference(source_address))?;
            let recipient_address = meta_lock_send_details.recipient.address;
            let recipient = context
                .account_by_address(&recipient_address)
                .map_err(|_| TransactionRejectReason::InvalidAccountReference(recipient_address))?;

            if !lock_configuration.is_recipient(&recipient.account_index()) {
                return Err(TransactionRejectReason::LockRecipientNotPermitted(
                    lock.lock_id().clone(),
                    recipient_address,
                )
                .into());
            }

            lock_configuration.controller().validate_operation(
                &bsq,
                transaction_execution.sender_account_address(),
                transaction_execution.sender_account(),
                &lock_controller::LockOperation::Send(meta_lock_send_details.clone()),
            )?;

            let mut token = block_state
                .token_by_id(context, &meta_lock_send_details.token)?
                .map_err(|TokenNotFoundByIdError(token_id)| {
                    TransactionRejectReason::NonExistentTokenId(token_id)
                })?;
            let token_configuration = token.token_p9_base.token_configuration(context)?;
            let raw_amount = parse_raw_amount(
                &token_configuration,
                meta_lock_send_details.amount,
                operation_index,
            )?;
            let old_locked = token.get_locked_balance_for_account(
                context,
                source.account_index(),
                lock.lock_id(),
            )?;
            let new_locked = match old_locked.checked_sub(raw_amount) {
                Some(new_locked) => new_locked,
                None => {
                    return Err(token_balance_insufficient_reject_reason(
                        operation_index,
                        &token_configuration,
                        InsufficientBalanceError {
                            available: old_locked,
                            required: raw_amount,
                        },
                    )
                    .into());
                }
            };
            token.set_locked_balance_for_account(
                context,
                source.account_index(),
                lock.lock_id(),
                new_locked,
            )?;

            source
                .update_token_account_balance(
                    context,
                    token.token_p9_base.token_index(),
                    RawTokenAmountDelta::Subtract(raw_amount),
                )
                .map_err(|_err: OverflowError| {
                    BlockStateFailure::Invariant(
                        "Transfer source token amount overflow".to_string(),
                    )
                })?;
            recipient
                .update_token_account_balance(
                    context,
                    token.token_p9_base.token_index(),
                    RawTokenAmountDelta::Add(raw_amount),
                )
                .map_err(|_err: OverflowError| {
                    BlockStateFailure::Invariant(
                        "Transfer destination token amount overflow".to_string(),
                    )
                })?;

            let token_index = token.token_p9_base.token_index();
            block_state.update_token(context, token)?;

            let mut destroy_lock = false;
            if old_locked != raw_amount || old_locked == RawTokenAmount(0) {
                // No lock balance ref is removed unless the full remaining locked amount is sent.
            } else if !lock.remove_lock_balance_ref(source.account_index(), token_index) {
                // Nothing to update if there was no corresponding lock balance ref.
            } else if lock.lock_balance_refs().is_empty()
                && !lock_configuration_keeps_alive(&lock_configuration)
            {
                block_state.delete_lock(context, lock.lock_id())?;
                destroy_lock = true;
            } else {
                block_state.update_lock(context, lock)?;
            }

            let memo = meta_lock_send_details.memo.map(transactions::Memo::from);
            events.push(BlockItemEvent::TokenTransfer(TokenTransferEvent {
                token_id: token_configuration.token_id,
                from: TokenHolder::Account(source_address),
                to: TokenHolder::Account(recipient_address),
                amount: TokenAmount::from_raw(raw_amount.0, token_configuration.decimals),
                memo,
                from_lock: Some(meta_lock_send_details.lock.clone()),
                to_lock: None,
            }));
            if destroy_lock {
                events.push(BlockItemEvent::LockDestroyed(events::LockDestroyEvent {
                    lock_id: meta_lock_send_details.lock,
                }));
            }
        }
        LockOperation::Return(meta_lock_return_details) => {
            // TODO: (COR-2306) charge.
            let bsq = ExecutionTimeBlockStateP11 {
                block_state: block_state.clone(),
                context: context.clone(),
            };
            let mut lock = block_state
                .lock_by_id(context, &meta_lock_return_details.lock)?
                .map_err(|err| TransactionRejectReason::NonExistentLockId(err.0))?;

            let lock_configuration = lock.lock_configuration(context);
            if lock_configuration
                .expiry()
                .is_expired(transaction_execution.timestamp())
            {
                return Err(TransactionRejectReason::LockExpired(lock.lock_id().clone()).into());
            }

            let source_address = meta_lock_return_details.source.address;
            let source = context
                .account_by_address(&source_address)
                .map_err(|_| TransactionRejectReason::InvalidAccountReference(source_address))?;

            lock_configuration.controller().validate_operation(
                &bsq,
                transaction_execution.sender_account_address(),
                transaction_execution.sender_account(),
                &lock_controller::LockOperation::Return(meta_lock_return_details.clone()),
            )?;

            let mut token = block_state
                .token_by_id(context, &meta_lock_return_details.token)?
                .map_err(|TokenNotFoundByIdError(token_id)| {
                    TransactionRejectReason::NonExistentTokenId(token_id)
                })?;
            let token_configuration = token.token_p9_base.token_configuration(context)?;
            let raw_amount = parse_raw_amount(
                &token_configuration,
                meta_lock_return_details.amount,
                operation_index,
            )?;
            let old_locked = token.get_locked_balance_for_account(
                context,
                source.account_index(),
                lock.lock_id(),
            )?;
            let new_locked = match old_locked.checked_sub(raw_amount) {
                Some(new_locked) => new_locked,
                None => {
                    return Err(token_balance_insufficient_reject_reason(
                        operation_index,
                        &token_configuration,
                        InsufficientBalanceError {
                            available: old_locked,
                            required: raw_amount,
                        },
                    )
                    .into());
                }
            };
            token.set_locked_balance_for_account(
                context,
                source.account_index(),
                lock.lock_id(),
                new_locked,
            )?;
            let token_index = token.token_p9_base.token_index();
            block_state.update_token(context, token)?;

            let mut destroy_lock = false;
            if old_locked != raw_amount || old_locked == RawTokenAmount(0) {
                // No lock balance ref is removed unless the full remaining locked amount is sent.
            } else if !lock.remove_lock_balance_ref(source.account_index(), token_index) {
                // Nothing to update if there was no corresponding lock balance ref.
            } else if lock.lock_balance_refs().is_empty()
                && !lock_configuration_keeps_alive(&lock_configuration)
            {
                block_state.delete_lock(context, lock.lock_id())?;
                destroy_lock = true;
            } else {
                block_state.update_lock(context, lock)?;
            }

            let memo = meta_lock_return_details.memo.map(transactions::Memo::from);
            events.push(BlockItemEvent::TokenTransfer(TokenTransferEvent {
                token_id: token_configuration.token_id,
                from: TokenHolder::Account(source_address),
                to: TokenHolder::Account(source_address),
                amount: TokenAmount::from_raw(raw_amount.0, token_configuration.decimals),
                memo,
                from_lock: Some(meta_lock_return_details.lock.clone()),
                to_lock: None,
            }));
            if destroy_lock {
                events.push(BlockItemEvent::LockDestroyed(events::LockDestroyEvent {
                    lock_id: meta_lock_return_details.lock,
                }));
            }
        }
        LockOperation::Create(meta_lock_create_details) => {
            let bsq = ExecutionTimeBlockStateP11 {
                block_state: block_state.clone(),
                context: context.clone(),
            };

            let config = meta_lock_create_details.config;
            let account_index = transaction_execution.sender_account().account_index();
            let sequence_number = transaction_execution.transaction_sequence_number();
            let creation_order = transaction_execution.next_lock_creation_order();
            let lock_id = LockId::new(account_index, sequence_number, creation_order);
            let controller = LockController::new(&bsq, config.controller)?;

            let recipients = config
                .recipients
                .iter()
                .map(
                    |recipient| match context.account_by_address(&recipient.address) {
                        Ok(account) => Ok(account.account_index()),
                        Err(_) => Err(TransactionRejectReason::InvalidAccountReference(
                            recipient.address,
                        )),
                    },
                )
                .collect::<Result<Vec<_>, TransactionRejectReason>>()?;
            let configuration = LockConfiguration::new(recipients, config.expiry, controller);

            let config = get_lock_config(&bsq, &configuration).map_err(|err| {
                BlockStateFailure::Invariant(format!(
                    "Failed to get lock config for created lock: {err}"
                ))
            })?;
            let event = events::LockCreateEvent {
                lock_id: lock_id.clone(),
                lock_config: RawCbor::from(cbor::cbor_encode(&config)),
            };
            events.push(BlockItemEvent::LockCreated(event));

            block_state.create_lock(context, lock_id.clone(), configuration)?;
        }
        LockOperation::Cancel(meta_lock_cancel_details) => {
            let bsq = ExecutionTimeBlockStateP11 {
                block_state: block_state.clone(),
                context: context.clone(),
            };

            // TODO: (COR-2306) charge.
            let lock = block_state
                .lock_by_id(context, &meta_lock_cancel_details.lock)?
                .map_err(|err| TransactionRejectReason::NonExistentLockId(err.0))?;

            let lock_configuration = lock.lock_configuration(context);
            let memo: Option<transactions::Memo> = meta_lock_cancel_details
                .memo
                .clone()
                .map(transactions::Memo::from);

            if !lock_configuration
                .expiry()
                .is_expired(transaction_execution.timestamp())
            {
                lock_configuration.controller().validate_operation(
                    &bsq,
                    transaction_execution.sender_account_address(),
                    transaction_execution.sender_account(),
                    &lock_controller::LockOperation::Cancel(meta_lock_cancel_details),
                )?;
            }
            for (account_index, token_index) in lock.lock_balance_refs() {
                let mut token = block_state.token_by_index(context, token_index)?;
                balance_operations::unlock_balance(
                    context,
                    events,
                    &mut token,
                    account_index,
                    lock.lock_id(),
                    &memo,
                )?;
                block_state.update_token(context, token)?;
            }
            block_state.delete_lock(context, lock.lock_id())?;
            let event = events::LockDestroyEvent {
                lock_id: lock.lock_id().clone(),
            };
            events.push(BlockItemEvent::LockDestroyed(event));
        }
    }
    Ok(())
}

fn lock_configuration_keeps_alive(configuration: &LockConfiguration) -> bool {
    match configuration.controller() {
        LockControllerConfig::SimpleV0(controller) => controller.keep_alive,
    }
}

fn get_available_balance<C: EntityContextTypes>(
    context: &EntityContext<C>,
    token: &TokenP11,
    account: &Account,
) -> BlockStateResult<RawTokenAmount> {
    let total = account.account_token_balance(context, token.token_p9_base.token_index());
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

fn parse_raw_amount(
    token_configuration: &TokenConfiguration,
    amount: BaseTokenAmount,
    operation_index: usize,
) -> Result<RawTokenAmount, TransactionRejectReason> {
    if amount.decimals() != token_configuration.decimals {
        Err(token_deserialization_failure_reject_reason(
            token_configuration,
            operation_index,
            format!(
                "Token amount decimals mismatch: expected {}, found {}",
                token_configuration.decimals,
                amount.decimals()
            ),
        ))
    } else {
        Ok(RawTokenAmount(amount.value()))
    }
}

fn token_deserialization_failure_reject_reason(
    token_configuration: &TokenConfiguration,
    operation_index: usize,
    cause: String,
) -> TransactionRejectReason {
    let _ = operation_index;
    let (reason_type, details) =
        TokenModuleRejectReason::DeserializationFailure(DeserializationFailureRejectReason {
            cause: Some(cause),
        })
        .encode_reject_reason();

    TransactionRejectReason::TokenUpdateTransactionFailed(EncodedTokenModuleRejectReason {
        token_id: token_configuration.token_id.clone(),
        reason_type: reason_type.to_type_discriminator(),
        details: Some(details),
    })
}

fn token_balance_insufficient_reject_reason(
    operation_index: usize,
    token_configuration: &TokenConfiguration,
    error: InsufficientBalanceError,
) -> TransactionRejectReason {
    let (reason_type, details) =
        TokenModuleRejectReason::TokenBalanceInsufficient(TokenBalanceInsufficientRejectReason {
            index: operation_index as u64,
            available_balance: BaseTokenAmount::from_raw(
                error.available.0,
                token_configuration.decimals,
            ),
            required_balance: BaseTokenAmount::from_raw(
                error.required.0,
                token_configuration.decimals,
            ),
        })
        .encode_reject_reason();

    TransactionRejectReason::TokenUpdateTransactionFailed(EncodedTokenModuleRejectReason {
        token_id: token_configuration.token_id.clone(),
        reason_type: reason_type.to_type_discriminator(),
        details: Some(details),
    })
}
