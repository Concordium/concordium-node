//! Scheduler implementation for protocol-level token updates. This module implements execution
//! of transactions related to protocol-level tokens.

use crate::locks::lock_controller::LockController;
use crate::locks::{get_lock_config, lock_controller};
use crate::protocol_level_tokens::balance_operations;
use crate::protocol_level_tokens::token_module::errors::InsufficientBalanceError;
use crate::protocol_level_tokens::token_module::{
    TokenUpdateError, check_transfer_constraints, token_update_error_internal_to_external,
};
use crate::scheduler::TransactionFailure;
use crate::transaction_execution::TransactionExecution;
use concordium_base::base::AccountIndex;
use concordium_base::common::cbor::{self};
use concordium_base::protocol_level_locks::{LockId, LockRecipients as CborLockRecipients};
use concordium_base::protocol_level_tokens::meta_operations::{
    LockOperation, MetaLockCancelDetails, MetaLockCreateDetails, MetaLockFundDetails,
    MetaLockReturnDetails, MetaLockSendDetails,
};
use concordium_base::protocol_level_tokens::{
    DeserializationFailureRejectReason, RawCbor, TokenAmount as BaseTokenAmount,
    TokenBalanceInsufficientRejectReason, TokenModuleRejectReason,
};
use concordium_base::transactions;
use plt_block_state::block_state::ExecutionTimeBlockStateP11;
use plt_block_state::entity::accounts::Accounts;
use plt_block_state::entity::block_state::TokenNotFoundByIdError;
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::failure::BlockStateFailure;
use plt_block_state::persistent::protocol_level_locks::p11::{
    LockConfiguration, LockControllerConfig, LockRecipients,
};
use plt_block_state::persistent::protocol_level_tokens::p9::TokenConfiguration;
use plt_scheduler_types::types::events::{self, BlockItemEvent};
use plt_scheduler_types::types::reject_reasons::{
    EncodedTokenModuleRejectReason, TransactionRejectReason,
};
use plt_scheduler_types::types::tokens::RawTokenAmount;

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
        LockOperation::Fund(details) => execute_lock_fund(
            context,
            transaction_execution,
            block_state,
            operation_index,
            details,
            events,
        ),
        LockOperation::Send(details) => execute_lock_send(
            context,
            transaction_execution,
            block_state,
            operation_index,
            details,
            events,
        ),
        LockOperation::Return(details) => execute_lock_return(
            context,
            transaction_execution,
            block_state,
            operation_index,
            details,
            events,
        ),
        LockOperation::Create(details) => {
            execute_lock_create(context, transaction_execution, block_state, details, events)
        }
        LockOperation::Cancel(details) => {
            execute_lock_cancel(context, transaction_execution, block_state, details, events)
        }
    }
}

fn execute_lock_fund<C: EntityContextTypes>(
    context: &mut EntityContext<C>,
    transaction_execution: &TransactionExecution,
    block_state: &mut BlockStateP11,
    operation_index: usize,
    details: MetaLockFundDetails,
    events: &mut Vec<BlockItemEvent>,
) -> Result<(), TransactionFailure>
where
    EntityContext<C>: Clone,
{
    // TODO: (COR-2306) charge.
    let bsq = ExecutionTimeBlockStateP11 {
        block_state: block_state.clone(),
        context: context.clone(),
    };
    let mut lock = block_state
        .lock_by_id(context, &details.lock)?
        .map_err(|err| TransactionRejectReason::NonExistentLockId(err.0))?;

    let lock_configuration = lock.lock_configuration(context);
    if lock_configuration
        .expiry
        .is_expired(transaction_execution.timestamp())
    {
        return Err(TransactionRejectReason::LockExpired(lock.lock_id().clone()).into());
    }

    lock_configuration.controller.validate_operation(
        &bsq,
        transaction_execution.sender_account_address(),
        transaction_execution.sender_account(),
        &lock_controller::LockOperation::Fund(details.clone()),
    )?;

    let mut token = block_state.token_by_id(context, &details.token)?.map_err(
        |TokenNotFoundByIdError(token_id)| TransactionRejectReason::NonExistentTokenId(token_id),
    )?;
    let token_configuration = token.token_p9_base.token_configuration(context)?;
    let raw_amount = parse_raw_amount(&token_configuration, details.amount, operation_index)?;

    let memo = details.memo.map(transactions::Memo::from);
    let is_new_holder = match balance_operations::lock_amount(
        context,
        events,
        &mut token,
        transaction_execution.sender_account(),
        transaction_execution.sender_account_address(),
        lock.lock_id(),
        raw_amount,
        memo,
    )? {
        Ok(is_new_holder) => is_new_holder,
        Err(err) => {
            return Err(token_balance_insufficient_reject_reason(
                operation_index,
                &token_configuration,
                err,
            )
            .into());
        }
    };

    let token_index = token.token_p9_base.token_index();
    block_state.update_token(context, token)?;

    if is_new_holder {
        lock.add_lock_balance_ref(
            transaction_execution.sender_account().account_index(),
            token_index,
        );
        block_state.update_lock(context, lock)?;
    }
    Ok(())
}

fn execute_lock_send<C: EntityContextTypes>(
    context: &mut EntityContext<C>,
    transaction_execution: &TransactionExecution,
    block_state: &mut BlockStateP11,
    operation_index: usize,
    details: MetaLockSendDetails,
    events: &mut Vec<BlockItemEvent>,
) -> Result<(), TransactionFailure>
where
    EntityContext<C>: Clone,
{
    // TODO: (COR-2306) charge.
    let bsq = ExecutionTimeBlockStateP11 {
        block_state: block_state.clone(),
        context: context.clone(),
    };
    let lock = block_state
        .lock_by_id(context, &details.lock)?
        .map_err(|err| TransactionRejectReason::NonExistentLockId(err.0))?;

    let lock_configuration = lock.lock_configuration(context);
    if lock_configuration
        .expiry
        .is_expired(transaction_execution.timestamp())
    {
        return Err(TransactionRejectReason::LockExpired(lock.lock_id().clone()).into());
    }

    let source_address = details.source.address;
    let source = context
        .account_by_address(&source_address)
        .map_err(|_| TransactionRejectReason::InvalidAccountReference(source_address))?;
    let recipient_address = details.recipient.address;
    let recipient = context
        .account_by_address(&recipient_address)
        .map_err(|_| TransactionRejectReason::InvalidAccountReference(recipient_address))?;

    let mut token = block_state.token_by_id(context, &details.token)?.map_err(
        |TokenNotFoundByIdError(token_id)| TransactionRejectReason::NonExistentTokenId(token_id),
    )?;
    let token_configuration = token.token_p9_base.token_configuration(context)?;

    if let Err(err) = check_transfer_constraints(
        context,
        &token.token_p9_base,
        &source,
        source_address,
        &recipient,
        recipient_address,
    ) {
        let err = token_update_error_internal_to_external(
            &token_configuration,
            operation_index,
            "transfer",
            err,
        )?;
        return Err(token_update_error_reject_reason(&token_configuration, err));
    }

    if !lock_configuration
        .recipients
        .is_recipient(&recipient.account_index())
    {
        return Err(TransactionRejectReason::LockRecipientNotPermitted(
            lock.lock_id().clone(),
            recipient_address,
        )
        .into());
    }

    lock_configuration.controller.validate_operation(
        &bsq,
        transaction_execution.sender_account_address(),
        transaction_execution.sender_account(),
        &lock_controller::LockOperation::Send(details.clone()),
    )?;

    let raw_amount = parse_raw_amount(&token_configuration, details.amount, operation_index)?;

    let memo = details.memo.map(transactions::Memo::from);
    let remaining_locked = balance_operations::send_locked_amount(
        context,
        events,
        &mut token,
        &source,
        source_address,
        &recipient,
        recipient_address,
        lock.lock_id(),
        raw_amount,
        memo,
    )?
    .map_err(|err| {
        token_balance_insufficient_reject_reason(operation_index, &token_configuration, err)
    })?;

    let token_index = token.token_p9_base.token_index();
    block_state.update_token(context, token)?;

    if remaining_locked == RawTokenAmount(0) {
        remove_lock_balance_ref(
            context,
            block_state,
            events,
            lock,
            &lock_configuration,
            source.account_index(),
            token_index,
            details.lock,
        )?;
    }

    Ok(())
}

fn execute_lock_return<C: EntityContextTypes>(
    context: &mut EntityContext<C>,
    transaction_execution: &TransactionExecution,
    block_state: &mut BlockStateP11,
    operation_index: usize,
    details: MetaLockReturnDetails,
    events: &mut Vec<BlockItemEvent>,
) -> Result<(), TransactionFailure>
where
    EntityContext<C>: Clone,
{
    // TODO: (COR-2306) charge.
    let bsq = ExecutionTimeBlockStateP11 {
        block_state: block_state.clone(),
        context: context.clone(),
    };
    let lock = block_state
        .lock_by_id(context, &details.lock)?
        .map_err(|err| TransactionRejectReason::NonExistentLockId(err.0))?;

    let lock_configuration = lock.lock_configuration(context);
    if lock_configuration
        .expiry
        .is_expired(transaction_execution.timestamp())
    {
        return Err(TransactionRejectReason::LockExpired(lock.lock_id().clone()).into());
    }

    let source_address = details.source.address;
    let source = context
        .account_by_address(&source_address)
        .map_err(|_| TransactionRejectReason::InvalidAccountReference(source_address))?;

    lock_configuration.controller.validate_operation(
        &bsq,
        transaction_execution.sender_account_address(),
        transaction_execution.sender_account(),
        &lock_controller::LockOperation::Return(details.clone()),
    )?;

    let mut token = block_state.token_by_id(context, &details.token)?.map_err(
        |TokenNotFoundByIdError(token_id)| TransactionRejectReason::NonExistentTokenId(token_id),
    )?;
    let token_configuration = token.token_p9_base.token_configuration(context)?;
    let raw_amount = parse_raw_amount(&token_configuration, details.amount, operation_index)?;

    let memo = details.memo.map(transactions::Memo::from);
    let remaining_locked = balance_operations::return_locked_amount(
        context,
        events,
        &mut token,
        source.account_index(),
        source_address,
        lock.lock_id(),
        raw_amount,
        memo,
    )?
    .map_err(|err| {
        token_balance_insufficient_reject_reason(operation_index, &token_configuration, err)
    })?;

    let token_index = token.token_p9_base.token_index();
    block_state.update_token(context, token)?;

    if remaining_locked == RawTokenAmount(0) {
        remove_lock_balance_ref(
            context,
            block_state,
            events,
            lock,
            &lock_configuration,
            source.account_index(),
            token_index,
            details.lock,
        )?;
    }

    Ok(())
}

fn execute_lock_create<C: EntityContextTypes>(
    context: &mut EntityContext<C>,
    transaction_execution: &mut TransactionExecution,
    block_state: &mut BlockStateP11,
    details: MetaLockCreateDetails,
    events: &mut Vec<BlockItemEvent>,
) -> Result<(), TransactionFailure>
where
    EntityContext<C>: Clone,
{
    let bsq = ExecutionTimeBlockStateP11 {
        block_state: block_state.clone(),
        context: context.clone(),
    };

    let account_index = transaction_execution.sender_account().account_index();
    let sequence_number = transaction_execution.transaction_sequence_number();
    let creation_order = transaction_execution.next_lock_creation_order();
    let lock_id = LockId::new(account_index, sequence_number, creation_order);
    let concordium_base::protocol_level_locks::LockConfig {
        recipients,
        expiry,
        controller: controller_config,
    } = details.config;
    let controller = LockController::new(&bsq, controller_config)?;

    let recipients = match recipients {
        CborLockRecipients::Any => LockRecipients::Any,
        CborLockRecipients::Limited(recipients) => {
            let recipients = recipients
                .into_iter()
                .map(
                    |recipient| match context.account_by_address(&recipient.address) {
                        Ok(account) => Ok(account.account_index()),
                        Err(_) => Err(TransactionRejectReason::InvalidAccountReference(
                            recipient.address,
                        )),
                    },
                )
                .collect::<Result<Vec<_>, TransactionRejectReason>>()?;
            LockRecipients::from(recipients)
        }
    };
    let configuration = LockConfiguration {
        recipients,
        expiry,
        controller,
    };

    let config = get_lock_config(&bsq, &configuration).map_err(|err| {
        BlockStateFailure::Invariant(format!("Failed to get lock config for created lock: {err}"))
    })?;
    let event = events::LockCreateEvent {
        lock_id: lock_id.clone(),
        lock_config: RawCbor::from(cbor::cbor_encode(&config)),
    };
    events.push(BlockItemEvent::LockCreated(event));

    block_state.create_lock(context, lock_id.clone(), configuration)?;
    Ok(())
}

fn execute_lock_cancel<C: EntityContextTypes>(
    context: &mut EntityContext<C>,
    transaction_execution: &TransactionExecution,
    block_state: &mut BlockStateP11,
    details: MetaLockCancelDetails,
    events: &mut Vec<BlockItemEvent>,
) -> Result<(), TransactionFailure>
where
    EntityContext<C>: Clone,
{
    let bsq = ExecutionTimeBlockStateP11 {
        block_state: block_state.clone(),
        context: context.clone(),
    };

    // TODO: (COR-2306) charge.
    let lock = block_state
        .lock_by_id(context, &details.lock)?
        .map_err(|err| TransactionRejectReason::NonExistentLockId(err.0))?;

    let lock_configuration = lock.lock_configuration(context);
    let memo: Option<transactions::Memo> = details.memo.clone().map(transactions::Memo::from);

    if !lock_configuration
        .expiry
        .is_expired(transaction_execution.timestamp())
    {
        lock_configuration.controller.validate_operation(
            &bsq,
            transaction_execution.sender_account_address(),
            transaction_execution.sender_account(),
            &lock_controller::LockOperation::Cancel(details),
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
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn remove_lock_balance_ref<C: EntityContextTypes>(
    context: &EntityContext<C>,
    block_state: &mut BlockStateP11,
    events: &mut Vec<BlockItemEvent>,
    mut lock: plt_block_state::entity::protocol_level_locks::p11::LockP11,
    lock_configuration: &LockConfiguration,
    account_index: AccountIndex,
    token_index: plt_block_state::persistent::protocol_level_tokens::p9::TokenIndex,
    lock_id: concordium_base::protocol_level_locks::LockId,
) -> Result<(), TransactionFailure> {
    if !lock.remove_lock_balance_ref(account_index, token_index) {
        // No lock state change needed: either the account still holds a non-zero balance
        // controlled by the lock, or there was no balance reference to remove.
        return Ok(());
    }
    if lock.lock_balance_refs().is_empty() && !lock_configuration_keeps_alive(lock_configuration) {
        block_state.delete_lock(context, &lock_id)?;
        events.push(BlockItemEvent::LockDestroyed(events::LockDestroyEvent {
            lock_id,
        }));
    } else {
        block_state.update_lock(context, lock)?;
    }
    Ok(())
}

fn lock_configuration_keeps_alive(configuration: &LockConfiguration) -> bool {
    match &configuration.controller {
        LockControllerConfig::SimpleV0(controller) => controller.keep_alive,
    }
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

fn token_update_error_reject_reason(
    token_configuration: &TokenConfiguration,
    err: TokenUpdateError,
) -> TransactionFailure {
    match err {
        TokenUpdateError::OutOfEnergy(_) => TransactionRejectReason::OutOfEnergy.into(),
        TokenUpdateError::TokenModuleReject(reject_reason) => {
            let (reason_type, details) = reject_reason.encode_reject_reason();
            TransactionRejectReason::TokenUpdateTransactionFailed(EncodedTokenModuleRejectReason {
                token_id: token_configuration.token_id.clone(),
                reason_type: reason_type.to_type_discriminator(),
                details: Some(details),
            })
            .into()
        }
    }
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
