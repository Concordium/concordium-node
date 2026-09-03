use crate::failure::{ResultWithBlockStateFailure, ResultWithBlockStateFailureExt};
use crate::protocol_level_locks::{
    lock_configuration, lock_configuration::get_lock_config, lock_controller,
};
use crate::protocol_level_tokens::token_module::check_transfer_constraints;
use crate::protocol_level_tokens::{balance_operations, reject, token_amount, token_module};
use crate::transaction_execution::TransactionExecution;
use concordium_base::base::AccountIndex;
use concordium_base::common::cbor;
use concordium_base::contracts_common::Duration;
use concordium_base::protocol_level_locks::LockRecipients as CborLockRecipients;
use concordium_base::protocol_level_locks::{
    LockAccountFunds, LockId, LockInfo, LockedTokenAmount,
};
use concordium_base::protocol_level_tokens::TokenAmount;
use concordium_base::protocol_level_tokens::meta_operations::{
    LockOperation, MetaLockCancelDetails, MetaLockCreateDetails, MetaLockFundDetails,
    MetaLockReturnDetails, MetaLockSendDetails,
};
use concordium_base::protocol_level_tokens::{CborHolderAccount, RawCbor};
use concordium_base::transactions;
use plt_block_state::entity::accounts::Accounts;
use plt_block_state::entity::block_state::LockNotFoundByIdError;
use plt_block_state::entity::block_state::TokenNotFoundByIdError;
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::protocol_level_locks::p11::LocksP11;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::external::AccountNotFoundByIndexError;
use plt_block_state::failure::{BlockStateFailure, BlockStateResult};
use plt_block_state::persistent::protocol_level_locks::p11::{
    LockConfiguration, LockControllerConfig, LockRecipients,
};
use plt_scheduler_types::types::events::{self, BlockItemEvent};
use plt_scheduler_types::types::reject_reasons::TransactionRejectReason;
use plt_scheduler_types::types::tokens::RawTokenAmount;
use std::collections::BTreeMap;

/// Get the [`LockId`]s of all protocol-level locks registered on the chain at the
/// end of the block.
///
/// NOTE: this is a naive implementation. We might need to optimize with a streaming solution
/// instead, to not load all locks in existence into memory all at once.
pub fn query_lock_list<C: EntityContextTypes>(
    context: &EntityContext<C>,
    block_state: &BlockStateP11,
) -> BlockStateResult<Vec<LockId>> {
    block_state.lock_list(context)
}

/// Query [`LockInfo`] a lock.
///
/// The function builds the [`LockInfo`] from the locks static [`LockConfiguration`] and
/// the non-static per-`(account, token)` balances held by the lock.
pub fn query_lock_info<C: EntityContextTypes>(
    context: &EntityContext<C>,
    block_state: &BlockStateP11,
    lock_id: &LockId,
) -> ResultWithBlockStateFailure<RawCbor, LockNotFoundByIdError> {
    let lock = block_state.lock_by_id(context, lock_id)??;
    let configuration = lock.lock_configuration(context)?;

    // Resolve recipients (block-state `AccountIndex`es) into `CborHolderAccount` values
    // by looking up each account's canonical address.
    let recipients = lock_configuration::get_recipients(context, &configuration)?;

    // Convert the lock controller configuration into the CBOR `LockController` shape used
    // by the `lock-info` payload. Variant-specific resolution (e.g. expanding grant
    // `AccountIndex`es to `CborHolderAccount`) lives on the per-variant
    // `crate::locks::lock_controller::LockController` impl.
    let controller = lock_controller::to_cbor_controller(context, &configuration.controller)?;

    // Group the tracked `(account, token)` balances by account so we emit a single
    // `LockAccountFunds` entry per account.
    let mut funds_by_account: BTreeMap<AccountIndex, Vec<LockedTokenAmount>> = BTreeMap::new();
    for (account_index, token_index) in lock.lock_balance_refs() {
        let token = block_state.token_by_index(context, token_index)?;
        let token_configuration = token.token_p9_base.token_configuration(context)?;

        // for each locked balance record for the lock, get the locked token amount recorded in the
        // account state of the token.
        let raw_balance =
            token_module::query_locked_balance(context, &token, account_index, lock.lock_id())?;
        let amount = TokenAmount::from_raw(raw_balance.into(), token_configuration.decimals);
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

    let lock_info = LockInfo {
        lock: lock.lock_id().clone(),
        recipients,
        expiry: configuration.expiry,
        controller,
        metadata: configuration.metadata.clone(),
        funds,
    };

    Ok(RawCbor::from(cbor::cbor_encode(&lock_info)))
}

/// Context shared by a lock operation within a meta update.
pub struct LockOperationContext<'a> {
    /// Maximum permitted duration for a newly created lock.
    pub max_lock_duration: Duration,
    /// Zero-based index of the operation in the meta update.
    pub operation_index: usize,
    /// Events emitted by the operation.
    pub events: &'a mut Vec<BlockItemEvent>,
}

/// Execute [`LockOperation`].
pub fn execute_lock_operation<C: EntityContextTypes>(
    context: &mut EntityContext<C>,
    transaction_execution: &mut TransactionExecution,
    block_state: &mut BlockStateP11,
    locks: &mut LocksP11,
    lock_context: LockOperationContext<'_>,
    lock_operation: LockOperation,
) -> ResultWithBlockStateFailure<(), TransactionRejectReason> {
    match lock_operation {
        LockOperation::Fund(details) => execute_lock_fund(
            context,
            transaction_execution,
            block_state,
            locks,
            lock_context.operation_index,
            details,
            lock_context.events,
        ),
        LockOperation::Send(details) => execute_lock_send(
            context,
            transaction_execution,
            block_state,
            locks,
            lock_context.operation_index,
            details,
            lock_context.events,
        ),
        LockOperation::Return(details) => execute_lock_return(
            context,
            transaction_execution,
            block_state,
            locks,
            lock_context.operation_index,
            details,
            lock_context.events,
        ),
        LockOperation::Create(details) => execute_lock_create(
            context,
            transaction_execution,
            block_state,
            locks,
            lock_context.max_lock_duration,
            details,
            lock_context.events,
        ),
        LockOperation::Cancel(details) => execute_lock_cancel(
            context,
            transaction_execution,
            block_state,
            locks,
            details,
            lock_context.events,
        ),
    }
}

fn execute_lock_fund<C: EntityContextTypes>(
    context: &mut EntityContext<C>,
    transaction_execution: &TransactionExecution,
    block_state: &mut BlockStateP11,
    locks: &mut LocksP11,
    operation_index: usize,
    details: MetaLockFundDetails,
    events: &mut Vec<BlockItemEvent>,
) -> ResultWithBlockStateFailure<(), TransactionRejectReason> {
    // TODO: (COR-2306) charge.
    let mut lock = locks
        .by_id(context, &details.lock)?
        .map_err(|err| TransactionRejectReason::NonExistentLockId(err.0))?;

    let lock_configuration = lock.lock_configuration(context)?;
    if lock_configuration
        .expiry
        .is_expired(transaction_execution.timestamp())
    {
        return Err(TransactionRejectReason::LockExpired(lock.lock_id().clone()).into());
    }

    lock_controller::validate_operation(
        &lock_configuration.controller,
        transaction_execution.sender_account_address(),
        transaction_execution.sender_account(),
        &lock_controller::LockOperation::Fund(details.clone()),
    )?;

    let mut token = block_state.token_by_id(context, &details.token)?.map_err(
        |TokenNotFoundByIdError(token_id)| TransactionRejectReason::NonExistentTokenId(token_id),
    )?;
    let token_configuration = token.token_p9_base.token_configuration(context)?;
    let raw_amount = token_amount::to_raw_token_amount(&token_configuration, details.amount)
        .map_err(|err| {
            reject::deserialization_failure_amount_decimals_mismatch(&token_configuration, err)
        })?;

    let memo = details.memo.map(transactions::Memo::from);
    let is_new_holder = balance_operations::lock_amount(
        context,
        events,
        &mut token,
        transaction_execution.sender_account(),
        transaction_execution.sender_account_address(),
        lock.lock_id(),
        raw_amount,
        memo,
    )
    .map_nested_err(|err| {
        reject::insufficient_balance(&token_configuration, operation_index, err)
    })?;

    let token_index = token.token_p9_base.token_index();
    block_state.update_token(context, token)?;

    if is_new_holder {
        lock.add_lock_balance_ref(
            transaction_execution.sender_account().account_index(),
            token_index,
        );
        locks.update(context, lock)?;
    }
    Ok(())
}

fn execute_lock_send<C: EntityContextTypes>(
    context: &mut EntityContext<C>,
    transaction_execution: &TransactionExecution,
    block_state: &mut BlockStateP11,
    locks: &mut LocksP11,
    operation_index: usize,
    details: MetaLockSendDetails,
    events: &mut Vec<BlockItemEvent>,
) -> ResultWithBlockStateFailure<(), TransactionRejectReason> {
    // TODO: (COR-2306) charge.
    let lock = locks
        .by_id(context, &details.lock)?
        .map_err(|err| TransactionRejectReason::NonExistentLockId(err.0))?;

    let lock_configuration = lock.lock_configuration(context)?;
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

    check_transfer_constraints(
        context,
        &token.token_p9_base,
        &source,
        source_address,
        &recipient,
        recipient_address,
        operation_index,
    )?;

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

    lock_controller::validate_operation(
        &lock_configuration.controller,
        transaction_execution.sender_account_address(),
        transaction_execution.sender_account(),
        &lock_controller::LockOperation::Send(details.clone()),
    )?;

    let raw_amount = token_amount::to_raw_token_amount(&token_configuration, details.amount)
        .map_err(|err| {
            reject::deserialization_failure_amount_decimals_mismatch(&token_configuration, err)
        })?;

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
    )
    .map_nested_err(|err| {
        reject::insufficient_balance(&token_configuration, operation_index, err)
    })?;

    let token_index = token.token_p9_base.token_index();
    block_state.update_token(context, token)?;

    if remaining_locked == RawTokenAmount::from(0) {
        remove_lock_balance_ref(
            context,
            block_state,
            locks,
            events,
            lock_configuration_keeps_alive(&lock_configuration),
            lock,
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
    locks: &mut LocksP11,
    operation_index: usize,
    details: MetaLockReturnDetails,
    events: &mut Vec<BlockItemEvent>,
) -> ResultWithBlockStateFailure<(), TransactionRejectReason> {
    // TODO: (COR-2306) charge.
    let lock = locks
        .by_id(context, &details.lock)?
        .map_err(|err| TransactionRejectReason::NonExistentLockId(err.0))?;

    let lock_configuration = lock.lock_configuration(context)?;
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

    lock_controller::validate_operation(
        &lock_configuration.controller,
        transaction_execution.sender_account_address(),
        transaction_execution.sender_account(),
        &lock_controller::LockOperation::Return(details.clone()),
    )?;

    let mut token = block_state.token_by_id(context, &details.token)?.map_err(
        |TokenNotFoundByIdError(token_id)| TransactionRejectReason::NonExistentTokenId(token_id),
    )?;
    let token_configuration = token.token_p9_base.token_configuration(context)?;
    let raw_amount = token_amount::to_raw_token_amount(&token_configuration, details.amount)
        .map_err(|err| {
            reject::deserialization_failure_amount_decimals_mismatch(&token_configuration, err)
        })?;

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
    )
    .map_nested_err(|err| {
        reject::insufficient_balance(&token_configuration, operation_index, err)
    })?;

    let token_index = token.token_p9_base.token_index();
    block_state.update_token(context, token)?;

    if remaining_locked == RawTokenAmount::from(0) {
        remove_lock_balance_ref(
            context,
            block_state,
            locks,
            events,
            lock_configuration_keeps_alive(&lock_configuration),
            lock,
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
    locks: &mut LocksP11,
    max_lock_duration: Duration,
    details: MetaLockCreateDetails,
    events: &mut Vec<BlockItemEvent>,
) -> ResultWithBlockStateFailure<(), TransactionRejectReason> {
    let account_index = transaction_execution.sender_account().account_index();
    let sequence_number = transaction_execution.transaction_sequence_number();
    let creation_order = transaction_execution.next_lock_creation_order();
    let lock_id = LockId::new(account_index, sequence_number, creation_order);
    let concordium_base::protocol_level_locks::LockConfig {
        recipients,
        expiry,
        controller: controller_config,
        metadata,
    } = details.config;

    let transaction_timestamp = transaction_execution.timestamp();

    if expiry.is_expired(transaction_timestamp) {
        return Err(TransactionRejectReason::LockExpired(lock_id).into());
    }

    let Some(expiry_millis) = expiry.seconds.checked_mul(1000) else {
        return Err(TransactionRejectReason::LockDurationTooLong(lock_id).into());
    };

    if expiry_millis - transaction_timestamp.timestamp_millis() > max_lock_duration.millis() {
        return Err(TransactionRejectReason::LockDurationTooLong(lock_id).into());
    }

    let controller =
        lock_controller::from_cbor_controller(context, block_state, controller_config)?;

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
            LockRecipients::try_from(recipients)
                .map_err(|_| TransactionRejectReason::SerializationFailure)?
        }
    };
    let configuration = LockConfiguration {
        recipients,
        expiry,
        controller,
        metadata,
    };

    let config = get_lock_config(context, &configuration)?;
    let event = events::LockCreateEvent {
        lock_id: lock_id.clone(),
        lock_config: RawCbor::from(cbor::cbor_encode(&config)),
    };
    events.push(BlockItemEvent::LockCreated(event));

    locks.create(context, &lock_id, configuration)?;
    Ok(())
}

fn execute_lock_cancel<C: EntityContextTypes>(
    context: &mut EntityContext<C>,
    transaction_execution: &TransactionExecution,
    block_state: &mut BlockStateP11,
    locks: &mut LocksP11,
    details: MetaLockCancelDetails,
    events: &mut Vec<BlockItemEvent>,
) -> ResultWithBlockStateFailure<(), TransactionRejectReason> {
    // TODO: (COR-2306) charge.
    let lock = locks
        .by_id(context, &details.lock)?
        .map_err(|err| TransactionRejectReason::NonExistentLockId(err.0))?;

    let lock_configuration = lock.lock_configuration(context)?;
    let memo: Option<transactions::Memo> = details.memo.clone().map(transactions::Memo::from);

    if !lock_configuration
        .expiry
        .is_expired(transaction_execution.timestamp())
    {
        lock_controller::validate_operation(
            &lock_configuration.controller,
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
    locks.delete(context, lock.lock_id())?;
    let event = events::LockDestroyEvent {
        lock_id: lock.lock_id().clone(),
    };
    events.push(BlockItemEvent::LockDestroyed(event));
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn remove_lock_balance_ref<C: EntityContextTypes>(
    context: &EntityContext<C>,
    _block_state: &mut BlockStateP11,
    locks: &mut LocksP11,
    events: &mut Vec<BlockItemEvent>,
    lock_keeps_alive: bool,
    mut lock: plt_block_state::entity::protocol_level_locks::p11::LockP11,
    account_index: AccountIndex,
    token_index: plt_block_state::persistent::protocol_level_tokens::p9::TokenIndex,
    lock_id: LockId,
) -> ResultWithBlockStateFailure<(), TransactionRejectReason> {
    if !lock.remove_lock_balance_ref(account_index, token_index) {
        // No lock state change needed: either the account still holds a non-zero balance
        // controlled by the lock, or there was no balance reference to remove.
        return Ok(());
    }
    if lock.lock_balance_refs().is_empty() && !lock_keeps_alive {
        locks.delete(context, &lock_id)?;
        events.push(BlockItemEvent::LockDestroyed(events::LockDestroyEvent {
            lock_id,
        }));
    } else {
        locks.update(context, lock)?;
    }
    Ok(())
}

fn lock_configuration_keeps_alive(configuration: &LockConfiguration) -> bool {
    match &configuration.controller {
        LockControllerConfig::SimpleV0(controller) => controller.keep_alive,
    }
}
