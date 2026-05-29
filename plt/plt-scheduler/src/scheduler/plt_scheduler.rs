//! Scheduler implementation for protocol-level token updates. This module implements execution
//! of transactions related to protocol-level tokens.

use crate::locks::lock_controller::LockController;
use crate::locks::{get_lock_config, lock_controller};

use crate::scheduler::TransactionFailure;
use crate::protocol_level_tokens::balance_operations;
use crate::transaction_execution::TransactionExecution;
use concordium_base::common::cbor::{self};
use concordium_base::protocol_level_locks::LockId;
use concordium_base::protocol_level_tokens::RawCbor;
use concordium_base::protocol_level_tokens::meta_operations::LockOperation;
use concordium_base::transactions;
use plt_block_state::block_state::ExecutionTimeBlockStateP11;
use plt_block_state::entity::accounts::Accounts;
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::failure::BlockStateFailure;
use plt_block_state::persistent::protocol_level_locks::p11::LockConfiguration;
use plt_scheduler_types::types::events::{self, BlockItemEvent};
use plt_scheduler_types::types::reject_reasons::TransactionRejectReason;

/// Execute [`LockOperation`]
pub fn execute_lock_operation<C: EntityContextTypes>(
    context: &mut EntityContext<C>,
    transaction_execution: &mut TransactionExecution,
    block_state: &mut BlockStateP11,
    lock_operation: LockOperation,
    events: &mut Vec<BlockItemEvent>,
) -> Result<(), TransactionFailure>
where
    EntityContext<C>: Clone,
{
    match lock_operation {
        LockOperation::Fund(_meta_lock_fund_details) => todo!(),
        LockOperation::Send(_meta_lock_send_details) => todo!(),
        LockOperation::Return(_meta_lock_return_details) => todo!(),
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

            // We reconstruct the lock config for the event, rather than using
            // the original one from the transaction. This results in a config
            // that is in a canonical form.
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
                && !lock_configuration.controller().validate_operation(
                    &bsq,
                    transaction_execution.sender_account(),
                    &lock_controller::LockOperation::Cancel(meta_lock_cancel_details),
                )
            {
                // The lock is neither expired, nor is the sender authorized to
                // cancel the lock, so we reject the transaction.
                return Err(TransactionFailure::RejectReason(
                    TransactionRejectReason::LockCancelNotAuthorized(
                        lock.lock_id().clone(),
                        transaction_execution.sender_account_address(),
                    ),
                ));
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
