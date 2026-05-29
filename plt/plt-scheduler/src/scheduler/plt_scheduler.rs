//! Scheduler implementation for protocol-level token updates. This module implements execution
//! of transactions related to protocol-level tokens.

use crate::locks::lock_controller::LockController;
use crate::locks::{get_lock_config, lock_controller};

use crate::scheduler::TransactionExecutionError;
use crate::token_context;
use crate::transaction_execution::TransactionExecution;
use concordium_base::common::cbor::{self};
use concordium_base::protocol_level_locks::LockId;
use concordium_base::protocol_level_tokens::RawCbor;
use concordium_base::protocol_level_tokens::meta_operations::LockOperation;
use concordium_base::transactions;
use plt_block_state::block_state_interface::{BlockStateOperations, BlockStateQuery};
use plt_block_state::entity::accounts::Accounts;
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::failure::{BlockStateFailure, BlockStateResult};
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
) -> BlockStateResult<Result<(), TransactionRejectReason>> {
    match lock_operation {
        LockOperation::Fund(_meta_lock_fund_details) => todo!(),
        LockOperation::Send(_meta_lock_send_details) => todo!(),
        LockOperation::Return(_meta_lock_return_details) => todo!(),
        LockOperation::Create(meta_lock_create_details) => {
            let config = meta_lock_create_details.config;
            let account_index = block_state.account_index(transaction_execution.sender_account());
            let sequence_number = transaction_execution.transaction_sequence_number();
            let creation_order = transaction_execution.next_lock_creation_order();
            let lock_id = LockId::new(account_index, sequence_number, creation_order);
            let controller = LockController::new(block_state, config.controller)?;

            let recipients = config
                .recipients
                .iter()
                .map(
                    |recipient| match block_state.account_by_address(&recipient.address) {
                        Ok(account) => Ok(block_state.account_index(&account)),
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
            let config = get_lock_config(block_state, &configuration).map_err(|err| {
                TransactionExecutionError::BlockStateFailure(BlockStateFailure::Invariant(format!(
                    "Failed to get lock config for created lock: {err}"
                )))
            })?;
            let event = events::LockCreateEvent {
                lock_id: lock_id.clone(),
                lock_config: RawCbor::from(cbor::cbor_encode(&config)),
            };
            events.push(BlockItemEvent::LockCreated(event));

            block_state.create_lock(lock_id.clone(), configuration);

            Ok(())
        }
        LockOperation::Cancel(meta_lock_cancel_details) => {
            // TODO: (COR-2306) charge.
            let lock = block_state
                .lock_by_id(&meta_lock_cancel_details.lock)
                .map_err(|err| TransactionRejectReason::NonExistentLockId(err.0))?;

            let lock_configuration = block_state.lock_configuration(&lock);
            let memo: Option<transactions::Memo> = meta_lock_cancel_details
                .memo
                .clone()
                .map(transactions::Memo::from);

            if !lock_configuration
                .expiry()
                .is_expired(transaction_execution.timestamp())
                && !lock_configuration.controller().validate_operation(
                    block_state,
                    transaction_execution.sender_account(),
                    &lock_controller::LockOperation::Cancel(meta_lock_cancel_details),
                )
            {
                // The lock is neither expired, nor is the sender authorized to
                // cancel the lock, so we reject the transaction.
                return Err(TransactionRejectReason::LockCancelNotAuthorized(
                    lock.lock_id().clone(),
                    transaction_execution.sender_account_address(),
                )
                .into());
            }
            for (account_index, token) in block_state.lock_balances(&lock).collect::<Vec<_>>() {
                let mut token_p11 = block_state.token_p11(&token);
                token_context::unlock_balance(
                    block_state.context(),
                    events,
                    &mut token_p11,
                    account_index,
                    lock.lock_id(),
                    &memo,
                )?;
                block_state.update_token_p11(token_p11);
            }
            block_state.delete_lock(lock.lock_id());
            let event = events::LockDestroyEvent {
                lock_id: lock.lock_id().clone(),
            };
            events.push(BlockItemEvent::LockDestroyed(event));

            Ok(())
        }
    }
}
