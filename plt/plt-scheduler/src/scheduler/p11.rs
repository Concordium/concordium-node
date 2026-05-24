use crate::protocol_level_tokens;
use crate::scheduler::{ChainUpdateExecutionError, TransactionExecutionError};
use crate::transaction_execution::TransactionExecution;
use concordium_base::base::{Energy, Nonce};
use concordium_base::contracts_common::AccountAddress;
use concordium_base::transactions::Payload;
use concordium_base::updates::UpdatePayload;
use plt_block_state::entity::accounts::Account;
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_scheduler_types::types::execution::{ChainUpdateOutcome, TransactionExecutionSummary};

/// Execute a transaction payload modifying `block_state` accordingly.
/// Returns the events produced if successful, otherwise a reject reason. Additionally, the
/// amount of energy used by the execution is returned. The returned values are represented
/// via the type [`TransactionExecutionSummary`].
///
/// NOTICE: The caller must ensure to rollback state changes in case of the transaction being rejected.
///
/// # Arguments
///
/// - `sender_account` The account initiating the transaction (signer of the transaction)
/// - `sender_account_address` The address of the account initiating the transaction (from the transaction header)
/// - `transaction_sequence_number` The sequence number of the transaction
/// - `block_state` Block state that can be queried and updated during execution.
/// - `payload` The transaction payload to execute
/// - `energy_limit` The payload to execute
///
/// # Errors
///
/// - [`TransactionExecutionError`] If executing the transaction fails with an unrecoverable error.
///   Returning this error will terminate the scheduler.
pub fn execute_transaction<C: EntityContextTypes>(
    context: &mut EntityContext<C>,
    block_state: &mut BlockStateP11,
    sender_account: Account,
    sender_account_address: AccountAddress,
    transaction_sequence_number: Nonce,
    payload: Payload,
    energy_limit: Energy,
) -> Result<TransactionExecutionSummary, TransactionExecutionError> {
    let mut execution = TransactionExecution::new(
        energy_limit,
        sender_account,
        sender_account_address,
        transaction_sequence_number,
    );

    let outcome = match payload {
        Payload::TokenUpdate { payload } => {
            protocol_level_tokens::p11::execute_token_update_transaction(
                context,
                &mut execution,
                block_state,
                payload,
            )?
        }
        Payload::MetaUpdate { payload } => {
            // todo ar
            todo!()
            // plt_scheduler::execute_meta_update_transaction(&mut execution, block_state, payload)?
        }
        _ => return Err(TransactionExecutionError::UnexpectedPayload),
    };

    Ok(TransactionExecutionSummary {
        outcome,
        energy_used: execution.energy_used(),
    })
}

/// Execute a chain update modifying `block_state` accordingly.
/// Returns the events produced if successful, otherwise a failure kind.
///
/// NOTICE: The caller must ensure to rollback state changes in case a failure kind is returned.
///
/// # Arguments
///
/// - `block_state` Block state that can be queried and updated during execution.
/// - `payload` The chain update payload to execute
///
/// # Errors
///
/// - [`ChainUpdateExecutionError`] If executing the chain update failed in an unrecoverable way.
///   Returning this error will terminate the scheduler.
pub fn execute_chain_update<C: EntityContextTypes>(
    context: &mut EntityContext<C>,
    block_state: &mut BlockStateP11,
    payload: UpdatePayload,
) -> Result<ChainUpdateOutcome, ChainUpdateExecutionError> {
    match payload {
        UpdatePayload::CreatePlt(create_plt) => {
            protocol_level_tokens::p11::execute_create_plt_chain_update(
                context,
                block_state,
                create_plt,
            )
        }
        _ => Err(ChainUpdateExecutionError::UnexpectedPayload),
    }
}
