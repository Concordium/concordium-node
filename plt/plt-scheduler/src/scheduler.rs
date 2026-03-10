//! Entry points to calling the scheduler. The scheduler is responsible for executing
//! transaction and update instruction payloads.

use concordium_base::transactions::Payload;
use concordium_base::updates::UpdatePayload;
use plt_block_state::block_state::{blob_store, external};
use plt_scheduler_interface::transaction_execution_interface::TransactionExecution;
use plt_scheduler_types::types::execution::{ChainUpdateOutcome, TransactionExecutionSummary};

mod p10;
mod p11;
mod plt_scheduler;

pub trait SchedulerOperations {
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
    fn execute_chain_update(
        &mut self,
        external: &mut impl external::ExternalBlockStateOperations,
        payload: UpdatePayload,
    ) -> Result<ChainUpdateOutcome, ChainUpdateExecutionError>;

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
    /// - `block_state` Block state that can be queried and updated during execution.
    /// - `payload` The transaction payload to execute
    /// - `energy_limit` The payload to execute
    ///
    /// # Errors
    ///
    /// - [`TransactionExecutionError`] If executing the transaction fails with an unrecoverable error.
    ///   Returning this error will terminate the scheduler.
    fn execute_transaction(
        &mut self,
        execution: &mut TransactionExecution,
        _loader: &mut impl blob_store::BackingStoreLoad,
        external: &mut impl external::ExternalBlockStateOperations,
        payload: Payload,
    ) -> Result<TransactionExecutionSummary, TransactionExecutionError>;
}

/// Unrecoverable error executing transaction. This represents the
/// return value of [`execute_transaction`] for transactions that cannot
/// be correctly executed.
#[derive(Debug, thiserror::Error)]
pub enum TransactionExecutionError {
    #[error("Unexpected transaction payload that cannot be handled")]
    UnexpectedPayload,
    /// An invariant in the state that should be enforced
    /// is broken. This is generally an error that should never happen and is unrecoverable.
    #[error("State invariant broken: {0}")]
    StateInvariantBroken(String),
}

/// Account with given id does not exist
#[derive(Debug, thiserror::Error)]
pub enum ChainUpdateExecutionError {
    #[error("Unexpected chain update payload that cannot be handled")]
    UnexpectedPayload,
    /// An invariant in the state that should be enforced
    /// is broken. This is generally an error that should never happen and is unrecoverable.
    #[error("State invariant broken: {0}")]
    StateInvariantBroken(String),
}
