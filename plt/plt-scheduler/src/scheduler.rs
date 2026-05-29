//! Entry points to calling the scheduler. The scheduler is responsible for executing
//! transaction and update instruction payloads.

use plt_block_state::failure::BlockStateFailure;
use plt_scheduler_types::types::reject_reasons::TransactionRejectReason;

pub mod p11;
pub mod p9;
// todo ar
// mod plt_scheduler;

/// Unrecoverable error executing transaction. This represents the
/// return value of [`execute_transaction`] for transactions that cannot
/// be correctly executed.
#[derive(Debug, thiserror::Error)]
pub enum TransactionExecutionError {
    #[error("Unexpected transaction payload that cannot be handled")]
    UnexpectedPayload,
    /// Error in the block state. This is generally an error that should never happen and is unrecoverable.
    #[error("Block state failure: {0}")]
    BlockStateFailure(#[from] BlockStateFailure),
}

// todo ar what to do with TransactionFailure
/// Failure mode for executing a transaction. A transaction may either be
/// reject, which means it is included in the block but does not have any effect,
/// or it may fail with an unrecoverable error, leading to a panic.
#[derive(Debug, thiserror::Error)]
pub enum TransactionFailure {
    /// The transaction was rejected, but can be included in a block.
    #[error("transaction rejected")]
    Reject(TransactionRejectReason),
    /// An unrecoverable error occurred when executing the transaction.
    #[error("unrecoverable: {0}")]
    Error(#[from] TransactionExecutionError),
}

impl From<TransactionRejectReason> for TransactionFailure {
    fn from(value: TransactionRejectReason) -> Self {
        Self::Reject(value)
    }
}


/// Account with given id does not exist
#[derive(Debug, thiserror::Error)]
pub enum ChainUpdateExecutionError {
    #[error("Unexpected chain update payload that cannot be handled")]
    UnexpectedPayload,
    /// Error in the block state. This is generally an error that should never happen and is unrecoverable.
    #[error("Block state failure: {0}")]
    BlockStateFailure(#[from] BlockStateFailure),
}
