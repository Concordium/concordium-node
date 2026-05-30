//! Entry points to calling the scheduler. The scheduler is responsible for executing
//! transaction and update instruction payloads.

use plt_block_state::failure::BlockStateFailure;
use plt_scheduler_types::types::reject_reasons::TransactionRejectReason;

pub mod p11;
pub mod p9;
mod plt_scheduler;

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

/// [`BlockStateFailure`] and [`TransactionRejectReason`] flattened into one error
/// for convenience.
#[derive(Debug, thiserror::Error)]
pub enum TransactionFailure {
    /// The transaction was rejected, but can be included in a block.
    #[error("Transaction rejected")]
    RejectReason(TransactionRejectReason),
    /// An unrecoverable error occurred in block state when executing the transaction.
    #[error("Block state failure: {0}")]
    BlockStateFailure(#[from] BlockStateFailure),
}

impl From<TransactionRejectReason> for TransactionFailure {
    fn from(reject_reason: TransactionRejectReason) -> Self {
        Self::RejectReason(reject_reason)
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
