//! Entry points to calling the scheduler. The scheduler is responsible for executing
//! transaction and update instruction payloads.

use crate::transaction_execution::TransactionExecution;
use concordium_base::base::{Energy, Nonce, ProtocolVersion};
use concordium_base::contracts_common::AccountAddress;
use concordium_base::transactions::Payload;
use concordium_base::updates::UpdatePayload;
use plt_block_state::block_state_interface::BlockStateOperations;
use plt_block_state::failure::BlockStateFailure;
use plt_scheduler_types::types::execution::{ChainUpdateOutcome, TransactionExecutionSummary};

pub mod helpers;
mod p11;
mod p9;
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

/// Account with given id does not exist
#[derive(Debug, thiserror::Error)]
pub enum ChainUpdateExecutionError {
    #[error("Unexpected chain update payload that cannot be handled")]
    UnexpectedPayload,
    /// Error in the block state. This is generally an error that should never happen and is unrecoverable.
    #[error("Block state failure: {0}")]
    BlockStateFailure(#[from] BlockStateFailure),
}
