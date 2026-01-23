//! Types used specifically related to block item execution.

use crate::types::events::BlockItemEvent;
use crate::types::reject_reasons::TransactionRejectReason;
use concordium_base::base::Energy;

/// Summary of execution a transaction.
#[derive(Debug, Clone)]
pub struct TransactionExecutionSummary {
    /// Outcome of executing the transaction.
    /// If transaction was successful, this is a list of events that represents
    /// the changes that were applied to the chain state by the transaction. The same changes
    /// have been applied via the `block_state` argument to [`execute_transaction`]. If the transaction was
    /// rejected, the only change to the chain state is the charge of energy. If the transaction is rejected,
    /// the caller of [`execute_transaction`] must make sure that changes to the given `block_state` are rolled back.
    pub outcome: TransactionOutcome,
    /// Energy used by the execution. This is always less than the `energy_limit` argument given to [`execute_transaction`].
    pub energy_used: Energy,
}

/// Outcome of executing a transaction that was correctly executed (not resulting in [`TransactionExecutionError`]).
#[derive(Debug, Clone)]
pub enum TransactionOutcome {
    /// The transaction was successfully applied.
    Success(Vec<BlockItemEvent>),
    /// The transaction was rejected, but the transaction
    /// is included in the block as a rejected transaction.
    Rejected(TransactionRejectReason),
}
