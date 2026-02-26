//! Types used specifically related to block item execution.

use crate::types::events::BlockItemEvent;
use crate::types::reject_reasons::TransactionRejectReason;
use concordium_base::base::Energy;
use concordium_base::common::{Buffer, Put, Serial};
use concordium_base::protocol_level_tokens::{TokenId, TokenModuleRef};

/// Summary of execution a transaction.
#[derive(Debug, Clone)]
pub struct TransactionExecutionSummary {
    /// Outcome of executing the transaction. The transaction can either be successful or rejected.
    /// If the transaction is rejected, the changes to the block state must be rolled back.
    pub outcome: TransactionOutcome,
    /// Energy used by the execution. This is always less than the `energy_limit` argument given to `execute_transaction`.
    pub energy_used: Energy,
}

/// Outcome of executing a transaction that was correctly executed (not resulting in the unrecoverable error `TransactionExecutionError`).
///
/// If the transaction was successful, this is a list of events that represents
/// the changes that were applied to the block state by the transaction. If the transaction was
/// rejected, it is a reject reason, and the changes to the block state must be rolled back.
#[derive(Debug, Clone)]
pub enum TransactionOutcome {
    /// The transaction was successfully applied.
    Success(Vec<BlockItemEvent>),
    /// The transaction was rejected. The transaction
    /// is included in the block as a rejected transaction.
    /// The changes to the block state must be rolled back.
    Rejected(TransactionRejectReason),
}

/// Outcome of executing a chain update that was correctly executed (not resulting in the unrecoverable error `ChainUpdateExecutionError`).
///
/// If the chain update was successful, this is a list of events that represents
/// the changes that were applied to the block state by the chain update. If the chain update
/// failed, it is a failure kind, and the changes to the block state must be rolled back.
#[derive(Debug, Clone)]
pub enum ChainUpdateOutcome {
    /// The chain update was successfully applied.
    Success(Vec<BlockItemEvent>),
    /// The chain update failed and is not included in the block.
    /// The changes to the block state must be rolled back.
    Failed(FailureKind),
}

/// Reasons for the execution of a block item to fail.
///
/// Corresponding Haskell type: `Concordium.Types.Execution.FailureKind`
#[derive(Debug, Clone)]
pub enum FailureKind {
    /// A protocol-level token with the given token ID already exists.
    DuplicateTokenId(TokenId),
    /// The token module encountered an error when initializing the protocol-level token.
    TokenInitializeFailure(String),
    /// The token module reference is unknown or invalid.
    InvalidTokenModuleRef(TokenModuleRef),
}

impl Serial for FailureKind {
    fn serial<B: Buffer>(&self, out: &mut B) {
        match self {
            FailureKind::DuplicateTokenId(token_id) => {
                out.put(&17u8);
                out.put(token_id);
            }
            FailureKind::TokenInitializeFailure(error) => {
                out.put(&18u8);
                out.put(error);
            }
            FailureKind::InvalidTokenModuleRef(module_ref) => {
                out.put(&19u8);
                out.put(module_ref);
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::types::execution::FailureKind;
    use concordium_base::common;
    use concordium_base::protocol_level_tokens::TokenModuleRef;

    #[test]
    fn test_duplicate_token_id_serial() {
        let failure_kind = FailureKind::DuplicateTokenId("tokenid1".parse().unwrap());

        let bytes = common::to_bytes(&failure_kind);
        assert_eq!(hex::encode(&bytes), "1108746f6b656e696431");
    }

    #[test]
    fn test_token_initialize_failure_serial() {
        let failure_kind = FailureKind::TokenInitializeFailure("error1".parse().unwrap());

        let bytes = common::to_bytes(&failure_kind);
        assert_eq!(hex::encode(&bytes), "1200000000000000066572726f7231");
    }

    #[test]
    fn test_invalid_token_module_ref_serial() {
        let failure_kind = FailureKind::InvalidTokenModuleRef(TokenModuleRef::from([5; 32]));

        let bytes = common::to_bytes(&failure_kind);
        assert_eq!(
            hex::encode(&bytes),
            "130505050505050505050505050505050505050505050505050505050505050505"
        );
    }
}
