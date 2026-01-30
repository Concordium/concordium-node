//! Types used specifically related to block item execution.

use crate::types::events::BlockItemEvent;
use crate::types::reject_reasons::TransactionRejectReason;
use concordium_base::base::Energy;
use concordium_base::common::{Buffer, Put, Serial};
use concordium_base::protocol_level_tokens::{TokenId, TokenModuleRef};

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

/// Reasons for the execution of a transaction to fail on the current block state.
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

/// Serial implementation using "discriminators" mathing the variant position in the
/// Haskell type: `Concordium.Types.Execution.FailureKind`
/// (notice this type has no binary serialization defined currently)
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
