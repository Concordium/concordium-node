//! Entry points to calling the scheduler. The scheduler is responsible for executing
//! transaction and update instruction payloads.

use crate::token_module::errors::{TokenStateInvariantError, TokenTransferError};
use crate::token_module::util;
use crate::transaction_execution::{TransactionContext, TransactionExecution};
use concordium_base::base::ProtocolVersion;
use concordium_base::protocol_level_tokens::{
    TokenBalanceInsufficientRejectReason, TokenModuleRejectReason,
};
use concordium_base::transactions::Payload;
use concordium_base::updates::UpdatePayload;
use plt_block_state::block_state_interface::BlockStateOperations;
use plt_block_state::persistent::protocol_level_tokens::p9::TokenConfiguration;
use plt_scheduler_types::types::execution::{ChainUpdateOutcome, TransactionExecutionSummary};
use plt_scheduler_types::types::reject_reasons::{
    EncodedTokenModuleRejectReason, TransactionRejectReason,
};

pub mod helpers;
mod plt_scheduler;

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

impl From<TokenStateInvariantError> for TransactionFailure {
    fn from(value: TokenStateInvariantError) -> Self {
        Self::Error(TransactionExecutionError::StateInvariantBroken(
            value.to_string(),
        ))
    }
}

impl TransactionFailure {
    fn from_token_transfer_error(
        index: u64,
        token_configuration: &TokenConfiguration,
        error: TokenTransferError,
    ) -> Self {
        match error {
            TokenTransferError::StateInvariantViolation(token_state_invariant_error) => {
                token_state_invariant_error.into()
            }
            TokenTransferError::InsufficientBalance(insufficient_balance_error) => {
                let (reason, details) = TokenModuleRejectReason::TokenBalanceInsufficient(
                    TokenBalanceInsufficientRejectReason {
                        index,
                        available_balance: util::to_token_amount(
                            token_configuration,
                            insufficient_balance_error.available,
                        ),
                        required_balance: util::to_token_amount(
                            token_configuration,
                            insufficient_balance_error.required,
                        ),
                    },
                )
                .encode_reject_reason();
                Self::Reject(TransactionRejectReason::TokenUpdateTransactionFailed(
                    EncodedTokenModuleRejectReason {
                        token_id: token_configuration.token_id.clone(),
                        reason_type: reason.to_type_discriminator(),
                        details: Some(details),
                    },
                ))
            }
        }
    }
}

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
pub fn execute_transaction<BSO: BlockStateOperations>(
    transaction_context: TransactionContext,
    sender_account: BSO::Account,
    block_state: &mut BSO,
    payload: Payload,
) -> Result<TransactionExecutionSummary, TransactionExecutionError> {
    let mut execution = TransactionExecution::new(transaction_context, sender_account);

    match payload {
        Payload::TokenUpdate { payload } => {
            let outcome = plt_scheduler::execute_token_update_transaction(
                &mut execution,
                block_state,
                payload,
            )?;

            Ok(TransactionExecutionSummary {
                outcome,
                energy_used: execution.energy_used(),
            })
        }
        Payload::MetaUpdate { payload }
            if block_state.protocol_version() >= ProtocolVersion::P11 =>
        {
            let outcome = plt_scheduler::execute_meta_update_transaction(
                &mut execution,
                block_state,
                payload,
            )?;

            Ok(TransactionExecutionSummary {
                outcome,
                energy_used: execution.energy_used(),
            })
        }
        _ => Err(TransactionExecutionError::UnexpectedPayload),
    }
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
pub fn execute_chain_update<BSO: BlockStateOperations>(
    block_state: &mut BSO,
    payload: UpdatePayload,
) -> Result<ChainUpdateOutcome, ChainUpdateExecutionError> {
    match payload {
        UpdatePayload::CreatePlt(create_plt) => {
            plt_scheduler::execute_create_plt_chain_update(block_state, create_plt)
        }
        _ => Err(ChainUpdateExecutionError::UnexpectedPayload),
    }
}
