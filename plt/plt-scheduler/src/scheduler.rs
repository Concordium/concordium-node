//! Entry points to calling the scheduler. The scheduler is responsible for executing
//! transaction and update instruction payloads.

use crate::block_state_interface::BlockStateOperations;
use crate::types::events::TransactionEvent;
use crate::types::reject_reasons::TransactionRejectReason;
use concordium_base::base::Energy;
use concordium_base::protocol_level_tokens::{TokenId, TokenModuleRef};
use concordium_base::transactions::Payload;
use concordium_base::updates::UpdatePayload;
use plt_scheduler_interface::{OutOfEnergyError, TransactionExecution};

mod plt_scheduler;

/// Tracks the energy remaining and some context during the execution.
struct TransactionExecutionImpl<Account> {
    // /// The remaining energy tracked spent during the execution.
    // remaining_energy: Energy,
    /// The account which signed as the sender of the transaction.
    sender_account: Account,
}

impl<Account: Clone> TransactionExecution for TransactionExecutionImpl<Account> {
    type Account = Account;

    fn sender_account(&self) -> Account {
        self.sender_account.clone()
    }

    fn tick_energy(&mut self, _energy: Energy) -> Result<(), OutOfEnergyError> {
        // implement as part of https://linear.app/concordium/issue/PSR-37/energy-charge
        Ok(())
    }
}

/// Error executing transaction. This does not include reject reasons,
/// which are handled at a higher level.
#[derive(Debug, thiserror::Error)]
pub enum TransactionExecutionError {
    #[error("Unexpected transaction payload that cannot be handled")]
    UnexpectedPayload,
    /// An invariant in the state that should be enforced
    /// is broken. This is generally an error that should never happen and is unrecoverable.
    #[error("State invariant broken: {0}")]
    TokenStateInvariantBroken(String),
}

/// Execute a transaction payload modifying `transaction_execution` and `block_state` accordingly.
/// Returns the events produced if successful otherwise a reject reason.
///
/// The caller must ensure to rollback state changes in case of the transaction being rejected.
pub fn execute_transaction<BSO: BlockStateOperations>(
    sender_account: BSO::Account,
    block_state: &mut BSO,
    payload: Payload,
) -> Result<Result<Vec<TransactionEvent>, TransactionRejectReason>, TransactionExecutionError>
where
    BSO::Account: Clone,
{
    let mut execution = TransactionExecutionImpl { sender_account };

    // handle energy as part of https://linear.app/concordium/issue/PSR-37/energy-charge

    match payload {
        Payload::TokenUpdate { payload } => {
            plt_scheduler::execute_plt_transaction(&mut execution, block_state, payload)
        }
        _ => Err(TransactionExecutionError::UnexpectedPayload),
    }
}

/// Account with given id does not exist
#[derive(Debug, thiserror::Error)]
pub enum UpdateInstructionExecutionError {
    #[error("Unexpected update instruction payload that cannot be handled")]
    UnexpectedPayload,
    #[error("Initialization of token in token module failed: {0}")]
    ModuleTokenInitializationFailed(String),
    #[error("Token with specified id already exists: {0}")]
    TokenIdAlreadyUsed(TokenId),
    #[error("Unknown token module: {0:?}")]
    UnknownTokenModuleRef(TokenModuleRef),
}

/// Execute an update instruction payload modifying `block_state` accordingly.
/// Returns the events produced if successful.
pub fn execute_update_instruction<BSO: BlockStateOperations>(
    block_state: &mut BSO,
    payload: UpdatePayload,
) -> Result<Vec<TransactionEvent>, UpdateInstructionExecutionError>
where
    BSO::Account: Clone,
{
    match payload {
        UpdatePayload::CreatePlt(create_plt) => {
            plt_scheduler::execute_plt_create_instruction(block_state, create_plt)
        }
        _ => Err(UpdateInstructionExecutionError::UnexpectedPayload),
    }
}
