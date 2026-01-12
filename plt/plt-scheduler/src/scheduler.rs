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
    /// Limit for how much energy the execution can use. An [`OutOfEnergy`] error is
    /// returned if the limit is reached.
    energy_limit: Energy,
    /// Energy used so far by execution. Energy is always charged in advance for each step executed.
    energy_used: Energy,
    /// The account which signed as the sender of the transaction.
    sender_account: Account,
}

impl<Account: Clone> TransactionExecution for TransactionExecutionImpl<Account> {
    type Account = Account;

    fn sender_account(&self) -> Account {
        self.sender_account.clone()
    }

    fn tick_energy(&mut self, energy: Energy) -> Result<(), OutOfEnergyError> {
        // self.energy_limit - self.energy_used should never underflow, but we safeguard with checked_sub
        if self
            .energy_limit
            .checked_sub(self.energy_used)
            .ok_or(OutOfEnergyError)?
            >= energy
        {
            self.energy_used = self.energy_used + energy;
            Ok(())
        } else {
            // Charge all available energy in case of limit is reached
            self.energy_used = self.energy_limit;
            Err(OutOfEnergyError)
        }
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

/// Result of execution a transaction, unless [`TransactionExecutionError`] is returned.
pub struct TransactionExecutionResult {
    /// Result of executing the transaction.
    /// If transaction was successful, this is a list of events that represents
    /// the changes that were applied to the chain state by the transaction. The same changes
    /// have been applied via the `block_state` argument to [`execute_transaction`]. If the transaction was
    /// rejected, the only change to the chain state is the charge of energy. If the transaction is rejected,
    /// the caller of [`execute_transaction`] must make sure that changes to the given `block_state` are rolled back.
    pub result: Result<Vec<TransactionEvent>, TransactionRejectReason>,
    /// Energy used by the execution. This is always less than the `energy_limit` argument given to [`execute_transaction`].
    pub energy_used: Energy,
}

/// Execute a transaction payload modifying `block_state` accordingly.
/// Returns the events produced if successful, otherwise a reject reason. Additionally, the
/// amount of energy used by the execution is returned. The returned values are represented
/// via the type [`TransactionExecutionResult`].
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
pub fn execute_transaction<BSO: BlockStateOperations>(
    sender_account: BSO::Account,
    block_state: &mut BSO,
    payload: Payload,
    energy_limit: Energy,
) -> Result<TransactionExecutionResult, TransactionExecutionError>
where
    BSO::Account: Clone,
{
    let mut execution = TransactionExecutionImpl {
        energy_limit,
        energy_used: Energy::default(),
        sender_account,
    };

    match payload {
        Payload::TokenUpdate { payload } => {
            let result =
                plt_scheduler::execute_plt_transaction(&mut execution, block_state, payload)?;

            Ok(TransactionExecutionResult {
                result,
                energy_used: execution.energy_used,
            })
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

/// Execute an update instruction modifying `block_state` accordingly.
/// Returns the events produced if successful.
///
/// NOTICE: The caller must ensure to rollback state changes in case an error is returned.
///
/// # Arguments
///
/// - `block_state` Block state that can be queried and updated during execution.
/// - `payload` The update instruction payload to execute
///
/// # Errors
///
/// - [`UpdateInstructionExecutionError`] If executing the update instruction failed.
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
