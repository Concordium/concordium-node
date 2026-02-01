//! Entry points to calling the scheduler. The scheduler is responsible for executing
//! transaction and update instruction payloads.

use crate::block_state_interface::BlockStateOperations;
use concordium_base::base::Energy;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::transactions::Payload;
use concordium_base::updates::UpdatePayload;
use plt_scheduler_interface::error::OutOfEnergyError;
use plt_scheduler_interface::transaction_execution_interface::TransactionExecution;
use plt_types::types::execution::{ChainUpdateOutcome, TransactionExecutionSummary};

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
    /// The address of the account which signed as the sender of the transaction.
    sender_account_address: AccountAddress,
}

impl<Account: Clone> TransactionExecution for TransactionExecutionImpl<Account> {
    type Account = Account;

    fn sender_account(&self) -> Account {
        self.sender_account.clone()
    }

    fn sender_account_address(&self) -> AccountAddress {
        self.sender_account_address
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
pub fn execute_transaction<BSO: BlockStateOperations>(
    sender_account: BSO::Account,
    sender_account_address: AccountAddress,
    block_state: &mut BSO,
    payload: Payload,
    energy_limit: Energy,
) -> Result<TransactionExecutionSummary, TransactionExecutionError>
where
    BSO::Account: Clone,
{
    let mut execution = TransactionExecutionImpl {
        energy_limit,
        energy_used: Energy::default(),
        sender_account,
        sender_account_address,
    };

    match payload {
        Payload::TokenUpdate { payload } => {
            let outcome = plt_scheduler::execute_token_update_transaction(
                &mut execution,
                block_state,
                payload,
            )?;

            Ok(TransactionExecutionSummary {
                outcome,
                energy_used: execution.energy_used,
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
) -> Result<ChainUpdateOutcome, ChainUpdateExecutionError>
where
    BSO::Account: Clone,
{
    match payload {
        UpdatePayload::CreatePlt(create_plt) => {
            plt_scheduler::execute_create_plt_chain_update(block_state, create_plt)
        }
        _ => Err(ChainUpdateExecutionError::UnexpectedPayload),
    }
}
