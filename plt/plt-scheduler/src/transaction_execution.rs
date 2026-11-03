//! Context for transaction execution.

use concordium_base::base::{Energy, Nonce};
use concordium_base::contracts_common::{AccountAddress, Timestamp};
use plt_block_state::entity::accounts::Account;

/// Transaction execution ran out of energy.
#[derive(Debug, thiserror::Error)]
#[error("Execution out of energy")]
pub struct OutOfEnergyError;

#[derive(Debug, Clone)]
pub struct TransactionContext {
    /// Limit for how much energy the execution can use. An [`OutOfEnergyError`] error is
    /// returned if the limit is reached.
    pub energy_limit: Energy,
    /// The address of the account which signed as the sender of the transaction. This need not be
    /// the canonical address of the account, it can be an account alias.
    pub sender_account_address: AccountAddress,
    /// The sequence number of the transaction as specified in the transaction header.
    pub transaction_sequence_number: Nonce,
    /// Timestamp of the block in which the transaction is executed.
    pub block_timestamp: Timestamp,
}

/// Tracks the energy remaining and some context during the execution.
pub struct TransactionExecution {
    /// Limit for how much energy the execution can use. An [`OutOfEnergyError`] error is
    /// returned if the limit is reached.
    energy_limit: Energy,
    /// Energy used so far by execution. Energy is always charged in advance for each step executed.
    energy_used: Energy,
    /// The account which signed as the sender of the transaction.
    sender_account: Account,
    /// The address of the account which signed as the sender of the transaction. This need not be
    /// the canonical address of the account, it can be an account alias.
    sender_account_address: AccountAddress,
    /// The sequence number of the transaction as specified in the transaction header.
    transaction_sequence_number: Nonce,
    /// Timestamp of the block in which the transaction is executed.
    block_timestamp: Timestamp,
    /// The number of locks that have been created during the execution of the transaction so far.
    /// This is used to generate unique lock IDs for locks created during execution.
    locks_created: u64,
}

impl TransactionExecution {
    /// Construct new transaction execution context.
    pub fn new(transaction_context: TransactionContext, sender_account: Account) -> Self {
        Self {
            energy_used: 0.into(),
            energy_limit: transaction_context.energy_limit,
            sender_account,
            sender_account_address: transaction_context.sender_account_address,
            transaction_sequence_number: transaction_context.transaction_sequence_number,
            block_timestamp: transaction_context.block_timestamp,
            locks_created: 0,
        }
    }

    /// The account initiating the transaction.
    pub fn sender_account(&self) -> &Account {
        &self.sender_account
    }

    /// The account address of the account initiating the transaction. This need
    /// not be canonical address of the account, it can be an alias.
    pub fn sender_account_address(&self) -> AccountAddress {
        self.sender_account_address
    }

    /// Energy used so far by execution.
    pub fn energy_used(&self) -> Energy {
        self.energy_used
    }

    /// Reduce the available energy for the execution.
    ///
    /// # Arguments
    ///
    /// - `energy` The amount of energy to charge.
    ///
    /// # Errors
    ///
    /// - [`OutOfEnergyError`] If the available energy is smaller than the ticked amount.
    pub fn tick_energy(&mut self, energy: Energy) -> Result<(), OutOfEnergyError> {
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

    /// The sequence number of the transaction as specified in the transaction header.
    pub fn transaction_sequence_number(&self) -> Nonce {
        self.transaction_sequence_number
    }

    /// The timestamp of the block in which the transaction is executed.
    pub fn timestamp(&self) -> Timestamp {
        self.block_timestamp
    }

    /// Get the next lock creation order number and increment the counter.
    pub fn next_lock_creation_order(&mut self) -> u64 {
        let creation_order = self.locks_created;
        self.locks_created += 1;
        creation_order
    }
}
