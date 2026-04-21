//! Context for transaction execution.

use concordium_base::base::{AccountIndex, Energy};
use concordium_base::contracts_common::AccountAddress;

/// Transaction execution ran out of energy.
#[derive(Debug, thiserror::Error)]
#[error("Execution out of energy")]
pub struct OutOfEnergyError;

/// Tracks the energy remaining and some context during the execution.
pub struct TransactionExecution {
    /// Limit for how much energy the execution can use. An [`OutOfEnergy`] error is
    /// returned if the limit is reached.
    energy_limit: Energy,
    /// Energy used so far by execution. Energy is always charged in advance for each step executed.
    energy_used: Energy,
    /// The account which signed as the sender of the transaction.
    sender_account: AccountIndex,
    /// The address of the account which signed as the sender of the transaction. This need not be
    /// the canonical address of the account, it can be an account alias.
    sender_account_address: AccountAddress,
}

impl TransactionExecution {
    /// Construct new transaction execution context.
    pub fn new(
        energy_limit: Energy,
        sender_account: AccountIndex,
        sender_account_address: AccountAddress,
    ) -> Self {
        Self {
            energy_used: 0.into(),
            energy_limit,
            sender_account,
            sender_account_address,
        }
    }

    /// The account initiating the transaction.
    pub fn sender_account(&self) -> AccountIndex {
        self.sender_account
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
}
