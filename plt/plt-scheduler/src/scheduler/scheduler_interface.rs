//! Interface to the scheduler seen by lower level module, like the protocol-level tokens scheduler.

use concordium_base::base::Energy;

/// Operations and context related to transaction execution.
pub trait TransactionExecution {
    /// Opaque type that represents an account on chain.
    /// The account is guaranteed to exist on chain, when holding an instance of this type.
    type Account;

    /// The account initiating the transaction.
    fn sender_account(&self) -> Self::Account;

    /// Reduce the available energy for the execution.
    ///
    /// # Arguments
    ///
    /// - `energy` The amount of energy to charge.
    ///
    /// # Errors
    ///
    /// - [`OutOfEnergyError`] If the available energy is smaller than the ticked amount.
    fn tick_energy(&mut self, energy: Energy) -> Result<(), OutOfEnergyError>;
}

/// Transaction execution ran out of energy.
#[derive(Debug)]
pub struct OutOfEnergyError;

// todo ar move to crate
