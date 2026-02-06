//! Interface/context for transaction execution.

use crate::error::OutOfEnergyError;
use concordium_base::base::Energy;
use concordium_base::contracts_common::AccountAddress;

/// Operations and context related to transaction execution. This is the abstraction
/// seen in the transaction execution logic in the scheduler and in the token module.
pub trait TransactionExecution {
    /// Opaque type that represents an account on chain.
    /// The account is guaranteed to exist on chain, when holding an instance of this type.
    type Account;

    /// The account initiating the transaction.
    fn sender_account(&self) -> Self::Account;

    /// The account address of the account initiating the transaction.
    fn sender_account_address(&self) -> AccountAddress;

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
