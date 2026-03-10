//! Interface/context for transaction execution.

use crate::token_kernel_interface::AccountWithAddress;
use concordium_base::base::Energy;
use concordium_base::contracts_common::AccountAddress;

/// Transaction execution ran out of energy.
#[derive(Debug, thiserror::Error)]
#[error("Execution out of energy")]
pub struct OutOfEnergyError;

/// Operations and context related to transaction execution. This is the abstraction
/// seen in the transaction execution logic in the scheduler and in the token module.
pub trait TransactionExecution {
    /// Opaque type that represents an account on chain.
    /// The account is guaranteed to exist on chain, when holding an instance of this type.
    type Account;

    /// The account initiating the transaction.
    fn sender_account(&self) -> &Self::Account {
        &self.sender_account_with_address().account
    }

    /// The account address of the account initiating the transaction. This need
    /// not be canonical address of the account, it can be an alias.
    fn sender_account_address(&self) -> AccountAddress {
        self.sender_account_with_address().account_address
    }

    /// The account initiating the transaction together with the account address. The address
    /// need not be canonical address of the account, it can be an alias
    fn sender_account_with_address(&self) -> &AccountWithAddress<Self::Account>;

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
