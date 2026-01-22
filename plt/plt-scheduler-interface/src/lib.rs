//! Interface to the scheduler seen by lower level module, like the protocol-level tokens scheduler.

use concordium_base::base::{AccountIndex, Energy};
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

/// Transaction execution ran out of energy.
#[derive(Debug, thiserror::Error)]
#[error("Execution out of energy")]
pub struct OutOfEnergyError;

/// Account with given address does not exist
#[derive(Debug, thiserror::Error)]
#[error("Account with address {0} does not exist")]
pub struct AccountNotFoundByAddressError(pub AccountAddress);

/// Account with given index does not exist
#[derive(Debug, thiserror::Error)]
#[error("Account with index {0} does not exist")]
pub struct AccountNotFoundByIndexError(pub AccountIndex);

/// Account representing (read-only) account state.
///
/// The account is guaranteed to exist on chain, when holding an instance of this type.
#[derive(Debug)]
pub struct AccountWithCanonicalAddress<Account> {
    /// Opaque type that represents an account on chain.
    pub account: Account,
    /// The canonical account address of the account, i.e. the address used as part of the
    /// credential deployment and not an alias.
    pub canonical_account_address: AccountAddress,
}
