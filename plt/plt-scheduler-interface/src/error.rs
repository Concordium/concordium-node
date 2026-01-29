use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;

/// Account with given address does not exist
#[derive(Debug, thiserror::Error)]
#[error("Account with address {0} does not exist")]
pub struct AccountNotFoundByAddressError(pub AccountAddress);

/// Account with given index does not exist
#[derive(Debug, thiserror::Error)]
#[error("Account with index {0} does not exist")]
pub struct AccountNotFoundByIndexError(pub AccountIndex);

/// Transaction execution ran out of energy.
#[derive(Debug, thiserror::Error)]
#[error("Execution out of energy")]
pub struct OutOfEnergyError;
