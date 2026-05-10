use crate::block_state_interface::{AccountNotFoundByAddressError, AccountNotFoundByIndexError};
use crate::entity::accounts::{Account, AccountWithCanonicalAddress};
use crate::entity::{EntityContext, EntityContextTypes};
use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;

pub mod p10;
pub mod p11;
pub mod p9;

/// Trait that defines block state operations related to accounts.
pub trait Accounts {
    /// Lookup the account using an account address.
    fn account_by_address<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        address: &AccountAddress,
    ) -> Result<Account, AccountNotFoundByAddressError>;

    /// Lookup the account using an account index. Returns both the opaque account
    /// representation and the account canonical address.
    fn account_by_index<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        account_index: AccountIndex,
    ) -> Result<AccountWithCanonicalAddress, AccountNotFoundByIndexError>;
}
