use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use plt_block_state::entity::accounts::{Account, AccountWithCanonicalAddress};
use plt_block_state::entity::block_state::p9::BlockStateP9;
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::external::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, ExternalBlockStateQuery,
};

// todo ar just use enum instead?

/// Trait that defines block state operations related to accounts.
pub trait AccountsT {
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

impl AccountsT for BlockStateP9 {
    fn account_by_address<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        address: &AccountAddress,
    ) -> Result<Account, AccountNotFoundByAddressError> {
        let account_index = context.external.account_index_by_account_address(address)?;
        Ok(Account::from_existing_account(account_index))
    }

    fn account_by_index<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        account_index: AccountIndex,
    ) -> Result<AccountWithCanonicalAddress, AccountNotFoundByIndexError> {
        let canonical_account_address = context
            .external
            .account_canonical_address_by_account_index(account_index)?;

        let account = Account::from_existing_account(account_index);

        Ok(AccountWithCanonicalAddress {
            account,
            canonical_account_address,
        })
    }
}

impl AccountsT for BlockStateP11 {
    fn account_by_address<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        address: &AccountAddress,
    ) -> Result<Account, AccountNotFoundByAddressError> {
        let account_index = context.external.account_index_by_account_address(address)?;
        Ok(Account::from_existing_account(account_index))
    }

    fn account_by_index<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        account_index: AccountIndex,
    ) -> Result<AccountWithCanonicalAddress, AccountNotFoundByIndexError> {
        let canonical_account_address = context
            .external
            .account_canonical_address_by_account_index(account_index)?;

        let account = Account::from_existing_account(account_index);

        Ok(AccountWithCanonicalAddress {
            account,
            canonical_account_address,
        })
    }
}
