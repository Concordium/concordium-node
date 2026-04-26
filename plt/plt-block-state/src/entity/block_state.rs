mod p10;
mod p11;
mod p9;

pub trait BlockState {
    /// Lookup the account using an account address.
    fn account_by_address(
        &self,
        address: &AccountAddress,
    ) -> Result<Self::Account, AccountNotFoundByAddressError>;

    /// Lookup the account using an account index. Returns both the opaque account
    /// representation and the account canonical address.
    fn account_by_index(
        &self,
        index: AccountIndex,
    ) -> Result<AccountWithCanonicalAddress<Self::Account>, AccountNotFoundByIndexError>;

    /// Get the account index for the account.
    fn account_index(&self, account: &Self::Account) -> AccountIndex;
}