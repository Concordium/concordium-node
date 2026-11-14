use crate::entity::{EntityContext, EntityContextTypes};
use crate::external::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, ExternalBlockStateOperations,
    ExternalBlockStateQuery, OverflowError, RawTokenAmountDelta, TokenAccountState,
};
use crate::persistent::protocol_level_tokens::p9::TokenIndex;
use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use plt_scheduler_types::types::tokens::RawTokenAmount;

/// Account with its canonical address.
///
/// The account is guaranteed to exist on chain, when holding an instance of this type.
#[derive(Debug)]
pub struct AccountWithCanonicalAddress {
    /// Account on chain.
    pub account: Account,
    /// The canonical account address of the account, i.e. the address used as part of the
    /// credential deployment and not an alias.
    pub canonical_account_address: AccountAddress,
}

/// Representation of block state account.
///
/// The account is guaranteed to exist on chain, when holding an instance of this type.
#[derive(Debug, Clone)]
pub struct Account {
    /// Account index for and account that we know exists in the block state.
    pub(crate) account_index: AccountIndex,
}

impl Account {
    /// Create account from an account index for an account that must exist.
    pub fn from_existing_account(account_index: AccountIndex) -> Self {
        Self { account_index }
    }

    /// Get the account index for the account.
    pub fn account_index(&self) -> AccountIndex {
        self.account_index
    }

    /// Get the token balance of the account.
    pub fn account_token_balance<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        token_index: TokenIndex,
    ) -> RawTokenAmount {
        context
            .external
            .read_token_account_balance(self.account_index, token_index)
    }

    /// Get token account states. It returns states for all tokens
    /// that the account holds.
    pub fn token_account_states<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
    ) -> impl Iterator<Item = (TokenIndex, TokenAccountState)> {
        context
            .external
            .token_account_states(self.account_index)
            .into_iter()
    }

    /// Update the token balance of an account.
    ///
    /// # Arguments
    ///
    /// - `token` The token to update.
    /// - `account` The account to update.
    /// - `amount_delta` The token balance delta.
    ///
    /// # Errors
    ///
    /// - [`OverflowError`] The update would overflow or underflow (result in negative balance)
    ///   the token balance on the account.
    pub fn update_token_account_balance<C: EntityContextTypes>(
        &self,
        context: &mut EntityContext<C>,
        token_index: TokenIndex,
        amount_delta: RawTokenAmountDelta,
    ) -> Result<(), OverflowError> {
        context
            .external
            .update_token_account_balance(self.account_index, token_index, amount_delta)
    }

    /// Initialize the balance of the given account to zero if it didn't have a balance before.
    /// It has the observable effect that the token is then returned when querying the tokens
    /// for an account. Should be called if the token module account state is set,
    /// in order to make sure the token is returned when querying token account info.
    ///
    /// If the account already has a balance for the token in context, the operation has no effect
    ///
    /// # Arguments
    ///
    /// - `token` The token to touch state for in the account.
    /// - `account` The account to touch token state for.
    pub fn touch_token_account<C: EntityContextTypes>(
        &self,
        context: &mut EntityContext<C>,
        token_index: TokenIndex,
    ) {
        context
            .external
            .touch_token_account(self.account_index, token_index)
    }
}

/// Trait that defines block state operations related to accounts.
pub trait Accounts {
    /// Lookup the account using an account address.
    fn account_by_address(
        &self,
        address: &AccountAddress,
    ) -> Result<Account, AccountNotFoundByAddressError>;

    /// Lookup the account using an account index. Returns both the opaque account
    /// representation and the account canonical address.
    fn account_by_index(
        &self,
        account_index: AccountIndex,
    ) -> Result<AccountWithCanonicalAddress, AccountNotFoundByIndexError>;
}

impl<C: EntityContextTypes> Accounts for EntityContext<C> {
    fn account_by_address(
        &self,
        address: &AccountAddress,
    ) -> Result<Account, AccountNotFoundByAddressError> {
        let account_index = self.external.account_index_by_account_address(address)?;
        Ok(Account::from_existing_account(account_index))
    }

    fn account_by_index(
        &self,
        account_index: AccountIndex,
    ) -> Result<AccountWithCanonicalAddress, AccountNotFoundByIndexError> {
        let canonical_account_address = self
            .external
            .account_canonical_address_by_account_index(account_index)?;

        let account = Account::from_existing_account(account_index);

        Ok(AccountWithCanonicalAddress {
            account,
            canonical_account_address,
        })
    }
}
