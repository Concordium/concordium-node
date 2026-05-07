use crate::block_state::external::{
    ExternalBlockStateOperations, ExternalBlockStateQuery, TokenAccountState,
};
use crate::block_state_interface::{OverflowError, RawTokenAmountDelta};
use crate::entity::protocol_level_tokens::p9::TokenEntityP9;
use crate::entity::{EntityContext, EntityContextTypes};
use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use plt_scheduler_types::types::tokens::RawTokenAmount;

/// Account representing (read-only) account state.
///
/// The account is guaranteed to exist on chain, when holding an instance of this type.
#[derive(Debug)]
pub struct AccountWithCanonicalAddress {
    /// Opaque type that represents an account on chain.
    pub account: Account,
    /// The canonical account address of the account, i.e. the address used as part of the
    /// credential deployment and not an alias.
    pub canonical_account_address: AccountAddress,
}

/// Block state account
#[derive(Debug)]
pub struct Account {
    /// Account index for and account that we know exists in the block state.
    pub(crate) account_index: AccountIndex,
}

impl Account {
    /// Get the account index for the account.
    pub fn account_index(&self) -> AccountIndex {
        self.account_index
    }

    /// Get the token balance of the account.
    pub fn account_token_balance<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        token: &TokenEntityP9<'_>,
    ) -> RawTokenAmount {
        context
            .external
            .read_token_account_balance(self.account_index, token.token_index)
    }

    /// Get token account states. It returns states for all tokens
    /// that the account holds.
    pub fn token_account_states<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
    ) -> impl Iterator<Item = (Self::Token, TokenAccountState)> {
        context.external.token_account_states(self.account_index)
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
        token: &TokenEntityP9<'_>,
        amount_delta: RawTokenAmountDelta,
    ) -> Result<(), OverflowError> {
        context.external.update_token_account_balance(
            self.account_index,
            token.token_index,
            amount_delta,
        )
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
    pub fn touch_token_account<C: EntityContextTypes>(&self, context: &mut EntityContext<C>, token: &TokenEntityP9<'_>) {
        context
            .external
            .touch_token_account(self.account_index, token.token_index)
    }
}
