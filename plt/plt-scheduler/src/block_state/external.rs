//! Interactions with block state managed externally in Haskell.

use crate::block_state::types::{TokenAccountState, TokenIndex};
use crate::block_state_interface::{OverflowError, RawTokenAmountDelta};
use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use plt_scheduler_interface::error::{AccountNotFoundByAddressError, AccountNotFoundByIndexError};
use plt_types::types::tokens::RawTokenAmount;

/// Type definition for queries to externally managed parts of the block state.
/// This state is managed in Haskell.
pub trait ExternalBlockStateQuery {
    /// Read the account token balance from the block state.
    ///
    /// # Arguments
    ///
    /// - `account_index` The index of the account to update a token balance for.
    ///   Must be a valid account index of an existing account.
    /// - `token_index` The index of the token. Must be a valid token index of an existing token.
    fn read_token_account_balance(
        &self,
        account: AccountIndex,
        token: TokenIndex,
    ) -> RawTokenAmount;

    /// Get account canonical address by account index. Returns an error
    /// if the account does not exist.
    ///
    /// # Arguments
    ///
    /// - `account_index` Index of the (possibly existing) account to get.
    fn account_canonical_address_by_account_index(
        &self,
        account_index: AccountIndex,
    ) -> Result<AccountAddress, AccountNotFoundByIndexError>;

    /// Get account index by account address (canonical address or alias address).
    /// Returns an error if the account does not exist.
    ///
    /// # Arguments
    ///
    /// - `account_address` Address of the (possibly existing) account to get.
    fn account_index_by_account_address(
        &self,
        account_address: &AccountAddress,
    ) -> Result<AccountIndex, AccountNotFoundByAddressError>;

    /// Get token account states for an account. Returns pairs of the token index and the
    /// token account state for the token.
    ///
    /// # Arguments
    ///
    /// - `account_index` The index of the account to get token account states for. Must be a valid account index of an existing account.
    fn token_account_states(
        &self,
        account_index: AccountIndex,
    ) -> Vec<(TokenIndex, TokenAccountState)>;
}

/// Type definition for operations to externally managed parts of the block state.
/// This state is managed in Haskell.
pub trait ExternalBlockStateOperations: ExternalBlockStateQuery {
    /// Update the account token balance in the block state.
    /// Returns an error if the balance change would result in a negative balance
    /// or a balance above the representable amount.
    ///
    /// # Arguments
    ///
    /// - `account_index` The index of the account to update a token balance for. Must be a valid account index of an existing account.
    /// - `token_index` The index of the token. Must be a valid token index of an existing token.
    /// - `amount_delta` The amount to add to or subtract from the balance.
    fn update_token_account_balance(
        &mut self,
        account: AccountIndex,
        token: TokenIndex,
        amount_delta: RawTokenAmountDelta,
    ) -> Result<(), OverflowError>;

    /// Increment the PLT chain update sequence number.
    fn increment_plt_update_sequence_number(&mut self);
}
