//! Interactions with block state managed externally in Haskell.

use crate::block_state::types::{TokenAccountState, TokenIndex};
use crate::block_state_interface::{OverflowError, RawTokenAmountDelta};
use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use plt_scheduler_interface::error::{AccountNotFoundByAddressError, AccountNotFoundByIndexError};
use plt_types::types::tokens::RawTokenAmount;

/// Trait allowing reading the account token balance from the block state.
///
/// The token account block state (which includes the balance) is currently managed in Haskell.
///
/// # Arguments
///
/// - `account_index` The index of the account to update a token balance for.
///   Must be a valid account index of an existing account.
/// - `token_index` The index of the token. Must be a valid token index of an existing token.
pub trait ReadTokenAccountBalance {
    /// Get the balance of the token on the account.
    fn read_token_account_balance(
        &self,
        account: AccountIndex,
        token: TokenIndex,
    ) -> RawTokenAmount;
}

/// Trait allowing updating the account token balance in the block state.
/// Returns an error if the balance change would result in a negative balance
/// or a balance above the representable amount.
///
/// The token account block state (which includes the balance) is currently managed in Haskell.
///
/// # Arguments
///
/// - `account_index` The index of the account to update a token balance for. Must be a valid account index of an existing account.
/// - `token_index` The index of the token. Must be a valid token index of an existing token.
/// - `amount_delta` The amount to add to or subtract from the balance.
pub trait UpdateTokenAccountBalance {
    /// Update the balance of the token on the account.
    fn update_token_account_balance(
        &mut self,
        account: AccountIndex,
        token: TokenIndex,
        amount_delta: RawTokenAmountDelta,
    ) -> Result<(), OverflowError>;
}

/// Trait allowing incrementing the PLT update instruction sequence number.
pub trait IncrementPltUpdateSequenceNumber {
    /// Increment the PLT update instruction sequence number.
    fn increment_plt_update_sequence_number(&mut self);
}

/// Trait allowing getting account by account index. Returns an error
/// if the account does not exist.
///
/// # Arguments
///
/// - `account_index` Index of the (possibly existing) account to get.
pub trait GetCanonicalAddressByAccountIndex {
    /// Get account by index.
    fn account_canonical_address_by_account_index(
        &self,
        account_index: AccountIndex,
    ) -> Result<AccountAddress, AccountNotFoundByIndexError>;
}

/// Trait allowing getting account index by account address (canonical address or alias address).
/// Returns an error if the account does not exist.
///
/// # Arguments
///
/// - `account_address` Address of the (possibly existing) account to get.
pub trait GetAccountIndexByAddress {
    /// Get account by address.
    fn account_index_by_account_address(
        &self,
        account_address: &AccountAddress,
    ) -> Result<AccountIndex, AccountNotFoundByAddressError>;
}

/// Trait allowing getting token account states for an account.
///
/// The token account block state is currently managed in Haskell.
///
/// # Arguments
///
/// - `account_index` The index of the account to get token account states for. Must be a valid account index of an existing account.
pub trait GetTokenAccountStates {
    /// Get token account states for an account. Returns pairs of the token index and the
    /// token account state for the token.
    fn token_account_states(
        &self,
        account_index: AccountIndex,
    ) -> Vec<(TokenIndex, TokenAccountState)>;
}
