//! Interactions with the part of the block state that is managed externally in Haskell.

// todo ar move all modules into persistent, except this module

use crate::block_state_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, OverflowError, RawTokenAmountDelta,
};
use crate::entity::protocol_level_tokens::p9::TokenIndex;
use concordium_base::base::AccountIndex;
use concordium_base::common::Serialize;
use concordium_base::contracts_common::AccountAddress;
use plt_scheduler_types::types::tokens::RawTokenAmount;

/// Token account state at block state level.
///
/// Corresponding Haskell type: `Concordium.GlobalState.Persistent.Account.ProtocolLevelTokens.TokenAccountState`
#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize)]
pub struct TokenAccountState {
    /// Balance of the account
    pub balance: RawTokenAmount,
}

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

    /// Initialize the balance of the given account to zero if it didn't have a balance before.
    /// If the account already has a balance for the token in context, the operation has no effect
    ///
    /// # Arguments
    ///
    /// - `account_index` The index of the account to update a token balance for. Must be a valid account index of an existing account.
    /// - `token_index` The index of the token. Must be a valid token index of an existing token.
    fn touch_token_account(&mut self, account: AccountIndex, token: TokenIndex);

    /// Increment the PLT chain update sequence number.
    fn increment_plt_update_sequence_number(&mut self);
}

/// External block state stubs to be used in tests.
pub mod test_stub {
    use super::*;

    /// Non-accessible block state representing the Haskell maintained part of the block state.
    #[derive(Debug)]
    pub struct NoExternalBlockStateStub;


    impl ExternalBlockStateQuery for NoExternalBlockStateStub {
        fn read_token_account_balance(
            &self,
            _account: AccountIndex,
            _token: TokenIndex,
        ) -> RawTokenAmount {
            unreachable!()
        }

        fn account_canonical_address_by_account_index(
            &self,
            _account_index: AccountIndex,
        ) -> Result<AccountAddress, AccountNotFoundByIndexError> {
            unreachable!()
        }

        fn account_index_by_account_address(
            &self,
            _account_address: &AccountAddress,
        ) -> Result<AccountIndex, AccountNotFoundByAddressError> {
            unreachable!()
        }

        fn token_account_states(
            &self,
            _account_index: AccountIndex,
        ) -> Vec<(TokenIndex, TokenAccountState)> {
            unreachable!()
        }
    }

    impl ExternalBlockStateOperations for NoExternalBlockStateStub {
        fn update_token_account_balance(
            &mut self,
            _account: AccountIndex,
            _token: TokenIndex,
            _amount_delta: RawTokenAmountDelta,
        ) -> Result<(), OverflowError> {
            unreachable!()
        }

        fn touch_token_account(&mut self, _account: AccountIndex, _token: TokenIndex) {
            unreachable!()
        }

        fn increment_plt_update_sequence_number(&mut self) {
            unreachable!()
        }
    }

}
