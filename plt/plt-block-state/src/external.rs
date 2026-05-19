//! Interactions with the part of the block state that is managed externally in Haskell.

use crate::block_state_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, OverflowError, RawTokenAmountDelta,
};
use crate::persistent::protocol_level_tokens::p9::TokenIndex;
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
    use crate::entity::accounts::Account;
    use std::collections::BTreeMap;

    /// Non-accessible block state representing the Haskell maintained part of the block state.
    #[derive(Debug, Default, Clone)]
    pub struct UnreachableExternalBlockState;

    impl ExternalBlockStateQuery for UnreachableExternalBlockState {
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

    impl ExternalBlockStateOperations for UnreachableExternalBlockState {
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

    /// Stubbed block state representing the Haskell maintained part of the block state.
    #[derive(Debug, Default, Clone)]
    pub struct ExternalBlockStateStub {
        /// List of accounts in the stub.
        accounts: Vec<AccountStub>,
        /// PLT update instruction sequence number
        plt_update_instruction_sequence_number: u64,
    }

    impl ExternalBlockStateStub {
        /// Create account in the stub and return stub representation of the account.
        pub fn create_account(&mut self) -> Account {
            let index = self.accounts.len();
            let mut address = AccountAddress([0u8; 32]);
            address.0[..8].copy_from_slice(&index.to_be_bytes());
            let account = AccountStub {
                address,
                tokens: Default::default(),
            };
            let stub_index = AccountIndex::from(index as u64);
            self.accounts.push(account);

            Account::from_existing_account(stub_index)
        }

        /// Get the canonical address of an account in the stub
        pub fn account_canonical_address(&self, account: AccountIndex) -> AccountAddress {
            self.accounts[account.index as usize].address
        }

        /// Get next PLT update sequence number
        pub fn plt_update_instruction_sequence_number(&self) -> u64 {
            self.plt_update_instruction_sequence_number
        }
    }

    /// Internal representation of an account in [`BlockStateWithExternalStateStubbed`].
    #[derive(Debug, Clone)]
    struct AccountStub {
        /// The canonical account address of the account.
        address: AccountAddress,
        /// Tokens the account is holding
        tokens: BTreeMap<TokenIndex, AccountTokenStub>,
    }

    /// Internal representation of a token in an account.
    #[derive(Debug, Default, Clone)]
    struct AccountTokenStub {
        /// Account balance
        balance: RawTokenAmount,
    }

    impl ExternalBlockStateQuery for ExternalBlockStateStub {
        fn read_token_account_balance(
            &self,
            account: AccountIndex,
            token: TokenIndex,
        ) -> RawTokenAmount {
            self.accounts[account.index as usize]
                .tokens
                .get(&token)
                .map(|token| token.balance)
                .unwrap_or_default()
        }

        fn account_canonical_address_by_account_index(
            &self,
            account_index: AccountIndex,
        ) -> Result<AccountAddress, AccountNotFoundByIndexError> {
            if let Some(account) = self.accounts.get(account_index.index as usize) {
                Ok(account.address)
            } else {
                Err(AccountNotFoundByIndexError(account_index))
            }
        }

        fn account_index_by_account_address(
            &self,
            account_address: &AccountAddress,
        ) -> Result<AccountIndex, AccountNotFoundByAddressError> {
            self.accounts
                .iter()
                .enumerate()
                .find_map(|(i, account)| {
                    if account.address.is_alias(account_address) {
                        Some(AccountIndex::from(i as u64))
                    } else {
                        None
                    }
                })
                .ok_or(AccountNotFoundByAddressError(*account_address))
        }

        fn token_account_states(
            &self,
            account_index: AccountIndex,
        ) -> Vec<(TokenIndex, TokenAccountState)> {
            self.accounts[account_index.index as usize]
                .tokens
                .iter()
                .map(|(token, state)| {
                    let token_account_state = TokenAccountState {
                        balance: state.balance,
                    };

                    (*token, token_account_state)
                })
                .collect()
        }
    }

    impl ExternalBlockStateOperations for ExternalBlockStateStub {
        fn update_token_account_balance(
            &mut self,
            account: AccountIndex,
            token: TokenIndex,
            amount_delta: RawTokenAmountDelta,
        ) -> Result<(), OverflowError> {
            let balance = &mut self.accounts[account.index as usize]
                .tokens
                .entry(token)
                .or_default()
                .balance;
            match amount_delta {
                RawTokenAmountDelta::Add(add) => {
                    balance.0 = balance.0.checked_add(add.0).ok_or(OverflowError)?;
                }
                RawTokenAmountDelta::Subtract(subtract) => {
                    balance.0 = balance.0.checked_sub(subtract.0).ok_or(OverflowError)?;
                }
            }
            Ok(())
        }

        fn touch_token_account(&mut self, account: AccountIndex, token: TokenIndex) {
            self.accounts[account.index as usize]
                .tokens
                .entry(token)
                .or_default();
        }

        fn increment_plt_update_sequence_number(&mut self) {
            self.plt_update_instruction_sequence_number += 1;
        }
    }
}
