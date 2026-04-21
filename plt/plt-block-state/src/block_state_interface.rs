use crate::block_state::types::{
    AccountWithCanonicalAddress, TokenAccountState, TokenConfiguration, TokenIndex, TokenStateKey,
    TokenStateValue,
};
use crate::block_state::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, SimplisticTokenKeyValueState,
};
use concordium_base::base::{AccountIndex, ProtocolVersion};
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::TokenId;
use plt_scheduler_types::types::tokens::RawTokenAmount;

/// Change in [`RawTokenAmount`].
///
/// Represented as either add and subtract instead of a signed value, in order
/// to be able to represent the full range of possible deltas.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum RawTokenAmountDelta {
    /// Add the token amount
    Add(RawTokenAmount),
    /// Subtract the token amount
    Subtract(RawTokenAmount),
}

/// Account with given id does not exist
#[derive(Debug, thiserror::Error)]
#[error("Token with id {0} does not exist")]
pub struct TokenNotFoundByIdError(pub TokenId);

/// Queries on the state of a block in the chain.
pub trait BlockStateQuery {
    /// Get the [`TokenId`]s of all protocol-level tokens registered on the chain.
    ///
    /// If the protocol version does not support protocol-level tokens, this will return the empty
    /// list.
    fn plt_list(&self) -> impl Iterator<Item = TokenId>;

    /// Get the token associated with a [`TokenId`] (if it exists).
    /// The token ID is case-insensitive when looking up tokens by token ID.
    ///
    /// # Arguments
    ///
    /// - `token_id` The token id to get the [`TokenIndex`] of.
    fn token_by_id(&self, token_id: &TokenId) -> Result<TokenIndex, TokenNotFoundByIdError>;

    /// Convert a persistent token key-value state to a mutable one that can be updated by the scheduler.
    ///
    /// Updates to this state will only persist in the block state using [`BlockStateOperations::set_token_key_value_state`].
    ///
    /// # Arguments
    ///
    /// - `token` The token to get the token key-value state for.
    fn mutable_token_key_value_state(&self, token: TokenIndex) -> SimplisticTokenKeyValueState;

    /// Get the configuration of a protocol-level token.
    ///
    /// # Arguments
    ///
    /// - `token` The token to get the config for.
    fn token_configuration(&self, token: TokenIndex) -> TokenConfiguration;

    /// Get the circulating supply of a protocol-level token.
    ///
    /// # Arguments
    ///
    /// - `token` The token to get the circulating supply.
    fn token_circulating_supply(&self, token: TokenIndex) -> RawTokenAmount;

    /// Lookup the value for the given key in the given token key-value state. Returns `None` if
    /// no value exists for the given key.
    ///
    /// # Arguments
    ///
    /// - `token_key_value` The token module state to look up the value in.
    /// - `key` The token state key.
    fn lookup_token_state_value(
        &self,
        token_key_value: &SimplisticTokenKeyValueState,
        key: &TokenStateKey,
    ) -> Option<TokenStateValue>;

    /// Get iterator over key-value pairs with a shared prefix.
    ///
    /// # Arguments
    ///
    /// - `token_key_value` The token module state to look up the value in.
    /// - `prefix` The token state key prefix to iterate over.
    fn iter_token_state_prefix<'a>(
        &self,
        token_key_value: &'a SimplisticTokenKeyValueState,
        prefix: TokenStateKey,
    ) -> impl Iterator<Item = (&'a TokenStateKey, &'a TokenStateValue)>;

    /// Update the value for the given key in the given token key-value state. If `None` is
    /// specified as value, the entry is removed.
    ///
    /// # Arguments
    ///
    /// - `token_key_value` The token module state to update the value in.
    /// - `key` The token state key.
    /// - `value` The value to set. If `None`, the entry with the given key is removed.
    fn update_token_state_value(
        &self,
        token_key_value: &mut SimplisticTokenKeyValueState,
        key: &TokenStateKey,
        value: Option<TokenStateValue>,
    );

    /// Lookup the account using an account address.
    fn account_by_address(
        &self,
        address: &AccountAddress,
    ) -> Result<AccountIndex, AccountNotFoundByAddressError>;

    /// Lookup the account using an account index. Returns both the opaque account
    /// representation and the account canonical address.
    fn account_by_index(
        &self,
        index: AccountIndex,
    ) -> Result<AccountWithCanonicalAddress<AccountIndex>, AccountNotFoundByIndexError>;

    /// Get the token balance of the account.
    fn account_token_balance(&self, account: AccountIndex, token: TokenIndex) -> RawTokenAmount;

    /// Get token account states. It returns states for all tokens
    /// that the account holds.
    fn token_account_states(
        &self,
        account: AccountIndex,
    ) -> impl Iterator<Item = (TokenIndex, TokenAccountState)>;

    /// Query the protocol version of the block state.
    fn protocol_version(&self) -> ProtocolVersion;

    /// Whether the protocol version of the block supports RBAC token feature.
    fn support_rbac(&self) -> bool {
        self.protocol_version() >= ProtocolVersion::P11
    }

    /// Whether the protocol version of the block supports updating the token metadata.
    fn support_updating_metadata(&self) -> bool {
        self.protocol_version() >= ProtocolVersion::P11
    }
}

/// Operations on the state of a block in the chain.
pub trait BlockStateOperations: BlockStateQuery {
    /// Set the recorded total circulating supply for a protocol-level token.
    ///
    /// This should always be kept up-to-date with the total balance held in accounts.
    ///
    /// # Arguments
    ///
    /// - `token` The token.
    /// - `circulation_supply` The new total circulating supply for the token.
    fn set_token_circulating_supply(
        &mut self,
        token: TokenIndex,
        circulating_supply: RawTokenAmount,
    );

    /// Create a new token with the given configuration. The initial state will be empty
    /// and the initial supply will be 0. Returns representation of the created token.
    ///
    /// # Arguments
    ///
    /// - `configuration` The configuration for the token.
    ///
    /// # Preconditions
    ///
    /// The caller must ensure the following conditions are true, and failing to do so results in
    /// undefined behavior.
    ///
    /// - The `token` of the given configuration MUST NOT already be in use by a protocol-level
    ///   token, i.e. `assert_eq!(s.get_token_index(configuration.token_id), None)`.
    fn create_token(&mut self, configuration: TokenConfiguration) -> TokenIndex;

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
    fn update_token_account_balance(
        &mut self,
        token: TokenIndex,
        account: AccountIndex,
        amount_delta: RawTokenAmountDelta,
    ) -> Result<(), OverflowError>;

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
    fn touch_token_account(&mut self, token: TokenIndex, account: AccountIndex);

    /// Increment the update sequence number for Protocol Level Tokens (PLT).
    ///
    /// Unlike the other chain updates this is a separate function, since there is no queue associated with PLTs.
    fn increment_plt_update_instruction_sequence_number(&mut self);

    /// Convert a mutable token key-value state to a persistent one and store it in the block state.
    ///
    /// To ensure this is future-proof, the mutable state should not be used after this call.
    ///
    /// # Arguments
    ///
    /// - `token` The token index to update.
    /// - `mutable_token_module_state` The mutated state to set as the current token state.
    fn set_token_key_value_state(
        &mut self,
        token: TokenIndex,
        token_key_value_state: SimplisticTokenKeyValueState,
    );
}

/// The computation resulted in overflow (negative or above maximum value).
#[derive(Debug, thiserror::Error)]
#[error("Token amount overflow")]
pub struct OverflowError;
