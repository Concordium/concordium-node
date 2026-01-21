use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{TokenId, TokenModuleRef};
use plt_scheduler_interface::{AccountNotFoundByAddressError, AccountNotFoundByIndexError};
use plt_token_module::token_kernel_interface::{RawTokenAmount, TokenStateKey, TokenStateValue};

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

/// Static configuration for a protocol-level token.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct TokenConfiguration {
    /// The token ID in its canonical form. Token IDs are case-insensitive when compared,
    /// but the canonical token ID preserves the original casing specified when
    /// the token was created.
    pub token_id: TokenId,
    /// The token module reference.
    pub module_ref: TokenModuleRef,
    /// The number of decimal places used in the representation of the token.
    pub decimals: u8,
}

/// Token account state at block state level.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct TokenAccountBlockState {
    /// Balance of the account
    pub balance: RawTokenAmount,
}

/// Account with given id does not exist
#[derive(Debug, thiserror::Error)]
#[error("Token with id {0} does not exist")]
pub struct TokenNotFoundByIdError(pub TokenId);

/// Queries on the state of a block in the chain.
pub trait BlockStateQuery {
    /// Opaque type that represents the module managed key-value map.
    /// It defines a dynamic data model defined by the module.
    type TokenKeyValueState;

    /// Opaque type that represents an account on chain.
    /// The account is guaranteed to exist on chain, when holding an instance of this type.
    type Account;

    /// Opaque type that represents a token on chain.
    /// The token is guaranteed to exist on chain, when holding an instance of this type.
    type Token;

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
    /// - `token_id` The token id to get the [`Self::Token`] of.
    fn token_by_id(&self, token_id: &TokenId) -> Result<Self::Token, TokenNotFoundByIdError>;

    /// Convert a persistent token key-value state to a mutable one that can be updated by the scheduler.
    ///
    /// Updates to this state will only persist in the block state using [`BlockStateOperations::set_token_key_value_state`].
    ///
    /// # Arguments
    ///
    /// - `token` The token to get the token key-value state for.
    fn mutable_token_key_value_state(&self, token: &Self::Token) -> Self::TokenKeyValueState;

    /// Get the configuration of a protocol-level token.
    ///
    /// # Arguments
    ///
    /// - `token` The token to get the config for.
    fn token_configuration(&self, token: &Self::Token) -> TokenConfiguration;

    /// Get the circulating supply of a protocol-level token.
    ///
    /// # Arguments
    ///
    /// - `token` The token to get the circulating supply.
    fn token_circulating_supply(&self, token: &Self::Token) -> RawTokenAmount;

    /// Lookup the value for the given key in the given token key-value state. Returns `None` if
    /// no value exists for the given key.
    ///
    /// # Arguments
    ///
    /// - `token_module_map` The token module state to look up the value in.
    /// - `key` The token state key.
    fn lookup_token_state_value(
        &self,
        token_key_value: &Self::TokenKeyValueState,
        key: &TokenStateKey,
    ) -> Option<TokenStateValue>;

    /// Update the value for the given key in the given token key-value state. If `None` is
    /// specified as value, the entry is removed.
    ///
    /// # Arguments
    ///
    /// - `token_module_map` The token module state to update the value in.
    /// - `key` The token state key.
    /// - `value` The value to set. If `None`, the entry with the given key is removed.
    fn update_token_state_value(
        &self,
        token_key_value: &mut Self::TokenKeyValueState,
        key: &TokenStateKey,
        value: Option<TokenStateValue>,
    );

    /// Lookup the account using an account address.
    fn account_by_address(
        &self,
        address: &AccountAddress,
    ) -> Result<Self::Account, AccountNotFoundByAddressError>;

    /// Lookup the account using an account index.
    fn account_by_index(
        &self,
        index: AccountIndex,
    ) -> Result<Self::Account, AccountNotFoundByIndexError>;

    /// Get the account index for the account.
    fn account_index(&self, account: &Self::Account) -> AccountIndex;

    /// Get the canonical account address of the account, i.e. the address used as part of the
    /// credential deployment and not an alias.
    fn account_canonical_address(&self, account: &Self::Account) -> AccountAddress;

    /// Get the token balance of the account.
    fn account_token_balance(&self, account: &Self::Account, token: &Self::Token)
    -> RawTokenAmount;

    /// Get token account states. It returns states for all tokens
    /// that the account holds.
    fn token_account_states(
        &self,
        account: &Self::Account,
    ) -> impl Iterator<Item = (Self::Token, TokenAccountBlockState)>;
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
        token: &Self::Token,
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
    fn create_token(&mut self, configuration: TokenConfiguration) -> Self::Token;

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
        token: &Self::Token,
        account: &Self::Account,
        amount_delta: RawTokenAmountDelta,
    ) -> Result<(), OverflowError>;

    /// Touch the token in the account state. This initializes the token in the account state with a
    /// balance of zero if it does not already exist. It has the observable effect that the token
    /// is then returned when querying the tokens for an account.
    /// It only affects an account if the state for the token does not already exist.    
    ///
    /// # Arguments
    ///
    /// - `account` The account to update.
    /// - `token` The token to update.
    fn touch_token_account(&mut self, token: &Self::Token, account: &Self::Account);

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
        token: &Self::Token,
        token_key_value_state: Self::TokenKeyValueState,
    );
}

/// The computation resulted in overflow (negative or above maximum value).
#[derive(Debug, thiserror::Error)]
#[error("Token amount overflow")]
pub struct OverflowError;
