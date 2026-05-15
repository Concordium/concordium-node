use crate::entity::accounts::AccountWithCanonicalAddress;
use crate::external::TokenAccountState;
use concordium_base::base::{AccountIndex, ProtocolVersion};
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_locks::LockId;
use concordium_base::protocol_level_tokens::TokenId;
use plt_scheduler_types::types::tokens::RawTokenAmount;
use crate::persistent::protocol_level_tokens::p9::TokenConfiguration;

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

/// Lock with given id does not exist
#[derive(Debug)]
pub struct LockNotFoundByIdError(pub LockId);

/// Account with given address does not exist
#[derive(Debug, thiserror::Error)]
#[error("Account with address {0} does not exist")]
pub struct AccountNotFoundByAddressError(pub AccountAddress);

/// Account with given index does not exist
#[derive(Debug, thiserror::Error)]
#[error("Account with index {0} does not exist")]
pub struct AccountNotFoundByIndexError(pub AccountIndex);

/// Unrecoverable failure accessing the block state. This is generally an error that
/// should never happen and is unrecoverable.
///
/// If returned when **applying a block item to the block state**,
/// it may leave the block state in an indeterminate state. E.g. can parts of the effects
/// of processing the block item be applied, an others not. Hence, the resulting block
/// state should not be used.
///
/// If returned when **querying the block state**, the query itself fails,
/// but the block state is still in a valid state.
#[derive(Debug, thiserror::Error)]
pub enum BlockStateFailure {
    /// An error happened when decoding a block state value from the blob store.
    #[error("Error decoding state from blob store: {0}")]
    BlobStoreDecode(String),
    /// An invariant that must be true is broken. The invariant can either be in the
    /// stored block state or a runtime logical invariant related to the in-memory block state.
    #[error("State invariant broken: {0}")]
    Invariant(String),
    /// When looking up a value with in an owned
    /// [blob reference](super::block_state::blob_reference::hashed_cacheable_reference::HashedCacheableRef),
    /// a borrowed value was returned. This should generally never happen in they way we maintain
    /// blob references.
    #[error("Borrowed value found inside of owned value: {0}")]
    CowJoin(&'static str),
}

pub type BlockStateResult<T> = Result<T, BlockStateFailure>;

/// Key in the key-value state.
#[derive(Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct TokenStateKey(pub Vec<u8>);

/// Value in the key-value state.
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct TokenStateValue(pub Vec<u8>);

// todo remove as part of https://linear.app/concordium/issue/COR-2398/push-block-state-entity-model-into-the-scheduler
/// Queries on the state of a block in the chain.
pub trait BlockStateQuery {
    /// Opaque type that represents the thawed (mutable) token key-value map.
    type MutableTokenKeyValueState;

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
    fn plt_list(&self) -> impl ExactSizeIterator<Item = TokenId>;

    /// Get the token associated with a [`TokenId`] (if it exists).
    /// The token ID is case-insensitive when looking up tokens by token ID.
    ///
    /// # Arguments
    ///
    /// - `token_id` The token id to get the [`Self::Token`] of.
    fn token_by_id(&self, token_id: &TokenId) -> Result<Self::Token, TokenNotFoundByIdError>;

    /// Convert a persistent token key-value state to a mutable (thawed) one that can be updated by the scheduler.
    ///
    /// Updates to this state will only persist in the block state using [`BlockStateOperations::set_token_key_value_state`].
    ///
    /// # Arguments
    ///
    /// - `token` The token to thaw the token key-value state for.
    fn mutable_token_key_value_state(&self, token: &Self::Token)
    -> Self::MutableTokenKeyValueState;

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
    /// - `token_key_value` The token key-value state to look up the value in.
    /// - `key` The token state key.
    fn lookup_token_state_value(
        &self,
        token_key_value: &Self::MutableTokenKeyValueState,
        key: &TokenStateKey,
    ) -> Option<TokenStateValue>;

    /// Get iterator over key-value pairs with the given prefix in the
    /// token key-value state.
    ///
    /// # Arguments
    ///
    /// - `token_key_value` The token key-value state to iterator values in.
    /// - `prefix` The token state key prefix to iterate over.
    fn iter_token_state_prefix<'a>(
        &'a self,
        token_key_value: &Self::MutableTokenKeyValueState,
        prefix: &TokenStateKey,
    ) -> impl Iterator<Item = (TokenStateKey, TokenStateValue)> + use<'a, Self>;

    /// Update the value for the given key in the given thawed token key-value state. If `None` is
    /// specified as value, the entry is removed.
    ///
    /// # Arguments
    ///
    /// - `token_key_value` The thawed (mutable) token module state to update the value in.
    /// - `key` The token state key.
    /// - `value` The value to set. If `None`, the entry with the given key is removed.
    fn update_token_state_value(
        &self,
        token_key_value: &mut Self::MutableTokenKeyValueState,
        key: &TokenStateKey,
        value: Option<TokenStateValue>,
    );

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
    ) -> Result<AccountWithCanonicalAddress, AccountNotFoundByIndexError>;

    /// Get the account index for the account.
    fn account_index(&self, account: &Self::Account) -> AccountIndex;

    /// Get the token balance of the account.
    fn account_token_balance(&self, account: &Self::Account, token: &Self::Token)
    -> RawTokenAmount;

    /// Get token account states. It returns states for all tokens
    /// that the account holds.
    fn token_account_states(
        &self,
        account: &Self::Account,
    ) -> impl Iterator<Item = (Self::Token, TokenAccountState)>;

    /// Query the protocol version of the block state.
    fn protocol_version(&self) -> ProtocolVersion;

    /// Get the [`LockId`]s of all protocol-level locks registered on the chain at the
    /// end of the block.
    ///
    /// If the protocol version does not support protocol-level locks, this will return the empty
    /// list.
    fn lock_list(&self) -> impl ExactSizeIterator<Item = LockId>;

    /// Get the lock associated with a [`LockId`] (if it exists).
    ///
    /// # Arguments
    ///
    /// - `lock_id` The lock id to get the [`Self::Lock`] of.
    fn lock_by_id(&self, lock_id: &LockId) -> Result<LockId, LockNotFoundByIdError>;

    /// Get the configuration of a protocol-level lock.
    ///
    /// # Arguments
    ///
    /// - `lock` The lock to get the configuration for.
    fn lock_configuration(&self, lock: &LockId) -> LockConfiguration;

    /// Get the set of account/token balances currently tracked under a lock.
    ///
    /// Each returned pair identifies an account and token for which the lock may
    /// hold a non-zero locked balance. The corresponding amount is tracked in the
    /// token module state.
    ///
    /// # Arguments
    ///
    /// - `lock` The lock to get the tracked locked balances for.
    fn lock_balances(&self, lock: &LockId) -> impl Iterator<Item = (Self::Account, Self::Token)>;
}

// todo remove as part of https://linear.app/concordium/issue/COR-2398/push-block-state-entity-model-into-the-scheduler
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
    fn touch_token_account(&mut self, token: &Self::Token, account: &Self::Account);

    /// Increment the update sequence number for Protocol Level Tokens (PLT).
    ///
    /// Unlike the other chain updates this is a separate function, since there is no queue associated with PLTs.
    fn increment_plt_update_instruction_sequence_number(&mut self);

    /// Convert a mutable token key-value state into a persistent state and store it in the block state.
    ///
    /// The mutable state should not be used after this call.
    ///
    /// # Arguments
    ///
    /// - `token` The token index to update.
    /// - `mutable_token_module_state` The mutated state to set as the current token state.
    fn set_token_key_value_state(
        &mut self,
        token: &Self::Token,
        token_key_value_state: Self::MutableTokenKeyValueState,
    );

    /// Create a new PLT lock with the given configuration. The initial state will be empty.
    ///
    /// # Arguments
    ///
    /// - `lock_id` The ID of the PLT lock.
    /// - `configuration` The configuration for the PLT lock.
    ///
    /// # Preconditions
    ///
    /// The caller must ensure the following conditions are true, and failing to do so results in
    /// undefined behavior.
    ///
    /// - The `lock` of the given configuration MUST NOT already be in use by a protocol-level
    ///   lock, i.e. `assert_eq!(s.lock_by_id(lock_id).ok(), None)`.
    fn create_lock(&mut self, lock_id: LockId, configuration: LockConfiguration);

    /// Track that a lock holds a balance for the given account and token.
    ///
    /// This records the account/token pair in the lock state so it can later be
    /// queried through [`BlockStateQuery::lock_balances`].
    ///
    /// # Arguments
    ///
    /// - `lock` The lock to update.
    /// - `account` The account whose locked balance is tracked.
    /// - `token` The token whose locked balance is tracked.
    ///
    /// The caller must ensure the following conditions are true, and failing to do so results in
    /// undefined behavior.
    ///
    /// - The `lock` MUST already exist in the block state, i.e.
    ///   `s.lock_by_id(lock_id).expect("lock exists")`.
    fn add_lock_balance_ref(&mut self, lock: &LockId, account: &Self::Account, token: &Self::Token);
}

/// The computation resulted in overflow (negative or above maximum value).
#[derive(Debug, thiserror::Error)]
#[error("Token amount overflow")]
pub struct OverflowError;
