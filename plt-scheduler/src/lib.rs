use concordium_base::base::{AccountIndex, Energy};
use concordium_base::id::types::AccountAddress;
use concordium_base::protocol_level_tokens::TokenId;
use plt_deployment_unit::TokenRawAmount;

// Placeholder types to be defined or replaced with types from other crates.

pub type MutableTokenState = ();
pub type PLTConfiguration = ();
pub type TokenAmountDelta = ();

/// Index of the protocol-level token in the block state map of tokens.
#[derive(
    Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::Into, derive_more::From,
)]
pub struct TokenIndex {
    index: u64,
}

/// Operations on the state of a block in the chain.
///
/// This is abstracted in a trait to allow for a testing stub.
pub trait BlockStateOperations {
    // Protocol-level token state query interface.

    /// Get the [`TokenId`]s of all protocol-level tokens registered on the chain.
    ///
    /// If the protocol version does not support protocol-level tokens, this will return the empty
    /// list.
    fn get_plt_list(&self) -> impl std::iter::Iterator<Item = TokenId>;

    /// Get the [`TokenIndex`] associated with a [`TokenId`] (if it exists).
    ///
    /// # Arguments
    ///
    /// - `token_index` The token index to update.
    fn get_token_index(&self, token_id: TokenId) -> Option<TokenIndex>;

    /// Convert a persistent state to a mutable one that can be updated by the scheduler.
    ///
    /// Updates to this state will only persist in the block state using [`BlockStateOperations::set_token_state`].
    ///
    /// # Arguments
    ///
    /// - `token_index` The index of the token to get the state from.
    ///
    /// # Panics
    ///
    /// Panics if the token identified by `token_index` does not exist.
    fn get_mutable_token_state(&self, token_index: TokenIndex) -> MutableTokenState;

    /// Get the configuration of a protocol-level token.
    ///
    /// # Arguments
    ///
    /// - `token_index` The index of the token to get the config for.
    ///
    /// # Panics
    ///
    /// Panics if the token identified by `token_index` does not exist.
    fn get_token_configuration(&self, token_index: TokenIndex) -> PLTConfiguration;

    /// Get the circulating supply of a protocol-level token.
    ///
    /// # Arguments
    ///
    /// - `token_index` The index of the token to get the circulating supply.
    ///
    /// # Panics
    ///
    /// Panics if the token identified by `token_index` does not exist.
    fn get_token_circulating_supply(&self, token_index: TokenIndex) -> TokenRawAmount;

    /// Set the recorded total circulating supply for a protocol-level token.
    ///
    /// This should always be kept up-to-date with the total balance held in accounts.
    ///
    /// # Arguments
    ///
    /// - `token_index` The token index to update.
    /// - `circulation_supply` The new total circulating supply for the token.
    ///
    /// # Panics
    ///
    /// Panics if the token identified by `token_index` does not exist.
    fn set_token_circulating_supply(
        &mut self,
        token_index: TokenIndex,
        circulating_supply: TokenRawAmount,
    );

    /// Create a new token with the given configuration. The initial state will be empty
    /// and the initial supply will be 0. Returns the token index and the updated state.
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
    /// - The `token_id` of the given configuration MUST NOT already be in use by a protocol-level
    ///   token, i.e. `assert_eq!(s.get_token_index(configuration.token_id), None)`.
    /// - The [`PLTConfiguration`] MUST be valid and in particular the 'governance_account_index'
    ///   MUST reference a valid account.
    fn create_token(&mut self, configuration: PLTConfiguration) -> TokenIndex;

    /// Update the token balance of an account.
    ///
    /// # Arguments
    ///
    /// - `token_index` The token index to update.
    /// - `account_index` The account to update.
    /// - `amount_delta` The token balance delta.
    ///
    /// # Errors
    ///
    /// - [`OverflowError`] The update would overflow or underflow the token balance on the account.
    ///
    /// # Panics
    ///
    /// Panics if the token identified by `token_index` does not exist.
    fn update_token_account_balance(
        &mut self,
        token_index: TokenIndex,
        account_index: AccountIndex,
        amount_delta: TokenAmountDelta,
    ) -> Result<(), OverflowError>;

    /// Touch the token account. This initializes a token account state with a
    /// balance of zero. This only affects an account if its state for the token
    /// is empty.
    ///
    /// Returns `false`, if the account already contained a token account state.
    ///
    /// # Arguments
    ///
    /// - `token_index` The token index to update.
    /// - `account_index` The account to update.
    ///
    /// # Panics
    ///
    /// Panics if:
    ///
    /// - the token identified by `token_index` does not exist.
    /// - the account identified by `account_index` does not exist.
    #[must_use]
    fn touch_token_account(&mut self, token_index: TokenIndex, account_index: AccountIndex)
        -> bool;

    /// Increment the update sequence number for Protocol Level Tokens (PLT).
    ///
    /// Unlike the other chain updates this is a separate function, since there is no queue associated with PLTs.
    fn increment_plt_update_sequence_number(&mut self);

    /// Convert a mutable state to a persistent one and store it in the block state.
    ///
    /// To ensure this is future-proof, the mutable state should not be used after this call.
    ///
    /// # Arguments
    ///
    /// - `token_index` The token index to update.
    /// - `mutable_token_state` The mutated state to set as the current token state.
    ///
    /// # Panics
    ///
    /// Panics if the token identified by `token_index` does not exist.
    fn set_token_state(&mut self, token_index: TokenIndex, mutable_token_state: MutableTokenState);
}

/// Operations on the scheduler state.
pub trait SchedulerOperations {
    /// The account initiating the transaction.
    fn sender_account(&self) -> AccountIndex;

    /// The address of the account initiating the transaction.
    fn sender_account_address(&self) -> AccountAddress;

    /// Get the amount of energy remaining for the execution.
    fn get_energy(&self) -> Energy;

    /// Reduce the available energy for the execution.
    ///
    /// # Arguments
    ///
    /// - `energy` The amount of energy to charge.
    ///
    /// # Errors
    ///
    /// - [`OutOfEnergyError`] If the available energy is smaller than the ticked amount.
    fn tick_energy(&mut self, energy: Energy) -> Result<(), OutOfEnergyError>;
}

/// Transaction execution ran out of energy.
#[derive(Debug)]
pub struct OutOfEnergyError;

/// The computation resulted in overflow.
#[derive(Debug)]
pub struct OverflowError;

#[derive(Debug)]
pub enum TransactionRejectReason {}

pub type TransactionPayload = Vec<u8>;
pub type Events = ();

/// Execute a transaction payload modifying `scheduler` and `block_state` accordingly.
/// Returns the events produce if successful otherwise a reject reason.
pub fn execute_transaction(
    _scheduler: &mut impl SchedulerOperations,
    _block_state: &mut impl BlockStateOperations,
    _payload: TransactionPayload,
) -> Result<Events, TransactionRejectReason> {
    todo!()
}
