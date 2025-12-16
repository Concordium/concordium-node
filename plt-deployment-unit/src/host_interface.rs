//! Host interface for protocol-level tokens.
use concordium_base::base::{AccountIndex, Energy};
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::RawCbor;
use concordium_base::transactions::Memo;

pub type StateKey = Vec<u8>;
pub type StateValue = Vec<u8>;
pub type TokenEventType = String;
pub type TokenEventDetails = RawCbor;
pub type Parameter = RawCbor;
pub type TokenRawAmount = u64;

/// The account has insufficient balance.
#[derive(Debug)]
pub struct InsufficientBalanceError;

/// Update to state key failed because the key was locked by an iterator.
#[derive(Debug, thiserror::Error)]
#[error("State key is locked")]
pub struct LockedStateKeyError;

/// Mint exceed the representable amount.
#[derive(Debug, thiserror::Error)]
#[error("Amount not representable")]
pub struct AmountNotRepresentableError;

/// Operations provided by the deployment unit host.
///
/// This is abstracted in a trait to allow for a testing stub.
pub trait HostOperations {
    /// The type for the account object.
    ///
    /// The account is guaranteed to exist on chain, when holding an instance of this type.
    type Account;

    /// Lookup the account using an account address.
    fn account_by_address(&self, address: &AccountAddress) -> Option<Self::Account>;

    /// Lookup the account using an account index.
    fn account_by_index(&self, index: AccountIndex) -> Option<Self::Account>;

    /// Get the account index for the account.
    fn account_index(&self, account: &Self::Account) -> AccountIndex;

    /// Get the canonical account address of the account, i.e. the address used as part of the
    /// credential deployment and not an alias.
    fn account_canonical_address(&self, account: &Self::Account) -> AccountAddress;

    /// Get the token balance of the account.
    fn account_balance(&self, account: &Self::Account) -> TokenRawAmount;

    /// Update the balance of the given account to zero if it didn't have a balance before.
    ///
    /// Returns `true` if the balance wasn't present on the given account and `false` otherwise.
    fn touch(&mut self, account: &Self::Account) -> bool;

    /// Mint a specified amount and deposit it in the account.
    ///
    /// # Events
    ///
    /// This will produce a `TokenMintEvent` in the logs.
    ///
    /// # Errors
    ///
    /// - [`AmountNotRepresentableError`] The total supply would exceed the representable amount.
    fn mint(
        &mut self,
        account: &Self::Account,
        amount: TokenRawAmount,
    ) -> Result<(), AmountNotRepresentableError>;

    /// Burn a specified amount from the account.
    ///
    /// # Events
    ///
    /// This will produce a `TokenBurnEvent` in the logs.
    ///
    /// # Errors
    ///
    /// - [`InsufficientBalanceError`] The sender has insufficient balance.
    fn burn(
        &mut self,
        account: &Self::Account,
        amount: TokenRawAmount,
    ) -> Result<(), InsufficientBalanceError>;

    /// Transfer a token amount from one account to another, with an optional memo.
    ///
    /// # Events
    ///
    /// This will produce a `TokenTransferEvent` in the logs.
    ///
    /// # Errors
    ///
    /// - [`InsufficientBalanceError`] The sender has insufficient balance.
    fn transfer(
        &mut self,
        from: &Self::Account,
        to: &Self::Account,
        amount: TokenRawAmount,
        memo: Option<Memo>,
    ) -> Result<(), InsufficientBalanceError>;

    /// The current token circulation supply.
    fn circulating_supply(&self) -> TokenRawAmount;

    /// The number of decimals used in the presentation of the token amount.
    fn decimals(&self) -> u8;

    /// Lookup a key in the token state.
    fn get_token_state(&self, key: StateKey) -> Option<StateValue>;

    /// Set or clear a value in the token state at the corresponding key.
    ///
    /// Returns whether there was an existing entry.
    ///
    /// # Errors
    ///
    /// - [`LockedStateKeyError`] if the update failed because the key was locked by an iterator.
    fn set_token_state(
        &mut self,
        key: StateKey,
        value: Option<StateValue>,
    ) -> Result<bool, LockedStateKeyError>;

    /// Reduce the available energy for the PLT module execution.
    ///
    /// If the available energy is smaller than the given amount, the containing transaction will
    /// abort and the effects of the transaction will be rolled back.
    /// The energy is charged in any case (also in case of failure).
    fn tick_energy(&mut self, energy: Energy);

    /// Log a token module event with the specified type and details.
    ///
    /// # Events
    ///
    /// This will produce a `TokenModuleEvent` in the logs.
    fn log_token_event(&mut self, event_type: TokenEventType, event_details: TokenEventDetails);
}
