//! Token kernel interface for protocol-level tokens. The kernel handles all operations affecting token
//! balance and supply and manages the state and events related to balances and supply.

use concordium_base::base::{AccountIndex, Energy};
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{RawCbor, TokenModuleCborTypeDiscriminator};
use concordium_base::transactions::Memo;

pub type ModuleStateKey = Vec<u8>;
pub type ModuleStateValue = Vec<u8>;

/// Event produced from the effect of a token transaction.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct TokenModuleEvent {
    /// The type of event produced.
    pub event_type: TokenModuleCborTypeDiscriminator,
    /// The details of the event produced, in the raw byte encoded form.
    pub details: RawCbor,
}

/// Token amount without decimals specified. The token amount represented by
/// this type must always be represented with the number of decimals
/// the token natively has.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Ord, PartialOrd, Default)]
pub struct RawTokenAmount(pub u64);

/// The account has insufficient balance.
#[derive(Debug, thiserror::Error)]
#[error("Insufficient balance on account")]
pub struct InsufficientBalanceError {
    pub available: RawTokenAmount,
    pub required: RawTokenAmount,
}

/// Mint exceed the representable amount.
#[derive(Debug, thiserror::Error)]
#[error("Amount not representable")]
pub struct AmountNotRepresentableError;

/// Energy limit for execution reached.
#[derive(Debug, thiserror::Error)]
#[error("Out of energy")]
pub struct OutOfEnergyError;

/// Account with given address does not exist
#[derive(Debug, thiserror::Error)]
#[error("Account with address {0} does not exist")]
pub struct AccountNotFoundByAddressError(pub AccountAddress);

/// Account with given index does not exist
#[derive(Debug, thiserror::Error)]
#[error("Account with index {0} does not exist")]
pub struct AccountNotFoundByIndexError(pub AccountIndex);

/// An invariant in the token state that should be enforced
/// is broken. This is generally an error that should never happen and is unrecoverable.
#[derive(Debug, thiserror::Error)]
#[error("Token module state invariant broken: {0}")]
pub struct TokenStateInvariantError(pub String);

/// Represents the reasons why a query to the token module can fail.
#[derive(Debug, thiserror::Error)]
pub enum TransferError {
    #[error("{0}")]
    StateInvariantViolation(#[from] TokenStateInvariantError),
    #[error("Insufficient balance for transfer: {0}")]
    InsufficientBalance(#[from] InsufficientBalanceError),
}

/// Queries provided by the token kernel.
pub trait TokenKernelQueries {
    /// Opaque type that identifies an account on chain.
    /// The account is guaranteed to exist on chain, when holding an instance of this type.
    type Account;

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
    fn account_token_balance(&self, account: &Self::Account) -> RawTokenAmount;

    /// The current token circulation supply.
    fn circulating_supply(&self) -> RawTokenAmount;

    /// The number of decimals used in the presentation of the token amount.
    fn decimals(&self) -> u8;

    /// Lookup a key in the token state.
    fn lookup_token_module_state_value(&self, key: ModuleStateKey) -> Option<ModuleStateValue>;
}

/// Operations provided by the token kernel. The operations do not only allow modifying
/// token module state, but also indirectly affect the token state maintained by the token
/// kernel.
pub trait TokenKernelOperations: TokenKernelQueries {
    /// Update the balance of the given account to zero if it didn't have a balance before.
    fn touch(&mut self, account: &Self::Account);

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
        amount: RawTokenAmount,
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
        amount: RawTokenAmount,
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
    /// - [`TokenStateInvariantError`] If an internal token state invariant is broken.
    fn transfer(
        &mut self,
        from: &Self::Account,
        to: &Self::Account,
        amount: RawTokenAmount,
        memo: Option<Memo>,
    ) -> Result<(), TransferError>;

    /// Set or clear a value in the token state at the corresponding key.
    fn set_token_module_state_value(
        &mut self,
        key: ModuleStateKey,
        value: Option<ModuleStateValue>,
    );

    /// Log a token module event with the specified type and details.
    ///
    /// # Events
    ///
    /// This will produce a `TokenModuleEvent` in the logs.
    fn log_token_event(&mut self, event: TokenModuleEvent);
}

/// Operations and context related to transaction execution.
pub trait TokenKernelTransactionExecution {
    /// Opaque type that represents an account on chain.
    /// The account is guaranteed to exist on chain, when holding an instance of this type.
    type Account;

    /// The account initiating the transaction.
    fn sender_account(&self) -> Self::Account;

    /// Reduce the available energy for the PLT module execution.
    ///
    /// If the available energy is smaller than the given amount, an
    /// "out of energy" error will be returned, in which case the caller
    /// should stop execution and propagate the error upwards.
    /// The energy is charged in any case (also in case of failure).
    fn tick_energy(&mut self, energy: Energy) -> Result<(), OutOfEnergyError>;
}
