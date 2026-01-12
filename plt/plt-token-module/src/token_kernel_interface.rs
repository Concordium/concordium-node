//! Token kernel interface for protocol-level tokens. This is the interface seen
//! by the token module. The kernel handles all operations affecting token
//! balance and supply and manages the state and events related to balances and supply.

use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{RawCbor, TokenId, TokenModuleCborTypeDiscriminator};
use concordium_base::transactions::Memo;

pub type ModuleStateKey = Vec<u8>;
pub type ModuleStateValue = Vec<u8>;

/// Event produced from the effect of a token transaction.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct TokenModuleEvent {
    /// The canonical token id.
    pub token_id: TokenId,
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

impl RawTokenAmount {
    /// Maximum representable raw token amount.
    pub const MAX: Self = Self(u64::MAX);
}

/// The account has insufficient balance.
#[derive(Debug, thiserror::Error)]
#[error("Insufficient balance on account")]
pub struct InsufficientBalanceError {
    /// Balance available on account
    pub available: RawTokenAmount,
    /// Balance required on account
    pub required: RawTokenAmount,
}

/// Mint exceed the representable amount.
#[derive(Debug, thiserror::Error)]
#[error("Minting the requested amount would overflow the circulating supply amount")]
pub struct MintWouldOverflowError {
    pub requested_amount: RawTokenAmount,
    pub circulating_supply: RawTokenAmount,
    pub max_representable_amount: RawTokenAmount,
}

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

/// Represents the reasons why a token transfer may fail.
#[derive(Debug, thiserror::Error)]
pub enum TokenTransferError {
    #[error("{0}")]
    StateInvariantViolation(#[from] TokenStateInvariantError),
    #[error("Insufficient balance for transfer: {0}")]
    InsufficientBalance(#[from] InsufficientBalanceError),
}

/// Represents the reasons why a token mint may fail.
#[derive(Debug, thiserror::Error)]
pub enum TokenMintError {
    #[error("{0}")]
    StateInvariantViolation(#[from] TokenStateInvariantError),
    #[error("{0}")]
    MintWouldOverflow(#[from] MintWouldOverflowError),
}

/// Represents the reasons why a token burn may fail.
#[derive(Debug, thiserror::Error)]
pub enum TokenBurnError {
    #[error("{0}")]
    StateInvariantViolation(#[from] TokenStateInvariantError),
    #[error("Insufficient balance for burn: {0}")]
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
    fn touch_account(&mut self, account: &Self::Account);

    /// Mint a specified amount and deposit it in the account.
    ///
    /// # Events
    ///
    /// This will produce a `TokenMintEvent` in the logs.
    ///
    /// # Errors
    ///
    /// - [`MintWouldOverflowError`] The total supply would exceed the representable amount.
    fn mint(
        &mut self,
        account: &Self::Account,
        amount: RawTokenAmount,
    ) -> Result<(), TokenMintError>;

    /// Burn a specified amount from the account.
    ///
    /// # Events
    ///
    /// This will produce a `TokenBurnEvent` in the logs.
    ///
    /// # Errors
    ///
    /// - [`TokenBurnError::InsufficientBalance`] The sender has insufficient balance.
    /// - [`TokenBurnError::StateInvariantViolation`] If an internal token state invariant is broken.
    fn burn(
        &mut self,
        account: &Self::Account,
        amount: RawTokenAmount,
    ) -> Result<(), TokenBurnError>;

    /// Transfer a token amount from one account to another, with an optional memo.
    ///
    /// # Events
    ///
    /// This will produce a `TokenTransferEvent` in the logs.
    ///
    /// # Errors
    ///
    /// - [`TokenTransferError::InsufficientBalance`] The sender has insufficient balance.
    /// - [`TokenTransferError::StateInvariantViolation`] If an internal token state invariant is broken.
    fn transfer(
        &mut self,
        from: &Self::Account,
        to: &Self::Account,
        amount: RawTokenAmount,
        memo: Option<Memo>,
    ) -> Result<(), TokenTransferError>;

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
