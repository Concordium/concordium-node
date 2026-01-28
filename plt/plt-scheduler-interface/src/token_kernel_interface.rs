//! Token kernel interface for protocol-level tokens. This is the interface seen
//! by the token module. The kernel handles all operations affecting token
//! balance and supply and manages the state and events related to balances and supply.

use crate::error::{AccountNotFoundByAddressError, AccountNotFoundByIndexError};
use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{RawCbor, TokenModuleCborTypeDiscriminator};
use concordium_base::transactions::Memo;
use plt_types::types::tokens::RawTokenAmount;

pub type TokenStateKey = Vec<u8>;
pub type TokenStateValue = Vec<u8>;

/// Account representing (read-only) account state.
///
/// The account is guaranteed to exist on chain, when holding an instance of this type.
#[derive(Debug)]
pub struct AccountWithCanonicalAddress<Account> {
    /// Opaque type that represents an account on chain.
    pub account: Account,
    /// The canonical account address of the account, i.e. the address used as part of the
    /// credential deployment and not an alias.
    pub canonical_account_address: AccountAddress,
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
    /// Amount requested to be minted
    pub requested_amount: RawTokenAmount,
    /// Current circulating supply of the token
    pub current_supply: RawTokenAmount,
    /// Maximum representable token amount
    pub max_representable_amount: RawTokenAmount,
}

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

/// Queries provided by the token kernel. All queries are in context of
/// a specific token that the kernel is initialized with.
pub trait TokenKernelQueries {
    /// Opaque type that identifies an account on chain including the address it was connected with
    /// when looking up the account.
    /// The account is guaranteed to exist on chain, when holding an instance of this type.
    ///
    /// The type corresponds to `BlockStateQuery::Account` but includes the account address also.
    /// The account address is included to tie it together with the opaque identifier for the account
    /// in a way that cannot be manipulated by the token module.
    type AccountWithAddress;

    /// Lookup the account using an account address.
    fn account_by_address(
        &self,
        address: &AccountAddress,
    ) -> Result<Self::AccountWithAddress, AccountNotFoundByAddressError>;

    /// Lookup the account using an account index.
    /// Returns both the opaque account
    //  representation and the account canonical address.
    fn account_by_index(
        &self,
        index: AccountIndex,
    ) -> Result<AccountWithCanonicalAddress<Self::AccountWithAddress>, AccountNotFoundByIndexError>;

    /// Get the account index for the account.
    fn account_index(&self, account: &Self::AccountWithAddress) -> AccountIndex;

    /// Get the token balance of the account.
    fn account_token_balance(&self, account: &Self::AccountWithAddress) -> RawTokenAmount;

    /// The number of decimals used in the presentation of the token amount.
    fn decimals(&self) -> u8;

    /// Lookup a key in the token state.
    fn lookup_token_state_value(&self, key: TokenStateKey) -> Option<TokenStateValue>;
}

/// Operations provided by the token kernel. All operations are in context of
/// a specific token that the kernel is initialized with.
///
/// The operations do not only allow modifying
/// token module state, but also indirectly affect the token state maintained by the token
/// kernel.
pub trait TokenKernelOperations: TokenKernelQueries {
    /// Initialize the balance of the given account to zero if it didn't have a balance before.
    /// It has the observable effect that the token is then returned when querying the tokens
    /// for an account. Should be called if the token module account state is set,
    /// in order to make sure the token is returned when querying token account info.
    ///
    /// If the account already has a balance for the token in context, the operation has no effect
    fn touch_account(&mut self, account: &Self::AccountWithAddress);

    /// Mint a specified amount and deposit it in the account.
    ///
    /// # Events
    ///
    /// This will produce a `TokenMintEvent` in the logs.
    ///
    /// # Errors
    ///
    /// - [`TokenMintError::MintWouldOverflow`] The total supply would exceed the representable amount.
    /// - [`TokenMintError::StateInvariantViolation`] If an internal token state invariant is broken.
    fn mint(
        &mut self,
        account: &Self::AccountWithAddress,
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
        account: &Self::AccountWithAddress,
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
        from: &Self::AccountWithAddress,
        to: &Self::AccountWithAddress,
        amount: RawTokenAmount,
        memo: Option<Memo>,
    ) -> Result<(), TokenTransferError>;

    /// Set or clear a value in the token state at the corresponding key.
    fn set_token_state_value(&mut self, key: TokenStateKey, value: Option<TokenStateValue>);

    /// Log a token module event with the specified type and details.
    ///
    /// # Events
    ///
    /// This will produce a `TokenModuleEvent` in the logs.
    fn log_token_event(&mut self, event_type: TokenModuleCborTypeDiscriminator, details: RawCbor);
}
