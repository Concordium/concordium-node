//! Token kernel interface for protocol-level tokens. This is the interface seen
//! by the token module. The kernel handles all operations affecting token
//! balance and supply and manages the state and events related to balances and supply.
use plt_scheduler_types::types::tokens::RawTokenAmount;

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
