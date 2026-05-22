//! Errors used in the token module.

use plt_scheduler_types::types::tokens::RawTokenAmount;

/// Token amount decimals mismatch
#[derive(Debug, thiserror::Error)]
#[error("Token amount decimals mismatch: expected {expected}, found {found}")]
pub struct TokenAmountDecimalsMismatchError {
    /// Expected decimals
    pub expected: u8,
    /// Actual decimals
    pub found: u8,
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


