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

/// The computation resulted in overflow (negative or above maximum value).
#[derive(Debug, thiserror::Error)]
#[error("Token amount overflow")]
pub struct OverflowError;
