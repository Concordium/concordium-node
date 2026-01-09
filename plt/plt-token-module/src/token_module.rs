//! Implementation of the protocol-level token module. The capabilities of the token module
//! is defined by the token kernel, see [`TokenKernelOperations`].

mod initialize;
mod queries;
mod update;

pub use initialize::*;
pub use queries::*;
pub use update::*;

/// An invariant in the token module state that should be enforced
/// is broken.
#[derive(Debug, thiserror::Error)]
#[error("Token module state invariant broken: {0}")]
pub struct TokenModuleStateInvariantError(pub String);

#[derive(Debug, thiserror::Error)]
#[error("Token amount decimals mismatch: expected {expected}, found {found}")]
pub struct TokenAmountDecimalsMismatchError {
    pub expected: u8,
    pub found: u8,
}
