//! Implementation of the protocol-level token module. The token module implements
//! execution of [token operations](concordium_base::protocol_level_tokens::TokenOperation).
//!
//! The capabilities of the token module are defined and limited by the token kernel through
//! which all state modifications are performed,
//! see [`TokenKernelOperations`](crate::token_kernel_interface::TokenKernelOperations).
//!
//! It is the responsibility of the token module to charge energy for execution via
//! [`TransactionExecution`](plt_scheduler_interface::TransactionExecution),
//! and release control (return an error) if the energy limit is reached.
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
