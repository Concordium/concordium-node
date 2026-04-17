//! Implementation of the protocol-level token module. The token module implements
//! execution of [token operations](concordium_base::protocol_level_tokens::TokenOperation).
//!
//! It is the responsibility of the token module to charge energy for execution via
//! [`TransactionExecution`](crate::transaction_execution_interface::TransactionExecution),
//! and release control (return an error) if the energy limit is reached.
mod initialize;
mod queries;
mod update;

pub use initialize::*;
pub use queries::*;
pub use update::*;

#[derive(Debug, thiserror::Error)]
#[error("Token amount decimals mismatch: expected {expected}, found {found}")]
pub struct TokenAmountDecimalsMismatchError {
    pub expected: u8,
    pub found: u8,
}
