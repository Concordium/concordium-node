//! Implementation of the protocol-level token module. The token module implements
//! execution of [token operations](concordium_base::protocol_level_tokens::TokenOperation).
//!
//! It is the responsibility of the token module to charge energy for execution via
//! [`TransactionExecution`](crate::transaction_execution::TransactionExecution),
//! and release control (return an error) if the energy limit is reached.

// todo ar move into protocol_level_tokens

use concordium_base::protocol_level_tokens::TokenModuleRef;

mod balance_operations;
pub mod errors;
mod module;
mod util;

pub use module::initialize::*;
pub use module::queries::*;
pub use module::update::*;

/// Module ref for the currently implemented token module. It is the SHA-256 of "TokenModuleV0"
pub const TOKEN_MODULE_REF: TokenModuleRef = TokenModuleRef::new([
    0x5c, 0x5c, 0x26, 0x45, 0xdb, 0x84, 0xa7, 0x02, 0x6d, 0x78, 0xf2, 0x50, 0x17, 0x40, 0xf6, 0x0a,
    0x8c, 0xcb, 0x8f, 0xae, 0x5c, 0x16, 0x6d, 0xc2, 0x42, 0x80, 0x77, 0xfd, 0x9a, 0x69, 0x9a, 0x4a,
]);
