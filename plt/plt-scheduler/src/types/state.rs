//! Types representing token state.

use concordium_base::protocol_level_tokens::{RawCbor, TokenAmount, TokenModuleRef};

/// Token state at the block level
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TokenState {
    /// The reference of the module implementing this token.
    pub token_module_ref: TokenModuleRef,
    /// Number of decimals in the decimal number representation of amounts.
    pub decimals: u8,
    /// The total available token supply.
    pub total_supply: TokenAmount,
    /// Token module specific state, such as token name, feature flags, meta
    /// data.
    pub module_state: RawCbor,
}

/// State of a protocol level token associated with some account.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TokenAccountState {
    /// The token balance of the account.
    pub balance: TokenAmount,
    /// The token-module defined state of the account.
    pub module_state: Option<RawCbor>,
}
