//! Types returned by queries.

use crate::types::tokens::TokenAmount;
use concordium_base::protocol_level_tokens::{RawCbor, TokenId, TokenModuleRef};

/// Token state at the block level
///
/// Corresponding Haskell type: `Concordium.Types.Queries.Tokens.TokenState`
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

/// The token state at the block level.
///
/// Corresponding Haskell type: `Concordium.Types.Queries.Tokens.TokenInfo`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TokenInfo {
    /// The canonical identifier/symbol for the protocol level token.
    pub token_id: TokenId,
    /// The associated block level state.
    pub state: TokenState,
}

/// State of a protocol level token associated with some account.
///
/// Corresponding Haskell type: `Concordium.Types.Queries.Tokens.TokenAccountState`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TokenAccountState {
    /// The token balance of the account.
    pub balance: TokenAmount,
    /// The token-module defined state of the account.
    pub module_state: RawCbor,
}

/// State of a protocol level token associated with some account.
///
/// Corresponding Haskell type: `Concordium.Types.Queries.Tokens.Token`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TokenAccountInfo {
    /// The canonical identifier/symbol for the protocol level token.
    pub token_id: TokenId,
    /// The state of the token associated with the account.
    pub account_state: TokenAccountState,
}
