//! Types returned by queries.

use crate::types::state::{TokenAccountState, TokenState};
use concordium_base::protocol_level_tokens::TokenId;

/// The token state at the block level.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TokenInfo {
    /// The canonical identifier/symbol for the protocol level token.
    pub token_id: TokenId,
    /// The associated block level state.
    pub state: TokenState,
}

/// State of a protocol level token associated with some account.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TokenAccountInfo {
    /// The canonical identifier/symbol for the protocol level token.
    pub token_id: TokenId,
    /// The state of the token associated with the account.
    pub account_state: TokenAccountState,
}
