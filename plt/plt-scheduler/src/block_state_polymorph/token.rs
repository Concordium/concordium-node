use plt_block_state::entity::protocol_level_tokens::p9::{TokenP9, TokenP9Base};
use plt_block_state::entity::protocol_level_tokens::p11::TokenP11;

/// Access to token from code that is polymorphic over the protocol version.
pub enum TokenPX {
    TokenP9(TokenP9),
    TokenP11(TokenP11),
}

impl TokenPX {
    /// Access the token as a base P9 token.
    pub fn token_base(&self) -> &TokenP9Base {
        match self {
            TokenPX::TokenP9(token) => &token.token_base,
            TokenPX::TokenP11(token) => &token.token_base,
        }
    }

    /// Mutate the token as a base P9 token.
    pub fn token_base_mut(&mut self) -> &mut TokenP9Base {
        match self {
            TokenPX::TokenP9(token) => &mut token.token_base,
            TokenPX::TokenP11(token) => &mut token.token_base,
        }
    }
}
