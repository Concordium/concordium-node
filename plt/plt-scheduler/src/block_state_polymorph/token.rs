use plt_block_state::entity::protocol_level_tokens::p9::{TokenP9, TokenP9Base};
use plt_block_state::entity::protocol_level_tokens::p11::TokenP11;

/// Token on any protocol version.
pub enum TokenPX {
    TokenP9(TokenP9),
    TokenP11(TokenP11),
}

impl TokenPX {
    /// Access the token as a base P9 token.
    pub fn token_base(&self) -> &TokenP9Base {
        match self {
            Self::TokenP9(token) => &token.token_p9_base,
            Self::TokenP11(token) => &token.token_p9_base,
        }
    }

    /// Mutate the token as a base P9 token.
    pub fn token_base_mut(&mut self) -> &mut TokenP9Base {
        match self {
            Self::TokenP9(token) => &mut token.token_p9_base,
            Self::TokenP11(token) => &mut token.token_p9_base,
        }
    }
}

/// Access to token on any protocol version via a unique/mutable reference.
#[derive(Debug)]
pub enum TokenPXRefMut<'a> {
    TokenP9(&'a mut TokenP9),
    TokenP11(&'a mut TokenP11),
}

impl<'a> TokenPXRefMut<'a> {
    /// Access the token as a base P9 token.
    pub fn token_p9_base(&self) -> &TokenP9Base {
        match self {
            Self::TokenP9(token) => &token.token_p9_base,
            Self::TokenP11(token) => &token.token_p9_base,
        }
    }

    /// Mutate the token as a base P9 token.
    pub fn token_p9_base_mut(&mut self) -> &mut TokenP9Base {
        match self {
            Self::TokenP9(token) => &mut token.token_p9_base,
            Self::TokenP11(token) => &mut token.token_p9_base,
        }
    }

    /// Get non-mutable reference to the token.
    pub fn as_ref(&self) -> TokenPXRef<'_> {
        match self {
            Self::TokenP9(token) => TokenPXRef::TokenP9(token),
            Self::TokenP11(token) => TokenPXRef::TokenP11(token),
        }
    }

    /// Reborrow the mutable reference to the token.
    pub fn as_mut(&mut self) -> TokenPXRefMut<'_> {
        match self {
            Self::TokenP9(token) => TokenPXRefMut::TokenP9(token),
            Self::TokenP11(token) => TokenPXRefMut::TokenP11(token),
        }
    }
}

/// Access to a token on any protocol version via a shared reference.
#[derive(Debug, Clone, Copy)]
pub enum TokenPXRef<'a> {
    TokenP9(&'a TokenP9),
    TokenP11(&'a TokenP11),
}

impl<'a> TokenPXRef<'a> {
    /// Access the token as a base P9 token.
    pub fn token_base(&self) -> &'a TokenP9Base {
        match self {
            Self::TokenP9(token) => &token.token_p9_base,
            Self::TokenP11(token) => &token.token_p9_base,
        }
    }
}
