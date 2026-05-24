use plt_block_state::entity::protocol_level_tokens::p9::{TokenP9, TokenP9Base};
use plt_block_state::entity::protocol_level_tokens::p11::TokenP11;

// todo ar

/// Access to token from code that is polymorphic over the protocol version.
pub enum TokenPX {
    TokenP9(TokenP9),
    TokenP11(TokenP11),
}

impl TokenPX {
    /// Access the token as a base P9 token.
    pub fn token_base(&self) -> &TokenP9Base {
        match self {
            Self::TokenP9(token) => &token.token_base,
            Self::TokenP11(token) => &token.token_base,
        }
    }

    /// Mutate the token as a base P9 token.
    pub fn token_base_mut(&mut self) -> &mut TokenP9Base {
        match self {
            Self::TokenP9(token) => &mut token.token_base,
            Self::TokenP11(token) => &mut token.token_base,
        }
    }
}

/// Access to token from code that is polymorphic over the protocol version.
#[derive(Debug)]
pub enum TokenPXRefMut<'a> {
    TokenP9(&'a mut TokenP9),
    TokenP11(&'a mut TokenP11),
}

impl<'a> TokenPXRefMut<'a> {
    /// Access the token as a base P9 token.
    pub fn token_base(&self) -> &TokenP9Base {
        match self {
            Self::TokenP9(token) => &token.token_base,
            Self::TokenP11(token) => &token.token_base,
        }
    }

    /// Mutate the token as a base P9 token.
    pub fn token_base_mut(&mut self) -> &mut TokenP9Base {
        match self {
            Self::TokenP9(token) => &mut token.token_base,
            Self::TokenP11(token) => &mut token.token_base,
        }
    }

    pub fn as_ref(&self) -> TokenPXRef<'_> {
        match self {
            Self::TokenP9(token) => TokenPXRef::TokenP9(token),
            Self::TokenP11(token) => TokenPXRef::TokenP11(token),
        }
    }

    pub fn as_mut(& mut self) -> TokenPXRefMut<'_> {
        // todo ar
        todo!()
        // match self {
        //     Self::TokenP9(token) => Self::TokenP9(*token),
        //     Self::TokenP11(token) => Self::TokenP11(*token),
        // }
    }
}

/// Access to token from code that is polymorphic over the protocol version.
#[derive(Debug, Clone, Copy)]
pub enum TokenPXRef<'a> {
    TokenP9(&'a TokenP9),
    TokenP11(&'a TokenP11),
}

impl TokenPXRef<'_> {
    /// Access the token as a base P9 token.
    pub fn token_base(&self) -> &TokenP9Base {
        match self {
            Self::TokenP9(token) => &token.token_base,
            Self::TokenP11(token) => &token.token_base,
        }
    }
}
