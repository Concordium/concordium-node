use plt_block_state::entity::protocol_level_tokens::p9::TokenP9;
use plt_block_state::entity::protocol_level_tokens::p11::TokenP11;

// todo ar just use enum instead?

/// Access to token from code that is generic over the protocol version.
pub trait TokenT {
    /// Access the token as a P9 token.
    fn token_p9(&self) -> &TokenP9;

    /// Mutate the token as a P9 token.
    fn token_p9_mut(&mut self) -> &mut TokenP9;

    /// Access the token as a P11 token. Returns `None`
    /// if we are not on P11.
    fn token_p11(&self) -> Option<&TokenP11>;

    /// Mutate the token as a P11 token. Returns `None`
    /// if we are not on P11.
    fn token_p11_mut(&mut self) -> Option<&mut TokenP11>;
}

impl TokenT for TokenP9 {
    fn token_p9(&self) -> &TokenP9 {
        self
    }

    fn token_p9_mut(&mut self) -> &mut TokenP9 {
        self
    }

    fn token_p11(&self) -> Option<&TokenP11> {
        None
    }

    fn token_p11_mut(&mut self) -> Option<&mut TokenP11> {
        None
    }
}

impl TokenT for TokenP11 {
    fn token_p9(&self) -> &TokenP9 {
        &self.token_base
    }

    fn token_p9_mut(&mut self) -> &mut TokenP9 {
        &mut self.token_base
    }

    fn token_p11(&self) -> Option<&TokenP11> {
        Some(self)
    }

    fn token_p11_mut(&mut self) -> Option<&mut TokenP11> {
        Some(self)
    }
}
