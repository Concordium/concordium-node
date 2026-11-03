//! Persistent model for protocol-level tokens in the block state.

use concordium_base::protocol_level_tokens::TokenId;

pub mod p9;

/// Internally used, normalized token is. Used to identify token ids
/// as equal, even if casing differs.
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct NormalizedTokenId(String);

/// Normalize the given token id. Two tokens are the same, if their
/// normalized token ids are equal. Normalizing the token id
/// removes differences due to casing.
pub fn normalize_token_id(token_id: &TokenId) -> NormalizedTokenId {
    NormalizedTokenId(token_id.as_ref().to_ascii_lowercase())
}
