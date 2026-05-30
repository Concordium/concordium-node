use concordium_base::protocol_level_locks::LockId;
use concordium_base::protocol_level_tokens::TokenId;

pub mod p10;
pub mod p11;
pub mod p9;

/// Account with given id does not exist
#[derive(Debug, thiserror::Error)]
#[error("Token with id {0} does not exist")]
pub struct TokenNotFoundByIdError(pub TokenId);

/// Lock with given id does not exist
#[derive(Debug)]
pub struct LockNotFoundByIdError(pub LockId);
