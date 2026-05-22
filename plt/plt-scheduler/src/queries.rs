//! Implementation of queries related to protocol-level tokens.



use crate::locks;
use crate::token_context::TokenQueryContext;
use crate::token_module;
use concordium_base::common::cbor::cbor_encode;
use concordium_base::protocol_level_locks::LockId;
use concordium_base::protocol_level_tokens::{RawCbor, TokenId};
use plt_block_state::block_state_interface::{
    AccountNotFoundByIndexError, BlockStateQuery, BlockStateResult, LockNotFoundByIdError,
    TokenNotFoundByIdError,
};
use plt_block_state::entity::block_state::p9::BlockStateP9;
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::protocol_level_tokens::p9::TokenP9;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_scheduler_types::types::queries::{
    TokenAccountInfo, TokenAccountState, TokenAuthorizations, TokenInfo, TokenState,
};
use plt_scheduler_types::types::tokens::TokenAmount;








/// Get the [`LockId`]s of all protocol-level locks registered on the chain at the
/// end of the block.
///
/// NOTE: this is a naive implementation. We might need to optimize with a streaming solution
/// instead, to not load all locks in existance into memory all at once.
pub fn query_lock_list(block_state: &impl BlockStateQuery) -> Vec<LockId> {
    block_state.lock_list().collect()
}

/// Reasons why a lock query (e.g. [`query_lock_info`]) may fail.
#[derive(Debug, thiserror::Error)]
pub enum QueryLockError {
    /// The requested lock does not exist in the block state.
    #[error("Lock does not exist")]
    LockDoesNotExist,
    /// On-chain block state violates an invariant assumed by the
    /// query assembly path. These are unrecoverable for clients and surface as `Panic`
    /// over FFI.
    #[error("State invariant violation while assembling lock info: {0}")]
    StateInvariantViolation(String),
}

impl From<token_module::QueryTokenModuleError> for QueryLockError {
    fn from(err: token_module::QueryTokenModuleError) -> Self {
        QueryLockError::StateInvariantViolation(err.to_string())
    }
}

impl From<LockNotFoundByIdError> for QueryLockError {
    fn from(_: LockNotFoundByIdError) -> Self {
        QueryLockError::LockDoesNotExist
    }
}

impl From<AccountNotFoundByIndexError> for QueryLockError {
    fn from(err: AccountNotFoundByIndexError) -> Self {
        QueryLockError::StateInvariantViolation(err.to_string())
    }
}

/// Assemble the [`LockInfo`] CBOR payload for a lock.
///
/// Thin orchestrator: resolves `lock_id` to a [`LockConfiguration`] via the block state
/// and delegates payload assembly to
/// [`crate::locks::lock_configuration::LockInfoQuery::query_info`].
///
/// [`LockInfo`]: concordium_base::protocol_level_locks::LockInfo
/// [`LockConfiguration`]: plt_block_state::block_state::types::protocol_level_locks::LockConfiguration
pub fn query_lock_info<BSQ: BlockStateQuery>(
    block_state: &BSQ,
    lock_id: &LockId,
) -> Result<RawCbor, QueryLockError> {
    let lock = block_state.lock_by_id(lock_id)?;
    let configuration = block_state.lock_configuration(&lock);
    let lock_info = locks::get_lock_info(block_state, lock_id, &configuration)?;
    Ok(RawCbor::from(cbor_encode(&lock_info)))
}
