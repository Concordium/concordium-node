//! Implementation of queries related to protocol-level tokens.

use crate::locks;

use concordium_base::common::cbor::cbor_encode;
use concordium_base::protocol_level_locks::LockId;
use concordium_base::protocol_level_tokens::RawCbor;
use plt_block_state::entity::block_state::LockNotFoundByIdError;
use plt_block_state::entity::block_state::p9::BlockStateP9;
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::failure::{
    BlockStateResult, HigherLevelProtocolError, WithBlockStateFailure, WithBlockStateResult,
};

/// Get the [`LockId`]s of all protocol-level locks registered on the chain at the
/// end of the block.
pub fn query_lock_list_p9<C: EntityContextTypes>(
    _context: &EntityContext<C>,
    _block_state: &BlockStateP9,
) -> BlockStateResult<Vec<LockId>> {
    Ok(vec![])
}

/// Get the [`LockId`]s of all protocol-level locks registered on the chain at the
/// end of the block.
///
/// NOTE: this is a naive implementation. We might need to optimize with a streaming solution
/// instead, to not load all locks in existence into memory all at once.
pub fn query_lock_list<C: EntityContextTypes>(
    context: &EntityContext<C>,
    block_state: &BlockStateP11,
) -> BlockStateResult<Vec<LockId>> {
    block_state.lock_list(context)
}

/// Reasons why a lock query (e.g. [`query_lock_info`]) may fail.
#[derive(Debug, thiserror::Error)]
pub enum QueryLockError {
    /// The requested lock does not exist in the block state.
    #[error("Lock does not exist")]
    LockDoesNotExist,
}

impl HigherLevelProtocolError for QueryLockError {}

impl From<LockNotFoundByIdError> for QueryLockError {
    fn from(_: LockNotFoundByIdError) -> Self {
        QueryLockError::LockDoesNotExist
    }
}

/// Assemble the [`LockInfo`] CBOR payload for a lock.
pub fn query_lock_info_p9<C: EntityContextTypes>(
    _context: &EntityContext<C>,
    _block_state: &BlockStateP9,
    _lock_id: &LockId,
) -> WithBlockStateResult<RawCbor, QueryLockError> {
    Err(WithBlockStateFailure::Error(
        QueryLockError::LockDoesNotExist,
    ))
}

/// Assemble the [`LockInfo`] CBOR payload for a lock.
///
/// Thin orchestrator: resolves `lock_id` to a [`LockConfiguration`] via the block state
/// and delegates payload assembly to
/// [`crate::locks::lock_configuration::LockInfoQuery::query_info`].
///
/// [`LockInfo`]: concordium_base::protocol_level_locks::LockInfo
/// [`LockConfiguration`]: plt_block_state::block_state::types::protocol_level_locks::LockConfiguration
pub fn query_lock_info<C: EntityContextTypes>(
    context: &EntityContext<C>,
    block_state: &BlockStateP11,
    lock_id: &LockId,
) -> WithBlockStateResult<RawCbor, QueryLockError> {
    let lock = block_state
        .lock_by_id(context, lock_id)?
        .map_err(|_err: LockNotFoundByIdError| QueryLockError::LockDoesNotExist)?;
    let configuration = lock.lock_configuration(context)?;
    let lock_info = locks::get_lock_info(context, block_state, &lock, &configuration)?;
    Ok(RawCbor::from(cbor_encode(&lock_info)))
}
