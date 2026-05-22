use concordium_base::common::cbor;
use concordium_base::protocol_level_locks::LockId;
use concordium_base::protocol_level_tokens::RawCbor;
use plt_block_state::block_state_interface::{BlockStateResult, LockNotFoundByIdError};
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::utils;
use crate::locks;

/// Get the [`LockId`]s of all protocol-level locks registered on the chain at the
/// end of the block.
///
/// NOTE: this is a naive implementation. We might need to optimize with a streaming solution
/// instead, to not load all locks in existance into memory all at once.
pub fn query_lock_list<C: EntityContextTypes>(
    context: &EntityContext<C>,
    block_state: &BlockStateP11,
) -> BlockStateResult<Vec<LockId>> {
    block_state.lock_list(context)
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
) -> BlockStateResult<Result<RawCbor, LockNotFoundByIdError>> {
    let lock_res = block_state.lock_by_id(context, lock_id)?;
    let lock = match lock_res {
        Ok(lock) => lock,
        Err(err) => return Ok(Err(err)),
    };

    let configuration = lock.lock_configuration(context);
    let lock_info = locks::get_lock_info(block_state, lock_id, &configuration)?;
    Ok(Ok(RawCbor::from(cbor::cbor_encode(&lock_info))))
}
