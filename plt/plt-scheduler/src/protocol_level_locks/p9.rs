use crate::failure::{ResultWithBlockStateFailure, WithBlockStateFailure};
use concordium_base::protocol_level_locks::LockId;
use concordium_base::protocol_level_tokens::RawCbor;
use plt_block_state::entity::block_state::LockNotFoundByIdError;
use plt_block_state::entity::block_state::p9::BlockStateP9;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::failure::BlockStateResult;

/// Get the [`LockId`]s of all protocol-level locks registered on the chain at the
/// end of the block.
pub fn query_lock_list<C: EntityContextTypes>(
    _context: &EntityContext<C>,
    _block_state: &BlockStateP9,
) -> BlockStateResult<Vec<LockId>> {
    Ok(vec![])
}

/// Assemble the [`LockInfo`] CBOR payload for a lock.
pub fn query_lock_info<C: EntityContextTypes>(
    _context: &EntityContext<C>,
    _block_state: &BlockStateP9,
    lock_id: &LockId,
) -> ResultWithBlockStateFailure<RawCbor, LockNotFoundByIdError> {
    Err(WithBlockStateFailure::Error(LockNotFoundByIdError(
        lock_id.clone(),
    )))
}
