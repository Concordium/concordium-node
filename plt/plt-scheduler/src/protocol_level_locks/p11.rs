/// Get the [`LockId`]s of all protocol-level locks registered on the chain at the
/// end of the block.
///
/// NOTE: this is a naive implementation. We might need to optimize with a streaming solution
/// instead, to not load all locks in existance into memory all at once.
pub fn query_lock_list(block_state: &impl BlockStateQuery) -> Vec<LockId> {
    block_state.lock_list().collect()
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
