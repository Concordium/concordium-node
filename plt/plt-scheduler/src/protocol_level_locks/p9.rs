use concordium_base::protocol_level_locks::LockId;
use plt_block_state::entity::EntityContextTypes;

/// Get the [`LockId`]s of all protocol-level locks registered on the chain at the
/// end of the block.
pub fn query_lock_list<C: EntityContextTypes>() -> Vec<LockId> {
    Vec::new()
}
