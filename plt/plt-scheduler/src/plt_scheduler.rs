use crate::block_state_interface::{BlockStateOperations, SchedulerOperations};

pub struct TransactionRejectReason;

pub type Events = ();

/// Execute a transaction payload modifying `scheduler` and `block_state` accordingly.
/// Returns the events produce if successful otherwise a reject reason.
///
/// The caller must ensure to rollback state changes in case of the transaction being rejected.
pub fn execute_transaction(
    _scheduler: &mut impl SchedulerOperations,
    _block_state: &mut impl BlockStateOperations,
    _payload: &[u8],
) -> Result<Events, TransactionRejectReason> {
    todo!()
}
