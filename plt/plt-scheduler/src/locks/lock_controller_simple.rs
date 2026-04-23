use concordium_base::protocol_level_locks::LockId;
use plt_block_state::block_state_interface::BlockStateQuery;
use plt_scheduler_types::types::locks::LockControllerSimpleV0;

use crate::locks::lock_controller::{
    LockController, LockControllerRejectReason, LockInfo, LockOperation,
};

impl LockController for LockControllerSimpleV0 {
    // TODO: implemented as part of COR-2305
    fn validate_operation<BSQ: BlockStateQuery>(
        &self,
        _bsq: &BSQ,
        _sender: &BSQ::Account,
        _operation: &LockOperation,
    ) -> Result<(), LockControllerRejectReason> {
        todo!()
    }

    // TODO: implemented as part of COR-2309
    fn query_info<BSQ: BlockStateQuery>(&self, _bsq: &BSQ, _lock: &LockId) -> LockInfo {
        todo!()
    }
}
