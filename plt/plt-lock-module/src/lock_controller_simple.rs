use concordium_base::protocol_level_tokens::RawCbor;
use plt_block_state::block_state_interface::BlockStateQuery;
use plt_scheduler_types::types::locks::LockControllerSimpleV0;

use crate::lock_controller::{LockController, LockControllerRejectReason, LockOperation};

impl LockController for &LockControllerSimpleV0 {
    // TODO: implemented as part of COR-2305
    fn approve_operation<BSQ: BlockStateQuery>(
        &self,
        _bsq: &BSQ,
        _sender: &BSQ::Account,
        _lock: &BSQ::Lock,
        _operation: &LockOperation,
    ) -> Result<(), LockControllerRejectReason> {
        todo!()
    }

    // TODO: implemented as part of COR-2309
    fn query_info<BSQ: BlockStateQuery>(&self, _bsq: &BSQ, _lock: &BSQ::Lock) -> RawCbor {
        todo!()
    }
}
