use concordium_base::{
    base::AccountIndex, protocol_level_locks::LockId, protocol_level_tokens::RawCbor,
};
use plt_block_state::block_state_interface::{BlockStateOperations, BlockStateQuery};
use plt_scheduler_types::types::locks::LockControllerSimpleV0;

use crate::lock_controller::{LockController, LockControllerRejectReason, LockOperation};

impl LockController for &LockControllerSimpleV0 {
    // TODO: implemented as part of COR-2302
    fn initialize_state<BSO: BlockStateOperations>(
        &self,
        _bso: &mut BSO,
        _lock_id: &LockId,
    ) -> BSO::Lock {
        todo!()
    }

    // TODO: implemented as part of COR-2305
    fn approve_operation<BSQ: BlockStateQuery>(
        &self,
        _bsq: &BSQ,
        _sender: AccountIndex,
        _operation: &LockOperation,
    ) -> Result<(), LockControllerRejectReason> {
        todo!()
    }

    // TODO: implemented as part of COR-2309
    fn query_info<BSQ: BlockStateQuery>(&self, _bsq: &BSQ, _lock: &BSQ::Lock) -> RawCbor {
        todo!()
    }
}
