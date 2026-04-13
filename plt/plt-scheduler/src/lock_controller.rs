use concordium_base::{base::AccountIndex, protocol_level_tokens::RawCbor};
use plt_block_state::{
    block_state::types::LockConfiguration, block_state_interface::BlockStateQuery,
};
use plt_lock_module::{LockController, LockControllerRejectReason, LockOperation};
use plt_scheduler_types::types::locks::LockControllerConfig;

impl<BSQ: BlockStateQuery> LockController for (BSQ, BSQ::Lock) {
    // TODO: implemented as part of COR-2305
    fn approve_operation(
        &self,
        sender: AccountIndex,
        operation: &LockOperation,
    ) -> Result<(), LockControllerRejectReason> {
        todo!()
    }

    // TODO: implemented as part of COR-2309
    fn query_info(&self) -> RawCbor {
        todo!()
    }
}
