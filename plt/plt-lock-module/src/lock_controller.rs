//! Runtime interface for protocol-level lock controllers.

use concordium_base::protocol_level_tokens::RawCbor;
use plt_block_state::block_state_interface::BlockStateQuery;
use plt_scheduler_types::types::locks::LockControllerConfig;

/// Runtime lock operation model.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum LockOperation {}

/// Lock-controller specific rejection.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LockControllerRejectReason;

/// Runtime interface implemented by protocol-level locks.
pub trait LockController {
    /// Approve or reject a lock operation.
    ///
    /// * `bsq`: the block state to query on
    /// * `sender`: the transaction sender reference
    /// * `lock`: the lock reference. This is used instead of the lock ID from the operation.
    /// * `operation`: the lock operation to approve/reject.
    fn approve_operation<BSQ: BlockStateQuery>(
        &self,
        bsq: &BSQ,
        sender: &BSQ::Account,
        lock: &BSQ::Lock,
        operation: &LockOperation,
    ) -> Result<(), LockControllerRejectReason>;

    /// Query controller configuration and state as CBOR.
    fn query_info<BSQ: BlockStateQuery>(&self, bsq: &BSQ, lock: &BSQ::Lock) -> RawCbor;
}

impl LockController for LockControllerConfig {
    fn approve_operation<BSQ: BlockStateQuery>(
        &self,
        bsq: &BSQ,
        sender: &BSQ::Account,
        lock: &BSQ::Lock,
        operation: &LockOperation,
    ) -> Result<(), LockControllerRejectReason> {
        match self {
            LockControllerConfig::SimpleV0(lock_controller_simple_v0) => {
                lock_controller_simple_v0.approve_operation(bsq, sender, lock, operation)
            }
        }
    }

    fn query_info<BSQ: BlockStateQuery>(&self, bsq: &BSQ, lock: &BSQ::Lock) -> RawCbor {
        match self {
            LockControllerConfig::SimpleV0(lock_controller_simple_v0) => {
                lock_controller_simple_v0.query_info(bsq, lock)
            }
        }
    }
}
