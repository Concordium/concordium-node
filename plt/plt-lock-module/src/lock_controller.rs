//! Runtime interface for protocol-level lock controllers.

use concordium_base::{
    base::AccountIndex, protocol_level_locks::LockId, protocol_level_tokens::RawCbor,
};
use plt_block_state::block_state_interface::{BlockStateOperations, BlockStateQuery};
use plt_scheduler_types::types::locks::LockControllerConfig;

/// Temporary runtime lock operation model.
///
/// TODO: Replace this placeholder with the corresponding lock operation type
/// defined in `concordium_base` once it exists.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum LockOperation {}

/// Lock-controller specific rejection.
///
/// TODO: Replace this placeholder with the corresponding lock reject reason type
/// defined in `concordium_base` once it exists.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LockControllerRejectReason;

/// Runtime interface implemented by protocol-level locks.
pub trait LockController {
    fn initialize_state<BSO: BlockStateOperations>(
        &self,
        bso: &mut BSO,
        lock_id: &LockId,
    ) -> BSO::Lock;

    /// Approve or reject a lock operation.
    fn approve_operation<BSQ: BlockStateQuery>(
        &self,
        bsq: &BSQ,
        sender: AccountIndex,
        operation: &LockOperation,
    ) -> Result<(), LockControllerRejectReason>;

    /// Query controller configuration and state as CBOR.
    fn query_info<BSQ: BlockStateQuery>(&self, bsq: &BSQ, lock: &BSQ::Lock) -> RawCbor;
}

impl LockController for &LockControllerConfig {
    fn initialize_state<BSO: BlockStateOperations>(
        &self,
        bso: &mut BSO,
        lock_id: &LockId,
    ) -> BSO::Lock {
        match self {
            LockControllerConfig::SimpleV0(lock_controller_simple_v0) => {
                lock_controller_simple_v0.initialize_state(bso, lock_id)
            }
        }
    }

    fn approve_operation<BSQ: BlockStateQuery>(
        &self,
        bsq: &BSQ,
        sender: AccountIndex,
        operation: &LockOperation,
    ) -> Result<(), LockControllerRejectReason> {
        match self {
            LockControllerConfig::SimpleV0(lock_controller_simple_v0) => {
                lock_controller_simple_v0.approve_operation(bsq, sender, operation)
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
