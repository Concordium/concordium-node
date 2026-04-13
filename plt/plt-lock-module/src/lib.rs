//! Runtime interface for protocol-level lock controllers.

use concordium_base::{base::AccountIndex, protocol_level_tokens::RawCbor};
use plt_block_state::block_state::types::LockState;

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

/// Runtime interface implemented by protocol-level lock controllers.
pub trait LockController {
    /// Initialize controller-specific state for a newly created lock.
    fn init_state(&self) -> LockState;

    /// Approve or reject a lock operation.
    fn approve_operation(
        &self,
        sender: AccountIndex,
        operation: &LockOperation,
    ) -> Result<(), LockControllerRejectReason>;

    /// Query controller configuration and state as CBOR.
    fn query_info(&self) -> RawCbor;
}
