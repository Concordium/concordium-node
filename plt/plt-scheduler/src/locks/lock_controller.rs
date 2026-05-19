//! Runtime interface for protocol-level lock controllers.

use concordium_base::protocol_level_tokens::meta_operations::{
    MetaLockCancelDetails, MetaLockFundDetails, MetaLockReturnDetails, MetaLockSendDetails,
};
use plt_block_state::block_state_interface::{AccountNotFoundByIndexError, BlockStateQuery};
use plt_block_state::persistent::protocol_level_locks::p11::{
    LockControllerConfig, LockControllerSimpleV0,
};
use plt_scheduler_types::types::reject_reasons::TransactionRejectReason;

/// Runtime lock operation model. This corresponds to the "fund", "send", "return", and "cancel"
/// CBOR operations for interacting with locks from concordium-base.
#[derive(Debug, Clone, Eq, PartialEq)]
#[allow(dead_code)] // FIXME: remove this when all operations are implemented.
pub enum LockOperation {
    Fund(MetaLockFundDetails),
    Send(MetaLockSendDetails),
    Return(MetaLockReturnDetails),
    Cancel(MetaLockCancelDetails),
}

/// Runtime interface implemented by protocol-level locks.
pub trait LockController {
    /// Approve or reject a lock operation. Returns `true` if the operation is authorized.
    ///
    /// * `bsq`: the block state to query on
    /// * `sender`: the transaction sender reference
    /// * `lock`: the lock reference. This is used instead of the lock ID from the operation.
    /// * `operation`: the lock operation to approve/reject.
    fn validate_operation<BSQ: BlockStateQuery>(
        &self,
        bsq: &BSQ,
        sender: &BSQ::Account,
        operation: &LockOperation,
    ) -> bool;

    /// Convert this controller configuration to its canonical CBOR
    /// [`concordium_base::protocol_level_locks::LockController`] representation, used by the
    /// `lock-info` payload returned from `query_lock_info`.
    ///
    /// Resolves any block-state `AccountIndex` references (e.g. grant accounts) to their
    /// canonical [`CborHolderAccount`] form via `bsq`. Surfaces an
    /// [`AccountNotFoundByIndexError`] if a recorded `AccountIndex` cannot be
    /// looked up — that signals corrupted block state, since lock configurations are only
    /// allowed to reference accounts that exist at creation time.
    fn to_cbor_controller<BSQ: BlockStateQuery>(
        &self,
        bsq: &BSQ,
    ) -> Result<concordium_base::protocol_level_locks::LockController, AccountNotFoundByIndexError>;

    /// Controller configuration type used for constructing this controller.
    /// This is expected to be decoded CBOR derived from the `lockCreate`
    /// operation payload.
    type ControllerConfig;

    /// Construct this lock controller from the given configuration.
    fn new<BSQ: BlockStateQuery>(
        bsq: &BSQ,
        config: Self::ControllerConfig,
    ) -> Result<Self, TransactionRejectReason>
    where
        Self: Sized;
}

impl LockController for LockControllerConfig {
    fn validate_operation<BSQ: BlockStateQuery>(
        &self,
        bsq: &BSQ,
        sender: &BSQ::Account,
        operation: &LockOperation,
    ) -> bool {
        match self {
            LockControllerConfig::SimpleV0(lock_controller_simple_v0) => {
                lock_controller_simple_v0.validate_operation(bsq, sender, operation)
            }
        }
    }

    fn to_cbor_controller<BSQ: BlockStateQuery>(
        &self,
        bsq: &BSQ,
    ) -> Result<concordium_base::protocol_level_locks::LockController, AccountNotFoundByIndexError>
    {
        match self {
            LockControllerConfig::SimpleV0(lock_controller_simple_v0) => {
                lock_controller_simple_v0.to_cbor_controller(bsq)
            }
        }
    }

    type ControllerConfig = concordium_base::protocol_level_locks::LockController;

    fn new<BSQ: BlockStateQuery>(
        bsq: &BSQ,
        config: Self::ControllerConfig,
    ) -> Result<Self, TransactionRejectReason>
    where
        Self: Sized,
    {
        use concordium_base::protocol_level_locks::LockController::*;
        match config {
            SimpleV0(lock_controller_simple_v0) => Ok(LockControllerConfig::SimpleV0(
                LockControllerSimpleV0::new(bsq, lock_controller_simple_v0)?,
            )),
        }
    }
}
