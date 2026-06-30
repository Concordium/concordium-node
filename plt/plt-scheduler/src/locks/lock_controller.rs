//! Runtime interface for protocol-level lock controllers.

use crate::failure::WithBlockStateResult;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::meta_operations::{
    MetaLockCancelDetails, MetaLockFundDetails, MetaLockReturnDetails, MetaLockSendDetails,
};
use plt_block_state::entity::accounts::Account;
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::failure::BlockStateResult;
use plt_block_state::persistent::protocol_level_locks::p11::{
    LockControllerConfig, LockControllerSimpleV0,
};
use plt_scheduler_types::types::reject_reasons::TransactionRejectReason;

/// Runtime lock operation model. This corresponds to the "fund", "send", "return", and "cancel"
/// CBOR operations for interacting with locks from concordium-base.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum LockOperation {
    Fund(MetaLockFundDetails),
    Send(MetaLockSendDetails),
    Return(MetaLockReturnDetails),
    Cancel(MetaLockCancelDetails),
}

/// Runtime interface implemented by protocol-level locks.
pub trait LockController {
    /// Approve or reject a lock operation. Returns `Ok(())` if the operation is authorized, or
    /// a `TransactionRejectReason` if it is not.
    ///
    /// * `bsq`: the block state to query on
    /// * `sender`: the transaction sender reference
    /// * `operation`: the lock operation to approve/reject.
    fn validate_operation(
        &self,
        sender_address: AccountAddress,
        sender: &Account,
        operation: &LockOperation,
    ) -> Result<(), TransactionRejectReason>;

    /// Convert this controller configuration to its canonical CBOR
    /// [`concordium_base::protocol_level_locks::LockController`] representation, used by the
    /// `lock-info` payload returned from `query_lock_info`.
    ///
    /// Resolves any block-state `AccountIndex` references (e.g. grant accounts) to their
    /// canonical [`CborHolderAccount`] form via `bsq`. Surfaces an
    /// [`AccountNotFoundByIndexError`] if a recorded `AccountIndex` cannot be
    /// looked up — that signals corrupted block state, since lock configurations are only
    /// allowed to reference accounts that exist at creation time.
    fn to_cbor_controller<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
    ) -> BlockStateResult<concordium_base::protocol_level_locks::LockController>;

    /// Controller configuration type used for constructing this controller.
    /// This is expected to be decoded CBOR derived from the `lockCreate`
    /// operation payload.
    type ControllerConfig;

    /// Construct this lock controller from the given configuration.
    fn new<C: EntityContextTypes>(
        context: &EntityContext<C>,
        block_state: &BlockStateP11,
        config: Self::ControllerConfig,
    ) -> WithBlockStateResult<Self, TransactionRejectReason>
    where
        Self: Sized;
}

impl LockController for LockControllerConfig {
    fn validate_operation(
        &self,
        sender_address: AccountAddress,
        sender: &Account,
        operation: &LockOperation,
    ) -> Result<(), TransactionRejectReason> {
        match self {
            LockControllerConfig::SimpleV0(lock_controller_simple_v0) => {
                lock_controller_simple_v0.validate_operation(sender_address, sender, operation)
            }
        }
    }

    fn to_cbor_controller<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
    ) -> BlockStateResult<concordium_base::protocol_level_locks::LockController> {
        match self {
            LockControllerConfig::SimpleV0(lock_controller_simple_v0) => {
                lock_controller_simple_v0.to_cbor_controller(context)
            }
        }
    }

    type ControllerConfig = concordium_base::protocol_level_locks::LockController;

    fn new<C: EntityContextTypes>(
        context: &EntityContext<C>,
        block_state: &BlockStateP11,
        config: Self::ControllerConfig,
    ) -> WithBlockStateResult<Self, TransactionRejectReason>
    where
        Self: Sized,
    {
        use concordium_base::protocol_level_locks::LockController::*;
        match config {
            SimpleV0(lock_controller_simple_v0) => Ok(LockControllerConfig::SimpleV0(
                LockControllerSimpleV0::new(context, block_state, lock_controller_simple_v0)?,
            )),
        }
    }
}
