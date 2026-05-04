//! Runtime interface for protocol-level lock controllers.

use concordium_base::{
    protocol_level_locks::LockId,
    protocol_level_tokens::{CborHolderAccount, CborMemo, TokenAmount, TokenId},
};
use plt_block_state::block_state_interface::{AccountNotFoundByIndexError, BlockStateQuery};
use plt_scheduler_types::types::{
    locks::{LockControllerConfig, LockControllerSimpleV0},
    reject_reasons::TransactionRejectReason,
};

/// Runtime lock operation model. This corresponds to the "fund", "send", "return", and "cancel"
/// CBOR operations for interacting with locks from concordium-base.
#[derive(Debug, Clone, Eq, PartialEq)]
#[allow(dead_code)] // FIXME: remove when this is used.
pub enum LockOperation {
    Fund {
        lock: LockId,
        token: TokenId,
        amount: TokenAmount,
        memo: Option<CborMemo>,
    },
    Send {
        lock: LockId,
        token: TokenId,
        source: CborHolderAccount,
        amount: TokenAmount,
        recipient: CborHolderAccount,
        memo: Option<CborMemo>,
    },
    Return {
        lock: LockId,
        token: TokenId,
        source: CborHolderAccount,
        amount: TokenAmount,
        memo: Option<CborMemo>,
    },
    Cancel {
        lock: LockId,
        memo: Option<CborMemo>,
    },
}

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
    #[allow(dead_code)] // FIXME: remove when this is used.
    fn validate_operation<BSQ: BlockStateQuery>(
        &self,
        bsq: &BSQ,
        sender: &BSQ::Account,
        operation: &LockOperation,
    ) -> Result<(), LockControllerRejectReason>;

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
    ) -> Result<(), LockControllerRejectReason> {
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
