//! Runtime interface for protocol-level lock controllers.

use concordium_base::{
    protocol_level_locks::LockId,
    protocol_level_tokens::{CborHolderAccount, CborMemo, TokenAmount, TokenId},
};
use plt_block_state::block_state_interface::BlockStateQuery;
use plt_scheduler_types::types::locks::LockControllerConfig;

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

/// Placeholded type for the lock info returned when querying a lock for data.
/// FIXME: replace with the corresponding type in concordium-base as part of COR-2309
#[derive(Debug, Clone)]
pub struct LockInfo;

/// Runtime interface implemented by protocol-level locks.
#[allow(dead_code)] // FIXME: remove when this is used.
pub trait LockController {
    /// Approve or reject a lock operation.
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
    ) -> Result<(), LockControllerRejectReason>;

    /// Query controller configuration and state as CBOR.
    fn query_info<BSQ: BlockStateQuery>(&self, bsq: &BSQ, lock: &BSQ::Lock) -> LockInfo;
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

    fn query_info<BSQ: BlockStateQuery>(&self, bsq: &BSQ, lock: &BSQ::Lock) -> LockInfo {
        match self {
            LockControllerConfig::SimpleV0(lock_controller_simple_v0) => {
                lock_controller_simple_v0.query_info(bsq, lock)
            }
        }
    }
}
