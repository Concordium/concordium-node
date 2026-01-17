//! Reject reasons for transactions executed by the scheduler.
//!
//! A rejected transaction means that the transaction was included on chain, but
//! failed for some reason. The only effect of a rejected transaction is
//! the charge of energy.

use concordium_base::protocol_level_tokens::{RawCbor, TokenId, TokenModuleCborTypeDiscriminator};
use plt_scheduler_interface::OutOfEnergyError;

/// A reason for why a transaction was rejected.
///
/// Rejected means included in a
/// block, but the desired action was not achieved. The only effect of a
/// rejected transaction is paying for the energy used.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TransactionRejectReason {
    /// We ran of out energy to process this transaction.
    OutOfEnergy,
    /// The provided identifier does not match a token currently on chain.
    NonExistentTokenId(TokenId),
    /// The token module rejected the transaction.
    TokenModule(TokenModuleRejectReason),
}

impl From<OutOfEnergyError> for TransactionRejectReason {
    fn from(_err: OutOfEnergyError) -> Self {
        Self::OutOfEnergy
    }
}

/// Details provided by the token module in the event of rejecting a
/// transaction.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct TokenModuleRejectReason {
    /// The canonical token id.
    pub token_id: TokenId,
    /// The type of the reject reason.
    pub reason_type: TokenModuleCborTypeDiscriminator,
    /// (Optional) CBOR-encoded details.
    pub details: Option<RawCbor>,
}
