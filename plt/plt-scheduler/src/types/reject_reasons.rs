//! Reject reasons for transactions executed by the scheduler.
//!
//! A rejected transaction means that the transaction was included on chain, but
//! failed for some reason. The only effect of a rejected transaction is
//! the charge of energy.

use concordium_base::protocol_level_tokens::TokenId;
use plt_token_module::token_module::TokenModuleRejectReason;

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
