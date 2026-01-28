//! Reject reasons for transactions executed by the scheduler.
//!
//! A rejected transaction means that the transaction was included on chain, but
//! failed for some reason. The only effect of a rejected transaction is
//! the charge of energy.

use concordium_base::common::{Buffer, Put, Serial};
use concordium_base::protocol_level_tokens::{RawCbor, TokenId, TokenModuleCborTypeDiscriminator};

/// A reason for why a transaction was rejected.
///
/// Rejected means included in a
/// block, but the desired action was not achieved. The only effect of a
/// rejected transaction is paying for the energy used.
///
/// Corresponding Haskell type: `Concordium.Types.Execution.RejectReason`
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TransactionRejectReason {
    /// We ran of out energy to process this transaction.
    OutOfEnergy,
    /// The provided identifier does not match a token currently on chain.
    NonExistentTokenId(TokenId),
    /// The token module rejected the transaction.
    TokenUpdateTransactionFailed(EncodedTokenModuleRejectReason),
}

impl Serial for TransactionRejectReason {
    fn serial<B: Buffer>(&self, out: &mut B) {
        match self {
            TransactionRejectReason::OutOfEnergy => {
                out.put(&10u8);
            }
            TransactionRejectReason::NonExistentTokenId(token_id) => {
                out.put(&55u8);
                out.put(&token_id);
            }
            TransactionRejectReason::TokenUpdateTransactionFailed(reject_reason) => {
                out.put(&56u8);
                out.put(&reject_reason);
            }
        }
    }
}

/// Details provided by the token module in the event of rejecting a
/// transaction.
///
/// Corresponding Haskell type: `Concordium.Types.TokenModuleRejectReason`
#[derive(Debug, Clone, Eq, PartialEq, Hash, Serial)]
pub struct EncodedTokenModuleRejectReason {
    /// The canonical token id.
    pub token_id: TokenId,
    /// The type of the reject reason.
    pub reason_type: TokenModuleCborTypeDiscriminator,
    /// (Optional) CBOR-encoded details.
    pub details: Option<RawCbor>,
}

#[cfg(test)]
mod test {
    use crate::types::reject_reasons::{EncodedTokenModuleRejectReason, TransactionRejectReason};
    use concordium_base::common;
    use concordium_base::protocol_level_tokens::RawCbor;

    #[test]
    fn test_out_of_energy_reject_reason_serial() {
        let reject_reason = TransactionRejectReason::OutOfEnergy;

        let bytes = common::to_bytes(&reject_reason);
        assert_eq!(hex::encode(&bytes), "0a");
    }

    #[test]
    fn test_non_existent_token_id_reject_reason_serial() {
        let reject_reason = TransactionRejectReason::NonExistentTokenId("token1".parse().unwrap());

        let bytes = common::to_bytes(&reject_reason);
        assert_eq!(hex::encode(&bytes), "3706746f6b656e31");
    }

    #[test]
    fn test_token_update_transaction_failed_reject_reason_serial() {
        // without details
        let reject_reason =
            TransactionRejectReason::TokenUpdateTransactionFailed(EncodedTokenModuleRejectReason {
                token_id: "token1".parse().unwrap(),
                reason_type: "reject_reason_type1".parse().unwrap(),
                details: None,
            });

        let bytes = common::to_bytes(&reject_reason);
        assert_eq!(
            hex::encode(&bytes),
            "3806746f6b656e311372656a6563745f726561736f6e5f747970653100"
        );

        // with details
        let reject_reason =
            TransactionRejectReason::TokenUpdateTransactionFailed(EncodedTokenModuleRejectReason {
                token_id: "token1".parse().unwrap(),
                reason_type: "reject_reason_type1".parse().unwrap(),
                details: Some(RawCbor::from(vec![1, 2, 3])),
            });

        let bytes = common::to_bytes(&reject_reason);
        assert_eq!(
            hex::encode(&bytes),
            "3806746f6b656e311372656a6563745f726561736f6e5f74797065310100000003010203"
        );
    }
}
