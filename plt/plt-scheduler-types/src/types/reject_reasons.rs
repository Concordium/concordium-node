//! Reject reasons for transactions executed by the scheduler.
//!
//! A rejected transaction means that the transaction was included on chain, but
//! failed for some reason. The only effect of a rejected transaction is
//! the charge of energy.

use concordium_base::common::{Buffer, Put, Serial};
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_locks::LockId;
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
    /// Account does not exist.
    InvalidAccountReference(AccountAddress),
    /// The transaction payload could not be fully deserialized.
    SerializationFailure,
    /// We ran of out energy to process this transaction.
    OutOfEnergy,
    /// The provided identifier does not match a token currently on chain.
    NonExistentTokenId(TokenId),
    /// The token module rejected the transaction.
    TokenUpdateTransactionFailed(EncodedTokenModuleRejectReason),
    /// Lock ID does not exist.
    NonExistentLockId(LockId),
    /// The lock is expired.
    LockExpired(LockId),
    /// The account is not authorized to fund the lock.
    LockFundNotAuthorized(LockId, AccountAddress),
    /// The account is not authorized to send funds controlled by the lock.
    LockSendNotAuthorized(LockId, AccountAddress),
    /// The account is not authorized to return funds controlled by the lock.
    LockReturnNotAuthorized(LockId, AccountAddress),
    /// The account is not authorized to cancel the lock.
    LockCancelNotAuthorized(LockId, AccountAddress),
    /// The lock does not allow funding with the particular token.
    LockTokenNotPermitted(LockId, TokenId),
    /// The recipient is not permitted to receive funds controlled by the lock.
    LockRecipientNotPermitted(LockId, AccountAddress),
}

impl Serial for TransactionRejectReason {
    fn serial<B: Buffer>(&self, out: &mut B) {
        match self {
            TransactionRejectReason::InvalidAccountReference(address) => {
                out.put(&2u8);
                out.put(address);
            }
            TransactionRejectReason::SerializationFailure => {
                out.put(&9u8);
            }
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
            TransactionRejectReason::NonExistentLockId(lock_id) => {
                out.put(&57u8);
                out.put(&lock_id);
            }
            TransactionRejectReason::LockExpired(lock_id) => {
                out.put(&58u8);
                out.put(&lock_id);
            }
            TransactionRejectReason::LockFundNotAuthorized(lock_id, addr) => {
                out.put(&59u8);
                out.put(&lock_id);
                out.put(&addr);
            }
            TransactionRejectReason::LockSendNotAuthorized(lock_id, addr) => {
                out.put(&60u8);
                out.put(&lock_id);
                out.put(&addr);
            }
            TransactionRejectReason::LockReturnNotAuthorized(lock_id, addr) => {
                out.put(&61u8);
                out.put(&lock_id);
                out.put(&addr);
            }
            TransactionRejectReason::LockCancelNotAuthorized(lock_id, addr) => {
                out.put(&62u8);
                out.put(&lock_id);
                out.put(&addr);
            }
            TransactionRejectReason::LockTokenNotPermitted(lock_id, token_id) => {
                out.put(&63u8);
                out.put(&lock_id);
                out.put(&token_id);
            }
            TransactionRejectReason::LockRecipientNotPermitted(lock_id, addr) => {
                out.put(&64u8);
                out.put(&lock_id);
                out.put(&addr);
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
    use concordium_base::contracts_common::AccountAddress;
    use concordium_base::protocol_level_locks::LockId;
    use concordium_base::protocol_level_tokens::RawCbor;

    const ADDRESS: AccountAddress = AccountAddress([
        0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e,
        0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d,
        0x1e, 0x1f,
    ]);

    #[test]
    fn test_invalid_account_reference_reject_reason_serial() {
        let reject_reason = TransactionRejectReason::InvalidAccountReference(ADDRESS);
        let bytes = common::to_bytes(&reject_reason);
        assert_eq!(
            hex::encode(&bytes),
            "02000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
        );
    }

    #[test]
    fn test_serialization_failure_reject_reason_serial() {
        let reject_reason = TransactionRejectReason::SerializationFailure;

        let bytes = common::to_bytes(&reject_reason);
        assert_eq!(hex::encode(&bytes), "09")
    }

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

    #[test]
    fn test_non_existent_lock_id_reject_reason_serial() {
        let reject_reason =
            TransactionRejectReason::NonExistentLockId(LockId::new(0xfedcba, 0x1234, 5));
        let bytes = common::to_bytes(&reject_reason);
        assert_eq!(
            hex::encode(&bytes),
            "390000000000fedcba00000000000012340000000000000005"
        );
    }

    #[test]
    fn test_lock_expired_reject_reason_serial() {
        let reject_reason = TransactionRejectReason::LockExpired(LockId::new(0xfedcba, 0x1234, 5));
        let bytes = common::to_bytes(&reject_reason);
        assert_eq!(
            hex::encode(&bytes),
            "3a0000000000fedcba00000000000012340000000000000005"
        );
    }

    #[test]
    fn test_lock_fund_not_authorized_reject_reason_serial() {
        let reject_reason = TransactionRejectReason::LockFundNotAuthorized(
            LockId::new(0xfedcba, 0x1234, 5),
            ADDRESS,
        );

        let bytes = common::to_bytes(&reject_reason);
        assert_eq!(
            hex::encode(&bytes),
            "3b0000000000fedcba00000000000012340000000000000005000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
        );
    }

    #[test]
    fn test_lock_send_not_authorized_reject_reason_serial() {
        let reject_reason = TransactionRejectReason::LockSendNotAuthorized(
            LockId::new(0xfedcba, 0x1234, 5),
            ADDRESS,
        );

        let bytes = common::to_bytes(&reject_reason);
        assert_eq!(
            hex::encode(&bytes),
            "3c0000000000fedcba00000000000012340000000000000005000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
        );
    }

    #[test]
    fn test_lock_return_not_authorized_reject_reason_serial() {
        let reject_reason = TransactionRejectReason::LockReturnNotAuthorized(
            LockId::new(0xfedcba, 0x1234, 5),
            ADDRESS,
        );
        let bytes = common::to_bytes(&reject_reason);
        assert_eq!(
            hex::encode(&bytes),
            "3d0000000000fedcba00000000000012340000000000000005000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
        );
    }

    #[test]
    fn test_lock_cancel_not_authorized_reject_reason_serial() {
        let reject_reason = TransactionRejectReason::LockCancelNotAuthorized(
            LockId::new(0xfedcba, 0x1234, 5),
            ADDRESS,
        );
        let bytes = common::to_bytes(&reject_reason);
        assert_eq!(
            hex::encode(&bytes),
            "3e0000000000fedcba00000000000012340000000000000005000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
        );
    }

    #[test]
    fn test_lock_token_not_permitted_reject_reason_serial() {
        let reject_reason = TransactionRejectReason::LockTokenNotPermitted(
            LockId::new(0xfedcba, 0x1234, 5),
            "token1".parse().unwrap(),
        );
        let bytes = common::to_bytes(&reject_reason);
        assert_eq!(
            hex::encode(&bytes),
            "3f0000000000fedcba0000000000001234000000000000000506746f6b656e31"
        );
    }

    #[test]
    fn test_lock_recipient_not_permitted_reject_reason_serial() {
        let reject_reason = TransactionRejectReason::LockRecipientNotPermitted(
            LockId::new(0xfedcba, 0x1234, 5),
            ADDRESS,
        );
        let bytes = common::to_bytes(&reject_reason);
        assert_eq!(
            hex::encode(&bytes),
            "400000000000fedcba00000000000012340000000000000005000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
        );
    }
}
