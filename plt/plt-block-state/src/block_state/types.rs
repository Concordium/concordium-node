//! Types used specifically in the block state.

use concordium_base::base::AccountIndex;
use concordium_base::common::Serialize;
use concordium_base::common::types::TransactionTime;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{TokenId, TokenModuleRef};
use plt_scheduler_types::types::locks::LockController;
use plt_scheduler_types::types::tokens::RawTokenAmount;

/// Index of the protocol-level token in the block state map of tokens.
///
/// Corresponding Haskell type: `Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens.TokenIndex`
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct TokenIndex(pub u64);

/// Static configuration for a protocol-level token.
///
/// Corresponding Haskell type: `Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens.PLTConfiguration`
#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize)]
pub struct TokenConfiguration {
    /// The token ID in its canonical form. Token IDs are case-insensitive when compared,
    /// but the canonical token ID preserves the original casing specified when
    /// the token was created.
    pub token_id: TokenId,
    /// The token module reference.
    pub module_ref: TokenModuleRef,
    /// The number of decimal places used in the representation of the token.
    pub decimals: u8,
}

/// Token account state at block state level.
///
/// Corresponding Haskell type: `Concordium.GlobalState.Persistent.Account.ProtocolLevelTokens.TokenAccountState`
#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize)]
pub struct TokenAccountState {
    /// Balance of the account
    pub balance: RawTokenAmount,
}

pub type TokenStateKey = Vec<u8>;
pub type TokenStateValue = Vec<u8>;

/// Lock configuration at the block state level.
#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
pub struct LockConfiguration {
    /// Accounts that can receive funds from this lock.
    #[size_length = 2]
    pub recipients: Vec<AccountIndex>,
    /// Expiry time of the lock (seconds since epoch).
    pub expiry: TransactionTime,
    /// Controller configuration for the lock.
    pub controller: LockController,
}

/// A token amount and the associated token ID as a pair.
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Serialize)]
pub struct TokenAndAmount {
    /// The token ID corresponding to the `amount`.
    pub token_id: TokenIndex,
    /// The amount of tokens as an unscaled integer value.
    pub amount: RawTokenAmount,
}

/// Account representing (read-only) account state.
///
/// The account is guaranteed to exist on chain, when holding an instance of this type.
#[derive(Debug)]
pub struct AccountWithCanonicalAddress<Account> {
    /// Opaque type that represents an account on chain.
    pub account: Account,
    /// The canonical account address of the account, i.e. the address used as part of the
    /// credential deployment and not an alias.
    pub canonical_account_address: AccountAddress,
}

#[cfg(test)]
mod test {
    use super::*;
    use concordium_base::common;
    use concordium_base::protocol_level_tokens::TokenModuleRef;

    #[test]
    fn test_token_configuration_serial() {
        let token_configuration = TokenConfiguration {
            token_id: "tokenid1".parse().unwrap(),
            module_ref: TokenModuleRef::from([5; 32]),
            decimals: 4,
        };

        let bytes = common::to_bytes(&token_configuration);
        assert_eq!(
            hex::encode(&bytes),
            "08746f6b656e696431050505050505050505050505050505050505050505050505050505050505050504"
        );

        let token_configuration_deserialized: TokenConfiguration =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(token_configuration_deserialized, token_configuration);
    }

    #[test]
    fn test_token_account_state_serial() {
        let state = TokenAccountState {
            balance: RawTokenAmount(10),
        };

        let bytes = common::to_bytes(&state);
        assert_eq!(hex::encode(&bytes), "0a");

        let state_deserialized: TokenAccountState =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(state_deserialized, state);
    }

    #[test]
    fn test_lock_configuration_serial() {
        use concordium_base::common::types::TransactionTime;
        use concordium_base::protocol_level_locks::LockControllerSimpleV0Capability;
        use plt_scheduler_types::types::locks::{
            LockControllerSimpleV0, LockControllerSimpleV0Grant,
        };

        let lock_config = LockConfiguration {
            recipients: vec![AccountIndex::from(1u64), AccountIndex::from(2u64)],
            expiry: TransactionTime::from(1000u64),
            controller: LockController::SimpleV0(LockControllerSimpleV0 {
                grants: vec![LockControllerSimpleV0Grant {
                    account: AccountIndex::from(1u64),
                    roles: vec![LockControllerSimpleV0Capability::Fund],
                }],
                tokens: vec!["token1".parse().unwrap()],
                keep_alive: true,
                memo: None,
            }),
        };

        let bytes = common::to_bytes(&lock_config);
        assert_eq!(
            hex::encode(&bytes),
            "00020000000000000001000000000000000200000000000003e800000100000000000000010100000106746f6b656e310100"
        );

        let deserialized: LockConfiguration =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(deserialized, lock_config);
    }

    #[test]
    fn test_lock_configuration_serial_empty_recipients() {
        use concordium_base::common::types::TransactionTime;

        let lock_config = LockConfiguration {
            recipients: vec![],
            expiry: TransactionTime::from(500u64),
            controller: LockController::SimpleV0(
                plt_scheduler_types::types::locks::LockControllerSimpleV0 {
                    grants: vec![],
                    tokens: vec![],
                    keep_alive: false,
                    memo: None,
                },
            ),
        };

        let bytes = common::to_bytes(&lock_config);
        assert_eq!(hex::encode(&bytes), "000000000000000001f400000000000000");

        let deserialized: LockConfiguration =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(deserialized, lock_config);
    }

    #[test]
    fn test_token_and_amount_serial() {
        let token_and_amount = TokenAndAmount {
            token_id: TokenIndex(2),
            amount: RawTokenAmount(1000),
        };

        let bytes = common::to_bytes(&token_and_amount);
        assert_eq!(hex::encode(&bytes), "00000000000000028768");

        let token_and_amount_deserialized: TokenAndAmount =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(token_and_amount_deserialized, token_and_amount);
    }
}
