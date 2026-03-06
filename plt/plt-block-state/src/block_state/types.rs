//! Types used specifically in the block state.

use concordium_base::common::Serialize;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{TokenId, TokenModuleRef};
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
}
