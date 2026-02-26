//! Types returned by queries.

use crate::types::tokens::TokenAmount;
use concordium_base::common::Serialize;
use concordium_base::protocol_level_tokens::{RawCbor, TokenId, TokenModuleRef};

/// Token state at the block level
///
/// Corresponding Haskell type: `Concordium.Types.Queries.Tokens.TokenState`
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub struct TokenState {
    /// The reference of the module implementing this token.
    pub token_module_ref: TokenModuleRef,
    /// Number of decimals in the decimal number representation of amounts.
    pub decimals: u8,
    /// The total available token supply.
    pub total_supply: TokenAmount,
    /// Token module specific state, such as token name, feature flags, meta
    /// data.
    pub module_state: RawCbor,
}

/// The token state at the block level.
///
/// Corresponding Haskell type: `Concordium.Types.Queries.Tokens.TokenInfo`
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub struct TokenInfo {
    /// The canonical identifier/symbol for the protocol level token.
    pub token_id: TokenId,
    /// The associated block level state.
    pub state: TokenState,
}

/// State of a protocol level token associated with some account.
///
/// Corresponding Haskell type: `Concordium.Types.Queries.Tokens.TokenAccountState`
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub struct TokenAccountState {
    /// The token balance of the account.
    pub balance: TokenAmount,
    /// The token-module defined state of the account.
    pub module_state: RawCbor,
}

/// State of a protocol level token associated with some account.
///
/// Corresponding Haskell type: `Concordium.Types.Queries.Tokens.Token`
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub struct TokenAccountInfo {
    /// The canonical identifier/symbol for the protocol level token.
    pub token_id: TokenId,
    /// The state of the token associated with the account.
    pub account_state: TokenAccountState,
}

#[cfg(test)]
mod test {
    use concordium_base::{
        common,
        protocol_level_tokens::{RawCbor, TokenId, TokenModuleRef},
    };

    use crate::types::{
        queries::{TokenAccountInfo, TokenAccountState, TokenInfo, TokenState},
        tokens::{RawTokenAmount, TokenAmount},
    };

    fn module_state_fixture() -> RawCbor {
        vec![1, 2, 3].into()
    }

    fn token_amount_fixture(decimals: u8) -> TokenAmount {
        TokenAmount {
            amount: RawTokenAmount(100),
            decimals,
        }
    }

    fn token_state_fixture() -> TokenState {
        TokenState {
            token_module_ref: TokenModuleRef::from([1; 32]),
            decimals: 10,
            total_supply: token_amount_fixture(10),
            module_state: module_state_fixture(),
        }
    }

    fn token_id_fixture() -> TokenId {
        "token"
            .to_string()
            .try_into()
            .expect("token id must be valid")
    }

    fn token_account_state_fixture() -> TokenAccountState {
        TokenAccountState {
            balance: token_amount_fixture(10),
            module_state: module_state_fixture(),
        }
    }

    #[test]
    fn test_token_state_serial() {
        let token_state = token_state_fixture();

        let bytes = common::to_bytes(&token_state);
        assert_eq!(
            hex::encode(&bytes),
            "01010101010101010101010101010101010101010101010101010101010101010a640a00000003010203"
        );

        let deserialized = common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(token_state, deserialized);
    }

    #[test]
    fn test_token_info_serial() {
        let token_info = TokenInfo {
            token_id: token_id_fixture(),
            state: token_state_fixture(),
        };

        let bytes = common::to_bytes(&token_info);
        assert_eq!(
            hex::encode(&bytes),
            "05746f6b656e01010101010101010101010101010101010101010101010101010101010101010a640a00000003010203"
        );

        let deserialized = common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(token_info, deserialized);
    }

    #[test]
    fn test_token_account_state_serial() {
        let token_account_state = token_account_state_fixture();

        let bytes = common::to_bytes(&token_account_state);
        assert_eq!(hex::encode(&bytes), "640a00000003010203");

        let deserialized = common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(token_account_state, deserialized);
    }

    #[test]
    fn test_token_account_info_serial() {
        let token_account_info = TokenAccountInfo {
            token_id: token_id_fixture(),
            account_state: token_account_state_fixture(),
        };

        let bytes = common::to_bytes(&token_account_info);
        assert_eq!(hex::encode(&bytes), "05746f6b656e640a00000003010203");

        let deserialized = common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(token_account_info, deserialized);
    }
}
