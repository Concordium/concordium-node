use crate::block_state::blob_reference::hashed_buffered_reference::HashedCacheableRef;
use crate::block_state::blob_store::{
    BackingStoreLoad, BackingStoreStore, DecodeError, Loadable, Storable,
};
use crate::block_state::cacheable::Cacheable;
use crate::block_state::hash;
use crate::block_state::hash::Hashable;
use crate::block_state::lfmb_tree::{LFMBTree, LFMBTreeKey};
use concordium_base::common::{Buffer, Serialize};
use concordium_base::hashes::Hash;
use concordium_base::protocol_level_tokens::{TokenId, TokenModuleRef};
use plt_scheduler_types::types::tokens::RawTokenAmount;
use std::collections::BTreeMap;
use std::io::Read;

/// Block state for protocol level tokens
#[derive(Debug, Clone)]
pub struct ProtocolLevelTokens {
    /// Simplistic state that is used as a temporary implementation of the block state
    tokens: LFMBTree<TokenIndex, HashedCacheableRef<Token>>,
}

impl ProtocolLevelTokens {
    pub fn empty() -> Self {
        Self {
            tokens: LFMBTree::empty(),
        }
    }
}

impl Storable for ProtocolLevelTokens {
    fn store_to_buffer(&self, mut buffer: impl Buffer, mut storer: impl BackingStoreStore) {
        self.tokens.store_to_buffer(&mut buffer, &mut storer);
    }
}

impl Loadable for ProtocolLevelTokens {
    fn load_from_buffer(mut buffer: impl Read) -> Result<Self, DecodeError> {
        let tokens = Loadable::load_from_buffer(&mut buffer)?;

        Ok(Self { tokens })
    }
}

impl Cacheable for ProtocolLevelTokens {
    fn cache_reference_values(&self, loader: impl BackingStoreLoad) -> Result<(), DecodeError> {
        self.tokens.cache_reference_values(loader)
    }
}

impl Hashable for ProtocolLevelTokens {
    fn hash(&self, loader: impl BackingStoreLoad) -> Result<Hash, DecodeError> {
        self.tokens.hash(loader)
    }
}

/// Index of the protocol-level token in the block state map of tokens.
///
/// Corresponding Haskell type: `Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens.TokenIndex`
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct TokenIndex(pub u64);

impl LFMBTreeKey for TokenIndex {
    fn to_u64(self) -> u64 {
        self.0
    }

    fn from_u64(key: u64) -> Self {
        Self(key)
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    configuration: HashedCacheableRef<TokenConfiguration>,
    key_value_state: SimplisticTokenKeyValueState,
    circulating_supply: RawTokenAmount,
}

impl Storable for Token {
    fn store_to_buffer(&self, mut buffer: impl Buffer, mut storer: impl BackingStoreStore) {
        self.configuration.store_to_buffer(&mut buffer, &mut storer);
        self.key_value_state
            .store_to_buffer(&mut buffer, &mut storer);
        self.circulating_supply
            .store_to_buffer(&mut buffer, &mut storer);
    }
}

impl Loadable for Token {
    fn load_from_buffer(mut buffer: impl Read) -> Result<Self, DecodeError> {
        let configuration = Loadable::load_from_buffer(&mut buffer)?;
        let key_value_state = Loadable::load_from_buffer(&mut buffer)?;
        let circulating_supply = Loadable::load_from_buffer(&mut buffer)?;

        Ok(Self {
            configuration,
            key_value_state,
            circulating_supply,
        })
    }
}

impl Cacheable for Token {
    fn cache_reference_values(&self, loader: impl BackingStoreLoad) -> Result<(), DecodeError> {
        self.configuration.cache_reference_values(loader)
    }
}

impl Hashable for Token {
    fn hash(&self, mut loader: impl BackingStoreLoad) -> Result<Hash, DecodeError> {
        let config = self.configuration.hash(&mut loader)?;
        let key_value_state = self.key_value_state.hash(&mut loader)?;
        let circulating_supply = self.circulating_supply.hash(&mut loader)?;

        Ok(hash::hash_of_hashes(
            config,
            hash::hash_of_hashes(key_value_state, circulating_supply),
        ))
    }
}

pub type TokenStateKey = Vec<u8>;
pub type TokenStateValue = Vec<u8>;

// todo do real implementation of key-value store as part of https://linear.app/concordium/issue/PSR-11/port-the-plt-block-state-to-rust
#[derive(Debug, Clone, Default, Serialize)]
pub struct SimplisticTokenKeyValueState {
    state: BTreeMap<TokenStateKey, TokenStateValue>,
}

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
