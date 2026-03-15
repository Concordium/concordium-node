use crate::block_state::blob_reference::hashed_buffered_reference::HashedCacheableRef;
use crate::block_state::blob_store::{
    BackingStoreLoad, BackingStoreStore, DecodeError, Loadable, Storable, StoreSerialized,
};
use crate::block_state::cacheable::Cacheable;
use crate::block_state::hash;
use crate::block_state::hash::Hashable;
use crate::block_state::lfmb_tree::{LFMBTree, LFMBTreeKey};
use crate::block_state::types::protocol_level_tokens::{
    TokenConfiguration, TokenIndex, TokenStateKey, TokenStateValue,
};
use concordium_base::common::{Buffer, Serialize};
use concordium_base::hashes::Hash;
use concordium_base::protocol_level_tokens::TokenId;
use plt_scheduler_types::types::tokens::RawTokenAmount;
use std::collections::BTreeMap;
use std::io::Read;
use std::vec;

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

    pub fn plt_list(&self) -> impl Iterator<Item = TokenId> {
        // self.tokens.values(|token|token.)
        todo!() as vec::IntoIter<_>
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
    configuration: HashedCacheableRef<StoreSerialized<TokenConfiguration>>,
    key_value_state: StoreSerialized<SimplisticTokenKeyValueState>,
    circulating_supply: StoreSerialized<RawTokenAmount>,
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

// todo do real implementation of key-value store as part of https://linear.app/concordium/issue/PSR-11/port-the-plt-block-state-to-rust
#[derive(Debug, Clone, Default, Serialize)]
pub struct SimplisticTokenKeyValueState {
    state: BTreeMap<TokenStateKey, TokenStateValue>,
}
