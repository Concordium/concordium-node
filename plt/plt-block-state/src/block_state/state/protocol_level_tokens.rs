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
use crate::block_state::utils::OwnedOrBorrowed;
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

    pub fn plt_list(&self, loader: &impl BackingStoreLoad) -> impl Iterator<Item = TokenId> {
        self.tokens.values(loader, |token_ref| {
            token_ref.with_value(loader, |token| {
                token.configuration.with_value(loader, |conf| match conf {
                    OwnedOrBorrowed::Owned(v) => v.0.token_id,
                    OwnedOrBorrowed::Borrowed(r) => r.0.token_id.clone(),
                })
            })
        })
    }

    pub fn mutable_token_key_value_state(
        &self,
        loader: &impl BackingStoreLoad,
        token_index: TokenIndex,
    ) -> SimplisticTokenKeyValueState {
        self.tokens
            .lookup(loader, token_index, |token_ref| {
                token_ref.with_value(loader, |token| match token {
                    OwnedOrBorrowed::Owned(v) => v.key_value_state.0,
                    OwnedOrBorrowed::Borrowed(r) => r.key_value_state.0.clone(),
                })
            })
            .expect("token out found by index")
    }

    pub fn token_configuration(
        &self,
        loader: &impl BackingStoreLoad,
        token_index: TokenIndex,
    ) -> TokenConfiguration {
        self.tokens
            .lookup(loader, token_index, |token_ref| {
                token_ref.with_value(loader, |token| {
                    token
                        .configuration
                        .with_value(loader, |conf| conf.into_owned().0)
                })
            })
            .expect("token out found by index")
    }

    pub fn token_circulating_supply(
        &self,
        loader: &impl BackingStoreLoad,
        token_index: TokenIndex,
    ) -> RawTokenAmount {
        self.tokens
            .lookup(loader, token_index, |token_ref| {
                token_ref.with_value(loader, |token| token.circulating_supply.0)
            })
            .expect("token out found by index")
    }
}

impl Storable for ProtocolLevelTokens {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BackingStoreStore) {
        self.tokens.store_to_buffer(&mut buffer, storer);
    }
}

impl Loadable for ProtocolLevelTokens {
    fn load_from_buffer(mut buffer: impl Read) -> Result<Self, DecodeError> {
        let tokens = Loadable::load_from_buffer(&mut buffer)?;

        Ok(Self { tokens })
    }
}

impl Cacheable for ProtocolLevelTokens {
    fn cache_reference_values(&self, loader: &impl BackingStoreLoad) -> Result<(), DecodeError> {
        self.tokens.cache_reference_values(loader)
    }
}

impl Hashable for ProtocolLevelTokens {
    fn hash(&self, loader: &impl BackingStoreLoad) -> Result<Hash, DecodeError> {
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
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BackingStoreStore) {
        self.configuration.store_to_buffer(&mut buffer, storer);
        self.key_value_state.store_to_buffer(&mut buffer, storer);
        self.circulating_supply.store_to_buffer(&mut buffer, storer);
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
    fn cache_reference_values(&self, loader: &impl BackingStoreLoad) -> Result<(), DecodeError> {
        self.configuration.cache_reference_values(loader)
    }
}

impl Hashable for Token {
    fn hash(&self, loader: &impl BackingStoreLoad) -> Result<Hash, DecodeError> {
        let config = self.configuration.hash(loader)?;
        let key_value_state = self.key_value_state.hash(loader)?;
        let circulating_supply = self.circulating_supply.hash(loader)?;

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

impl SimplisticTokenKeyValueState {
    pub fn lookup_value(&self, key: &TokenStateKey) -> Option<TokenStateValue> {
        self.state.get(key).cloned()
    }

    pub fn update_value(&mut self, key: &TokenStateKey, value: Option<TokenStateValue>) {
        if let Some(value) = value {
            self.state.insert(key.clone(), value);
        } else {
            self.state.remove(key);
        }
    }
}
