//! Persistent model for protocol-level tokens in the block state.

use crate::block_state::blob_reference::hashed_cacheable_reference::HashedCacheableRef;
use crate::block_state::blob_store::{
    BlobStoreLoad, BlobStoreStore, Loadable, Storable, StoreSerialized,
};
use crate::block_state::cacheable::Cacheable;
use crate::block_state::hash;
use crate::block_state::hash::Hashable;
use crate::block_state::lfmb_tree::{LfmbTree, LfmbTreeKey};
use crate::block_state::types::protocol_level_tokens::{
    TokenConfiguration, TokenIndex, TokenStateKey, TokenStateValue,
};
use crate::block_state::utils::OwnedOrBorrowed;
use crate::block_state_interface::{BlockStateFailure, BlockStateResult};
use concordium_base::common::{Buffer, Serialize};
use concordium_base::hashes::Hash;
use concordium_base::protocol_level_tokens::TokenId;
use plt_scheduler_types::types::tokens::RawTokenAmount;
use std::collections::BTreeMap;
use std::io::Read;

/// Block state for protocol level tokens
#[derive(Debug, Clone, Default)]
pub struct ProtocolLevelTokens {
    tokens: LfmbTree<TokenIndex, Token>,
    token_id_map: im::HashMap<NormalizedTokenId, TokenIndex>,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
struct NormalizedTokenId(String);

fn normalize_token_id(token_id: &TokenId) -> NormalizedTokenId {
    NormalizedTokenId(token_id.as_ref().to_ascii_lowercase())
}

impl ProtocolLevelTokens {
    pub fn empty() -> Self {
        Self {
            tokens: LfmbTree::empty(),
            token_id_map: im::HashMap::new(),
        }
    }

    pub fn plt_list(
        &self,
        loader: &impl BlobStoreLoad,
    ) -> impl ExactSizeIterator<Item = BlockStateResult<TokenId>> {
        self.tokens.values(loader, |_token_index, token| {
            Ok(match token.configuration.value(loader)? {
                OwnedOrBorrowed::Owned(v) => v.0.token_id,
                OwnedOrBorrowed::Borrowed(r) => r.0.token_id.clone(),
            })
        })
    }

    pub fn token_by_id(&self, token_id: &TokenId) -> Option<TokenIndex> {
        self.token_id_map
            .get(&normalize_token_id(token_id))
            .copied()
    }

    pub fn mutable_token_key_value_state(
        &self,
        loader: &impl BlobStoreLoad,
        token_index: TokenIndex,
    ) -> BlockStateResult<SimplisticTokenKeyValueState> {
        self.tokens
            .lookup_value(loader, token_index, |token| {
                Ok(match token {
                    OwnedOrBorrowed::Owned(v) => v.key_value_state.0,
                    OwnedOrBorrowed::Borrowed(r) => r.key_value_state.0.clone(),
                })
            })
            .ok_or_else(|| {
                BlockStateFailure::Invariant(format!("token not found by index: {:?}", token_index))
            })?
    }

    pub fn token_configuration(
        &self,
        loader: &impl BlobStoreLoad,
        token_index: TokenIndex,
    ) -> BlockStateResult<TokenConfiguration> {
        self.tokens
            .lookup_value(loader, token_index, |token| {
                Ok(token.configuration.value(loader)?.into_owned().0)
            })
            .ok_or_else(|| {
                BlockStateFailure::Invariant(format!("token not found by index: {:?}", token_index))
            })?
    }

    pub fn token_circulating_supply(
        &self,
        loader: &impl BlobStoreLoad,
        token_index: TokenIndex,
    ) -> BlockStateResult<RawTokenAmount> {
        self.tokens
            .lookup_value(loader, token_index, |token| Ok(token.circulating_supply.0))
            .ok_or_else(|| {
                BlockStateFailure::Invariant(format!("token not found by index: {:?}", token_index))
            })?
    }

    pub fn set_token_circulating_supply(
        self,
        loader: &impl BlobStoreLoad,
        token_index: TokenIndex,
        circulating_supply: RawTokenAmount,
    ) -> BlockStateResult<ProtocolLevelTokens> {
        Ok(ProtocolLevelTokens {
            tokens: self
                .tokens
                .update_value(loader, token_index, |token| {
                    Ok(Token {
                        circulating_supply: StoreSerialized(circulating_supply),
                        ..token.into_owned()
                    })
                })
                .ok_or_else(|| {
                    BlockStateFailure::Invariant(format!(
                        "token not found by index: {:?}",
                        token_index
                    ))
                })??,
            ..self
        })
    }

    pub fn create_token(
        self,
        loader: &impl BlobStoreLoad,
        configuration: TokenConfiguration,
    ) -> BlockStateResult<(TokenIndex, ProtocolLevelTokens)> {
        let normalized_token_id = normalize_token_id(&configuration.token_id);

        let token = Token {
            configuration: HashedCacheableRef::new(StoreSerialized(configuration)),
            key_value_state: Default::default(),
            circulating_supply: Default::default(),
        };

        let (token_index, tokens) = self.tokens.insert_value(loader, token)?;
        // Do a "lazy" clone of the map, see https://docs.rs/im/latest/im/#when-does-cloning-happen
        let mut token_id_map = self.token_id_map.clone();
        token_id_map.insert(normalized_token_id, token_index);

        Ok((
            token_index,
            ProtocolLevelTokens {
                tokens,
                token_id_map,
            },
        ))
    }

    pub fn set_token_key_value_state(
        self,
        loader: &impl BlobStoreLoad,
        token_index: TokenIndex,
        token_key_value_state: SimplisticTokenKeyValueState,
    ) -> BlockStateResult<ProtocolLevelTokens> {
        Ok(ProtocolLevelTokens {
            tokens: self
                .tokens
                .update_value(loader, token_index, |token| {
                    Ok(Token {
                        key_value_state: StoreSerialized(token_key_value_state),
                        ..token.into_owned()
                    })
                })
                .ok_or_else(|| {
                    BlockStateFailure::Invariant(format!(
                        "token not found by index: {:?}",
                        token_index
                    ))
                })??,
            ..self
        })
    }
}

impl Storable for ProtocolLevelTokens {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        self.tokens.store_to_buffer(&mut buffer, storer);
    }
}

impl Loadable for ProtocolLevelTokens {
    fn load_from_buffer(
        mut buffer: impl Read,
        loader: &impl BlobStoreLoad,
    ) -> BlockStateResult<Self> {
        let tokens: LfmbTree<TokenIndex, Token> = Loadable::load_from_buffer(&mut buffer, loader)?;
        // To construct the full token id to token index map, we need to read the LFMBTree from
        // the blob store. This is not ideal. If the state is to be cached after loading, we would
        // rather wait until it is cached in memory before constructing the map.
        let token_id_map = tokens
            .values(loader, |token_index, plt| {
                let conf = plt.configuration.value(loader)?;
                Ok((normalize_token_id(&conf.0.token_id), token_index))
            })
            .collect::<BlockStateResult<im::HashMap<_, _>>>()?;

        Ok(Self {
            tokens,
            token_id_map,
        })
    }
}

impl Cacheable for ProtocolLevelTokens {
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        self.tokens.cache_reference_values(loader)
    }
}

impl Hashable for ProtocolLevelTokens {
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        self.tokens.hash(loader)
    }
}

impl LfmbTreeKey for TokenIndex {
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
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        self.configuration.store_to_buffer(&mut buffer, storer);
        self.key_value_state.store_to_buffer(&mut buffer, storer);
        self.circulating_supply.store_to_buffer(&mut buffer, storer);
    }
}

impl Loadable for Token {
    fn load_from_buffer(
        mut buffer: impl Read,
        loader: &impl BlobStoreLoad,
    ) -> BlockStateResult<Self> {
        let configuration = Loadable::load_from_buffer(&mut buffer, loader)?;
        let key_value_state = Loadable::load_from_buffer(&mut buffer, loader)?;
        let circulating_supply = Loadable::load_from_buffer(&mut buffer, loader)?;

        Ok(Self {
            configuration,
            key_value_state,
            circulating_supply,
        })
    }
}

impl Cacheable for Token {
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        self.configuration.cache_reference_values(loader)
    }
}

impl Hashable for Token {
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        let config = self.configuration.hash(loader)?;
        let key_value_state = self.key_value_state.hash(loader)?;
        let circulating_supply = self.circulating_supply.hash(loader)?;

        Ok(hash::hash_of_hashes(
            config,
            hash::hash_of_hashes(key_value_state, circulating_supply),
        ))
    }
}

// todo do real implementation of key-value store as part of ar/psr-84-use-smart-contract-trie-for-key-value-store
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

impl SimplisticTokenKeyValueState {
    pub fn iter_prefix(
        &self,
        prefix: TokenStateKey,
    ) -> impl Iterator<Item = (&TokenStateKey, &TokenStateValue)> {
        // This is just a temporary implementation and will not scale.
        // However implementation should be trivial once using the actual trie.
        let mut out = Vec::new();
        for (key, value) in self.state.iter() {
            if key.starts_with(prefix.as_slice()) {
                out.push((key, value));
            }
        }
        out.into_iter()
    }
}
