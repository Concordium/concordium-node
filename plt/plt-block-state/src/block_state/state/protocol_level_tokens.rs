//! Persistent model for protocol-level tokens in the block state.

use crate::block_state::blob_reference::hashed_cacheable_reference::HashedCacheableRef;
use crate::block_state::blob_store::{
    BlobStoreLoad, BlobStoreStore, Loadable, Storable, StoreSerialized,
};
use crate::block_state::cacheable::Cacheable;
use crate::block_state::hash::Hashable;
use crate::block_state::lfmb_tree::{LfmbTree, LfmbTreeKey};
use crate::block_state::types::protocol_level_tokens::{TokenConfiguration, TokenIndex};
use crate::block_state::utils::OwnedOrBorrowed;
use crate::block_state::{hash, smart_contract_trie};
use crate::block_state_interface::{BlockStateFailure, BlockStateResult};
use concordium_base::common::Buffer;
use concordium_base::hashes::Hash;
use concordium_base::protocol_level_tokens::TokenId;
use plt_scheduler_types::types::tokens::RawTokenAmount;
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
    ) -> BlockStateResult<smart_contract_trie::MutableState> {
        self.tokens
            .lookup_value(loader, token_index, |token| {
                token
                    .key_value_state
                    .with_value(loader, |key_value_state| Ok(key_value_state.thaw()))
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
            key_value_state: HashedCacheableRef::new(smart_contract_trie::PersistentState::empty()),
            circulating_supply: StoreSerialized(RawTokenAmount(0)),
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
        mut token_key_value_state: smart_contract_trie::MutableState,
    ) -> BlockStateResult<ProtocolLevelTokens> {
        let frozen_key_value_state = token_key_value_state.freeze(loader);

        Ok(ProtocolLevelTokens {
            tokens: self
                .tokens
                .update_value(loader, token_index, |token| {
                    Ok(Token {
                        key_value_state: HashedCacheableRef::new(frozen_key_value_state),
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
    key_value_state: HashedCacheableRef<smart_contract_trie::PersistentState>,
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
        self.configuration.cache_reference_values(loader)?;
        self.key_value_state.cache_reference_values(loader)?;
        Ok(())
    }
}

impl Hashable for Token {
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        let config = self.configuration.hash(loader)?;
        let key_value_state = self.key_value_state.hash(loader)?;
        let state = hash::hash_of_serialization((key_value_state, self.circulating_supply.0));

        Ok(hash::hash_of_hashes(config, state))
    }
}
