//! Persistent model for protocol-level tokens in the block state.

use crate::block_state::blob_reference::hashed_cacheable_reference::HashedCacheableRef;
use crate::block_state::blob_store::{
    BlobStoreLoad, BlobStoreStore, Loadable, Storable, StoreSerialized,
};
use crate::block_state::cacheable::Cacheable;
use crate::block_state::hash::Hashable;
use crate::block_state::lfmb_tree::{LfmbTree, LfmbTreeKey};
use crate::block_state::{hash, smart_contract_trie};
use crate::block_state_interface::BlockStateResult;
use crate::entity::protocol_level_tokens::TokenConfiguration;
use concordium_base::common::{Buffer, Serialize};
use concordium_base::hashes::Hash;
use concordium_base::protocol_level_tokens::TokenId;
use plt_scheduler_types::types::tokens::RawTokenAmount;
use std::io::Read;

/// Index of the protocol-level token in the block state map of tokens.
///
/// Corresponding Haskell type: `Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens.TokenIndex`
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct TokenIndex(pub u64);

/// Block state for protocol level tokens
#[derive(Debug, Clone, Default)]
pub struct PersistentPlTokens {
    pub tokens: LfmbTree<TokenIndex, PersistentPlToken>,
    pub token_id_map: im::HashMap<NormalizedTokenId, TokenIndex>,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
struct NormalizedTokenId(String);

pub fn normalize_token_id(token_id: &TokenId) -> NormalizedTokenId {
    NormalizedTokenId(token_id.as_ref().to_ascii_lowercase())
}

impl Storable for PersistentPlTokens {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        self.tokens.store_to_buffer(&mut buffer, storer);
    }
}

impl Loadable for PersistentPlTokens {
    fn load_from_buffer(
        mut buffer: impl Read,
        loader: &impl BlobStoreLoad,
    ) -> BlockStateResult<Self> {
        let tokens: LfmbTree<TokenIndex, PersistentPlToken> =
            Loadable::load_from_buffer(&mut buffer, loader)?;
        // To construct the full token id to token index map, we need to read the LFMBTree from
        // the blob store. This is not ideal. If the state is to be cached after loading, we would
        // rather wait until it is cached in memory before constructing the map.
        let token_id_map = tokens
            .values(loader)
            .map(|item| {
                let (token_index, plt) = item?;
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

impl Cacheable for PersistentPlTokens {
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        self.tokens.cache_reference_values(loader)
    }
}

impl Hashable for PersistentPlTokens {
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
pub struct PersistentPlToken {
    pub configuration: HashedCacheableRef<StoreSerialized<TokenConfiguration>>,
    pub key_value_state: HashedCacheableRef<smart_contract_trie::PersistentState>,
    pub circulating_supply: StoreSerialized<RawTokenAmount>,
}

impl Storable for PersistentPlToken {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        self.configuration.store_to_buffer(&mut buffer, storer);
        self.key_value_state.store_to_buffer(&mut buffer, storer);
        self.circulating_supply.store_to_buffer(&mut buffer, storer);
    }
}

impl Loadable for PersistentPlToken {
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

impl Cacheable for PersistentPlToken {
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        self.configuration.cache_reference_values(loader)?;
        self.key_value_state.cache_reference_values(loader)?;
        Ok(())
    }
}

impl Hashable for PersistentPlToken {
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        let config = self.configuration.hash(loader)?;
        let key_value_state = self.key_value_state.hash(loader)?;
        let state = hash::hash_of_serialization((key_value_state, self.circulating_supply.0));

        Ok(hash::hash_of_hashes(config, state))
    }
}
