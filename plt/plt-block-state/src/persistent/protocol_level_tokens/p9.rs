use crate::failure::BlockStateResult;
use crate::persistent::blob_reference::hashed_cacheable_reference::HashedCacheableRef;
use crate::persistent::blob_store::{
    BlobStoreLoad, BlobStoreStore, Loadable, Storable, StoreSerialized,
};
use crate::persistent::cacheable::Cacheable;
use crate::persistent::hash::Hashable;
use crate::persistent::lfmb_tree::{LfmbTree, LfmbTreeKey};
use crate::persistent::protocol_level_tokens::NormalizedTokenId;
use crate::persistent::{hash, protocol_level_tokens, smart_contract_trie};
use crate::utils::Cow;
use concordium_base::common::{Buffer, Serialize};
use concordium_base::hashes::Hash;
use concordium_base::protocol_level_tokens::{TokenId, TokenModuleRef};
use plt_scheduler_types::types::tokens::RawTokenAmount;
use std::io::Read;

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

/// Block state for protocol level tokens on P9 and later protocols that uses the same representation.
#[derive(Debug, Clone, Default)]
pub struct PersistentTokensP9 {
    /// Persistent tree with tokens by token index. Token are never deleted.
    pub(crate) tokens: LfmbTree<TokenIndex, PersistentTokenP9>,
    /// Map for normalized token id to token index. This map is represented in memory
    /// only and reconstructed each time tokens are loaded.
    pub(crate) token_id_map: im::HashMap<NormalizedTokenId, TokenIndex>,
}

impl Storable for PersistentTokensP9 {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        self.tokens.store_to_buffer(&mut buffer, storer);
    }
}

impl Loadable for PersistentTokensP9 {
    fn load_from_buffer(
        mut buffer: impl Read,
        loader: &impl BlobStoreLoad,
    ) -> BlockStateResult<Self> {
        let tokens: LfmbTree<TokenIndex, PersistentTokenP9> =
            Loadable::load_from_buffer(&mut buffer, loader)?;
        // To construct the full token id to token index map, we need to read the LFMBTree from
        // the blob store. This is not ideal. If the state is to be cached after loading, we would
        // rather wait until it is cached in memory before constructing the map.
        let token_id_map = tokens
            .values(loader)
            .map(|item| {
                let (token_index, plt) = item?;
                let conf = plt.configuration.value(loader)?;
                Ok((
                    protocol_level_tokens::normalize_token_id(&conf.0.token_id),
                    token_index,
                ))
            })
            .collect::<BlockStateResult<im::HashMap<_, _>>>()?;

        Ok(Self {
            tokens,
            token_id_map,
        })
    }
}

impl Cacheable for PersistentTokensP9 {
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        self.tokens.cache_reference_values(loader)
    }
}

impl Hashable for PersistentTokensP9 {
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

/// Persistent protocol-level token on P9 and later protocols that uses the same representation.
#[derive(Debug, Clone)]
pub struct PersistentTokenP9 {
    /// Static configuration of the token that never changes.
    pub(crate) configuration: HashedCacheableRef<StoreSerialized<TokenConfiguration>>,
    /// Dynamic key-value state for values related to the token.
    pub(crate) key_value_state: HashedCacheableRef<smart_contract_trie::PersistentState>,
    /// Current circulating supply of the token.
    pub(crate) circulating_supply: StoreSerialized<RawTokenAmount>,
}

impl<'b> Cow<'b, PersistentTokenP9> {
    /// Move [`Cow`] to configuration.
    pub fn cow_project_configuration(
        self,
    ) -> Cow<'b, HashedCacheableRef<StoreSerialized<TokenConfiguration>>> {
        match self {
            Cow::Owned(this) => Cow::Owned(this.configuration),
            Cow::Borrowed(this) => Cow::Borrowed(&this.configuration),
        }
    }
}

impl Storable for PersistentTokenP9 {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        self.configuration.store_to_buffer(&mut buffer, storer);
        self.key_value_state.store_to_buffer(&mut buffer, storer);
        self.circulating_supply.store_to_buffer(&mut buffer, storer);
    }
}

impl Loadable for PersistentTokenP9 {
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

impl Cacheable for PersistentTokenP9 {
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        self.configuration.cache_reference_values(loader)?;
        self.key_value_state.cache_reference_values(loader)?;
        Ok(())
    }
}

impl Hashable for PersistentTokenP9 {
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        let config = self.configuration.hash(loader)?;
        let key_value_state = self.key_value_state.hash(loader)?;
        let state = hash::hash_of_serialization((key_value_state, self.circulating_supply.0));

        Ok(hash::hash_of_hashes(config, state))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::external::TokenAccountState;
    use concordium_base::common;
    use concordium_base::protocol_level_tokens::TokenModuleRef;
    use plt_scheduler_types::types::tokens::RawTokenAmount;

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
