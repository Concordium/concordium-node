use crate::block_state::blob_reference::hashed_cacheable_reference::HashedCacheableRef;
use crate::block_state::blob_store::{BlobStoreLoad, StoreSerialized};
use crate::block_state::external::ExternalBlockStateOperations;
use crate::block_state::hash::Hashable;
use crate::block_state::smart_contract_trie;
use crate::block_state::utils::OwnedOrBorrowed;
use crate::block_state_interface::{BlockStateFailure, BlockStateResult};
use crate::entity::protocol_level_tokens::p9::{TokenEntityP9, TokenConfiguration};
use crate::entity::protocol_level_tokens::p11::TokenEntityP11;
use crate::persistent::block_state::p11::PersistentBlockStateP11;
use crate::persistent::protocol_level_tokens;
use crate::persistent::protocol_level_tokens::p9::{PersistentPlTokenP9, TokenIndex};
use concordium_base::protocol_level_tokens::TokenId;
use plt_scheduler_types::types::tokens::RawTokenAmount;

/// P11 block state.
#[derive(Debug)]
pub struct BlockStateP11<'a, L, E> {
    /// Persistent block state.
    pub(crate) persistent: OwnedOrBorrowed<'a, PersistentBlockStateP11>,
    /// Blob store loader.
    pub(crate) store_loader: &'a L,
    /// Part of block state that is managed externally.
    pub(crate) external: &'a E,
}

impl<'a, L: BlobStoreLoad, E: ExternalBlockStateOperations> BlockStateP11<'a, L, E> {
    /// Get the token associated with a [`TokenId`] (if it exists).
    /// The token ID is case-insensitive when looking up tokens by token ID.
    ///
    /// # Arguments
    ///
    /// - `token_id` The token id to get the [`Self::Token`] of.
    pub fn token_by_id(
        &self,
        token_id: &TokenId,
    ) -> BlockStateResult<Option<TokenEntityP11<'a, L>>> {
        let token_index_option = *self
            .persistent
            .tokens
            .value(self.store_loader)?
            .token_id_map
            .get(&protocol_level_tokens::normalize_token_id(token_id));

        let Some(token_index) = token_index_option else {
            return Ok(None);
        };

        self.thaw_token(token_index).map(Some)
    }

    /// Create a new token with the given configuration. The initial state will be empty
    /// and the initial supply will be 0. Returns representation of the created token.
    ///
    /// # Arguments
    ///
    /// - `configuration` The configuration for the token.
    pub fn create_token(
        &mut self,
        configuration: TokenConfiguration,
    ) -> BlockStateResult<TokenEntityP11<'a, L>> {
        let normalized_token_id =
            protocol_level_tokens::normalize_token_id(&configuration.token_id);

        let persistent_token = PersistentPlTokenP9 {
            configuration: HashedCacheableRef::new(StoreSerialized(configuration)),
            key_value_state: HashedCacheableRef::new(smart_contract_trie::PersistentState::empty()),
            circulating_supply: StoreSerialized(RawTokenAmount(0)),
        };

        let token_index;
        let mut new_tokens = self
            .persistent
            .tokens
            .value(self.store_loader)?
            .into_owned_or_clone();
        (token_index, new_tokens.tokens) = new_tokens
            .tokens
            .insert_value(self.store_loader, persistent_token)?;
        new_tokens
            .token_id_map
            .insert(normalized_token_id, token_index);

        self.persistent.to_mut().tokens = HashedCacheableRef::new(new_tokens);

        self.thaw_token(token_index)
    }

    fn thaw_token(&self, token_index: TokenIndex) -> BlockStateResult<TokenEntityP11<'a, L>> {
        // todo ar aliasing check?

        let persistent_token = self
            .persistent
            .tokens
            .value(self.store_loader)?
            .tokens
            .lookup_value(self.store_loader, token_index)?
            .ok_or_else(|| {
                BlockStateFailure::Invariant(format!("Token not found by index: {:?}", token_index))
            })?;

        let mutable_key_value_state = persistent_token
            .key_value_state
            .value(self.store_loader)?
            .thaw();

        let token_p9 = TokenEntityP9 {
            token_index,
            persistent: persistent_token,
            mutable_key_value_state,
            store_loader: self.store_loader,
        };

        Ok(TokenEntityP11 { token_p9 })
    }

    pub fn freeze_token(&mut self, mut token: TokenEntityP9<'a, L>) -> BlockStateResult<()> {
        if token.mutable_key_value_state.is_dirty() {
            token.persistent.to_mut().key_value_state =
                HashedCacheableRef::new(token.mutable_key_value_state.freeze(self.store_loader));
        }

        // todo ar check if token is dirty?
        let mut new_tokens = self
            .persistent
            .tokens
            .value(self.store_loader)?
            .into_owned_or_clone();
        new_tokens.tokens = new_tokens
            .tokens
            .update_value(self.store_loader, token.token_index, |_| {
                Ok(token.persistent.into_owned_or_clone())
            })?
            .ok_or_else(|| {
                BlockStateFailure::Invariant(format!(
                    "Token not found by index: {:?}",
                    token.token_index
                ))
            })?;
        self.persistent.to_mut().tokens = HashedCacheableRef::new(new_tokens);

        Ok(())
    }
}
