use crate::block_state::blob_reference::hashed_cacheable_reference::HashedCacheableRef;
use crate::block_state::blob_store::{BlobStoreLoad, StoreSerialized};
use crate::block_state::external::ExternalBlockStateOperations;
use crate::block_state::hash::Hashable;
use crate::block_state::smart_contract_trie;
use crate::block_state::utils::OwnedOrBorrowed;
use crate::block_state_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, BlockStateFailure, BlockStateResult,
};
use crate::entity::accounts::{Account, AccountWithCanonicalAddress};
use crate::entity::protocol_level_tokens::p9::{TokenEntityP9, TokenConfiguration};
use crate::persistent;
use crate::persistent::block_state::p9::PersistentBlockStateP9;
use crate::persistent::protocol_level_tokens::p9::{PersistentPlTokenP9, TokenIndex};
use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::TokenId;
use plt_scheduler_types::types::tokens::RawTokenAmount;

/// P9 block state.
#[derive(Debug)]
pub struct BlockStateP9<'a, L, E> {
    /// Persistent block state.
    pub(crate) persistent: OwnedOrBorrowed<'a, PersistentBlockStateP9>,
    /// Blob store loader.
    pub(crate) store_loader: &'a L,
    /// Part of block state that is managed externally.
    pub(crate) external: &'a E,
}

impl<'a, L: BlobStoreLoad, E: ExternalBlockStateOperations> BlockStateP9<'a, L, E> {
    /// Get the [`TokenId`]s of all protocol-level tokens.
    pub fn plt_list(&self) -> impl ExactSizeIterator<Item = BlockStateResult<TokenId>> {
        self.persistent
            .tokens
            .tokens
            .values(self.store_loader)
            .map(|item| {
                Ok(match item?.1.configuration.value(self.store_loader)? {
                    OwnedOrBorrowed::Owned(v) => v.0.token_id,
                    OwnedOrBorrowed::Borrowed(r) => r.0.token_id.clone(),
                })
            })
    }

    /// Get the token associated with a [`TokenId`] (if it exists).
    /// The token ID is case-insensitive when looking up tokens by token ID.
    ///
    /// # Arguments
    ///
    /// - `token_id` The token id to get the [`Self::Token`] of.
    pub fn token_by_id(
        &self,
        token_id: &TokenId,
    ) -> BlockStateResult<Option<TokenEntityP9<'a, L>>> {
        let token_index_option = *self.persistent.tokens.token_id_map.get(
            &persistent::protocol_level_tokens::normalize_token_id(token_id),
        );

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
    ) -> BlockStateResult<TokenEntityP9<'a, L>> {
        let normalized_token_id =
            persistent::protocol_level_tokens::normalize_token_id(&configuration.token_id);

        let persistent_token = PersistentPlTokenP9 {
            configuration: HashedCacheableRef::new(StoreSerialized(configuration)),
            key_value_state: HashedCacheableRef::new(smart_contract_trie::PersistentState::empty()),
            circulating_supply: StoreSerialized(RawTokenAmount(0)),
        };

        let token_index;
        (token_index, self.persistent.to_mut().tokens.tokens) = self
            .persistent
            .tokens
            .tokens
            .insert_value(self.store_loader, persistent_token)?;
        self.persistent
            .to_mut()
            .tokens
            .token_id_map
            .insert(normalized_token_id, token_index);

        self.thaw_token(token_index)
    }

    fn thaw_token(&self, token_index: TokenIndex) -> BlockStateResult<TokenEntityP9<'a, L>> {
        // todo ar aliasing check?

        let persistent_token = self
            .persistent
            .tokens
            .tokens
            .lookup_value(self.store_loader, token_index)?
            .ok_or_else(|| {
                BlockStateFailure::Invariant(format!("Token not found by index: {:?}", token_index))
            })?;

        let mutable_key_value_state = persistent_token
            .key_value_state
            .value(self.store_loader)?
            .thaw();

        Ok(TokenEntityP9 {
            token_index,
            persistent: persistent_token,
            mutable_key_value_state,
            store_loader: self.store_loader,
        })
    }

    pub fn freeze_token(&mut self, mut token: TokenEntityP9<'a, L>) -> BlockStateResult<()> {
        if token.mutable_key_value_state.is_dirty() {
            token.persistent.to_mut().key_value_state =
                HashedCacheableRef::new(token.mutable_key_value_state.freeze(self.store_loader));
        }

        // todo ar check if token is dirty?
        self.persistent.to_mut().tokens.tokens = self
            .persistent
            .tokens
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

        Ok(())
    }

    /// Increment the update sequence number for Protocol Level Tokens (PLT).
    ///
    /// Unlike the other chain updates this is a separate function, since there is no queue associated with PLTs.
    fn increment_plt_update_instruction_sequence_number(&mut self) {
        self.external.increment_plt_update_sequence_number()
    }

    /// Lookup the account using an account address.
    fn account_by_address(
        &self,
        address: &AccountAddress,
    ) -> Result<Account<'a, E>, AccountNotFoundByAddressError> {
        let account_index = self.external.account_index_by_account_address(address)?;
        Ok(Account {
            account_index,
            external: self.external,
        })
    }

    /// Lookup the account using an account index. Returns both the opaque account
    /// representation and the account canonical address.
    fn account_by_index(
        &self,
        account_index: AccountIndex,
    ) -> Result<AccountWithCanonicalAddress<'a, E>, AccountNotFoundByIndexError> {
        let canonical_account_address = self
            .external
            .account_canonical_address_by_account_index(account_index)?;

        let account = Account {
            account_index,
            external: self.external,
        };

        Ok(AccountWithCanonicalAddress {
            account,
            canonical_account_address,
        })
    }
}
