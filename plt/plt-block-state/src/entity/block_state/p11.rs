use crate::block_state::blob_reference::hashed_cacheable_reference::HashedCacheableRef;
use crate::block_state::blob_store::StoreSerialized;
use crate::block_state::external::{ExternalBlockStateOperations, ExternalBlockStateQuery};
use crate::block_state::smart_contract_trie;
use crate::block_state::utils::Cow;
use crate::block_state_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, BlockStateFailure,
    BlockStateResult, TokenNotFoundByIdError,
};
use crate::entity::accounts::{Account, AccountWithCanonicalAddress};
use crate::entity::protocol_level_tokens::p9::{TokenConfiguration, TokenP9};
use crate::entity::protocol_level_tokens::p11::TokenP11;
use crate::entity::{EntityContext, EntityContextTypes};
use crate::persistent::block_state::p11::PersistentBlockStateP11;
use crate::persistent::protocol_level_tokens;
use crate::persistent::protocol_level_tokens::p9::{PersistentTokenP9, TokenIndex};
use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::TokenId;
use plt_scheduler_types::types::tokens::RawTokenAmount;

/// P11 block state.
#[derive(Debug)]
pub struct BlockStateP11<'a> {
    /// Persistent block state.
    pub(crate) persistent: Cow<'a, PersistentBlockStateP11>,
}

impl<'a> BlockStateP11<'a> {
    /// Get the token associated with a [`TokenId`] (if it exists).
    /// The token ID is case-insensitive when looking up tokens by token ID.
    ///
    /// If the token is changed, it must be written back with [`Self::update_token`]
    /// for applying the changes.
    ///
    /// # Arguments
    ///
    /// - `token_id` The token id to get the [`Self::Token`] of.
    pub fn token_by_id<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        token_id: &TokenId,
    ) -> BlockStateResult<Result<TokenP11<'_>, TokenNotFoundByIdError>> {
        let tokens = self.persistent.tokens.value(&context.loader)?;
        let token_index_option = tokens
            .token_id_map
            .get(&protocol_level_tokens::normalize_token_id(token_id));

        let Some(&token_index) = token_index_option else {
            return Ok(Err(TokenNotFoundByIdError(token_id.clone())));
        };

        self.token_by_index(context, token_index).map(Ok)
    }

    /// Create a new token with the given configuration. The initial state will be empty
    /// and the initial supply will be 0. Returns representation of the created token.
    ///
    /// # Arguments
    ///
    /// - `configuration` The configuration for the token.
    pub fn create_token<C: EntityContextTypes>(
        &mut self,
        context: &EntityContext<C>,
        configuration: TokenConfiguration,
    ) -> BlockStateResult<TokenIndex> {
        let normalized_token_id =
            protocol_level_tokens::normalize_token_id(&configuration.token_id);

        let persistent_token = PersistentTokenP9 {
            configuration: HashedCacheableRef::new(StoreSerialized(configuration)),
            key_value_state: HashedCacheableRef::new(smart_contract_trie::PersistentState::empty()),
            circulating_supply: StoreSerialized(RawTokenAmount(0)),
        };

        let token_index;
        let mut new_tokens = self.persistent.tokens.value(&context.loader)?.into_owned();
        (token_index, new_tokens.tokens) = new_tokens
            .tokens
            .insert_value(&context.loader, persistent_token)?;
        new_tokens
            .token_id_map
            .insert(normalized_token_id, token_index);

        self.persistent.to_mut().tokens = HashedCacheableRef::new(new_tokens);

        Ok(token_index)
    }

    /// Get the token with the given [`TokenIndex`].
    /// Returns a [`BlockStateFailure`] if the token does not exist.
    ///
    /// If the token is changed, it must be written back with [`Self::update_token`]
    /// for applying the changes.
    ///
    /// # Arguments
    ///
    /// - `token_index` The index of the token.
    pub fn token_by_index<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        token_index: TokenIndex,
    ) -> BlockStateResult<TokenP11<'_>> {
        let persistent_token = self
            .persistent
            .tokens
            .value(&context.loader)?
            .cow_project_tokens()
            .bind_lookup_value(
                &context.loader,
                token_index,
                "token by index in PersistentTokensP9",
            )?
            .ok_or_else(|| {
                BlockStateFailure::Invariant(format!("Token not found by index: {:?}", token_index))
            })?;

        let mutable_key_value_state = persistent_token
            .key_value_state
            .value(&context.loader)?
            .thaw();

        let token_p9 = TokenP9 {
            token_index,
            persistent: persistent_token,
            mutable_key_value_state,
        };

        Ok(TokenP11 { token_p9 })
    }

    /// Update the token in the block state. Any modifications
    /// to [`TokenP11`] are not applied before the token is updated.
    pub fn update_token<C: EntityContextTypes>(
        &mut self,
        context: &EntityContext<C>,
        mut token: TokenP11<'_>,
    ) -> BlockStateResult<()> {
        if token.token_p9.mutable_key_value_state.is_dirty() {
            token.token_p9.persistent.to_mut().key_value_state = HashedCacheableRef::new(
                token
                    .token_p9
                    .mutable_key_value_state
                    .freeze(&context.loader),
            );
        }

        let mut new_tokens = self.persistent.tokens.value(&context.loader)?.into_owned();
        new_tokens.tokens = new_tokens
            .tokens
            .update_value(&context.loader, token.token_p9.token_index, |_| {
                Ok(token.token_p9.persistent.into_owned())
            })?
            .ok_or_else(|| {
                BlockStateFailure::Invariant(format!(
                    "Token not found by index: {:?}",
                    token.token_p9.token_index
                ))
            })?;
        self.persistent.to_mut().tokens = HashedCacheableRef::new(new_tokens);

        Ok(())
    }

    /// Increment the update sequence number for Protocol Level Tokens (PLT).
    ///
    /// Unlike the other chain updates this is a separate function, since there is no queue associated with PLTs.
    pub fn increment_plt_update_instruction_sequence_number<C: EntityContextTypes>(
        &mut self,
        context: &mut EntityContext<C>,
    ) {
        context.external.increment_plt_update_sequence_number()
    }

    /// Lookup the account using an account address.
    pub fn account_by_address<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        address: &AccountAddress,
    ) -> Result<Account, AccountNotFoundByAddressError> {
        let account_index = context.external.account_index_by_account_address(address)?;
        Ok(Account { account_index })
    }

    /// Lookup the account using an account index. Returns both the opaque account
    /// representation and the account canonical address.
    pub fn account_by_index<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        account_index: AccountIndex,
    ) -> Result<AccountWithCanonicalAddress, AccountNotFoundByIndexError> {
        let canonical_account_address = context
            .external
            .account_canonical_address_by_account_index(account_index)?;

        let account = Account { account_index };

        Ok(AccountWithCanonicalAddress {
            account,
            canonical_account_address,
        })
    }
}
