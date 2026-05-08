use crate::block_state::blob_reference::hashed_cacheable_reference::HashedCacheableRef;
use crate::block_state_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, BlockStateResult,
    TokenNotFoundByIdError,
};
use crate::entity::accounts::{Account, AccountWithCanonicalAddress};
use crate::entity::protocol_level_tokens::p9::{TokenConfiguration, TokenIndex};
use crate::entity::protocol_level_tokens::p11::TokenP11;
use crate::entity::{EntityContext, EntityContextTypes, protocol_level_tokens};
use crate::persistent::block_state::p11::PersistentBlockStateP11;
use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::TokenId;
use crate::external::{ExternalBlockStateOperations, ExternalBlockStateQuery};

/// P11 block state.
#[derive(Debug, Default)]
pub struct BlockStateP11 {
    /// Persistent block state.
    pub(crate) persistent: PersistentBlockStateP11,
}

impl BlockStateP11 {
    /// Get the [`TokenId`]s of all protocol-level tokens.
    pub fn plt_list<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
    ) -> BlockStateResult<impl ExactSizeIterator<Item = BlockStateResult<TokenId>>> {
        let persistent_tokens = self.persistent.tokens.value(&context.loader)?.into_owned();
        Ok(
            protocol_level_tokens::p9::plt_list(context, &persistent_tokens)
                .collect::<Vec<_>>()
                .into_iter(),
        ) // todo ar fix iterator
    }

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
    ) -> BlockStateResult<Result<TokenP11, TokenNotFoundByIdError>> {
        let token_index_option = protocol_level_tokens::p9::token_index_by_id(
            &*self.persistent.tokens.value(&context.loader)?,
            token_id,
        );

        let Some(token_index) = token_index_option else {
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
        let mut new_tokens = self.persistent.tokens.value(&context.loader)?.into_owned();
        let token_index =
            protocol_level_tokens::p9::create_token(context, &mut new_tokens, configuration)?;
        self.persistent.tokens = HashedCacheableRef::new(new_tokens);

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
    ) -> BlockStateResult<TokenP11> {
        let token_p9 = protocol_level_tokens::p9::token_by_index(
            context,
            &*self.persistent.tokens.value(&context.loader)?,
            token_index,
        )?;

        Ok(TokenP11 { token_p9 })
    }

    /// Update the token in the block state. Any modifications
    /// to [`TokenP11`] are not applied before the token is updated.
    pub fn update_token<C: EntityContextTypes>(
        &mut self,
        context: &EntityContext<C>,
        token: TokenP11,
    ) -> BlockStateResult<()> {
        let mut new_tokens = self.persistent.tokens.value(&context.loader)?.into_owned();
        protocol_level_tokens::p9::update_token(context, &mut new_tokens, token.token_p9)?;
        self.persistent.tokens = HashedCacheableRef::new(new_tokens);

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
