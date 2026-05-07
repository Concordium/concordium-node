use crate::block_state::external::{ExternalBlockStateOperations, ExternalBlockStateQuery};
use crate::block_state::utils::Cow;
use crate::block_state_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, BlockStateResult,
    TokenNotFoundByIdError,
};
use crate::entity::accounts::{Account, AccountWithCanonicalAddress};
use crate::entity::protocol_level_tokens::p9::{TokenConfiguration, TokenP9};
use crate::entity::{EntityContext, EntityContextTypes, protocol_level_tokens};
use crate::persistent::block_state::p9::PersistentBlockStateP9;
use crate::persistent::protocol_level_tokens::TokenIndex;
use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::TokenId;
// todo ar fjern lifetimes

/// P9 block state.
#[derive(Debug)]
pub struct BlockStateP9<'a> {
    /// Persistent block state.
    pub(crate) persistent: Cow<'a, PersistentBlockStateP9>,
}

impl<'a> BlockStateP9<'a> {
    /// Get the [`TokenId`]s of all protocol-level tokens.
    pub fn plt_list<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
    ) -> impl ExactSizeIterator<Item = BlockStateResult<Cow<'_, TokenId>>> {
        protocol_level_tokens::p9::plt_list(context, &self.persistent.tokens)
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
    ) -> BlockStateResult<Result<TokenP9<'_>, TokenNotFoundByIdError>> {
        let token_index_option =
            protocol_level_tokens::p9::token_index_by_id(&self.persistent.tokens, token_id);

        let Some(token_index) = token_index_option else {
            return Ok(Err(TokenNotFoundByIdError(token_id.clone())));
        };

        self.token_by_index(context, token_index).map(Ok)
    }

    /// Create a new token with the given configuration. The initial state will be empty
    /// and the initial supply will be 0. Returns index of the created token.
    ///
    /// # Arguments
    ///
    /// - `configuration` The configuration for the token.
    pub fn create_token<C: EntityContextTypes>(
        &mut self,
        context: &EntityContext<C>,
        configuration: TokenConfiguration,
    ) -> BlockStateResult<TokenIndex> {
        protocol_level_tokens::p9::create_token(
            context,
            &mut self.persistent.to_mut().tokens,
            configuration,
        )
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
    ) -> BlockStateResult<TokenP9<'_>> {
        protocol_level_tokens::p9::token_by_index(context, &self.persistent.tokens, token_index)
    }

    /// Update the token in the block state. Any modifications
    /// to [`TokenP9`] are not applied before the token is updated.
    pub fn update_token<C: EntityContextTypes>(
        &mut self,
        context: &EntityContext<C>,
        token: TokenP9<'_>,
    ) -> BlockStateResult<()> {
        protocol_level_tokens::p9::update_token(
            context,
            &mut self.persistent.to_mut().tokens,
            token,
        )
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
