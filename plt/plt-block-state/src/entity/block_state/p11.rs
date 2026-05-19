use crate::block_state_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, BlockStateResult,
    LockNotFoundByIdError, TokenNotFoundByIdError,
};
use crate::entity::accounts::{Account, AccountWithCanonicalAddress};
use crate::entity::block_state::Accounts;
use crate::entity::protocol_level_locks::p11::LockP11;
use crate::entity::protocol_level_tokens::p11::TokenP11;
use crate::entity::{
    EntityContext, EntityContextTypes, protocol_level_locks, protocol_level_tokens,
};
use crate::external::{ExternalBlockStateOperations, ExternalBlockStateQuery};
use crate::persistent::blob_reference::hashed_cacheable_reference::HashedCacheableRef;
use crate::persistent::block_state::p11::PersistentBlockStateP11;
use crate::persistent::protocol_level_locks::p11::LockConfiguration;
use crate::persistent::protocol_level_tokens::p9::{TokenConfiguration, TokenIndex};
use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_locks::LockId;
use concordium_base::protocol_level_tokens::TokenId;

/// P11 block state.
#[derive(Debug, Default, Clone)]
pub struct BlockStateP11 {
    /// Persistent block state.
    pub persistent: PersistentBlockStateP11,
}

impl BlockStateP11 {
    /// Get the [`TokenId`]s of all protocol-level tokens.
    pub fn plt_list<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
    ) -> BlockStateResult<Vec<TokenId>> {
        protocol_level_tokens::p9::plt_list(
            context,
            &*self.persistent.tokens.value(&context.loader)?,
        )
        .collect::<BlockStateResult<Vec<_>>>()
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
    /// to [`TokenP11`] are only applied when the token is updated.
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

    /// Create a new PLT lock with the given configuration. The initial state will be empty.
    /// Returns [`BlockStateFailure::Invariant`] if a lock with the given id already exists.
    ///
    /// # Arguments
    ///
    /// - `lock_id` The ID of the PLT lock.
    /// - `configuration` The configuration for the PLT lock.
    pub fn create_lock<C: EntityContextTypes>(
        &mut self,
        context: &EntityContext<C>,
        lock_id: LockId,
        configuration: LockConfiguration,
    ) -> BlockStateResult<()> {
        let mut new_locks = self.persistent.locks.value(&context.loader)?.into_owned();
        protocol_level_locks::p11::create_lock(context, &mut new_locks, lock_id, configuration)?;

        self.persistent.locks = HashedCacheableRef::new(new_locks);

        Ok(())
    }

    /// Delete the lock with the given [`LockId`] if it exists. Returns the
    /// deleted lock if it existed, or `None` if it did not exist.
    ///
    /// # Arguments
    /// - `lock_id` The ID of the PLT lock to delete.
    pub fn delete_lock<C: EntityContextTypes>(
        &mut self,
        context: &EntityContext<C>,
        lock_id: &LockId,
    ) -> BlockStateResult<Option<LockP11>> {
        let mut new_locks = self.persistent.locks.value(&context.loader)?.into_owned();
        let existing = protocol_level_locks::p11::delete_lock(context, &mut new_locks, lock_id)?;
        if existing.is_some() {
            // We only need to update the locks if a lock was actually deleted,
            // otherwise we would be unnecessarily updating the block state.
            self.persistent.locks = HashedCacheableRef::new(new_locks);
        }

        Ok(existing)
    }

    /// Get the [`LockId`]s of all protocol-level locks registered on the chain at the
    /// end of the block.
    pub fn lock_list<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
    ) -> BlockStateResult<Vec<LockId>> {
        Ok(protocol_level_locks::p11::lock_list(
            context,
            &*self.persistent.locks.value(&context.loader)?,
        )
        .collect())
    }

    /// Get the lock associated with a [`LockId`] (if it exists).
    ///
    /// # Arguments
    ///
    /// - `lock_id` The lock id to get the [`Self::Lock`] of.
    pub fn lock_by_id<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        lock_id: &LockId,
    ) -> BlockStateResult<Result<LockP11, LockNotFoundByIdError>> {
        let lock_option = protocol_level_locks::p11::lock_by_id(
            context,
            &*self.persistent.locks.value(&context.loader)?,
            lock_id.clone(),
        )?;

        Ok(lock_option.ok_or_else(|| LockNotFoundByIdError(lock_id.clone())))
    }

    /// Update the lock in the block state. Any modifications
    /// to [`LockP11`] are only applied when the lock is updated.
    pub fn update_lock<C: EntityContextTypes>(
        &mut self,
        context: &EntityContext<C>,
        lock: LockP11,
    ) -> BlockStateResult<()> {
        let mut new_locks = self.persistent.locks.value(&context.loader)?.into_owned();
        protocol_level_locks::p11::update_lock(context, &mut new_locks, lock)?;
        self.persistent.locks = HashedCacheableRef::new(new_locks);

        Ok(())
    }
}

impl Accounts for BlockStateP11 {
    fn account_by_address<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        address: &AccountAddress,
    ) -> Result<Account, AccountNotFoundByAddressError> {
        let account_index = context.external.account_index_by_account_address(address)?;
        Ok(Account::from_existing_account(account_index))
    }

    fn account_by_index<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        account_index: AccountIndex,
    ) -> Result<AccountWithCanonicalAddress, AccountNotFoundByIndexError> {
        let canonical_account_address = context
            .external
            .account_canonical_address_by_account_index(account_index)?;

        let account = Account::from_existing_account(account_index);

        Ok(AccountWithCanonicalAddress {
            account,
            canonical_account_address,
        })
    }
}
