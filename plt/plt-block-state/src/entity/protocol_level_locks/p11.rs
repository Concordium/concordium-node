use crate::entity::{EntityContext, EntityContextTypes};
use crate::failure::{BlockStateFailure, BlockStateResult};
use crate::persistent::blob_store::StoreSerialized;
use crate::persistent::protocol_level_locks::p11::{
    BalanceReferenceKey, LockConfig, PersistentLockP11, PersistentLocksP11,
};
use crate::persistent::protocol_level_tokens::p9::TokenIndex;
use crate::utils;
use concordium_base::base::AccountIndex;
use concordium_base::protocol_level_locks::LockId;

pub(crate) fn create_lock<C: EntityContextTypes>(
    context: &EntityContext<C>,
    persistent_locks: &mut PersistentLocksP11,
    lock_id: &LockId,
    configuration: LockConfig,
) -> BlockStateResult<()> {
    if persistent_locks.contains_key(&context.store, lock_id)? {
        return Err(BlockStateFailure::Invariant(format!(
            "lock with id {lock_id:?} already exists"
        )));
    }
    *persistent_locks = persistent_locks.insert_or_update_entry(
        &context.store,
        lock_id,
        PersistentLockP11::new(configuration),
    )?;
    Ok(())
}

pub(crate) fn delete_lock<C: EntityContextTypes>(
    context: &EntityContext<C>,
    persistent_locks: &mut PersistentLocksP11,
    lock_id: &LockId,
) -> BlockStateResult<bool> {
    let Some(locks) = persistent_locks.delete_entry(&context.store, lock_id)? else {
        return Ok(false);
    };
    *persistent_locks = locks;
    Ok(true)
}

pub(crate) fn update_lock<C: EntityContextTypes>(
    context: &EntityContext<C>,
    persistent_locks: &mut PersistentLocksP11,
    lock: LockP11,
) -> BlockStateResult<()> {
    if !persistent_locks.contains_key(&context.store, &lock.lock_id)? {
        return Err(BlockStateFailure::Invariant(format!(
            "Lock not found by ID: {:?}",
            lock.lock_id
        )));
    }
    *persistent_locks =
        persistent_locks.insert_or_update_entry(&context.store, &lock.lock_id, lock.persistent)?;
    Ok(())
}

pub(crate) fn lock_by_id<C: EntityContextTypes>(
    context: &EntityContext<C>,
    locks: &PersistentLocksP11,
    lock_id: LockId,
) -> BlockStateResult<Option<LockP11>> {
    let Some(persistent) = locks.lookup_value(&context.store, &lock_id)? else {
        return Ok(None);
    };
    Ok(Some(LockP11 {
        lock_id,
        persistent: persistent.into_owned(),
    }))
}

pub(crate) fn lock_list<C: EntityContextTypes>(
    context: &EntityContext<C>,
    locks: &PersistentLocksP11,
) -> BlockStateResult<Vec<LockId>> {
    locks
        .iter(&context.store)
        .map(|entry| entry.map(|(lock_id, _)| lock_id))
        .collect()
}

/// Representation of protocol-level lock on P11 and later protocols with compatible model.
#[derive(Debug)]
pub struct LockP11 {
    pub(crate) lock_id: LockId,
    /// Persistent model of the protocol-level lock.
    pub(crate) persistent: PersistentLockP11,
}

impl LockP11 {
    /// Get the ID of the lock.
    pub fn lock_id(&self) -> &LockId {
        &self.lock_id
    }

    /// Get the configuration of the protocol-level lock.
    pub fn lock_configuration<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
    ) -> BlockStateResult<utils::Cow<'_, LockConfig>> {
        Ok(self.persistent.configuration.value(&context.store)?.map(
            |configuration| configuration.0,
            |configuration| &configuration.0,
        ))
    }

    /// Iterate the account/token balances currently tracked under the lock.
    ///
    /// Each pair identifies an account and token for which the lock may hold a
    /// non-zero locked balance. The corresponding amount is tracked in the token
    /// module state. Entries are read lazily from the persistent trie.
    pub fn iter_lock_balance_refs<'a, C: EntityContextTypes>(
        &'a self,
        context: &'a EntityContext<C>,
    ) -> impl Iterator<Item = BlockStateResult<(AccountIndex, TokenIndex)>> + 'a {
        self.persistent
            .locked_balances
            .iter(&context.store)
            .map(|entry| entry.map(|(key, _)| (key.0, key.1)))
    }

    /// Get the account/token balances currently tracked under the lock.
    ///
    /// This collects all balance references. Use [`Self::iter_lock_balance_refs`]
    /// when references can be processed one at a time.
    pub fn lock_balance_refs<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
    ) -> BlockStateResult<Vec<(AccountIndex, TokenIndex)>> {
        self.iter_lock_balance_refs(context).collect()
    }

    /// Track that the lock holds a balance for the given account and token.
    ///
    /// Returns an error if the persistent trie cannot be accessed.
    pub fn add_lock_balance_ref<C: EntityContextTypes>(
        &mut self,
        context: &EntityContext<C>,
        account_index: AccountIndex,
        token_index: TokenIndex,
    ) -> BlockStateResult<()> {
        self.persistent.locked_balances = self.persistent.locked_balances.insert_or_update_entry(
            &context.store,
            &BalanceReferenceKey(account_index, token_index),
            StoreSerialized(()),
        )?;
        Ok(())
    }

    /// Stop tracking that the lock holds a balance for the given account and token.
    ///
    /// Returns whether the pair was tracked, or an error if the persistent trie cannot be accessed.
    pub fn remove_lock_balance_ref<C: EntityContextTypes>(
        &mut self,
        context: &EntityContext<C>,
        account_index: AccountIndex,
        token_index: TokenIndex,
    ) -> BlockStateResult<bool> {
        let Some(references) = self.persistent.locked_balances.delete_entry(
            &context.store,
            &BalanceReferenceKey(account_index, token_index),
        )?
        else {
            return Ok(false);
        };
        self.persistent.locked_balances = references;
        Ok(true)
    }

    /// Returns whether the lock tracks no balance references.
    pub fn has_no_balance_refs(&self) -> bool {
        self.persistent.locked_balances.size() == 0
    }
}
