use crate::entity::{EntityContext, EntityContextTypes};
use crate::failure::{BlockStateFailure, BlockStateResult};
use crate::persistent::blob_reference::hashed_cacheable_reference::HashedCacheableRef;
use crate::persistent::blob_store::StoreSerialized;
use crate::persistent::protocol_level_locks::p11::{
    LockConfiguration, LockIndex, PersistentLockP11, PersistentLocksP11,
};
use crate::persistent::protocol_level_tokens::p9::TokenIndex;
use crate::utils;
use concordium_base::base::AccountIndex;
use concordium_base::protocol_level_locks::LockId;

/// List all non-deleted lock ids in *no particular order*.
pub(crate) fn lock_list<'a, C: EntityContextTypes>(
    _context: &EntityContext<C>,
    persistent_locks: &'a PersistentLocksP11,
) -> impl Iterator<Item = &'a LockId> {
    persistent_locks.lock_id_map.keys()
}

pub(crate) fn create_lock<C: EntityContextTypes>(
    context: &EntityContext<C>,
    persistent_locks: &mut PersistentLocksP11,
    configuration: LockConfiguration,
) -> BlockStateResult<()> {
    let lock_id = configuration.lock_id.clone();
    let persistent = PersistentLockP11 {
        locked_balances: Default::default(),
        configuration: HashedCacheableRef::new(StoreSerialized(configuration)),
    };
    let (lock_index, updated_locks) = persistent_locks
        .locks
        .insert_value(&context.store, Some(persistent))?;
    persistent_locks.locks = updated_locks;
    let existing = persistent_locks
        .lock_id_map
        .insert(lock_id.clone(), lock_index);
    if existing.is_some() {
        return Err(BlockStateFailure::Invariant(format!(
            "lock with id {:?} already exists",
            lock_id
        )));
    }

    Ok(())
}

pub(crate) fn delete_lock<C: EntityContextTypes>(
    context: &EntityContext<C>,
    persistent_locks: &mut PersistentLocksP11,
    lock_id: &LockId,
) -> BlockStateResult<bool> {
    let Some(lock_index) = persistent_locks.lock_id_map.remove(lock_id) else {
        return Ok(false);
    };
    persistent_locks.locks = persistent_locks
        .locks
        .update_value(&context.store, lock_index, |_| Ok(None))?
        .ok_or_else(|| {
            BlockStateFailure::Invariant(format!("Lock not found by index: {:?}", lock_id))
        })?;
    Ok(true)
}

pub(crate) fn update_lock<C: EntityContextTypes>(
    context: &EntityContext<C>,
    persistent_locks: &mut PersistentLocksP11,
    lock: LockP11,
) -> BlockStateResult<()> {
    persistent_locks.locks = persistent_locks
        .locks
        .update_value(&context.store, lock.lock_index, |_| {
            Ok(Some(lock.persistent))
        })?
        .ok_or_else(|| {
            BlockStateFailure::Invariant(format!("Lock not found by index: {:?}", lock.lock_index))
        })?;
    Ok(())
}

pub(crate) fn lock_by_id<C: EntityContextTypes>(
    context: &EntityContext<C>,
    persistent_locks: &PersistentLocksP11,
    lock_id: LockId,
) -> BlockStateResult<Option<LockP11>> {
    let Some(&lock_index) = persistent_locks.lock_id_map.get(&lock_id) else {
        return Ok(None);
    };
    let Some(persistent) = persistent_locks
        .locks
        .lookup_value(&context.store, lock_index)?
    else {
        return Err(BlockStateFailure::Invariant(format!(
            "No lock entry found for lock index {} ({lock_id})",
            lock_index.0
        )));
    };
    let Some(persistent) = persistent.to_owned() else {
        // Lock is deleted.
        return Ok(None);
    };
    Ok(Some(LockP11 {
        lock_index,
        persistent,
    }))
}

/// Representation of protocol-level lock on P11 and later protocols with compatible model.
#[derive(Debug)]
pub struct LockP11 {
    pub(crate) lock_index: LockIndex,
    /// Persistent model of the protocol-level lock.
    pub(crate) persistent: PersistentLockP11,
}

impl LockP11 {
    /// Get the internal block state index of the lock.
    pub fn lock_index(&self) -> LockIndex {
        self.lock_index
    }

    /// Get the configuration of the protocol-level lock.
    pub fn lock_configuration<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
    ) -> BlockStateResult<utils::Cow<'_, LockConfiguration>> {
        self.persistent
            .configuration
            .value(&context.store)
            .map(|cow| cow.cow_project())
    }

    /// Get the set of account/token balances currently tracked under the lock.
    ///
    /// Each returned pair identifies an account and token for which the lock may
    /// hold a non-zero locked balance. The corresponding amount is tracked in the
    /// token module state.
    pub fn lock_balance_refs(&self) -> Vec<(AccountIndex, TokenIndex)> {
        self.persistent.locked_balances.0.iter().cloned().collect()
    }

    /// Track that the lock holds a balance for the given account and token.
    ///
    /// This records the account/token pair in the lock state so it can later be
    /// queried through [`Self::lock_balance_refs`].
    ///
    /// # Arguments
    ///
    /// - `account_index` The index of the account whose locked balance is tracked.
    /// - `token_index` Index of the token whose locked balance is tracked.
    pub fn add_lock_balance_ref(&mut self, account_index: AccountIndex, token_index: TokenIndex) {
        self.persistent
            .locked_balances
            .0
            .insert((account_index, token_index));
    }

    /// Stop tracking that the lock holds a balance for the given account and token.
    /// This removes the account/token pair from the lock state, so it will no longer be
    /// returned by [`Self::lock_balance_refs`].
    ///
    /// # Arguments
    ///
    /// - `account_index` The index of the account whose locked balance is no longer tracked.
    /// - `token_index` Index of the token whose locked balance is no longer tracked.
    ///
    /// # Returns
    /// `true` if the account/token pair was previously tracked and has been removed,
    /// `false` if the account/token pair was not previously tracked.
    pub fn remove_lock_balance_ref(
        &mut self,
        account_index: AccountIndex,
        token_index: TokenIndex,
    ) -> bool {
        self.persistent
            .locked_balances
            .0
            .remove(&(account_index, token_index))
    }
}
