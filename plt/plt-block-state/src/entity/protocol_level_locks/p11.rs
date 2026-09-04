use crate::entity::block_state::LockNotFoundByIdError;
use crate::entity::{EntityContext, EntityContextTypes};
use crate::failure::{BlockStateFailure, BlockStateResult};
use crate::persistent::blob_store::StoreSerialized;
use crate::persistent::protocol_level_locks::p11::{
    LockConfiguration, PersistentLockP11, PersistentLocksP11, lock_id_from_key, lock_id_key,
    persistent_lock_from_value, persistent_lock_value,
};
use crate::persistent::protocol_level_tokens::p9::TokenIndex;
use crate::persistent::smart_contract_trie::{MutableState, TrieState};
use crate::utils;
use concordium_base::base::AccountIndex;
use concordium_base::protocol_level_locks::LockId;

/// Transaction-local lock state. Changes are committed to its persistent representation only
/// after the transaction succeeds.
#[derive(Debug)]
pub struct LocksP11 {
    persistent: PersistentLocksP11,
    mutable: Option<MutableState>,
}

impl LocksP11 {
    /// Create transaction-local locks from the persistent lock state.
    pub fn from_persistent(persistent: PersistentLocksP11) -> Self {
        Self {
            persistent,
            mutable: None,
        }
    }

    /// Freeze and return the persistent lock state if it was modified.
    ///
    /// Returns `None` when no lock mutation occurred, preserving the existing
    /// persistent lock reference.
    pub fn commit<C: EntityContextTypes>(
        mut self,
        context: &EntityContext<C>,
    ) -> Option<PersistentLocksP11> {
        let mut locks = self.mutable.filter(|locks| locks.is_dirty())?;
        self.persistent.locks = locks.freeze(&context.store);
        Some(self.persistent)
    }

    fn mutable<C: EntityContextTypes>(
        &mut self,
        _context: &EntityContext<C>,
    ) -> BlockStateResult<&mut MutableState> {
        if self.mutable.is_none() {
            self.mutable = Some(self.persistent.locks.thaw());
        }
        self.mutable.as_mut().ok_or_else(|| {
            BlockStateFailure::Invariant("mutable locks were not initialized".to_string())
        })
    }

    /// Create a lock with `lock_id` and `configuration`.
    ///
    /// Returns a block-state failure if the lock cannot be persisted.
    pub fn create<C: EntityContextTypes>(
        &mut self,
        context: &EntityContext<C>,
        lock_id: &LockId,
        configuration: LockConfiguration,
    ) -> BlockStateResult<()> {
        let locks = self.mutable(context)?;
        create_lock(context, locks, lock_id, configuration)
    }
    /// Delete the lock identified by `lock_id`.
    ///
    /// Returns whether the lock existed, or a block-state failure if the trie cannot be updated.
    pub fn delete<C: EntityContextTypes>(
        &mut self,
        context: &EntityContext<C>,
        lock_id: &LockId,
    ) -> BlockStateResult<bool> {
        let locks = self.mutable(context)?;
        delete_lock(context, locks, lock_id)
    }
    /// Update `lock` in the transaction-local state.
    ///
    /// Returns a block-state failure if the lock cannot be persisted.
    pub fn update<C: EntityContextTypes>(
        &mut self,
        context: &EntityContext<C>,
        lock: LockP11,
    ) -> BlockStateResult<()> {
        let locks = self.mutable(context)?;
        update_lock(context, locks, lock)
    }
    /// Look up the lock identified by `lock_id`.
    ///
    /// Returns [`LockNotFoundByIdError`] when no matching lock exists, or a
    /// block-state failure if its persistent value cannot be decoded.
    pub fn by_id<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        lock_id: &LockId,
    ) -> BlockStateResult<Result<LockP11, LockNotFoundByIdError>> {
        let lock = match &self.mutable {
            Some(locks) => lock_by_id(context, locks, lock_id.clone()),
            None => lock_by_id(context, &self.persistent.locks, lock_id.clone()),
        }?;
        Ok(lock.ok_or_else(|| LockNotFoundByIdError(lock_id.clone())))
    }
    /// List all lock identifiers in no particular order.
    ///
    /// Returns a block-state failure if a stored lock identifier cannot be decoded.
    pub fn list<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
    ) -> BlockStateResult<Vec<LockId>> {
        match &self.mutable {
            Some(locks) => lock_list(context, locks),
            None => lock_list(context, &self.persistent.locks),
        }
    }
}

pub(crate) fn create_lock<C: EntityContextTypes>(
    context: &EntityContext<C>,
    locks: &mut MutableState,
    lock_id: &LockId,
    configuration: LockConfiguration,
) -> BlockStateResult<()> {
    let key = lock_id_key(lock_id);
    if locks.lookup_value(&context.store, &key).is_some() {
        return Err(BlockStateFailure::Invariant(format!(
            "lock with id {lock_id:?} already exists"
        )));
    }
    let persistent = PersistentLockP11 {
        locked_balances: Default::default(),
        configuration: StoreSerialized(configuration),
    };
    locks.insert_value(&context.store, &key, persistent_lock_value(&persistent))
}

pub(crate) fn delete_lock<C: EntityContextTypes>(
    context: &EntityContext<C>,
    locks: &mut MutableState,
    lock_id: &LockId,
) -> BlockStateResult<bool> {
    let key = lock_id_key(lock_id);
    if locks.lookup_value(&context.store, &key).is_none() {
        return Ok(false);
    }
    locks.delete_value(&context.store, &key)?;
    Ok(true)
}

pub(crate) fn update_lock<C: EntityContextTypes>(
    context: &EntityContext<C>,
    locks: &mut MutableState,
    lock: LockP11,
) -> BlockStateResult<()> {
    let key = lock_id_key(&lock.lock_id);
    if locks.lookup_value(&context.store, &key).is_none() {
        return Err(BlockStateFailure::Invariant(format!(
            "Lock not found by ID: {:?}",
            lock.lock_id
        )));
    }
    locks.insert_value(
        &context.store,
        &key,
        persistent_lock_value(&lock.persistent),
    )
}

pub(crate) fn lock_by_id<C: EntityContextTypes>(
    context: &EntityContext<C>,
    locks: &impl TrieState,
    lock_id: LockId,
) -> BlockStateResult<Option<LockP11>> {
    let key = lock_id_key(&lock_id);
    let Some(value) = locks.lookup_value(&context.store, &key) else {
        return Ok(None);
    };
    Ok(Some(LockP11 {
        lock_id,
        persistent: persistent_lock_from_value(&value)?,
    }))
}

pub(crate) fn lock_list<C: EntityContextTypes>(
    context: &EntityContext<C>,
    locks: &impl TrieState,
) -> BlockStateResult<Vec<LockId>> {
    locks
        .keys_with_prefix(&context.store, &[])?
        .into_iter()
        .map(|key| lock_id_from_key(&key))
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
        _context: &EntityContext<C>,
    ) -> BlockStateResult<utils::Cow<'_, LockConfiguration>> {
        Ok(utils::Cow::Borrowed(&self.persistent.configuration.0))
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::entity::entity_test_stub;
    use crate::persistent::protocol_level_locks::p11::{
        LockControllerConfig, LockControllerSimpleV0, LockRecipients,
    };
    use concordium_base::common::types::TransactionTime;

    fn configuration() -> LockConfiguration {
        LockConfiguration {
            recipients: LockRecipients::Any,
            expiry: TransactionTime::from(0),
            controller: LockControllerConfig::SimpleV0(
                LockControllerSimpleV0::new(Vec::new(), Vec::new(), false, None).unwrap(),
            ),
            metadata: None,
        }
    }

    #[test]
    fn committing_read_only_locks_preserves_persistent_state() {
        let context = entity_test_stub::new_no_external_context();
        let locks = LocksP11::from_persistent(PersistentLocksP11::default());

        assert!(
            locks
                .by_id(&context, &LockId::new(1, 1, 0))
                .unwrap()
                .is_err()
        );
        assert!(locks.commit(&context).is_none());
    }

    #[test]
    fn lookup_and_list_do_not_decode_unrelated_lock_values() {
        let context = entity_test_stub::new_no_external_context();
        let lock_id = LockId::new(1, 1, 0);
        let unrelated_lock_id = LockId::new(2, 1, 0);
        let missing_lock_id = LockId::new(3, 1, 0);
        let mut locks = PersistentLocksP11::default();
        let mut trie = locks.locks.thaw();
        create_lock(&context, &mut trie, &lock_id, configuration()).unwrap();
        trie.insert_value(&context.store, &lock_id_key(&unrelated_lock_id), vec![0])
            .unwrap();
        locks.locks = trie.freeze(&context.store);

        // The invalid unrelated value must not affect the requested lookup.
        assert_eq!(
            lock_by_id(&context, &locks.locks, lock_id.clone())
                .unwrap()
                .unwrap()
                .lock_id(),
            &lock_id
        );
        // The invalid unrelated value must not affect a missing lookup.
        assert!(
            lock_by_id(&context, &locks.locks, missing_lock_id)
                .unwrap()
                .is_none()
        );
        // The invalid unrelated value must not affect key-only listing.
        let mut ids = lock_list(&context, &locks.locks).unwrap();
        ids.sort();
        assert_eq!(ids, vec![lock_id, unrelated_lock_id]);
    }
}
