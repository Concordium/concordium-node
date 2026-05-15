use crate::block_state_interface::{BlockStateFailure, BlockStateResult};
use crate::entity::{EntityContext, EntityContextTypes};
use crate::persistent::protocol_level_locks::p11::{
    LockConfiguration, PersistentLockP11, PersistentLocksP11,
};
use crate::persistent::protocol_level_tokens::p9::TokenIndex;
use concordium_base::base::AccountIndex;
use concordium_base::protocol_level_locks::LockId;

pub(crate) fn lock_list<C: EntityContextTypes>(
    _context: &EntityContext<C>,
    persistent_locks: &PersistentLocksP11,
) -> impl ExactSizeIterator<Item = LockId> {
    persistent_locks.locks.0.keys().cloned()
}

pub(crate) fn create_lock<C: EntityContextTypes>(
    _context: &EntityContext<C>,
    persistent_locks: &mut PersistentLocksP11,
    lock_id: LockId,
    configuration: LockConfiguration,
) -> BlockStateResult<()> {
    let persistent = PersistentLockP11 {
        locked_balances: Default::default(),
        configuration,
    };
    let existing = persistent_locks.locks.0.insert(lock_id.clone(), persistent);
    if existing.is_some() {
        return Err(BlockStateFailure::Invariant(format!(
            "lock with id {:?} already exists",
            lock_id
        )));
    }

    Ok(())
}

pub(crate) fn update_lock<C: EntityContextTypes>(
    _context: &EntityContext<C>,
    persistent_locks: &mut PersistentLocksP11,
    lock: LockP11,
) -> BlockStateResult<()> {
    persistent_locks
        .locks
        .0
        .insert(lock.lock_id, lock.persistent);
    Ok(())
}

pub(crate) fn lock_by_id<C: EntityContextTypes>(
    _context: &EntityContext<C>,
    persistent_locks: &PersistentLocksP11,
    lock_id: LockId,
) -> BlockStateResult<Option<LockP11>> {
    let Some(persistent) = persistent_locks.locks.0.get(&lock_id) else {
        return Ok(None);
    };

    Ok(Some(LockP11 {
        lock_id,
        persistent: persistent.clone(),
    }))
}

/// Representation of protocol-level token on P9 and later protocols with compatible model.
#[derive(Debug)]
pub struct LockP11 {
    /// Token index
    pub(crate) lock_id: LockId,
    /// Persistent model of the protoco-level token.
    pub(crate) persistent: PersistentLockP11,
}

impl LockP11 {
    /// Get the id of the lock.
    pub fn lock_id(&self) -> &LockId {
        &self.lock_id
    }

    /// Get the configuration of the protocol-level lock.
    pub fn lock_configuration<C: EntityContextTypes>(
        &self,
        _context: &EntityContext<C>,
    ) -> LockConfiguration {
        self.persistent.configuration.clone()
    }

    /// Get the set of account/token balances currently tracked under the lock.
    ///
    /// Each returned pair identifies an account and token for which the lock may
    /// hold a non-zero locked balance. The corresponding amount is tracked in the
    /// token module state.
    pub fn lock_balance_refs(&self) -> Vec<(AccountIndex, TokenIndex)> {
        self.persistent.locked_balances.iter().cloned().collect()
    }

    /// Track that the lock holds a balance for the given account and token.
    ///
    /// This records the account/token pair in the lock state so it can later be
    /// queried through [`Self::lock_balance_refs`].
    ///
    /// # Arguments
    ///
    /// - `account` The account whose locked balance is tracked.
    /// - `token` The token whose locked balance is tracked.
    pub fn add_lock_balance_ref(&mut self, account: AccountIndex, token: TokenIndex) {
        self.persistent.locked_balances.insert((account, token));
    }
}
