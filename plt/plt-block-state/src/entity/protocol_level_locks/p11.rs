use crate::block_state_interface::{BlockStateFailure, BlockStateResult};
use crate::entity::{EntityContext, EntityContextTypes};
use crate::persistent::protocol_level_locks::p11::{
    LockConfiguration, PersistentLockP11, PersistentLocksP11,
};
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
}
