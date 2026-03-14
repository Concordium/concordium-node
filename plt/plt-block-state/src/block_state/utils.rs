use std::ops::Deref;
use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};

/// Value of type `T` that is either owned or borrowed.
pub enum OwnedOrBorrowed<'a, T> {
    Owned(T),
    Borrowed(&'a T),
}

impl<T> Deref for OwnedOrBorrowed<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            OwnedOrBorrowed::Owned(v) => v,
            OwnedOrBorrowed::Borrowed(r) => r,
        }
    }
}

/// A link to a shared occurrence of a value `V`.
/// This is used in this module to construct trees, allowing for sharing of
/// values in trees and subtrees in case of the persistent tree.
///
/// This [LockRef] achieves the following properties
/// - it is cheap to clone
/// - it allows for inner mutability
/// - it is safe to use in a concurrent context.
#[derive(Debug)]
pub struct LockRef<V> {
    link: Arc<RwLock<V>>,
}

impl<V> Clone for LockRef<V> {
    fn clone(&self) -> Self {
        Self {
            link: self.link.clone(),
        }
    }
}

impl<V> LockRef<V> {
    /// Create new [`LockRef`] with given value.
    pub fn new(value: V) -> Self {
        Self {
            link: Arc::new(RwLock::new(value)),
        }
    }

    /// Get read access guard for the linked value.
    pub fn read(&self) -> RwLockReadGuard<'_, V> {
        self.link.read().expect("Link poisoned")
    }

    /// Get  write access guard for the linked value.
    pub fn write(&self) -> RwLockWriteGuard<'_, V> {
        self.link.write().expect("Link poisoned")
    }
}
