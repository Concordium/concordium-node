//! Block state utility types and functions.

use std::ops::Deref;

/// Value of type `T` that is either owned or borrowed.
pub enum OwnedOrBorrowed<'a, T> {
    Owned(T),
    Borrowed(&'a T),
}

impl<'a, T: Clone> OwnedOrBorrowed<'a, T> {
    /// Convert the possibly owned value into an owned value, by cloning
    /// if the value is represented by a reference.
    pub fn into_owned(self) -> T {
        match self {
            OwnedOrBorrowed::Owned(v) => v,
            OwnedOrBorrowed::Borrowed(r) => r.clone(),
        }
    }
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
