//! Block state utility types and functions.

use std::ops::Deref;

/// Value of type `T` that is either owned or borrowed.
///
/// We use our own type instead of `Cow`, since we don't want to require
/// `T` to implement `Clone` which `Cow` does.
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum OwnedOrBorrowed<'a, T> {
    Owned(T),
    Borrowed(&'a T),
}

impl<'a, T> OwnedOrBorrowed<'a, T> {
    /// Convert the possibly owned value into an owned value, by cloning
    /// if the value is represented by a reference.
    pub fn into_owned(self) -> T
    where
        T: Clone,
    {
        match self {
            OwnedOrBorrowed::Owned(v) => v,
            OwnedOrBorrowed::Borrowed(r) => r.clone(),
        }
    }

    /// Return [`Self`] with new lifetime, if it is owned, else `None`.
    pub fn new_lifetime_if_owned<'b>(self) -> Option<OwnedOrBorrowed<'b, T>> {
        match self {
            OwnedOrBorrowed::Owned(v) => Some(OwnedOrBorrowed::Owned(v)),
            OwnedOrBorrowed::Borrowed(_) => None,
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
