//! Block state utility types and functions.

use concordium_base::common::cbor;
use concordium_base::common::cbor::{
    CborDeserialize, CborSerializationResult, SerializationOptions, UnknownMapKeys,
};
use std::ops::Deref;

/// Value of type `T` that is either owned or borrowed.
///
/// We use our own type instead of [`std::borrow::Cow`], since we don't want to require
/// `T` to implement `Clone` which `Cow` does.
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Cow<'a, T> {
    /// Value is owned
    Owned(T),
    /// Value is borrowed
    Borrowed(&'a T),
}

impl<'a, T> Cow<'a, T> {
    /// Convert the possibly owned value into an owned value, by cloning
    /// if the value is represented by a reference.
    pub fn into_owned_or_clone(self) -> T
    where
        T: Clone,
    {
        match self {
            Self::Owned(v) => v,
            Self::Borrowed(r) => r.clone(),
        }
    }

    /// Acquires a mutable reference to the owned form of the data.
    ///
    /// Clones the data if it is not already owned.
    pub fn to_mut(&mut self) -> &mut T
    where
        T: Clone,
    {
        match *self {
            Self::Borrowed(borrowed) => {
                *self = Self::Owned(borrowed.clone());
                match *self {
                    Self::Borrowed(..) => unreachable!(),
                    Self::Owned(ref mut owned) => owned,
                }
            }
            Self::Owned(ref mut owned) => owned,
        }
    }
}

impl<T> Deref for Cow<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            Cow::Owned(v) => v,
            Cow::Borrowed(r) => r,
        }
    }
}

/// Decode given CBOR using decode options set to suit the token module. The decode options
/// will generally be strict.
pub fn cbor_decode<T: CborDeserialize>(cbor: impl AsRef<[u8]>) -> CborSerializationResult<T> {
    let decode_options = SerializationOptions {
        unknown_map_keys: UnknownMapKeys::Fail,
    };
    cbor::cbor_decode_with_options(cbor, decode_options)
}
