use crate::block_state::blob_reference::BlobReference;
use concordium_base::common;
use concordium_base::common::{Buffer, Deserial, Get, Put, Serial};
use std::any;
use std::io::Read;

/// Trait implemented by types that can be used to store binary data, and return
/// a handle for loading data. Dual to [`BackingStoreLoad`].
pub trait BackingStoreStore {
    /// Store the provided value and return a reference that can be used
    /// to load it.
    fn store_raw(&mut self, data: impl AsRef<[u8]>) -> BlobReference;
}


/// Trait implemented by types that can load data from given locations.
/// Dual to [`BackingStoreStore`].
pub trait BackingStoreLoad {
    /// Load the provided value from the given location. The implementation of
    /// this should match [BackingStoreStore::store_raw].
    fn load_raw(&mut self, location: BlobReference) -> Vec<u8>;
}


/// A trait implemented by types that can be loaded from a [backing store](BackingStoreLoad).
pub trait Loadable: Sized {
    /// Load value from the bytes in the given `buffer` that has been retrieved from the backing store.
    /// If the value is composed of [`BlobReference`]s, these references should not
    /// have their values loaded into memory as a result of the [`Self::load_from_buffer`] operation.
    /// As such, [`Self::load_from_buffer`] is a "shallow" operation.
    fn load_from_buffer(buffer: impl Read) -> Result<Self, DecodeError>;
}

/// A trait implemented by types that can be stored to a [backing store](BackingStoreStore).
pub trait Storable {
    /// Store the value in the given `buffer` that will be written to the backing store.
    /// Notice that when storing the value, the operation must recursively store all
    /// values pointed to by the [`BlobReference`]s the value may be composed of,
    /// if these values are not already represented in the backing store.
    /// As such, `store` is a "deep" operation.
    fn store_to_buffer(&self, buffer: impl Buffer, storer: &mut impl BackingStoreStore);
}

impl<T: Storable> Storable for &T {
    fn store_to_buffer(&self, buffer: impl Buffer, storer: &mut  impl BackingStoreStore) {
        (**self).store_to_buffer(buffer, storer)
    }
}

impl<T: Storable> Storable for &mut T {
    fn store_to_buffer(&self, buffer: impl Buffer, storer: &mut impl BackingStoreStore) {
        (**self).store_to_buffer(buffer, storer)
    }
}

#[derive(Debug, Clone)]
pub struct StoreSerialized<T>(pub T);

impl<T: Deserial> Loadable for StoreSerialized<T> {
    fn load_from_buffer(mut buffer: impl Read) -> Result<Self, DecodeError> {
        Ok(StoreSerialized(buffer.get().into_decode_result()?))
    }
}

impl<T: Serial> Storable for StoreSerialized<T> {
    fn store_to_buffer(&self, mut buffer: impl Buffer, _storer: &mut impl BackingStoreStore) {
        buffer.put(&self.0);
    }
}

/// Load value from the backing store at the given `location`. The value will
/// not recursively load values pointed to by [`BlobReference`]s the value
/// may be composed of as part of this operation.
pub fn load_from_store<T: Loadable>(
     loader: &mut impl BackingStoreLoad,
    location: BlobReference,
) -> Result<T, DecodeError> {
    let bytes = loader.load_raw(location);
    let mut bytes_slice = bytes.as_slice();
    let value = T::load_from_buffer(&mut bytes_slice)?;
    if !bytes_slice.is_empty() {
        return Err(DecodeError(format!(
            "Bytes remaining after loading value of type {} from blob store",
            any::type_name::<T>()
        )));
    };
    Ok(value)
}

/// Store the value in the backing store, and return the reference to the stored value.
/// Notice that when storing the value, it will recursively store all values pointed to by
/// [`BlobReference`]s it may be composed of, if these values are not already represented
/// in the backing store.
pub fn store_to_store(
     storer: &mut impl BackingStoreStore,
    storable: impl Storable,
) -> BlobReference {
    let mut buffer = Vec::new();
    storable.store_to_buffer(&mut buffer,  storer);
    storer.store_raw(buffer)
}

/// Error decoding bytes that has been retrieved from the backing store.
#[derive(Debug, thiserror::Error)]
#[error("Error decoding bytes retrived from backing store: {0}")]
pub struct DecodeError(String);

impl From<String> for DecodeError {
    fn from(value: String) -> Self {
        Self(value)
    }
}

pub trait ParseResultExt<T> {
    fn into_decode_result(self) -> Result<T, DecodeError>;
}

impl<T> ParseResultExt<T> for common::ParseResult<T> {
    fn into_decode_result(self) -> Result<T, DecodeError> {
        self.map_err(|err| {
            DecodeError(format!(
                "Error loading value of type {} from blob store: {}",
                any::type_name::<T>(),
                err
            ))
        })
    }
}
