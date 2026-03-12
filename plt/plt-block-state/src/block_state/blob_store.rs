use crate::block_state::blob_reference::BlobReference;
use concordium_base::common;
use concordium_base::common::Buffer;
use std::any;
use std::io::Read;

/// Trait implemented by types that can be used to store binary data, and return
/// a handle for loading data. Dual to [`BackingStoreLoad`].
pub trait BackingStoreStore {
    /// Store the provided value and return a reference that can be used
    /// to load it.
    fn store_raw(&mut self, data: impl AsRef<[u8]>) -> BlobReference;
}

impl<T: BackingStoreStore> BackingStoreStore for &mut T {
    fn store_raw(&mut self, data: impl AsRef<[u8]>) -> BlobReference {
        (*self).store_raw(data)
    }
}

/// Trait implemented by types that can load data from given locations.
/// Dual to [`BackingStoreStore`].
pub trait BackingStoreLoad {
    /// Load the provided value from the given location. The implementation of
    /// this should match [BackingStoreStore::store_raw].
    fn load_raw(&mut self, location: BlobReference) -> Vec<u8>;
}

impl<T: BackingStoreLoad> BackingStoreLoad for &mut T {
    fn load_raw(&mut self, location: BlobReference) -> Vec<u8> {
        (*self).load_raw(location)
    }
}

/// A trait implemented by types that can be loaded from a [backing store](BackingStoreLoad).
pub trait Loadable: Sized {
    /// Load value from the given `source` bytes that has been retrieved from the backing store.
    /// If the value is composed of [`BlobReference`]s, these references should not
    /// have their values loaded into memory as a result of the [`Self::load`] operation.
    /// As such, [`Self::load`] is a "shallow" operation.
    fn load(source: impl Read) -> Result<Self, DecodeError>;
}

/// A trait implemented by types that can be stored to a [backing store](BackingStoreStore).
pub trait Storable {
    /// Store the value in the given `buffer` that will be written to the backing store.
    /// Notice that when storing the value, the operation must recursively store all
    /// values pointed to by the [`BlobReference`]s the value may be composed of,
    /// if these values are not already represented in the backing store.
    /// As such, `store` is a "deep" operation.
    fn store(&self, buffer: impl Buffer, storer: impl BackingStoreStore);
}

impl<T: Storable> Storable for &T {
    fn store(&self, buffer: impl Buffer, storer: impl BackingStoreStore) {
        (*self).store(buffer, storer)
    }
}

/// Load value from the backing store at the given `location`. The value will
/// not recursively load values pointed to by [`BlobReference`]s the value
/// may be composed of as part of this operation.
pub fn load_from_store<T: Loadable>(
    mut loader: impl BackingStoreLoad,
    location: BlobReference,
) -> Result<T, DecodeError> {
    let bytes = loader.load_raw(location);
    let mut bytes_slice = bytes.as_slice();
    let value = T::load(&mut bytes_slice)?;
    if !bytes_slice.is_empty() {
        return Err(DecodeError::Decode(format!(
            "Bytes remaining after loading {} from blob store",
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
    mut storer: impl BackingStoreStore,
    storable: impl Storable,
) -> BlobReference {
    let mut buffer = Vec::new();
    storable.store(&mut buffer, &mut storer);
    storer.store_raw(buffer)
}

/// An error that may occur when loading data from persistent storage.
#[derive(Debug, thiserror::Error)]
pub enum DecodeError {
    #[error("{0}")]
    Decode(String),
}

pub trait ParseResultExt<T> {
    fn into_decode_result(self) -> Result<T, DecodeError>;
}

impl<T> ParseResultExt<T> for common::ParseResult<T> {
    fn into_decode_result(self) -> Result<T, DecodeError> {
        self.map_err(|err| DecodeError::Decode(err.to_string()))
    }
}
