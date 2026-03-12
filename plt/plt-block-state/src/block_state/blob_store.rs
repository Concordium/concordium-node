use crate::block_state::types::blob_reference::BlobReference;
use concordium_base::common::{Buffer, Deserial, Serial};
use std::any;
use std::io::{Read, Write};

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

/// A trait implemented by types that can be loaded from a [BackingStoreLoad]
/// storage.
pub trait Loadable: Sized {
    /// Load value from the given backing store `source` bytes. If the value is a blob reference type,
    /// or contains nested blob references, these should not have their values loaded (they may
    /// be loaded by further operations on the returned value, like caching).
    fn load(source: impl Read) -> Result<Self, DecodeError>;
}

/// A trait implemented by types that can be stored to a [BackingStoreStore]
/// storage.
pub trait Storable {
    /// Store the value in the given backing store `buffer`.
    /// Notice that when storing the value, it must recursively store all values pointed to by
    /// nested blob references, if not already stored, in order to store itself.
    fn store(&self, buffer: impl Buffer, storer: impl BackingStoreStore);
}

impl<T: Storable> Storable for &T {
    fn store(&self, buffer: impl Buffer, storer: impl BackingStoreStore) {
        (*self).store(buffer, storer)
    }
}

/// Load value from the backing store at the given `location`. The value will
/// not recursively load values pointed to by nested blob references. Nested blob
/// references in the value will be loaded by when explicitly requested by
/// further operations (like caching) on the returned value.
pub fn load_from_store<T: Loadable>(
    mut loader: impl BackingStoreLoad,
    location: BlobReference,
) -> Result<T, DecodeError> {
    let mut bytes = loader.load_raw(location);
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
/// nested blob references, if not already stored, in order to store itself.
pub fn store_to_store(
    mut storer: impl BackingStoreStore,
    storable: impl Storable,
) -> BlobReference {
    let mut buffer = Vec::new();
    storable.store(&mut buffer, &mut storer);
    storer.store_raw(buffer)
}

// todo ar2 remove impls

// /// Adapter that stores its value into the backing store by plain serialization
// /// of the value.
// pub struct StoreSerialized<T>(pub T);
//
// impl<T: Serial> Storable for StoreSerialized<T> {
//     fn store(&self, mut buffer: impl Buffer, _storer: impl BackingStoreStore) {
//         self.0.serial(&mut buffer);
//     }
// }
//
// /// Adapter that loads its value from the backing store via plain serialization
// /// of the value.
// pub struct LoadSerialized<T>(pub T);
//
// impl<T: Deserial> Loadable for LoadSerialized<T> {
//     fn load(mut source: impl Read) -> Result<Self, DecodeError> {
//         let value = T::deserial(&mut source).map_err(|err| DecodeError::Decode(err.to_string()))?;
//         Ok(Self(value))
//     }
// }

/// An error that may occur when loading data from persistent storage.
#[derive(Debug, thiserror::Error)]
pub enum DecodeError {
    #[error("{0}")]
    Decode(String),
}
