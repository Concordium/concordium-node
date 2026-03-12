use crate::block_state::types::reference::BlobReference;
use concordium_base::common;
use concordium_base::common::{Deserial, Serial};
use std::io::Write;

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
    /// Load value from the backing store at the given `location`. The value is
    /// not expected to recursively load values pointed to by nested blob references. Nested blob
    /// references in the value should be loaded by when explicitly requested by
    /// further operations (like caching) on the returned value.
    fn load(loader: impl BackingStoreLoad, location: BlobReference) -> Result<Self, DecodeError>;
}

/// A trait implemented by types that can be stored to a [BackingStoreStore]
/// storage.
pub trait Storable {
    /// Store the value in the backing store, and return the reference to the stored value.
    /// If the value is already stored in the backing store, an existing reference can be returned.
    /// Calling `store` twice on values that are blob references should hence return the same reference twice.
    /// Notice that when storing the value, it must recursively store all values pointed to by
    /// nested blob references, if not already stored, in order to store itself.
    fn store(&self, storer: impl Write);
}

impl<T: Storable> Storable for & T {
    fn store(&self, storer: impl BackingStoreStore) -> BlobReference {
        (*self).store(storer)
    }
}

fn store(mut storer: impl BackingStoreStore, storable: impl Storable) -> BlobReference {
    let mut buffer = Vec::new();
    storable.store(&mut buffer);
    storer.store_raw(buffer)
}

/// Adapter that stores its value into the backing store by plain serialization
/// of the value.
pub struct StoreSerialized<T>(pub T);

impl<T: Serial> Storable for StoreSerialized<T> {
    fn store(&self, mut storer: impl BackingStoreStore) -> BlobReference {
        let mut buffer = Vec::new();
        self.serial(&mut buffer);
        storer.store_raw(buffer)
    }
}

/// Adapter that loads its value from the backing store via plain serialization
/// of the value.
pub struct LoadSerialized<T>(pub T);

impl<T: Deserial> Loadable for LoadSerialized<T> {
    fn load(mut loader: impl BackingStoreLoad, location: BlobReference) -> Result<Self, DecodeError> {
        let bytes = loader.load_raw(location);
        let value = common::from_bytes_complete(bytes)
            .map_err(|err| DecodeError::Decode(err.to_string()))?;
        Ok(value)
    }
}

/// An error that may occur when loading data from persistent storage.
#[derive(Debug, thiserror::Error)]
pub enum DecodeError {
    #[error("{0}")]
    Decode(String),
}
