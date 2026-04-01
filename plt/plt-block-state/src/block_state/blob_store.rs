//! Definition of the blob store interface via
//! [`BlobStoreLoad`] and [`BlobStoreStore`]. And definition of
//! the traits [`Loadable`] and [`Storable`] that block state components
//! must implement to be storable in the blob store.

use crate::block_state::blob_reference::BlobStoreLocation;
use crate::block_state_interface::{BlockStateError, BlockStateResult};
use concordium_base::common;
use concordium_base::common::{Buffer, Deserial, Get, Put, Serial};
use std::any;
use std::io::Read;

/// Trait implemented by types that can be used to store binary data, and return
/// a handle for loading data. Dual to [`BlobStoreLoad`].
pub trait BlobStoreStore {
    /// Store the provided value and return a reference that can be used
    /// to load it.
    fn store_raw(&mut self, data: impl AsRef<[u8]>) -> BlobStoreLocation;
}

/// Trait implemented by types that can load data from given locations.
/// Dual to [`BlobStoreStore`].
pub trait BlobStoreLoad {
    /// Load the provided value from the given location. The implementation of
    /// this should match [BlobStoreStore::store_raw].
    fn load_raw(&self, location: BlobStoreLocation) -> Vec<u8>;
}

/// A trait implemented by types that can be loaded from a [blob store](BlobStoreLoad).
pub trait Loadable: Sized {
    /// Load value from the bytes in the given `buffer` that has been retrieved from the blob store.
    /// If the value is composed of [blob references](super::blob_reference), these references should not
    /// have their values loaded into memory as a result of the [`Self::load_from_buffer`] operation.
    /// As such, [`Self::load_from_buffer`] is a "shallow" operation.
    fn load_from_buffer(buffer: impl Read) -> Result<Self, BlockStateError>;
}

/// A trait implemented by types that can be stored to a [blob store](BlobStoreStore).
pub trait Storable {
    /// Store the value in the given `buffer` that will be written to the blob store.
    /// Notice that when storing the value, the operation must recursively store all
    /// values pointed to by the [blob references](super::blob_reference) the value may be composed of,
    /// if these values are not already represented in the blob store.
    /// As such, `store` is a "deep" operation.
    fn store_to_buffer(&self, buffer: impl Buffer, storer: &mut impl BlobStoreStore);
}

impl<T: Storable> Storable for &T {
    fn store_to_buffer(&self, buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        (**self).store_to_buffer(buffer, storer)
    }
}

impl<T: Storable> Storable for &mut T {
    fn store_to_buffer(&self, buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        (**self).store_to_buffer(buffer, storer)
    }
}

/// Adapter for types implementing [`Serialize`](common::Serialize) that
/// allows them to be used as block state components.
#[derive(Debug, Clone, Copy, Default, Eq, PartialEq)]
pub struct StoreSerialized<T>(pub T);

impl<T: Deserial> Loadable for StoreSerialized<T> {
    fn load_from_buffer(mut buffer: impl Read) -> BlockStateResult<Self> {
        Ok(StoreSerialized(
            buffer.get().map_parse_err_to_block_state_err()?,
        ))
    }
}

impl<T: Serial> Storable for StoreSerialized<T> {
    fn store_to_buffer(&self, mut buffer: impl Buffer, _storer: &mut impl BlobStoreStore) {
        buffer.put(&self.0);
    }
}

/// Load value from the blob store at the given `location`. The value will
/// not recursively load values pointed to by [blob references](super::blob_reference)
/// is may be composed of as part of this operation.
pub fn load_from_store<T: Loadable>(
    loader: &impl BlobStoreLoad,
    location: BlobStoreLocation,
) -> BlockStateResult<T> {
    let bytes = loader.load_raw(location);
    let mut bytes_slice = bytes.as_slice();
    let value = T::load_from_buffer(&mut bytes_slice)?;
    if !bytes_slice.is_empty() {
        return Err(BlockStateError::BlobStoreDecode(format!(
            "Bytes remaining after loading value of type {} from blob store",
            any::type_name::<T>()
        )));
    };
    Ok(value)
}

/// Store the value in the blob store, and return the location of the stored value.
/// Notice that when storing the value, it will recursively store all values pointed to by
/// [blob references](super::blob_reference) it may be composed of, if these values are not already
/// stored in the blob store.
pub fn store_to_store(
    storer: &mut impl BlobStoreStore,
    storable: impl Storable,
) -> BlobStoreLocation {
    let mut buffer = Vec::new();
    storable.store_to_buffer(&mut buffer, storer);
    storer.store_raw(buffer)
}

/// Extension trait for [`common::ParseResult`] that allows mapping error type
/// to [`DecodeError`].
pub trait ParseResultExt<T> {
    /// Map the error type of [`common::ParseResult`] to [`BlockStateError`]
    fn map_parse_err_to_block_state_err(self) -> Result<T, BlockStateError>;
}

impl<T> ParseResultExt<T> for common::ParseResult<T> {
    fn map_parse_err_to_block_state_err(self) -> Result<T, BlockStateError> {
        self.map_err(|err| {
            BlockStateError::BlobStoreDecode(format!(
                "Error parsing bytes for value of type {} loaded from blob store: {}",
                any::type_name::<T>(),
                err
            ))
        })
    }
}

#[cfg(test)]
pub mod test_stub {
    use super::*;

    /// In-memory blob store stub implemented via a `Vec`.
    #[derive(Default, Debug)]
    pub struct BlobStoreStub(pub Vec<u8>);

    impl BlobStoreStore for BlobStoreStub {
        fn store_raw(&mut self, data: impl AsRef<[u8]>) -> BlobStoreLocation {
            let data = data.as_ref();
            let reference = BlobStoreLocation(self.0.len() as u64);
            self.0.put(data.len() as u64);
            self.0.extend_from_slice(data);
            reference
        }
    }

    impl BlobStoreLoad for BlobStoreStub {
        fn load_raw(&self, location: BlobStoreLocation) -> Vec<u8> {
            let mut source = self.0.get(location.0 as usize..).unwrap_or_else(|| {
                panic!(
                    "no bytes at given location in BlobStoreStub: {:?}",
                    location
                )
            });

            let length =
                Get::<u64>::get(&mut source).expect("read length from BlobStoreStub") as usize;

            source
                .get(..length)
                .expect("read data from BlobStoreStub")
                .to_vec()
        }
    }

    #[test]
    fn test_blob_store_stub() {
        let mut store = BlobStoreStub::default();
        let ref1 = store.store_raw([1, 2, 3]);
        let ref2 = store.store_raw([4, 5]);
        assert_eq!(store.load_raw(ref1), vec![1, 2, 3]);
        assert_eq!(store.load_raw(ref2), vec![4, 5]);
    }

    /// Blob store implementation that panics when read from or written to.
    #[derive(Default, Debug)]
    pub struct UnreachableBlobStore;

    impl BlobStoreStore for UnreachableBlobStore {
        fn store_raw(&mut self, _data: impl AsRef<[u8]>) -> BlobStoreLocation {
            unreachable!("UnreachableBlobStore")
        }
    }

    impl BlobStoreLoad for UnreachableBlobStore {
        fn load_raw(&self, _location: BlobStoreLocation) -> Vec<u8> {
            unreachable!("UnreachableBlobStore")
        }
    }
}
