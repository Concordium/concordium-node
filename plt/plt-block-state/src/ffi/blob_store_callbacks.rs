use crate::block_state::blob_reference::BlobStoreLocation;
use crate::block_state::blob_store::{BlobStoreLoad, BlobStoreStore};
use libc::size_t;

/// A [loader](BlobStoreLoad) implemented by an external function.
/// This is the dual to [`StoreCallback`].
///
/// Returns pointer to a uniquely owned [`Vec`]. The `Vec` should be allocated
/// by the callee using `copy_to_vec_ffi` in `wasm-chain-integration`.
/// The returned `Vec` must be deallocated by the caller.
pub type LoadCallback = extern "C" fn(BlobStoreLocation) -> *mut Vec<u8>;

/// A [storer](BlobStoreStore) implemented by an external function.
/// The function is passed a (shared) pointer to data to store, and the size of data. It
/// should return the location where the data can be loaded via a
/// [`LoadCallback`].
pub type StoreCallback = extern "C" fn(data: *const u8, len: size_t) -> BlobStoreLocation;

impl BlobStoreStore for StoreCallback {
    fn store_raw(&mut self, data: impl AsRef<[u8]>) -> BlobStoreLocation {
        let data_ref = data.as_ref();
        self(data_ref.as_ptr(), data_ref.len())
    }
}

impl BlobStoreLoad for LoadCallback {
    fn load_raw(&self, location: BlobStoreLocation) -> Vec<u8> {
        *unsafe { Box::from_raw(self(location)) }
    }
}

pub mod tests_helpers {
    use super::*;

    pub const UNIMPLEMENTED_LOAD_CALLBACK: LoadCallback = {
        extern "C" fn fun(_: blob_store::Reference) -> *mut Vec<u8> {
            unimplemented!()
        }
        fun
    };
}
