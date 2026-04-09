use crate::block_state::blob_store;
use crate::block_state::blob_store::{BackingStoreLoad, BackingStoreStore};
use libc::size_t;

/// A [loader](BackingStoreLoad) implemented by an external function.
/// This is the dual to [`StoreCallback`].
///
/// Returns pointer to a uniquely owned [`Vec`]. The `Vec` should be allocated
/// by the callee using `copy_to_vec_ffi` in `wasm-chain-integration`.
/// The returned `Vec` must be deallocated by the caller.
pub type LoadCallback = extern "C" fn(blob_store::Reference) -> *mut Vec<u8>;

/// A [storer](BackingStoreStore) implemented by an external function.
/// The function is passed a (shared) pointer to data to store, and the size of data. It
/// should return the location where the data can be loaded via a
/// [`LoadCallback`].
pub type StoreCallback = extern "C" fn(data: *const u8, len: size_t) -> blob_store::Reference;

impl BackingStoreStore for StoreCallback {
    fn store_raw(&mut self, data: &[u8]) -> blob_store::Reference {
        self(data.as_ptr(), data.len())
    }
}

impl BackingStoreLoad for LoadCallback {
    fn load_raw(&mut self, location: blob_store::Reference) -> Vec<u8> {
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
