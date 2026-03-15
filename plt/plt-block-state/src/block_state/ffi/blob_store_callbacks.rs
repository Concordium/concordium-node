use crate::block_state::blob_reference::BlobReference;
use crate::block_state::blob_store::{BackingStoreLoad, BackingStoreStore};
use libc::size_t;
use std::mem;

/// A [loader](BackingStoreLoad) implemented by an external function.
/// This is the dual to [`StoreCallback`].
///
/// Returns pointer to a uniquely owned [`Vec`].
/// The returned `Vec` must be deallocated by the caller.
pub type LoadCallback = extern "C" fn(BlobReference) -> *mut Vec<u8>;

/// A [storer](BackingStoreStore) implemented by an external function.
/// The function is passed a (shared) pointer to data to store, and the size of data. It
/// should return the location where the data can be loaded via a
/// [`LoadCallback`].
pub type StoreCallback = extern "C" fn(data: *const u8, len: size_t) -> BlobReference;

impl BackingStoreStore for StoreCallback {
    fn store_raw(&mut self, data: impl AsRef<[u8]>) -> BlobReference {
        let data_ref = data.as_ref();
        self(data_ref.as_ptr(), data_ref.len())
    }
}

impl BackingStoreLoad for LoadCallback {
    fn load_raw(&mut self, location: BlobReference) -> Vec<u8> {
        let vec_from_different_allocator = unsafe { Box::from_raw(self(location)) };

        let vec = vec_from_different_allocator.as_ref().clone();

        // todo free memory as part of https://linear.app/concordium/issue/COR-2113/fix-rust-allocator-issue-related-to-multiple-rust-cdylibs
        // vec_from_different_allocator is allocated in a different Rust allocator context (wasm-chain-integration), hence we cannot deallocate it here
        mem::forget(vec_from_different_allocator);

        vec
    }
}
