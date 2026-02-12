//! This module provides a C ABI for the Rust PLT block state.
//!
//! It is only available if the `ffi` feature is enabled.

use crate::block_state::{PltBlockStateSavepoint, blob_store};
use crate::ffi::blob_store_callbacks::{LoadCallback, StoreCallback};

/// Allocate a new empty PLT block state.
///
/// It is up to the caller to free this memory using [`ffi_free_plt_block_state`].
#[unsafe(no_mangle)]
extern "C" fn ffi_empty_plt_block_state() -> *mut PltBlockStateSavepoint {
    let block_state = PltBlockStateSavepoint::empty();
    Box::into_raw(Box::new(block_state))
}

/// Deallocate the PLT block state.
///
/// # Arguments
///
/// - `block_state` Unique pointer to the PLT block state.
///
/// # Safety
///
/// Caller must ensure:
///
/// - Argument `block_state` cannot be referenced by anyone else.
/// - Freeing is only ever done once.
#[unsafe(no_mangle)]
extern "C" fn ffi_free_plt_block_state(block_state: *mut PltBlockStateSavepoint) {
    assert!(!block_state.is_null(), "block_state is a null pointer.");
    let state = unsafe { Box::from_raw(block_state) };
    drop(state);
}

/// Compute the hash of the PLT block state.
///
/// # Arguments
///
/// - `load_callback` External function to call for loading bytes a reference from the blob store.
/// - `block_state` Pointer to the PLT block state to compute a hash for.
/// - `destination` Unique pointer with location to write the 32 bytes for the hash.
///
/// # Safety
///
/// Caller must ensure `destination` can hold 32 bytes for the hash.
#[unsafe(no_mangle)]
extern "C" fn ffi_hash_plt_block_state(
    mut load_callback: LoadCallback,
    destination: *mut u8,
    block_state: *const PltBlockStateSavepoint,
) {
    assert!(!block_state.is_null(), "block_state is a null pointer.");
    assert!(!destination.is_null(), "destination is a null pointer.");
    let block_state = unsafe { &*block_state };
    let hash = block_state.hash(&mut load_callback);
    unsafe {
        std::ptr::copy_nonoverlapping(hash.as_ptr(), destination, hash.len());
    }
}

/// Load a PLT block state from the blob store.
///
/// # Arguments
///
/// - `load_callback` External function to call for loading bytes a reference from the blob store.
/// - `blob_ref` Blob store reference to load the block state from.
///
/// # Safety
///
/// Caller must ensure `blob_ref` is a valid reference in the blob store.
#[unsafe(no_mangle)]
extern "C" fn ffi_load_plt_block_state(
    mut load_callback: LoadCallback,
    blob_ref: blob_store::Reference,
) -> *mut PltBlockStateSavepoint {
    // todo implement error handling for unrecoverable errors (instead of unwrap) in https://linear.app/concordium/issue/PSR-39/decide-and-implement-strategy-for-handling-panics-in-the-rust-code
    let block_state =
        blob_store::Loadable::load_from_location(&mut load_callback, blob_ref).unwrap();
    Box::into_raw(Box::new(block_state))
}

/// Store a PLT block state in the blob store.
///
/// # Arguments
///
/// - `store_callback` External function to call for storing bytes in the blob store returning a
///   reference.
/// - `block_state` The block state to store in the blob store.
///
/// # Safety
///
/// Caller must ensure `block_state` is non-null and points to a valid block state.
#[unsafe(no_mangle)]
extern "C" fn ffi_store_plt_block_state(
    mut store_callback: StoreCallback,
    block_state: *const PltBlockStateSavepoint,
) -> blob_store::Reference {
    assert!(!block_state.is_null(), "block_state is a null pointer.");
    let block_state = unsafe { &*block_state };
    block_state.store_update(&mut store_callback)
}

/// Migrate the PLT block state from one blob store to another.
///
/// # Arguments
///
/// - `load_callback` External function to call for loading bytes a reference from the blob store.
/// - `store_callback` External function to call for storing bytes in the blob store returning a
///   reference.
/// - `block_state` The block state to store in the blob store.
///
/// # Safety
///
/// Caller must ensure `block_state` is non-null and points to a valid block state.
#[unsafe(no_mangle)]
extern "C" fn ffi_migrate_plt_block_state(
    mut load_callback: LoadCallback,
    mut store_callback: StoreCallback,
    block_state: *const PltBlockStateSavepoint,
) -> *mut PltBlockStateSavepoint {
    assert!(!block_state.is_null(), "block_state is a null pointer.");
    let block_state = unsafe { &*block_state };
    let new_block_state = block_state.migrate(&mut load_callback, &mut store_callback);
    Box::into_raw(Box::new(new_block_state))
}

/// Cache the PLT block state into memory.
///
/// # Arguments
///
/// - `load_callback` External function to call for loading bytes a reference from the blob store.
/// - `block_state` The block state to store in the blob store.
///
/// # Safety
///
/// Caller must ensure `block_state` is non-null and points to a valid block state.
#[unsafe(no_mangle)]
extern "C" fn ffi_cache_plt_block_state(
    mut load_callback: LoadCallback,
    block_state: *const PltBlockStateSavepoint,
) {
    assert!(!block_state.is_null(), "block_state is a null pointer.");
    let block_state = unsafe { &*block_state };
    block_state.cache(&mut load_callback)
}
