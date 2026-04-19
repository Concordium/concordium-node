//! This module provides a C ABI for the Rust PLT block state.
//!
//! It is only available if the `ffi` feature is enabled.

use super::status;
use crate::block_state::blob_store::BlobStoreLocation;
use crate::block_state::cacheable::Cacheable;
use crate::block_state::hash::Hashable;
use crate::block_state::{BlockState, blob_store};
use crate::ffi::blob_store_callbacks::{LoadCallback, StoreCallback};

/// Allocate a new empty PLT block state and returns it.
///
/// The returned pointer is to a uniquely owned instance.
/// It must be freed by calling [`ffi_free_plt_block_state`].
#[unsafe(no_mangle)]
extern "C" fn ffi_empty_plt_block_state() -> *mut BlockState {
    let block_state = BlockState::empty();
    Box::into_raw(Box::new(block_state))
}

/// Deallocate the PLT block state.
///
/// This is called by the Haskell garbage collector, therefore we cannot handle
/// a status code unlike other FFI functions in this module.
///
/// # Arguments
///
/// - `block_state` Unique pointer to the PLT block state.
///
/// # Safety
///
/// - Argument `block_state` must be unique, non-null pointer to well-formed [`PltBlockStateSavepoint`].
///   No other pointers to the block state must exist.
/// - Freeing is only ever done once.
#[unsafe(no_mangle)]
extern "C" fn ffi_free_plt_block_state(block_state: *mut BlockState) {
    let panic_message = status::catch_unwind(move || {
        assert!(!block_state.is_null(), "block_state is a null pointer.");
        let state = unsafe { Box::from_raw(block_state) };
        drop(state);
    });
    if let Some(message) = panic_message {
        eprintln!("{}", message);
    }
}

/// Compute the hash of the PLT block state.
///
/// # Arguments
///
/// - `load_callback` External function to call for loading bytes a reference from the blob store.
/// - `block_state` Shared pointer to the PLT block state to compute a hash for.
/// - `destination` Unique pointer with location to write the 32 bytes for the hash.
///
/// # Safety
///
/// - Argument `load_callback` must be a valid function pointer to a function with a signature matching [`LoadCallback`].
/// - Argument `block_state` must be a non-null pointer to well-formed [`crate::block_state::PltBlockStateSavepoint`].
///   The pointer is to a shared instance, hence only valid for reading (writing only allowed through interior mutability).
/// - Argument `destination` must be non-null and valid for writes of 32 bytes.
#[unsafe(no_mangle)]
extern "C" fn ffi_hash_plt_block_state(
    load_callback: LoadCallback,
    block_state: *const BlockState,
    destination: *mut u8,
) -> status::FfiStatusCode {
    let panic_message = status::catch_unwind(move || {
        assert!(!block_state.is_null(), "block_state is a null pointer.");
        assert!(!destination.is_null(), "destination is a null pointer.");
        let block_state = unsafe { &*block_state };
        let hash = block_state
            .hash(&load_callback)
            .expect("Failed hashing block state");
        unsafe {
            std::ptr::copy_nonoverlapping(hash.as_ptr(), destination, hash.len());
        }
    });
    if let Some(message) = panic_message {
        eprintln!("{}", message);
        status::FfiStatusCode::Panic
    } else {
        status::FfiStatusCode::Success
    }
}

/// Load a PLT block state from the blob store and return it.
///
/// The returned pointer is to a uniquely owned instance.
/// It must be freed by calling [`ffi_free_plt_block_state`].
///
/// # Arguments
///
/// - `load_callback` External function to call for loading bytes from the blob store.
/// - `blob_ref` Blob store reference to load the block state from.
///
/// # Safety
///
/// - Argument `load_callback` must be a valid function pointer to a function with a signature matching [`LoadCallback`].
#[unsafe(no_mangle)]
extern "C" fn ffi_load_plt_block_state(
    load_callback: LoadCallback,
    blob_ref: BlobStoreLocation,
    destination: *mut *mut BlockState,
) -> status::FfiStatusCode {
    let panic_message = status::catch_unwind(move || {
        let block_state = blob_store::load_from_store(&load_callback, blob_ref)
            .expect("Failed loading block state");
        unsafe {
            *destination = Box::into_raw(Box::new(block_state));
        }
    });
    if let Some(message) = panic_message {
        eprintln!("{}", message);
        status::FfiStatusCode::Panic
    } else {
        status::FfiStatusCode::Success
    }
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
/// - Argument `load_callback` must be a valid function pointer to a function with a signature matching [`LoadCallback`].
/// - Argument `block_state` must be a non-null pointer to well-formed [`crate::block_state::PltBlockStateSavepoint`].
///   The pointer is to a shared instance, hence only valid for reading (writing only allowed through interior mutability).
#[unsafe(no_mangle)]
extern "C" fn ffi_store_plt_block_state(
    mut store_callback: StoreCallback,
    destination: *mut BlobStoreLocation,
    block_state: *const BlockState,
) -> status::FfiStatusCode {
    let panic_message = status::catch_unwind(move || {
        assert!(!block_state.is_null(), "block_state is a null pointer.");
        let block_state = unsafe { &*block_state };
        let reference = blob_store::store_to_store(&mut store_callback, block_state);
        unsafe {
            *destination = reference;
        }
    });
    if let Some(message) = panic_message {
        eprintln!("{}", message);
        status::FfiStatusCode::Panic
    } else {
        status::FfiStatusCode::Success
    }
}

/// Migrate the PLT block state from one blob store to another and return the migrated state.
///
/// The returned pointer is to a uniquely owned instance.
/// It must be freed by calling [`ffi_free_plt_block_state`].
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
/// - Argument `load_callback` must be a valid function pointer to a function with a signature matching [`LoadCallback`].
/// - Argument `store_callback` must be a valid function pointer to a function with a signature matching [`StoreCallback`].
/// - Argument `block_state` must be a non-null pointer to well-formed [`crate::block_state::PltBlockStateSavepoint`].
///   The pointer is to a shared instance, hence only valid for reading (writing only allowed through interior mutability).
#[unsafe(no_mangle)]
extern "C" fn ffi_migrate_plt_block_state(
    load_callback: LoadCallback,
    mut store_callback: StoreCallback,
    destination: *mut *mut BlockState,
    block_state: *const BlockState,
) -> status::FfiStatusCode {
    let panic_message = status::catch_unwind(move || {
        assert!(!block_state.is_null(), "block_state is a null pointer.");
        let block_state = unsafe { &*block_state };
        let new_block_state = block_state.migrate(&load_callback, &mut store_callback);
        unsafe {
            *destination = Box::into_raw(Box::new(new_block_state));
        }
    });
    if let Some(message) = panic_message {
        eprintln!("{}", message);
        status::FfiStatusCode::Panic
    } else {
        status::FfiStatusCode::Success
    }
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
/// - Argument `load_callback` must be a valid function pointer to a function with a signature matching [`LoadCallback`].
/// - Argument `block_state` must be a non-null pointer to well-formed [`crate::block_state::PltBlockStateSavepoint`].
///   The pointer is to a shared instance, hence only valid for reading (writing only allowed through interior mutability).
#[unsafe(no_mangle)]
extern "C" fn ffi_cache_plt_block_state(
    load_callback: LoadCallback,
    block_state: *const BlockState,
) -> status::FfiStatusCode {
    let panic_message = status::catch_unwind(move || {
        assert!(!block_state.is_null(), "block_state is a null pointer.");
        let block_state = unsafe { &*block_state };
        block_state
            .cache_reference_values(&load_callback)
            .expect("Failed caching block state");
    });
    if let Some(message) = panic_message {
        eprintln!("{}", message);
        status::FfiStatusCode::Panic
    } else {
        status::FfiStatusCode::Success
    }
}
