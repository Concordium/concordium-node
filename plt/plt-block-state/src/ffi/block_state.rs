//! This module provides a C ABI for the Rust PLT block state.
//!
//! It is only available if the `ffi` feature is enabled.

use super::status;
use crate::block_state::blob_store::BlobStoreLocation;
use crate::block_state::cacheable::Cacheable;
use crate::block_state::hash::Hashable;
use crate::block_state::{BlockState, blob_store};
use crate::ffi::blob_store_callbacks::{LoadCallback, StoreCallback};
use concordium_base::base::ProtocolVersion;

/// Allocate a new empty PLT block state.
///
/// - [`status::FfiStatusCode::Success`]: Creating the block state was successful.
/// - [`status::FfiStatusCode::Panic`]: Creating the block state resulted in an unrecoverable error or panic.
///
/// # Arguments
///
/// - `protocol_version` Protocol version for the block state to create.
/// - `block_state_out`: Location for writing the pointer of the new, empty block state.
///   The new block state is only written if return value is [`status::FfiStatusCode::Success`].
///   The pointer written is to a uniquely owned instance.
///   The caller must free the written block state using `ffi_free_plt_block_state` when it is no longer used.
///
/// # Safety
///
/// - Argument `block_state_out` must be a non-null and valid pointer for writing
#[unsafe(no_mangle)]
extern "C" fn ffi_empty_plt_block_state(
    protocol_version: u64,
    block_state_out: *mut *mut BlockState,
) -> status::FfiStatusCode {
    let panic_message = status::catch_unwind(move || {
        let protocol_version =
            ProtocolVersion::try_from(protocol_version).expect("Unknown protocol version");
        let block_state = BlockState::empty(protocol_version);
        unsafe {
            *block_state_out = Box::into_raw(Box::new(block_state));
        }
    });
    if let Some(message) = panic_message {
        eprintln!("{}", message);
        status::FfiStatusCode::Panic
    } else {
        status::FfiStatusCode::Success
    }
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
/// - Argument `block_state` must be unique, non-null pointer to well-formed [`BlockState`].
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
/// - [`status::FfiStatusCode::Success`]: Hashing the block state was successful.
/// - [`status::FfiStatusCode::Panic`]: Hashing the block state resulted in an unrecoverable error or panic.
///
/// # Arguments
///
/// - `load_callback` External function to call for loading bytes a reference from the blob store.
/// - `block_state` Shared pointer to the PLT block state to compute a hash for.
/// - `block_state_hash_out` Unique pointer with location to write the 32 bytes for the hash.
///   The hash is only written if return value is [`status::FfiStatusCode::Success`].
///
/// # Safety
///
/// - Argument `load_callback` must be a valid function pointer to a function with a signature matching [`LoadCallback`].
/// - Argument `block_state` must be a non-null pointer to well-formed [`BlockState`].
///   The pointer is to a shared instance, hence only valid for reading (writing only allowed through interior mutability).
/// - Argument `block_state_hash_out` must be non-null and valid for writes of 32 bytes.
#[unsafe(no_mangle)]
extern "C" fn ffi_hash_plt_block_state(
    load_callback: LoadCallback,
    block_state: *const BlockState,
    block_state_hash_out: *mut u8,
) -> status::FfiStatusCode {
    let panic_message = status::catch_unwind(move || {
        assert!(!block_state.is_null(), "block_state is a null pointer.");
        assert!(
            !block_state_hash_out.is_null(),
            "block_state_hash_out is a null pointer."
        );
        let block_state = unsafe { &*block_state };
        let hash = block_state
            .hash(&load_callback)
            .expect("Failed hashing block state");
        unsafe {
            std::ptr::copy_nonoverlapping(hash.as_ptr(), block_state_hash_out, hash.len());
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
/// - [`status::FfiStatusCode::Success`]: Loading the block state was successful.
/// - [`status::FfiStatusCode::Panic`]: Loading the block state resulted in an unrecoverable error or panic.
///
/// # Arguments
///
/// - `load_callback` External function to call for loading bytes from the blob store.
/// - `protocol_version` Protocol version for the block state to load.
/// - `blob_ref` Blob store reference to load the block state from.
/// - `block_state_out` Location for writing the pointer of the loaded block state.
///   The new block state is only written if return value is [`status::FfiStatusCode::Success`].
///   The pointer written is to a uniquely owned instance.
///   The caller must free the written block state using `ffi_free_plt_block_state` when it is no longer used.
///
/// # Safety
///
/// - Argument `load_callback` must be a valid function pointer to a function with a signature matching [`LoadCallback`].
/// - Argument `block_state_out` must be a non-null and valid pointer for writing
#[unsafe(no_mangle)]
extern "C" fn ffi_load_plt_block_state(
    load_callback: LoadCallback,
    blob_ref: BlobStoreLocation,
    protocol_version: u64,
    block_state_out: *mut *mut BlockState,
) -> status::FfiStatusCode {
    assert!(
        !block_state_out.is_null(),
        "block_state_out is a null pointer."
    );
    let panic_message = status::catch_unwind(move || {
        let protocol_version =
            ProtocolVersion::try_from(protocol_version).expect("Unknown protocol version");
        let block_state = BlockState::load_from_store(&load_callback, blob_ref, protocol_version)
            .expect("Failed loading the block state");
        unsafe {
            *block_state_out = Box::into_raw(Box::new(block_state));
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
/// - [`status::FfiStatusCode::Success`]: Storing the block state was successful.
/// - [`status::FfiStatusCode::Panic`]: Storing the block state resulted in an unrecoverable error or panic.
///
/// # Arguments
///
/// - `store_callback` External function to call for storing bytes in the blob store returning a
///   reference.
/// - `blob_ref_out` Location for writing the blob store location the block state is stored to.
///   The block state location is only written if return value is [`status::FfiStatusCode::Success`].
/// - `block_state` The block state to store in the blob store.
///
/// # Safety
///
/// - Argument `load_callback` must be a valid function pointer to a function with a signature matching [`LoadCallback`].
/// - Argument `blob_ref_out` must be a non-null and valid pointer for writing
/// - Argument `block_state` must be a non-null pointer to well-formed [`BlockState`].
///   The pointer is to a shared instance, hence only valid for reading (writing only allowed through interior mutability).
#[unsafe(no_mangle)]
extern "C" fn ffi_store_plt_block_state(
    mut store_callback: StoreCallback,
    blob_ref_out: *mut BlobStoreLocation,
    block_state: *const BlockState,
) -> status::FfiStatusCode {
    let panic_message = status::catch_unwind(move || {
        assert!(!block_state.is_null(), "block_state is a null pointer.");
        assert!(!blob_ref_out.is_null(), "blob_ref_out is a null pointer.");
        let block_state = unsafe { &*block_state };
        let reference = blob_store::store_to_store(&mut store_callback, block_state);
        unsafe {
            *blob_ref_out = reference;
        }
    });
    if let Some(message) = panic_message {
        eprintln!("{}", message);
        status::FfiStatusCode::Panic
    } else {
        status::FfiStatusCode::Success
    }
}

/// Migrate the PLT block state from one blob store to another.
///
/// - [`status::FfiStatusCode::Success`]: Migrating the block state was successful.
/// - [`status::FfiStatusCode::Panic`]: Migrating the block state resulted in an unrecoverable error or panic.
///
/// # Arguments
///
/// - `from_load_callback` External function to call for loading bytes a reference from
///   the blob store to migrate from.
/// - `to_store_callback` External function to call for storing bytes in the blob store
///   to migrate to.
/// - `to_protocol_version` Protocol version for the block state to migrate to.
/// - `new_block_state_out` Location for writing the pointer of the new, migrated block state.
///   The new block state is only written if return value is [`status::FfiStatusCode::Success`].
///   The pointer written is to a uniquely owned instance.
///   The caller must free the written block state using `ffi_free_plt_block_state` when it is no longer used.
/// - `block_state` Shared pointer to a block state to migrate from.
///
/// # Safety
///
/// - Argument `load_callback` must be a valid function pointer to a function with a signature matching [`LoadCallback`].
/// - Argument `store_callback` must be a valid function pointer to a function with a signature matching [`StoreCallback`].
/// - Argument `new_block_state_out` must be a non-null and valid pointer for writing
/// - Argument `block_state` must be a non-null pointer to well-formed [`BlockState`].
///   The pointer is to a shared instance, hence only valid for reading (writing only allowed through interior mutability).
#[unsafe(no_mangle)]
extern "C" fn ffi_migrate_plt_block_state(
    from_load_callback: LoadCallback,
    to_store_callback: StoreCallback,
    to_protocol_version: u64,
    new_block_state_out: *mut *mut BlockState,
    block_state: *const BlockState,
) -> status::FfiStatusCode {
    let panic_message = status::catch_unwind(move || {
        assert!(!block_state.is_null(), "block_state is a null pointer.");
        assert!(
            !new_block_state_out.is_null(),
            "new_block_state_out is a null pointer."
        );
        let from_block_state = unsafe { &*block_state };
        let to_protocol_version =
            ProtocolVersion::try_from(to_protocol_version).expect("Unknown protocol version");
        let new_block_state =
            from_block_state.migrate(from_load_callback, to_store_callback, to_protocol_version);
        unsafe {
            *new_block_state_out = Box::into_raw(Box::new(new_block_state));
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
/// - [`status::FfiStatusCode::Success`]: Caching the block state was successful.
/// - [`status::FfiStatusCode::Panic`]: Caching the block state resulted in an unrecoverable error or panic.
///
/// # Arguments
///
/// - `load_callback` External function to call for loading bytes a reference from the blob store.
/// - `block_state` The block state to store in the blob store.
///
/// # Safety
///
/// - Argument `load_callback` must be a valid function pointer to a function with a signature matching [`LoadCallback`].
/// - Argument `block_state` must be a non-null pointer to well-formed [`BlockState`].
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
