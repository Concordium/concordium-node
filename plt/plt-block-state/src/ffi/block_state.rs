//! This module provides a C ABI for the Rust PLT block state.
//!
//! It is only available if the `ffi` feature is enabled.

use concordium_base::base::ProtocolVersion;

use crate::block_state::blob_store::Loadable;
use crate::block_state::{
    blob_store, p10, p11, BlockStateOperations, BlockStateSavepoint, PltBlockStateHash,
};
use crate::ffi::blob_store_callbacks::{LoadCallback, StoreCallback};

/// Allocate a new empty PLT block state and returns it.
///
/// The returned pointer is to a uniquely owned instance.
/// It must be freed by calling [`ffi_free_plt_block_state`].
#[unsafe(no_mangle)]
extern "C" fn ffi_empty_plt_block_state(
    protocol_version: u64,
) -> *mut BlockStateSavepoint<OpaqueBlockState> {
    let protocol_version =
        ProtocolVersion::try_from(protocol_version).expect("Failed parsing protocol version");
    match protocol_version {
        ProtocolVersion::P1
        | ProtocolVersion::P2
        | ProtocolVersion::P3
        | ProtocolVersion::P4
        | ProtocolVersion::P5
        | ProtocolVersion::P6
        | ProtocolVersion::P7
        | ProtocolVersion::P8
        | ProtocolVersion::P9 => std::ptr::null_mut(),
        ProtocolVersion::P10 => Box::into_raw(Box::new(
            BlockStateSavepoint::<p10::PltBlockStateP10>::empty(),
        ))
        .cast(),
        ProtocolVersion::P11 => Box::into_raw(Box::new(
            BlockStateSavepoint::<p11::PltBlockStateP11>::empty(),
        ))
        .cast(),
    }
}

/// Deallocate the PLT block state.
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
extern "C" fn ffi_free_plt_block_state(block_state: *mut OpaqueBlockState, protocol_version: u64) {
    assert!(!block_state.is_null(), "block_state is a null pointer.");
    let protocol_version =
        ProtocolVersion::try_from(protocol_version).expect("Failed parsing protocol version");

    match protocol_version {
        ProtocolVersion::P1
        | ProtocolVersion::P2
        | ProtocolVersion::P3
        | ProtocolVersion::P4
        | ProtocolVersion::P5
        | ProtocolVersion::P6
        | ProtocolVersion::P7
        | ProtocolVersion::P8
        | ProtocolVersion::P9 => {}
        ProtocolVersion::P10 => {
            let block_state = block_state.cast::<BlockStateSavepoint<p10::PltBlockStateP10>>();
            drop(unsafe { Box::from_raw(block_state) })
        }
        ProtocolVersion::P11 => {
            let block_state = block_state.cast::<BlockStateSavepoint<p11::PltBlockStateP11>>();
            drop(unsafe { Box::from_raw(block_state) })
        }
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
    mut load_callback: LoadCallback,
    block_state: *const OpaqueBlockState,
    protocol_version: u64,
    destination: *mut u8,
) {
    assert!(!block_state.is_null(), "block_state is a null pointer.");
    assert!(!destination.is_null(), "destination is a null pointer.");
    let protocol_version =
        ProtocolVersion::try_from(protocol_version).expect("Failed parsing protocol version");
    let hash: PltBlockStateHash = match protocol_version {
        ProtocolVersion::P1
        | ProtocolVersion::P2
        | ProtocolVersion::P3
        | ProtocolVersion::P4
        | ProtocolVersion::P5
        | ProtocolVersion::P6
        | ProtocolVersion::P7
        | ProtocolVersion::P8
        | ProtocolVersion::P9 => {
            unimplemented!()
        }
        ProtocolVersion::P10 => {
            let block_state =
                unsafe { &*block_state.cast::<BlockStateSavepoint<p10::PltBlockStateP10>>() }
                    .state();
            block_state.hash(&mut load_callback)
        }
        ProtocolVersion::P11 => {
            let block_state =
                unsafe { &*block_state.cast::<BlockStateSavepoint<p11::PltBlockStateP11>>() }
                    .state();
            block_state.hash(&mut load_callback)
        }
    };

    unsafe {
        std::ptr::copy_nonoverlapping(hash.as_ptr(), destination, hash.len());
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
    mut load_callback: LoadCallback,
    protocol_version: u64,
    blob_ref: blob_store::Reference,
) -> *mut OpaqueBlockState {
    let protocol_version =
        ProtocolVersion::try_from(protocol_version).expect("Failed parsing protocol version");

    // todo implement error handling for unrecoverable errors (instead of unwrap) in https://linear.app/concordium/issue/PSR-39/decide-and-implement-strategy-for-handling-panics-in-the-rust-code
    match protocol_version {
        ProtocolVersion::P1
        | ProtocolVersion::P2
        | ProtocolVersion::P3
        | ProtocolVersion::P4
        | ProtocolVersion::P5
        | ProtocolVersion::P6
        | ProtocolVersion::P7
        | ProtocolVersion::P8
        | ProtocolVersion::P9 => unimplemented!(),
        ProtocolVersion::P10 => {
            let block_state = BlockStateSavepoint::<p10::PltBlockStateP10>::load_from_location(
                &mut load_callback,
                blob_ref,
            )
            .unwrap();
            Box::into_raw(Box::new(block_state)).cast()
        }
        ProtocolVersion::P11 => {
            let block_state = BlockStateSavepoint::<p11::PltBlockStateP11>::load_from_location(
                &mut load_callback,
                blob_ref,
            )
            .unwrap();
            Box::into_raw(Box::new(block_state)).cast()
        }
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
    block_state: *const OpaqueBlockState,
    protocol_version: u64,
) -> blob_store::Reference {
    assert!(!block_state.is_null(), "block_state is a null pointer.");
    let protocol_version =
        ProtocolVersion::try_from(protocol_version).expect("Failed parsing protocol version");
    match protocol_version {
        ProtocolVersion::P1
        | ProtocolVersion::P2
        | ProtocolVersion::P3
        | ProtocolVersion::P4
        | ProtocolVersion::P5
        | ProtocolVersion::P6
        | ProtocolVersion::P7
        | ProtocolVersion::P8
        | ProtocolVersion::P9 => unimplemented!(),
        ProtocolVersion::P10 => {
            let block_state =
                unsafe { &*block_state.cast::<BlockStateSavepoint<p10::PltBlockStateP10>>() };
            block_state.store_update(&mut store_callback)
        }
        ProtocolVersion::P11 => unimplemented!(),
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
    mut load_callback: LoadCallback,
    mut store_callback: StoreCallback,
    old_block_state: *const OpaqueBlockState,
    protocol_version: u64,
) -> *mut OpaqueBlockState {
    assert!(!old_block_state.is_null(), "block_state is a null pointer.");
    let protocol_version =
        ProtocolVersion::try_from(protocol_version).expect("Failed parsing protocol version");

    match protocol_version {
        ProtocolVersion::P1
        | ProtocolVersion::P2
        | ProtocolVersion::P3
        | ProtocolVersion::P4
        | ProtocolVersion::P5
        | ProtocolVersion::P6
        | ProtocolVersion::P7
        | ProtocolVersion::P8
        | ProtocolVersion::P9
        | ProtocolVersion::P10 => unimplemented!(),
        ProtocolVersion::P11 => {
            let old =
                unsafe { &*old_block_state.cast::<BlockStateSavepoint<p10::PltBlockStateP10>>() };
            let new = p11::PltBlockStateP11::migrate_from_p10(
                &mut load_callback,
                &mut store_callback,
                old.state(),
            );
            Box::into_raw(Box::new(BlockStateSavepoint::save(new))).cast()
        }
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
    mut load_callback: LoadCallback,
    block_state: *mut OpaqueBlockState,
    protocol_version: u64,
) {
    assert!(!block_state.is_null(), "block_state is a null pointer.");
    let protocol_version =
        ProtocolVersion::try_from(protocol_version).expect("Failed parsing protocol version");
    match protocol_version {
        ProtocolVersion::P1
        | ProtocolVersion::P2
        | ProtocolVersion::P3
        | ProtocolVersion::P4
        | ProtocolVersion::P5
        | ProtocolVersion::P6
        | ProtocolVersion::P7
        | ProtocolVersion::P8
        | ProtocolVersion::P9 => unimplemented!(),
        ProtocolVersion::P10 => {
            let block_state =
                unsafe { &mut *block_state.cast::<BlockStateSavepoint<p10::PltBlockStateP10>>() };
            block_state.cache(&mut load_callback)
        }
        ProtocolVersion::P11 => {
            let block_state =
                unsafe { &mut *block_state.cast::<BlockStateSavepoint<p11::PltBlockStateP11>>() };
            block_state.cache(&mut load_callback)
        }
    }
}

pub enum OpaqueBlockState {}
