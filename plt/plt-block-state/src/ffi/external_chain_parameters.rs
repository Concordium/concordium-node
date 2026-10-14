//! This module provides a C ABI for external chain parameters.
//!
//! It is only available if the `ffi` feature is enabled.

use super::status;
use crate::ffi::blob_store_callbacks::{LoadCallback, StoreCallback};
use crate::persistent::blob_store;
use crate::persistent::blob_store::BlobStoreLocation;
use crate::persistent::cacheable::Cacheable;
use crate::persistent::chain_parameters::PersistentChainParameters;
use crate::persistent::hash::Hashable;
use concordium_base::contracts_common::Duration;
use plt_scheduler_types::types::protocol_version::ProtocolVersion;

/// Allocate new external chain parameters with an initial maximum lock duration.
///
/// # Safety
///
/// - `params_out` must be non-null and valid for writing.
#[unsafe(no_mangle)]
extern "C" fn ffi_p11_new_external_chain_parameters(
    max_lock_duration: u64,
    params_out: *mut *mut PersistentChainParameters,
) -> status::FfiStatusCode {
    let panic_message = status::catch_unwind(move || {
        assert!(!params_out.is_null(), "params_out is a null pointer.");
        unsafe {
            *params_out = Box::into_raw(Box::new(
                PersistentChainParameters::p11_new_external_chain_parameters(
                    Duration::from_millis(max_lock_duration),
                ),
            ));
        }
    });
    if let Some(message) = panic_message {
        eprintln!("{}", message);
        status::FfiStatusCode::Panic
    } else {
        status::FfiStatusCode::Success
    }
}

/// Deallocate external chain parameters.
///
/// # Safety
///
/// - `params` must be a unique, non-null pointer to a well-formed [`PersistentChainParameters`].
#[unsafe(no_mangle)]
extern "C" fn ffi_free_external_chain_parameters(params: *mut PersistentChainParameters) {
    let panic_message = status::catch_unwind(move || {
        assert!(!params.is_null(), "params is a null pointer.");
        let params = unsafe { Box::from_raw(params) };
        drop(params);
    });
    if let Some(message) = panic_message {
        eprintln!("{}", message);
    }
}

/// Load external chain parameters from the blob store.
///
/// # Safety
///
/// - `load_callback` must be a valid blob-store load callback.
/// - `params_out` must be non-null and valid for writing.
#[unsafe(no_mangle)]
extern "C" fn ffi_load_external_chain_parameters(
    load_callback: LoadCallback,
    blob_ref: BlobStoreLocation,
    protocol_version: u64,
    params_out: *mut *mut PersistentChainParameters,
) -> status::FfiStatusCode {
    let panic_message = status::catch_unwind(move || {
        assert!(!params_out.is_null(), "params_out is a null pointer.");
        let protocol_version =
            ProtocolVersion::try_from(protocol_version).expect("Unknown protocol version");
        let params =
            PersistentChainParameters::load_from_store(&load_callback, blob_ref, protocol_version)
                .expect("Failed loading external chain parameters");
        unsafe {
            *params_out = Box::into_raw(Box::new(params));
        }
    });
    if let Some(message) = panic_message {
        eprintln!("{}", message);
        status::FfiStatusCode::Panic
    } else {
        status::FfiStatusCode::Success
    }
}

/// Store external chain parameters in the blob store.
///
/// # Safety
///
/// - `store_callback` must be a valid blob-store store callback.
/// - `blob_ref_out` must be non-null and valid for writing.
/// - `params` must be non-null and point to well-formed [`PersistentChainParameters`].
#[unsafe(no_mangle)]
extern "C" fn ffi_store_external_chain_parameters(
    mut store_callback: StoreCallback,
    blob_ref_out: *mut BlobStoreLocation,
    params: *const PersistentChainParameters,
) -> status::FfiStatusCode {
    let panic_message = status::catch_unwind(move || {
        assert!(!blob_ref_out.is_null(), "blob_ref_out is a null pointer.");
        assert!(!params.is_null(), "params is a null pointer.");
        let params = unsafe { &*params };
        let reference = blob_store::store_to_store(&mut store_callback, params);
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

/// Cache external chain parameters into memory.
///
/// # Safety
///
/// - `load_callback` must be a valid blob-store load callback.
/// - `params` must be non-null and point to well-formed [`PersistentChainParameters`].
#[unsafe(no_mangle)]
extern "C" fn ffi_cache_external_chain_parameters(
    load_callback: LoadCallback,
    params: *const PersistentChainParameters,
) -> status::FfiStatusCode {
    let panic_message = status::catch_unwind(move || {
        assert!(!params.is_null(), "params is a null pointer.");
        let params = unsafe { &*params };
        params
            .cache_reference_values(&load_callback)
            .expect("Failed caching external chain parameters");
    });
    if let Some(message) = panic_message {
        eprintln!("{}", message);
        status::FfiStatusCode::Panic
    } else {
        status::FfiStatusCode::Success
    }
}

/// Compute the hash of external chain parameters.
///
/// # Safety
///
/// - `load_callback` must be a valid blob-store load callback.
/// - `params` must be non-null and point to well-formed [`PersistentChainParameters`].
/// - `hash_out` must be non-null and valid for writes of 32 bytes.
#[unsafe(no_mangle)]
extern "C" fn ffi_hash_external_chain_parameters(
    load_callback: LoadCallback,
    params: *const PersistentChainParameters,
    hash_out: *mut u8,
) -> status::FfiStatusCode {
    let panic_message = status::catch_unwind(move || {
        assert!(!params.is_null(), "params is a null pointer.");
        assert!(!hash_out.is_null(), "hash_out is a null pointer.");
        let params = unsafe { &*params };
        let hash = params
            .hash(&load_callback)
            .expect("Failed hashing external chain parameters");
        unsafe {
            std::ptr::copy_nonoverlapping(hash.as_ptr(), hash_out, hash.len());
        }
    });
    if let Some(message) = panic_message {
        eprintln!("{}", message);
        status::FfiStatusCode::Panic
    } else {
        status::FfiStatusCode::Success
    }
}

/// Read `max_lock_duration` from external chain parameters.
///
/// # Safety
///
/// - `params` must be non-null and point to well-formed [`PersistentChainParameters`].
/// - `duration_out` must be non-null and valid for writing.
#[unsafe(no_mangle)]
extern "C" fn ffi_get_external_chain_parameters_max_lock_duration(
    params: *const PersistentChainParameters,
    duration_out: *mut u64,
) -> status::FfiStatusCode {
    let panic_message = status::catch_unwind(move || {
        assert!(!params.is_null(), "params is a null pointer.");
        assert!(!duration_out.is_null(), "duration_out is a null pointer.");
        let params = unsafe { &*params };
        let duration = match params {
            PersistentChainParameters::P11(params) => params.max_lock_duration,
        };
        unsafe {
            *duration_out = duration.millis();
        }
    });
    if let Some(message) = panic_message {
        eprintln!("{}", message);
        status::FfiStatusCode::Panic
    } else {
        status::FfiStatusCode::Success
    }
}
