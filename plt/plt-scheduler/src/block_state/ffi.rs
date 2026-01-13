//! This module provides a C ABI for the block state implementation of this library.
//!
//! It is only available if the `ffi` feature is enabled.

use crate::block_state::{self, TokenIndex, UpdateTokenAccountBalanceInBlockState, blob_store};
use crate::block_state_interface::{RawTokenAmountDelta, UnderOrOverflowError};
use concordium_base::base::AccountIndex;
use libc::size_t;

/// A [loader](BackingStoreLoad) implemented by an external function.
/// This is the dual to [`StoreCallback`]
pub type LoadCallback = extern "C" fn(blob_store::Reference) -> *mut Vec<u8>;

/// A [storer](BackingStoreStore) implemented by an external function.
/// The function is passed a pointer to data to store, and the size of data. It
/// should return the location where the data can be loaded via a
/// [`LoadCallback`].
pub type StoreCallback = extern "C" fn(data: *const u8, len: size_t) -> blob_store::Reference;

impl blob_store::BackingStoreStore for StoreCallback {
    #[inline]
    fn store_raw(&mut self, data: &[u8]) -> blob_store::StoreResult<blob_store::Reference> {
        Ok(self(data.as_ptr(), data.len()))
    }
}

impl blob_store::BackingStoreLoad for LoadCallback {
    type R = Vec<u8>;

    #[inline]
    fn load_raw(&mut self, location: blob_store::Reference) -> blob_store::LoadResult<Self::R> {
        Ok(*unsafe { Box::from_raw(self(location)) })
    }
}

/// Allocate a new empty PLT block state.
///
/// It is up to the caller to free this memory using [`ffi_free_plt_block_state`].
#[unsafe(no_mangle)]
extern "C" fn ffi_empty_plt_block_state() -> *mut block_state::BlockStateSavepoint {
    let block_state = block_state::BlockStateSavepoint::empty();
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
unsafe extern "C" fn ffi_free_plt_block_state(block_state: *mut block_state::BlockStateSavepoint) {
    debug_assert!(!block_state.is_null(), "Block state is a null pointer.");
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
unsafe extern "C" fn ffi_hash_plt_block_state(
    load_callback: LoadCallback,
    destination: *mut u8,
    block_state: *const block_state::BlockStateSavepoint,
) {
    debug_assert!(!block_state.is_null(), "Block state is a null pointer.");
    let block_state = unsafe { &*block_state };
    let hash = block_state.hash(load_callback);
    unsafe { std::ptr::copy_nonoverlapping(hash.as_ptr(), destination, hash.len()) };
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
) -> *mut block_state::BlockStateSavepoint {
    match blob_store::Loadable::load_from_location(&mut load_callback, blob_ref) {
        Ok(block_state) => Box::into_raw(Box::new(block_state)),
        Err(_) => std::ptr::null_mut(),
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
/// Caller must ensure `block_state` is non-null and points to a valid block state.
#[unsafe(no_mangle)]
unsafe extern "C" fn ffi_store_plt_block_state(
    mut store_callback: StoreCallback,
    block_state: *mut block_state::BlockStateSavepoint,
) -> blob_store::Reference {
    debug_assert!(!block_state.is_null(), "Block state is a null pointer.");
    let block_state = unsafe { &mut *block_state };
    match block_state.store_update(&mut store_callback) {
        Ok(r) => r,
        Err(_) => unreachable!(
            "Storing the block state can only fail if the writer fails. This is assumed not to happen."
        ),
    }
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
unsafe extern "C" fn ffi_migrate_plt_block_state(
    load_callback: LoadCallback,
    mut store_callback: StoreCallback,
    block_state: *mut block_state::BlockStateSavepoint,
) -> *mut block_state::BlockStateSavepoint {
    debug_assert!(!block_state.is_null(), "Block state is a null pointer.");
    let block_state = unsafe { &mut *block_state };
    match block_state.migrate(&load_callback, &mut store_callback) {
        Ok(new_block_state) => Box::into_raw(Box::new(new_block_state)),
        Err(_) => std::ptr::null_mut(),
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
/// Caller must ensure `block_state` is non-null and points to a valid block state.
#[unsafe(no_mangle)]
unsafe extern "C" fn ffi_cache_plt_block_state(
    load_callback: LoadCallback,
    block_state: *mut block_state::BlockStateSavepoint,
) {
    debug_assert!(!block_state.is_null(), "Block state is a null pointer.");
    let block_state = unsafe { &mut *block_state };
    block_state.cache(&load_callback)
}

/// External function for updating the token balance for an account.
///
/// Returns non-zero if the balance overflows.
///
/// # Arguments
///
/// - `account_index` The index of the account to update a token balance for.
/// - `token_index` The index of the token.
/// - `add_amount` The amount to add to the balance.
/// - `remove_amount` The amount to subtract from the balance.
///
/// # Safety
///
/// Argument `account_index` must be a valid account index of an existing account.
pub type UpdateTokenAccountBalanceCallback =
    extern "C" fn(account_index: u64, token_index: u64, amount: u64, add_amount: u8) -> u8;

impl UpdateTokenAccountBalanceInBlockState for UpdateTokenAccountBalanceCallback {
    fn update_token_account_balance(
        &mut self,
        token: TokenIndex,
        account: AccountIndex,
        amount_delta: RawTokenAmountDelta,
    ) -> Result<(), UnderOrOverflowError> {
        let result = self(
            account.index,
            token.0,
            match amount_delta {
                RawTokenAmountDelta::Add(amount) => amount.0,
                RawTokenAmountDelta::Subtract(amount) => amount.0,
            },
            match amount_delta {
                RawTokenAmountDelta::Add(_) => 1,
                RawTokenAmountDelta::Subtract(_) => 0,
            },
        );

        if result == 0 {
            Ok(())
        } else {
            Err(UnderOrOverflowError)
        }
    }
}
