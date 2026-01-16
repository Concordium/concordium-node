//! This module provides a C ABI for the Rust PLT scheduler library.
//!
//! It is only available if the `ffi` feature is enabled.

use crate::block_state::blob_store::{BackingStoreLoad, BackingStoreStore, LoadError, WriteError};
use crate::block_state::{
    BlockStateSavepoint, ExecutionTimeBlockState, ReadTokenAccountBalanceFromBlockState,
    TokenIndex, UpdateTokenAccountBalanceInBlockState, blob_store,
};
use crate::block_state_interface::{RawTokenAmountDelta, UnderOrOverflowError};
use crate::scheduler;
use concordium_base::base::AccountIndex;
use concordium_base::common;
use concordium_base::transactions::Payload;
use libc::size_t;
use plt_token_module::token_kernel_interface::RawTokenAmount;

/// C-binding for calling [`crate::execute_transaction`].
///
/// Returns a byte representing the status code, where the value should be interpreted as:
///
/// - `0` execution succeeded.
/// - `1` rejected due to ...
///
/// # Arguments
///
/// - `load_callback` External function to call for loading bytes a reference from the blob store.
/// - `update_token_account_balance_callback` External function to call updating the token balance of an account.
/// - `block_state` Unique pointer to a block state to mutate during execution.
/// - `payload` Pointer to transaction payload bytes.
/// - `payload_len` Byte length of transaction payload.
/// - `sender_account_index` The account index of the account which signed as the sender of the transaction.
/// - `remaining_energy` The remaining energy at the start of the execution.
/// - `block_state_out` Location for writing the pointer of the updated block state.
/// - `used_energy_out` Location for writing the energy used by the execution.
///
/// # Safety
///
/// - Argument `block_state` must be non-null point to well-formed [`crate::block_state::BlockState`].
/// - Argument `payload` must be non-null and valid for reads for `payload_len` many bytes.
#[unsafe(no_mangle)]
extern "C" fn ffi_execute_transaction(
    load_callback: LoadCallback,
    read_token_account_balance_callback: ReadTokenAccountBalanceCallback,
    update_token_account_balance_callback: UpdateTokenAccountBalanceCallback,
    block_state: *const BlockStateSavepoint,
    payload: *const u8,
    payload_len: size_t,
    sender_account_index: u64,
    _remaining_energy: u64,
    block_state_out: *mut *const BlockStateSavepoint,
    _used_energy_out: *mut u64,
) -> u8 {
    assert!(!block_state.is_null(), "Block state is a null pointer.");
    assert!(!payload.is_null(), "Payload is a null pointer.");

    let mut block_state = ExecutionTimeBlockState {
        inner_block_state: unsafe { (*block_state).new_generation() },
        load_callback,
        read_token_account_balance_callback,
        update_token_account_balance_callback,
    };

    let sender = AccountIndex::from(sender_account_index);
    let mut payload_bytes = unsafe { std::slice::from_raw_parts(payload, payload_len) };
    // todo finish error handling as part of https://linear.app/concordium/issue/PSR-21/use-the-rust-scheduler-in-the-haskell-scheduler
    let payload: Payload = common::from_bytes(&mut payload_bytes).unwrap();
    let result = scheduler::execute_transaction(sender, &mut block_state, payload).unwrap();

    let block_state = block_state.inner_block_state;

    // todo set remaining energy as part of https://linear.app/concordium/issue/PSR-21/use-the-rust-scheduler-in-the-haskell-scheduler

    match result {
        // todo map reject reasons and events as part of https://linear.app/concordium/issue/PSR-21/use-the-rust-scheduler-in-the-haskell-scheduler
        Ok(_) => {
            unsafe { *block_state_out = Box::into_raw(Box::new(block_state.savepoint())) };
            0
        }
        Err(_) => {
            unsafe { *block_state_out = std::ptr::null() };
            1
        }
    }
}

/// Allocate a new empty PLT block state.
///
/// It is up to the caller to free this memory using [`ffi_free_plt_block_state`].
#[unsafe(no_mangle)]
extern "C" fn ffi_empty_plt_block_state() -> *mut BlockStateSavepoint {
    let block_state = BlockStateSavepoint::empty();
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
extern "C" fn ffi_free_plt_block_state(block_state: *mut BlockStateSavepoint) {
    assert!(!block_state.is_null(), "Block state is a null pointer.");
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
    block_state: *const BlockStateSavepoint,
) {
    assert!(!block_state.is_null(), "Block state is a null pointer.");
    let block_state = unsafe { &*block_state };
    let hash = block_state.hash(&mut load_callback);
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
) -> *mut BlockStateSavepoint {
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
extern "C" fn ffi_store_plt_block_state(
    mut store_callback: StoreCallback,
    block_state: *mut BlockStateSavepoint,
) -> blob_store::Reference {
    assert!(!block_state.is_null(), "Block state is a null pointer.");
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
extern "C" fn ffi_migrate_plt_block_state(
    mut load_callback: LoadCallback,
    mut store_callback: StoreCallback,
    block_state: *mut BlockStateSavepoint,
) -> *mut BlockStateSavepoint {
    assert!(!block_state.is_null(), "Block state is a null pointer.");
    let block_state = unsafe { &mut *block_state };
    match block_state.migrate(&mut load_callback, &mut store_callback) {
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
extern "C" fn ffi_cache_plt_block_state(
    mut load_callback: LoadCallback,
    block_state: *mut BlockStateSavepoint,
) {
    assert!(!block_state.is_null(), "Block state is a null pointer.");
    let block_state = unsafe { &mut *block_state };
    block_state.cache(&mut load_callback)
}

/// A [loader](BackingStoreLoad) implemented by an external function.
/// This is the dual to [`StoreCallback`]
type LoadCallback = extern "C" fn(blob_store::Reference) -> *mut Vec<u8>;

/// A [storer](BackingStoreStore) implemented by an external function.
/// The function is passed a pointer to data to store, and the size of data. It
/// should return the location where the data can be loaded via a
/// [`LoadCallback`].
type StoreCallback = extern "C" fn(data: *const u8, len: size_t) -> blob_store::Reference;

impl BackingStoreStore for StoreCallback {
    fn store_raw(&mut self, data: &[u8]) -> Result<blob_store::Reference, WriteError> {
        Ok(self(data.as_ptr(), data.len()))
    }
}

impl BackingStoreLoad for LoadCallback {
    fn load_raw(&mut self, location: blob_store::Reference) -> Result<Vec<u8>, LoadError> {
        Ok(*unsafe { Box::from_raw(self(location)) })
    }
}

/// External function for updating the token balance for an account.
///
/// Returns non-zero if the balance overflows.
///
/// # Arguments
///
/// - `account_index` The index of the account to update a token balance for.
/// - `token_index` The index of the token.
/// - `amount` The amount to add to or subtract from the balance.
/// - `add_amount` If `1`, the amount will be added to the balance. If `0`, it will be subtracted.
///
/// # Safety
///
/// Argument `account_index` must be a valid account index of an existing account.
/// Argument `token_index` must be a valid token index of an existing token.
type UpdateTokenAccountBalanceCallback =
    extern "C" fn(account_index: u64, token_index: u64, amount: u64, add_amount: u8) -> u8;

impl UpdateTokenAccountBalanceInBlockState for UpdateTokenAccountBalanceCallback {
    fn update_token_account_balance(
        &mut self,
        account: AccountIndex,
        token: TokenIndex,
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

/// External function for reading the token balance for an account.
///
/// # Arguments
///
/// - `account_index` The index of the account to update a token balance for.
/// - `token_index` The index of the token.
///
/// # Safety
///
/// Argument `account_index` must be a valid account index of an existing account.
/// Argument `token_index` must be a valid token index of an existing token.
type ReadTokenAccountBalanceCallback = extern "C" fn(account_index: u64, token_index: u64) -> u64;

impl ReadTokenAccountBalanceFromBlockState for ReadTokenAccountBalanceCallback {
    fn read_token_account_balance(
        &self,
        account: AccountIndex,
        token: TokenIndex,
    ) -> RawTokenAmount {
        let value = self(account.index, token.0);

        RawTokenAmount(value)
    }
}
