//! This module provides a C ABI for the Rust PLT scheduler library.
//!
//! It is only available if the `ffi` feature is enabled.

use crate::block_state::blob_store::{BackingStoreLoad, BackingStoreStore};
use crate::block_state::{
    BlockStateExternal, BlockStateSavepoint, ExecutionTimeBlockState, ReadTokenAccountBalance,
    TokenIndex, UpdateTokenAccountBalance, blob_store,
};
use crate::block_state_interface::{OverflowError, RawTokenAmountDelta};
use crate::scheduler;
use crate::scheduler::TransactionOutcome;
use concordium_base::base::{AccountIndex, Energy};
use concordium_base::contracts_common::AccountAddress;
use concordium_base::transactions::Payload;
use concordium_base::{common, contracts_common};
use libc::size_t;
use plt_token_module::token_kernel_interface::RawTokenAmount;
use std::mem;

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
/// - `update_token_account_balance_callback` External function to call reading the token balance of an account.
/// - `update_token_account_balance_callback` External function to call updating the token balance of an account.
/// - `block_state` Unique pointer to a block state to mutate during execution.
/// - `payload` Pointer to transaction payload bytes.
/// - `payload_len` Byte length of transaction payload.
/// - `sender_account_index` The account index of the account which signed as the sender of the transaction.
/// - `sender_account_address` The account address of the account which signed as the sender of the transaction.
/// - `remaining_energy` The remaining energy at the start of the execution.
/// - `block_state_out` Location for writing the pointer of the updated block state.
/// - `used_energy_out` Location for writing the energy used by the execution.
///
/// # Safety
///
/// - Argument `block_state` must be non-null point to well-formed [`crate::block_state::BlockState`].
/// - Argument `payload` must be non-null and valid for reads for `payload_len` many bytes.
/// - Argument `sender_account_address` must be non-null and valid for reads for 32 bytes.
/// - Argument `block_state_out` must be a non-null and valid pointer
/// - Argument `used_energy_out` must be a non-null and valid pointer
#[unsafe(no_mangle)]
extern "C" fn ffi_execute_transaction(
    load_callback: LoadCallback,
    read_token_account_balance_callback: ReadTokenAccountBalanceCallback,
    update_token_account_balance_callback: UpdateTokenAccountBalanceCallback,
    block_state: *const BlockStateSavepoint,
    payload: *const u8,
    payload_len: size_t,
    sender_account_index: u64,
    sender_account_address: *const u8,
    remaining_energy: u64,
    block_state_out: *mut *const BlockStateSavepoint,
    used_energy_out: *mut u64,
) -> u8 {
    assert!(!block_state.is_null(), "Block state is a null pointer.");
    assert!(!payload.is_null(), "Payload is a null pointer.");

    let mut block_state = ExecutionTimeBlockState::<BlockStateCallbacks> {
        inner_block_state: unsafe { (*block_state).new_generation() },
        load_callback,
        read_token_account_balance_callback,
        update_token_account_balance_callback,
    };

    let sender_account_index = AccountIndex::from(sender_account_index);
    let sender_account_address = {
        let mut address_bytes = [0u8; contracts_common::ACCOUNT_ADDRESS_SIZE];
        unsafe {
            std::ptr::copy_nonoverlapping(
                sender_account_address,
                address_bytes.as_mut_ptr(),
                contracts_common::ACCOUNT_ADDRESS_SIZE,
            );
        }
        AccountAddress(address_bytes)
    };

    let sender = (sender_account_index, sender_account_address);

    let mut payload_bytes = unsafe { std::slice::from_raw_parts(payload, payload_len) };
    // todo implement error handling for unrecoverable errors in https://linear.app/concordium/issue/PSR-39/decide-and-implement-strategy-for-handling-panics-in-the-rust-code
    let payload: Payload = common::from_bytes(&mut payload_bytes).unwrap();

    let remaining_energy = Energy::from(remaining_energy);

    let result =
        scheduler::execute_transaction(sender, &mut block_state, payload, remaining_energy);

    // todo implement error handling for unrecoverable errors in https://linear.app/concordium/issue/PSR-39/decide-and-implement-strategy-for-handling-panics-in-the-rust-code
    let summary = result.unwrap();

    unsafe {
        *used_energy_out = summary.energy_used.energy;
    }

    match summary.outcome {
        // todo marshal reject reasons and events as part of https://linear.app/concordium/issue/PSR-44/implement-serialization-and-returning-events-and-reject-reasons
        TransactionOutcome::Success(_events) => {
            unsafe {
                *block_state_out =
                    Box::into_raw(Box::new(block_state.inner_block_state.savepoint()));
            }
            0
        }
        TransactionOutcome::Rejected(_reject_reason) => {
            unsafe {
                *block_state_out = std::ptr::null();
            };
            1
        }
    }
}

struct BlockStateCallbacks;

impl BlockStateExternal for BlockStateCallbacks {
    type BackingStoreLoad = LoadCallback;
    type ReadTokenAccountBalance = ReadTokenAccountBalanceCallback;
    type UpdateTokenAccountBalance = UpdateTokenAccountBalanceCallback;
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
    block_state: *mut BlockStateSavepoint,
) -> *mut BlockStateSavepoint {
    assert!(!block_state.is_null(), "Block state is a null pointer.");
    let block_state = unsafe { &mut *block_state };
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
    fn store_raw(&mut self, data: &[u8]) -> blob_store::Reference {
        self(data.as_ptr(), data.len())
    }
}

impl BackingStoreLoad for LoadCallback {
    fn load_raw(&mut self, location: blob_store::Reference) -> Vec<u8> {
        let vec_from_different_allocator = unsafe { Box::from_raw(self(location)) };

        let vec = vec_from_different_allocator.as_ref().clone();

        // todo free memory as part of https://linear.app/concordium/issue/COR-2113/fix-rust-allocator-issue-related-to-multiple-rust-cdylibs
        mem::forget(vec_from_different_allocator);

        vec
    }
}

/// External function for updating the token balance for an account.
///
/// Returns non-zero if the balance overflows.
///
/// # Arguments
///
/// - `account_index` The index of the account to update a token balance for. Must be a valid account index of an existing account.
/// - `token_index` The index of the token. Must be a valid token index of an existing token.
/// - `amount` The amount to add to or subtract from the balance.
/// - `add_amount` If `1`, the amount will be added to the balance. If `0`, it will be subtracted.
type UpdateTokenAccountBalanceCallback =
    extern "C" fn(account_index: u64, token_index: u64, amount: u64, add_amount: u8) -> u8;

impl UpdateTokenAccountBalance for UpdateTokenAccountBalanceCallback {
    fn update_token_account_balance(
        &mut self,
        account: AccountIndex,
        token: TokenIndex,
        amount_delta: RawTokenAmountDelta,
    ) -> Result<(), OverflowError> {
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
            Err(OverflowError)
        }
    }
}

/// External function for reading the token balance for an account.
///
/// # Arguments
///
/// - `account_index` The index of the account to update a token balance for. Must be a valid account index of an existing account.
/// - `token_index` The index of the token. Must be a valid token index of an existing token.
type ReadTokenAccountBalanceCallback = extern "C" fn(account_index: u64, token_index: u64) -> u64;

impl ReadTokenAccountBalance for ReadTokenAccountBalanceCallback {
    fn read_token_account_balance(
        &self,
        account: AccountIndex,
        token: TokenIndex,
    ) -> RawTokenAmount {
        let value = self(account.index, token.0);

        RawTokenAmount(value)
    }
}
