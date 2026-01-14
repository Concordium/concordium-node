//! This module provides a C ABI for the top-level functions of this library.
//!
//! It is only available if the `ffi` feature is enabled.

use crate::block_state::ExecutionTimeBlockState;
use crate::{block_state, scheduler};
use concordium_base::base::AccountIndex;
use concordium_base::common;
use concordium_base::transactions::Payload;
use libc::size_t;

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
unsafe extern "C" fn ffi_execute_transaction(
    load_callback: block_state::ffi::LoadCallback,
    read_token_account_balance_callback: block_state::ffi::ReadTokenAccountBalanceCallback,
    update_token_account_balance_callback: block_state::ffi::UpdateTokenAccountBalanceCallback,
    block_state: *const block_state::BlockStateSavepoint,
    payload: *const u8,
    payload_len: size_t,
    sender_account_index: u64,
    _remaining_energy: u64,
    block_state_out: *mut *const block_state::BlockStateSavepoint,
    _used_energy_out: *mut u64,
) -> u8 {
    debug_assert!(!block_state.is_null(), "Block state is a null pointer.");
    debug_assert!(!payload.is_null(), "Payload is a null pointer.");

    let mut block_state = ExecutionTimeBlockState {
        inner_block_state: unsafe { (*block_state).new_generation() },
        load_callback,
        read_token_account_balance_callback,
        update_token_account_balance_callback,
    };

    let sender_account_address = todo!();
    let sender = (
        AccountIndex::from(sender_account_index),
        sender_account_address,
    );
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
