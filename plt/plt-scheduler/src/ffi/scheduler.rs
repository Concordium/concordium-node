//! This module provides a C ABI for the Rust PLT scheduler query functions.
//!
//! It is only available if the `ffi` feature is enabled.

use crate::block_state::{ExecutionTimePltBlockState, PltBlockStateSavepoint};
use crate::ffi::blob_store_callbacks::LoadCallback;
use crate::ffi::block_state_callbacks::{
    ExternalBlockStateOperationCallbacks, ExternalBlockStateQueryCallbacks,
    GetAccountIndexByAddressCallback, GetCanonicalAddressByAccountIndexCallback,
    GetTokenAccountStatesCallback, IncrementPltUpdateSequenceNumberCallback,
    ReadTokenAccountBalanceCallback, UpdateTokenAccountBalanceCallback,
};
use crate::ffi::memory;
use crate::scheduler;
use concordium_base::base::{AccountIndex, Energy};
use concordium_base::contracts_common::AccountAddress;
use concordium_base::transactions::Payload;
use concordium_base::updates::UpdatePayload;
use concordium_base::{common, contracts_common};
use libc::size_t;
use plt_types::types::execution::{ChainUpdateOutcome, TransactionOutcome};

/// C-binding for calling [`scheduler::execute_transaction`].
///
/// Returns a byte representing the result:
///
/// - `0`: Transaction execution succeeded and transaction was applied to block state.
/// - `1`: Transaction was rejected with a reject reason. Block state changes applied
///   via callbacks must be rolled back.
///
/// # Arguments
///
/// - `load_callback` External function to call for loading bytes a reference from the blob store.
/// - `read_token_account_balance_callback` External function to call reading the token balance of an account.
/// - `update_token_account_balance_callback` External function to call updating the token balance of an account.
/// - `increment_plt_update_sequence_number_callback` External function for incrementing the PLT update instruction sequence number.
/// - `get_account_address_by_index_callback` External function for getting account canonical address by account index.
/// - `get_account_index_by_address_callback` External function for getting account index by account address.
/// - `get_token_account_states_callback` External function for getting token account states.
/// - `block_state` Shared pointer to a block state to use as input state to execution.
/// - `payload` Shared pointer to transaction payload bytes.
/// - `payload_len` Byte length of transaction payload.
/// - `sender_account_index` The account index of the account which signed as the sender of the transaction.
/// - `sender_account_address` The account address of the account which signed as the sender of the transaction.
/// - `remaining_energy` The remaining energy at the start of the execution.
/// - `block_state_out` Location for writing the pointer of the updated block state.
///   The block state is only written if return value is `0`.
///   The pointer written is to a uniquely owned instance.
///   The caller must free the written block state using `ffi_free_plt_block_state` when it is no longer used.
/// - `used_energy_out` Location for writing the energy used by the execution.
/// - `return_data_out` Location for writing pointer to array containing return data, which is either serialized events or reject reason.
///   If the return value is `0`, the data is a list of block item events. If the return value is `1`, it is a transaction reject reason.
///   The pointer written is to a uniquely owned array.
///   The caller must free the written array using `free_array_len_2` when it is no longer used.
/// - `return_data_len_out` Location for writing the length of the array whose pointer was written to `return_data_out`.
///
/// # Safety
///
/// - All callback arguments must be a valid function pointers to functions with a signature matching the
///   signature of Rust type of the function pointer.
/// - Argument `block_state` must be a non-null pointer to well-formed [`PltBlockStateSavepoint`].
///   The pointer is to a shared instance, hence only valid for reading (writing only allowed through interior mutability).
/// - Argument `payload` must be non-null and valid for reads for `payload_len` many bytes.
/// - Argument `sender_account_address` must be non-null and valid for reads for 32 bytes.
/// - Argument `block_state_out` must be a non-null and valid pointer for writing
/// - Argument `used_energy_out` must be a non-null and valid pointer for writing
/// - Argument `return_data_out` must be a non-null and valid pointer for writing
/// - Argument `return_data_len_out` must be a non-null and valid pointer for writing
#[unsafe(no_mangle)]
extern "C" fn ffi_execute_transaction(
    load_callback: LoadCallback,
    read_token_account_balance_callback: ReadTokenAccountBalanceCallback,
    update_token_account_balance_callback: UpdateTokenAccountBalanceCallback,
    increment_plt_update_sequence_number_callback: IncrementPltUpdateSequenceNumberCallback,
    get_account_index_by_address_callback: GetAccountIndexByAddressCallback,
    get_account_address_by_index_callback: GetCanonicalAddressByAccountIndexCallback,
    get_token_account_states_callback: GetTokenAccountStatesCallback,
    block_state: *const PltBlockStateSavepoint,
    payload: *const u8,
    payload_len: size_t,
    sender_account_index: u64,
    sender_account_address: *const u8,
    remaining_energy: u64,
    block_state_out: *mut *mut PltBlockStateSavepoint,
    used_energy_out: *mut u64,
    return_data_out: *mut *mut u8,
    return_data_len_out: *mut size_t,
) -> u8 {
    assert!(!block_state.is_null(), "block_state is a null pointer.");
    assert!(!payload.is_null(), "payload is a null pointer.");
    assert!(
        !block_state_out.is_null(),
        "block_state_out is a null pointer."
    );
    assert!(
        !used_energy_out.is_null(),
        "used_energy_out is a null pointer."
    );
    assert!(
        !sender_account_address.is_null(),
        "sender_account_address is a null pointer."
    );
    assert!(
        !return_data_len_out.is_null(),
        "return_data_len_out is a null pointer."
    );
    assert!(
        !return_data_out.is_null(),
        "return_data_out is a null pointer."
    );

    let external_callbacks = ExternalBlockStateOperationCallbacks {
        queries: ExternalBlockStateQueryCallbacks {
            read_token_account_balance_ptr: read_token_account_balance_callback,
            get_account_address_by_index_ptr: get_account_address_by_index_callback,
            get_account_index_by_address_ptr: get_account_index_by_address_callback,
            get_token_account_states_ptr: get_token_account_states_callback,
        },
        update_token_account_balance_ptr: update_token_account_balance_callback,
        increment_plt_update_sequence_number_ptr: increment_plt_update_sequence_number_callback,
    };

    let internal_block_state = unsafe { (*block_state).mutable_state() };
    let mut block_state = ExecutionTimePltBlockState {
        internal_block_state,
        backing_store_load: load_callback,
        external_block_state: external_callbacks,
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

    let payload_bytes = unsafe { std::slice::from_raw_parts(payload, payload_len) };
    // todo implement error handling for unrecoverable errors in https://linear.app/concordium/issue/PSR-39/decide-and-implement-strategy-for-handling-panics-in-the-rust-code
    let payload: Payload = common::from_bytes_complete(payload_bytes).unwrap();

    let remaining_energy = Energy::from(remaining_energy);

    let result = scheduler::execute_transaction(
        sender_account_index,
        sender_account_address,
        &mut block_state,
        payload,
        remaining_energy,
    );

    // todo implement error handling for unrecoverable errors in https://linear.app/concordium/issue/PSR-39/decide-and-implement-strategy-for-handling-panics-in-the-rust-code
    let summary = result.unwrap();

    unsafe {
        *used_energy_out = summary.energy_used.energy;
    }

    let (return_status, return_data) = match summary.outcome {
        TransactionOutcome::Success(events) => {
            unsafe {
                *block_state_out =
                    Box::into_raw(Box::new(block_state.internal_block_state.savepoint()));
            }
            (0, common::to_bytes(&events))
        }
        TransactionOutcome::Rejected(reject_reason) => (1, common::to_bytes(&reject_reason)),
    };

    let array = memory::alloc_array_from_vec(return_data);
    unsafe {
        *return_data_len_out = array.length;
        *return_data_out = array.array;
    }

    return_status
}

/// C-binding for calling [`scheduler::execute_chain_update`].
///
/// Returns a byte representing the result:
///
/// - `0`: Chain update execution succeeded and update was applied to block state.
/// - `1`: Chain update failed. Block state changes applied
///   via callbacks must be rolled back.
///
/// # Arguments
///
/// - `load_callback` External function to call for loading bytes a reference from the blob store.
/// - `read_token_account_balance_callback` External function to call reading the token balance of an account.
/// - `update_token_account_balance_callback` External function to call updating the token balance of an account.
/// - `increment_plt_update_sequence_number_callback` External function for incrementing the PLT update instruction sequence number.
/// - `get_account_address_by_index_callback` External function for getting account canonical address by account index.
/// - `get_account_index_by_address_callback` External function for getting account index by account address.
/// - `get_token_account_states_callback` External function for getting token account states.
/// - `block_state` Shared pointer to a block state to use as input state to execution.
/// - `payload` Shared pointer to chain update payload bytes.
/// - `payload_len` Byte length of chain update payload.
/// - `block_state_out` Location for writing the pointer of the updated block state.
///   The block state is only written if return value is `0`.
///   The pointer written is to a uniquely owned instance.
///   The caller must free the written block state using `ffi_free_plt_block_state` when it is no longer used.
/// - `return_data_out` Location for writing pointer to array containing return data, which is either serialized events or a failure kind.
///   If the return value is `0`, the data is a list of block item events. If the return value is `1`, it is a failure kind.
///   The pointer written is to a uniquely owned array.
///   The caller must free the written array using `free_array_len_2` when it is no longer used.
/// - `return_data_len_out` Location for writing the length of the array whose pointer was written to `return_data_out`.
///
/// # Safety
///
/// - All callback arguments must be a valid function pointers to functions with a signature matching the
///   signature of Rust type of the function pointer.
/// - Argument `block_state` must be a non-null pointer to well-formed [`crate::block_state::PltBlockStateSavepoint`].
///   The pointer is to a shared instance, hence only valid for reading (writing only allowed through interior mutability).
/// - Argument `payload` must be non-null and valid for reads for `payload_len` many bytes.
/// - Argument `block_state_out` must be a non-null and valid pointer for writing
/// - Argument `return_data_out` must be a non-null and valid pointer for writing
/// - Argument `return_data_len_out` must be a non-null and valid pointer for writing
#[unsafe(no_mangle)]
extern "C" fn ffi_execute_chain_update(
    load_callback: LoadCallback,
    read_token_account_balance_callback: ReadTokenAccountBalanceCallback,
    update_token_account_balance_callback: UpdateTokenAccountBalanceCallback,
    increment_plt_update_sequence_number_callback: IncrementPltUpdateSequenceNumberCallback,
    get_account_index_by_address_callback: GetAccountIndexByAddressCallback,
    get_account_address_by_index_callback: GetCanonicalAddressByAccountIndexCallback,
    get_token_account_states_callback: GetTokenAccountStatesCallback,
    block_state: *const PltBlockStateSavepoint,
    payload: *const u8,
    payload_len: size_t,
    block_state_out: *mut *mut PltBlockStateSavepoint,
    return_data_out: *mut *mut u8,
    return_data_len_out: *mut size_t,
) -> u8 {
    println!("called ffi_execute_chain_update"); // todo ar
    assert!(!block_state.is_null(), "block_state is a null pointer.");
    assert!(!payload.is_null(), "payload is a null pointer.");
    assert!(
        !block_state_out.is_null(),
        "block_state_out is a null pointer."
    );
    assert!(
        !return_data_len_out.is_null(),
        "return_data_len_out is a null pointer."
    );
    assert!(
        !return_data_out.is_null(),
        "return_data_out is a null pointer."
    );

    let external_callbacks = ExternalBlockStateOperationCallbacks {
        queries: ExternalBlockStateQueryCallbacks {
            read_token_account_balance_ptr: read_token_account_balance_callback,
            get_account_address_by_index_ptr: get_account_address_by_index_callback,
            get_account_index_by_address_ptr: get_account_index_by_address_callback,
            get_token_account_states_ptr: get_token_account_states_callback,
        },
        update_token_account_balance_ptr: update_token_account_balance_callback,
        increment_plt_update_sequence_number_ptr: increment_plt_update_sequence_number_callback,
    };

    let internal_block_state = unsafe { (*block_state).mutable_state() };
    let mut block_state = ExecutionTimePltBlockState {
        internal_block_state,
        backing_store_load: load_callback,
        external_block_state: external_callbacks,
    };

    let payload_bytes = unsafe { std::slice::from_raw_parts(payload, payload_len) };
    // todo implement error handling for unrecoverable errors in https://linear.app/concordium/issue/PSR-39/decide-and-implement-strategy-for-handling-panics-in-the-rust-code
    let payload: UpdatePayload = common::from_bytes_complete(payload_bytes).unwrap();

    let result = scheduler::execute_chain_update(&mut block_state, payload);

    // todo implement error handling for unrecoverable errors in https://linear.app/concordium/issue/PSR-39/decide-and-implement-strategy-for-handling-panics-in-the-rust-code
    let outcome = result.unwrap();

    let (return_status, return_data) = match outcome {
        ChainUpdateOutcome::Success(events) => {
            unsafe {
                *block_state_out =
                    Box::into_raw(Box::new(block_state.internal_block_state.savepoint()));
            }
            (0, common::to_bytes(&events))
        }
        ChainUpdateOutcome::Failed(failure_kind) => (1, common::to_bytes(&failure_kind)),
    };

    let array = memory::alloc_array_from_vec(return_data);
    unsafe {
        *return_data_len_out = array.length;
        *return_data_out = array.array;
    }

    return_status
}
