//! This module provides a C ABI for the Rust PLT scheduler functions.
//!
//! It is only available if the `ffi` feature is enabled.

use crate::block_state::{BlockStateExternal, ExecutionTimePltBlockState, PltBlockStateSavepoint};
use crate::ffi::blob_store_callbacks::LoadCallback;
use crate::ffi::block_state_callbacks::{
    GetAccountIndexByAddressCallback, GetCanonicalAddressByAccountIndexCallback,
    IncrementPltUpdateSequenceNumberCallback, ReadTokenAccountBalanceCallback,
    UpdateTokenAccountBalanceCallback,
};
use crate::scheduler;
use concordium_base::base::{AccountIndex, Energy};
use concordium_base::contracts_common::AccountAddress;
use concordium_base::transactions::Payload;
use concordium_base::{common, contracts_common};
use libc::size_t;
use plt_types::types::execution::TransactionOutcome;

/// Callbacks types definition
pub struct BlockStateCallbacks;

impl BlockStateExternal for BlockStateCallbacks {
    type ReadTokenAccountBalance = ReadTokenAccountBalanceCallback;
    type UpdateTokenAccountBalance = UpdateTokenAccountBalanceCallback;
    type IncrementPltUpdateSequenceNumber = IncrementPltUpdateSequenceNumberCallback;
    type GetCanonicalAddressByAccountIndex = GetCanonicalAddressByAccountIndexCallback;
    type GetAccountIndexByAddress = GetAccountIndexByAddressCallback;
}

/// C-binding for calling [`scheduler::execute_transaction`].
///
/// Returns a byte representing the result:
///
/// - `0`: Transaction execution succeeded and transaction was applied to block state.
/// - `1`: Transaction was rejected with a reject reason.
///
/// # Arguments
///
/// - `load_callback` External function to call for loading bytes a reference from the blob store.
/// - `update_token_account_balance_callback` External function to call reading the token balance of an account.
/// - `update_token_account_balance_callback` External function to call updating the token balance of an account.
/// - `increment_plt_update_sequence_number_callback` External function for incrementing the PLT update instruction sequence number.
/// - `get_account_address_by_index_callback` External function for getting account canonical address by account index.
/// - `get_account_index_by_address_callback` External function for getting account index by account address.
/// - `block_state` Unique pointer to a block state to mutate during execution.
/// - `payload` Pointer to transaction payload bytes.
/// - `payload_len` Byte length of transaction payload.
/// - `sender_account_index` The account index of the account which signed as the sender of the transaction.
/// - `sender_account_address` The account address of the account which signed as the sender of the transaction.
/// - `remaining_energy` The remaining energy at the start of the execution.
/// - `block_state_out` Location for writing the pointer of the updated block state.
///   The block state is only written if return value is `0`.
///   The caller must free the written block state using `ffi_free_plt_block_state` when it is no longer used.
/// - `used_energy_out` Location for writing the energy used by the execution.
/// - `return_data_out` Location for writing pointer to array containing return data, which is either serialized events or reject reason.
///   If the return value is `0`, the data is a list of block item events. If the return value is `1`, it is a transaction reject reason.
///   The caller must free the written block state using `free_array_len_2` when it is no longer used.    
/// - `return_data_len_out` Location for writing the length of the array whose pointer was written to `return_data_out`.
///
/// # Safety
///
/// - Argument `block_state` must be non-null point to well-formed [`crate::block_state::PltBlockState`].
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
    get_account_address_by_index_callback: GetCanonicalAddressByAccountIndexCallback,
    get_account_index_by_address_callback: GetAccountIndexByAddressCallback,
    block_state: *const PltBlockStateSavepoint,
    payload: *const u8,
    payload_len: size_t,
    sender_account_index: u64,
    sender_account_address: *const u8,
    remaining_energy: u64,
    block_state_out: *mut *const PltBlockStateSavepoint,
    used_energy_out: *mut u64,
    return_data_out: *mut *const u8,
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

    let mut block_state = ExecutionTimePltBlockState::<_, BlockStateCallbacks> {
        inner_block_state: unsafe { (*block_state).new_generation() },
        backing_store_load: load_callback,
        read_token_account_balance: read_token_account_balance_callback,
        update_token_account_balance: update_token_account_balance_callback,
        increment_plt_update_sequence_number: increment_plt_update_sequence_number_callback,
        get_account_address_by_index: get_account_address_by_index_callback,
        get_account_index_by_address: get_account_index_by_address_callback,
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

    let mut payload_bytes = unsafe { std::slice::from_raw_parts(payload, payload_len) };
    // todo implement error handling for unrecoverable errors in https://linear.app/concordium/issue/PSR-39/decide-and-implement-strategy-for-handling-panics-in-the-rust-code
    let payload: Payload = common::from_bytes(&mut payload_bytes).unwrap();

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

    let (return_status, mut return_data) = match summary.outcome {
        TransactionOutcome::Success(events) => {
            unsafe {
                *block_state_out =
                    Box::into_raw(Box::new(block_state.inner_block_state.savepoint()));
            }
            (0, common::to_bytes(&events))
        }
        TransactionOutcome::Rejected(reject_reason) => (1, common::to_bytes(&reject_reason)),
    };

    // shrink Vec should that we know capacity and length are equal (this is important when we later free with free_array_len_2)
    return_data.shrink_to_fit();
    // todo now we assert that capacity is equals to the length, but we should address that this may not be the case in a better way, see https://linear.app/concordium/issue/COR-2181/address-potentially-unsafe-behaviour-cased-by-using-shrink-to-fit
    assert_eq!(return_data.capacity(), return_data.len());
    unsafe {
        *return_data_len_out = return_data.len() as size_t;
        *return_data_out = return_data.as_mut_ptr();
    }
    std::mem::forget(return_data);

    return_status
}
