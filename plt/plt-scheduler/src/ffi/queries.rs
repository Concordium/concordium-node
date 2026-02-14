//! This module provides a C ABI for the Rust PLT scheduler functions.
//!
//! It is only available if the `ffi` feature is enabled.

use crate::block_state::{ExecutionTimePltBlockState, PltBlockStateSavepoint};
use crate::ffi::blob_store_callbacks::LoadCallback;
use crate::ffi::block_state_callbacks::{
    ExternalBlockStateQueryCallbacks, GetAccountIndexByAddressCallback,
    GetCanonicalAddressByAccountIndexCallback, GetTokenAccountStatesCallback,
    ReadTokenAccountBalanceCallback,
};
use crate::ffi::memory;
use crate::queries;
use concordium_base::common;
use libc::size_t;

/// C-binding for calling [`queries::plt_list`].
///
/// Returns a byte representing the result:
///
/// - `0`: Query succeeded
///
/// # Arguments
///
/// - `load_callback` External function to call for loading bytes a reference from the blob store.
/// - `read_token_account_balance_callback` External function to call reading the token balance of an account.
/// - `get_account_address_by_index_callback` External function for getting account canonical address by account index.
/// - `get_account_index_by_address_callback` External function for getting account index by account address.
/// - `get_token_account_states_callback` External function for getting token account states.
/// - `block_state` Shared pointer to a block state to use for queries.
/// - `return_data_out` Location for writing pointer to array containing return data, which is serialized tokens ids.
///   If the return value is `0`, the data is a list of token ids.
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
/// - Argument `return_data_out` must be a non-null and valid pointer for writing
/// - Argument `return_data_len_out` must be a non-null and valid pointer for writing
#[unsafe(no_mangle)]
extern "C" fn ffi_query_plt_list(
    load_callback: LoadCallback,
    read_token_account_balance_callback: ReadTokenAccountBalanceCallback,
    get_account_address_by_index_callback: GetCanonicalAddressByAccountIndexCallback,
    get_account_index_by_address_callback: GetAccountIndexByAddressCallback,
    get_token_account_states_callback: GetTokenAccountStatesCallback,
    block_state: *const PltBlockStateSavepoint,
    return_data_out: *mut *mut u8,
    return_data_len_out: *mut size_t,
) -> u8 {
    assert!(!block_state.is_null(), "block_state is a null pointer.");
    assert!(
        !return_data_len_out.is_null(),
        "return_data_len_out is a null pointer."
    );
    assert!(
        !return_data_out.is_null(),
        "return_data_out is a null pointer."
    );

    let external_callbacks = ExternalBlockStateQueryCallbacks {
        read_token_account_balance_ptr: read_token_account_balance_callback,
        get_account_address_by_index_ptr: get_account_address_by_index_callback,
        get_account_index_by_address_ptr: get_account_index_by_address_callback,
        get_token_account_states_ptr: get_token_account_states_callback,
    };

    let internal_block_state = unsafe { &*block_state };
    let block_state = ExecutionTimePltBlockState {
        internal_block_state,
        backing_store_load: load_callback,
        external_block_state: external_callbacks,
    };

    let token_ids = queries::plt_list(&block_state);

    let return_data = common::to_bytes(&token_ids);

    let array = memory::alloc_array_from_vec(return_data);
    unsafe {
        *return_data_len_out = array.length;
        *return_data_out = array.array;
    }

    // todo implement error handling for unrecoverable errors in https://linear.app/concordium/issue/PSR-39/decide-and-implement-strategy-for-handling-panics-in-the-rust-code
    0
}
