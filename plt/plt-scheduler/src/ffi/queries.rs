//! This module provides a C ABI for the Rust PLT scheduler functions.
//!
//! It is only available if the `ffi` feature is enabled.

use crate::block_state::external::{
    GetAccountIndexByAddress, GetCanonicalAddressByAccountIndex, GetTokenAccountStates,
    ReadTokenAccountBalance,
};
use crate::block_state::types::{TokenAccountState, TokenIndex};
use crate::block_state::{
    ExecutionTimePltBlockState, ExternalBlockStateQuery, PltBlockStateSavepoint,
};
use crate::ffi::blob_store_callbacks::LoadCallback;
use crate::ffi::block_state_callbacks::{
    GetAccountIndexByAddressCallback, GetCanonicalAddressByAccountIndexCallback,
    GetTokenAccountStatesCallback, ReadTokenAccountBalanceCallback,
};
use crate::queries;
use concordium_base::base::AccountIndex;
use concordium_base::common;
use concordium_base::contracts_common::AccountAddress;
use libc::size_t;
use plt_scheduler_interface::error::{AccountNotFoundByAddressError, AccountNotFoundByIndexError};
use plt_types::types::tokens::RawTokenAmount;

/// Callbacks types definition
struct ExternalBlockStateQueryCallbacks {
    /// External function for reading the token balance for an account.
    read_token_account_balance: ReadTokenAccountBalanceCallback,
    /// External function for fetching account address by index.
    get_account_address_by_index: GetCanonicalAddressByAccountIndexCallback,
    /// External function for fetching account index by address.
    get_account_index_by_address: GetAccountIndexByAddressCallback,
    /// External function for getting token account states.
    get_token_account_states: GetTokenAccountStatesCallback,
}

impl ReadTokenAccountBalance for ExternalBlockStateQueryCallbacks {
    fn read_token_account_balance(
        &self,
        account: AccountIndex,
        token: TokenIndex,
    ) -> RawTokenAmount {
        self.read_token_account_balance
            .read_token_account_balance(account, token)
    }
}

impl GetCanonicalAddressByAccountIndex for ExternalBlockStateQueryCallbacks {
    fn account_canonical_address_by_account_index(
        &self,
        account_index: AccountIndex,
    ) -> Result<AccountAddress, AccountNotFoundByIndexError> {
        self.get_account_address_by_index
            .account_canonical_address_by_account_index(account_index)
    }
}

impl GetAccountIndexByAddress for ExternalBlockStateQueryCallbacks {
    fn account_index_by_account_address(
        &self,
        account_address: &AccountAddress,
    ) -> Result<AccountIndex, AccountNotFoundByAddressError> {
        self.get_account_index_by_address
            .account_index_by_account_address(account_address)
    }
}

impl GetTokenAccountStates for ExternalBlockStateQueryCallbacks {
    fn token_account_states(
        &self,
        account_index: AccountIndex,
    ) -> Vec<(TokenIndex, TokenAccountState)> {
        self.get_token_account_states
            .token_account_states(account_index)
    }
}

impl ExternalBlockStateQuery for ExternalBlockStateQueryCallbacks {}

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
/// - `block_state` Pointer to a block state to use for queries.
/// - `return_data_out` Location for writing pointer to array containing return data, which is serialized tokens ids.
///   If the return value is `0`, the data is a list of token ids.
///   The caller must free the written block state using `free_array_len_2` when it is no longer used.    
/// - `return_data_len_out` Location for writing the length of the array whose pointer was written to `return_data_out`.
///
/// # Safety
///
/// - Argument `block_state` must be non-null point to well-formed [`crate::block_state::PltBlockStateSavepoint`].
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
    return_data_out: *mut *const u8,
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
        read_token_account_balance: read_token_account_balance_callback,
        get_account_address_by_index: get_account_address_by_index_callback,
        get_account_index_by_address: get_account_index_by_address_callback,
        get_token_account_states: get_token_account_states_callback,
    };

    let internal_block_state = unsafe { &*block_state };
    let block_state = ExecutionTimePltBlockState {
        internal_block_state,
        backing_store_load: load_callback,
        external_block_state: external_callbacks,
    };

    let token_ids = queries::plt_list(&block_state);

    let mut return_data = common::to_bytes(&token_ids);

    // shrink Vec should that we know capacity and length are equal (this is important when we later free with free_array_len_2)
    return_data.shrink_to_fit();
    // todo now we assert that capacity is equals to the length, but we should address that this may not be the case in a better way, see https://linear.app/concordium/issue/COR-2181/address-potentially-unsafe-behaviour-cased-by-using-shrink-to-fit
    assert_eq!(
        return_data.capacity(),
        return_data.len(),
        "vec capacity not equal to length after call to shrink_to_fit"
    );
    unsafe {
        *return_data_len_out = return_data.len() as size_t;
        *return_data_out = return_data.as_mut_ptr();
    }
    std::mem::forget(return_data);

    // todo implement error handling for unrecoverable errors in https://linear.app/concordium/issue/PSR-39/decide-and-implement-strategy-for-handling-panics-in-the-rust-code
    0
}
