//! This module provides a C ABI for the Rust PLT scheduler query functions.
//!
//! It is only available if the `ffi` feature is enabled.

use crate::block_state::external::{
    GetAccountIndexByAddress, GetCanonicalAddressByAccountIndex, GetTokenAccountStates,
    IncrementPltUpdateSequenceNumber, ReadTokenAccountBalance, UpdateTokenAccountBalance,
};
use crate::block_state::types::{TokenAccountState, TokenIndex};
use crate::block_state::{
    ExecutionTimePltBlockState, ExternalBlockStateOperations, ExternalBlockStateQuery,
    PltBlockStateSavepoint,
};
use crate::block_state_interface::{OverflowError, RawTokenAmountDelta};
use crate::ffi::blob_store_callbacks::LoadCallback;
use crate::ffi::block_state_callbacks::{
    GetAccountIndexByAddressCallback, GetCanonicalAddressByAccountIndexCallback,
    GetTokenAccountStatesCallback, IncrementPltUpdateSequenceNumberCallback,
    ReadTokenAccountBalanceCallback, UpdateTokenAccountBalanceCallback,
};
use crate::scheduler;
use concordium_base::base::{AccountIndex, Energy};
use concordium_base::contracts_common::AccountAddress;
use concordium_base::transactions::Payload;
use concordium_base::updates::UpdatePayload;
use concordium_base::{common, contracts_common};
use libc::size_t;
use plt_scheduler_interface::error::{AccountNotFoundByAddressError, AccountNotFoundByIndexError};
use plt_types::types::execution::{ChainUpdateOutcome, TransactionOutcome};
use plt_types::types::tokens::RawTokenAmount;

/// Callbacks types definition
struct ExternalBlockStateCallbacks {
    /// External function for reading the token balance for an account.
    read_token_account_balance: ReadTokenAccountBalanceCallback,
    /// External function for updating the token balance for an account.
    update_token_account_balance: UpdateTokenAccountBalanceCallback,
    /// External function for incrementing the PLT update sequence number.
    increment_plt_update_sequence_number: IncrementPltUpdateSequenceNumberCallback,
    /// External function for fetching account address by index.
    get_account_address_by_index: GetCanonicalAddressByAccountIndexCallback,
    /// External function for fetching account index by address.
    get_account_index_by_address: GetAccountIndexByAddressCallback,
    /// External function for getting token account states.
    get_token_account_states: GetTokenAccountStatesCallback,
}

impl ReadTokenAccountBalance for ExternalBlockStateCallbacks {
    fn read_token_account_balance(
        &self,
        account: AccountIndex,
        token: TokenIndex,
    ) -> RawTokenAmount {
        self.read_token_account_balance
            .read_token_account_balance(account, token)
    }
}

impl UpdateTokenAccountBalance for ExternalBlockStateCallbacks {
    fn update_token_account_balance(
        &mut self,
        account: AccountIndex,
        token: TokenIndex,
        amount_delta: RawTokenAmountDelta,
    ) -> Result<(), OverflowError> {
        self.update_token_account_balance
            .update_token_account_balance(account, token, amount_delta)
    }
}

impl IncrementPltUpdateSequenceNumber for ExternalBlockStateCallbacks {
    fn increment_plt_update_sequence_number(&mut self) {
        self.increment_plt_update_sequence_number
            .increment_plt_update_sequence_number()
    }
}

impl GetCanonicalAddressByAccountIndex for ExternalBlockStateCallbacks {
    fn account_canonical_address_by_account_index(
        &self,
        account_index: AccountIndex,
    ) -> Result<AccountAddress, AccountNotFoundByIndexError> {
        self.get_account_address_by_index
            .account_canonical_address_by_account_index(account_index)
    }
}

impl GetAccountIndexByAddress for ExternalBlockStateCallbacks {
    fn account_index_by_account_address(
        &self,
        account_address: &AccountAddress,
    ) -> Result<AccountIndex, AccountNotFoundByAddressError> {
        self.get_account_index_by_address
            .account_index_by_account_address(account_address)
    }
}

impl GetTokenAccountStates for ExternalBlockStateCallbacks {
    fn token_account_states(
        &self,
        account_index: AccountIndex,
    ) -> Vec<(TokenIndex, TokenAccountState)> {
        self.get_token_account_states
            .token_account_states(account_index)
    }
}

impl ExternalBlockStateQuery for ExternalBlockStateCallbacks {}

impl ExternalBlockStateOperations for ExternalBlockStateCallbacks {}

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
/// - `block_state` Pointer to a block state to use as input state to execution.
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
/// - Argument `block_state` must be non-null point to well-formed [`crate::block_state::PltBlockStateSavepoint`].
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
    get_token_account_states_callback: GetTokenAccountStatesCallback,
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

    let external_callbacks = ExternalBlockStateCallbacks {
        read_token_account_balance: read_token_account_balance_callback,
        update_token_account_balance: update_token_account_balance_callback,
        increment_plt_update_sequence_number: increment_plt_update_sequence_number_callback,
        get_account_address_by_index: get_account_address_by_index_callback,
        get_account_index_by_address: get_account_index_by_address_callback,
        get_token_account_states: get_token_account_states_callback,
    };

    let mut block_state = ExecutionTimePltBlockState {
        inner_block_state: unsafe { (*block_state).mutable_state() },
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
/// - `block_state` Pointer to a block state to use as input state to execution.
/// - `payload` Pointer to chain update payload bytes.
/// - `payload_len` Byte length of chain update payload.
/// - `block_state_out` Location for writing the pointer of the updated block state.
///   The block state is only written if return value is `0`.
///   The caller must free the written block state using `ffi_free_plt_block_state` when it is no longer used.
/// - `return_data_out` Location for writing pointer to array containing return data, which is either serialized events or a failure kind.
///   If the return value is `0`, the data is a list of block item events. If the return value is `1`, it is a failure kind.
///   The caller must free the written block state using `free_array_len_2` when it is no longer used.    
/// - `return_data_len_out` Location for writing the length of the array whose pointer was written to `return_data_out`.
///
/// # Safety
///
/// - Argument `block_state` must be non-null point to well-formed [`crate::block_state::PltBlockStateSavepoint`].
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
    get_account_address_by_index_callback: GetCanonicalAddressByAccountIndexCallback,
    get_account_index_by_address_callback: GetAccountIndexByAddressCallback,
    get_token_account_states_callback: GetTokenAccountStatesCallback,
    block_state: *const PltBlockStateSavepoint,
    payload: *const u8,
    payload_len: size_t,
    block_state_out: *mut *const PltBlockStateSavepoint,
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
        !return_data_len_out.is_null(),
        "return_data_len_out is a null pointer."
    );
    assert!(
        !return_data_out.is_null(),
        "return_data_out is a null pointer."
    );

    let external_callbacks = ExternalBlockStateCallbacks {
        read_token_account_balance: read_token_account_balance_callback,
        update_token_account_balance: update_token_account_balance_callback,
        increment_plt_update_sequence_number: increment_plt_update_sequence_number_callback,
        get_account_address_by_index: get_account_address_by_index_callback,
        get_account_index_by_address: get_account_index_by_address_callback,
        get_token_account_states: get_token_account_states_callback,
    };

    let mut block_state = ExecutionTimePltBlockState {
        inner_block_state: unsafe { (*block_state).mutable_state() },
        backing_store_load: load_callback,
        external_block_state: external_callbacks,
    };

    let payload_bytes = unsafe { std::slice::from_raw_parts(payload, payload_len) };
    // todo implement error handling for unrecoverable errors in https://linear.app/concordium/issue/PSR-39/decide-and-implement-strategy-for-handling-panics-in-the-rust-code
    let payload: UpdatePayload = common::from_bytes_complete(payload_bytes).unwrap();

    let result = scheduler::execute_chain_update(&mut block_state, payload);

    // todo implement error handling for unrecoverable errors in https://linear.app/concordium/issue/PSR-39/decide-and-implement-strategy-for-handling-panics-in-the-rust-code
    let outcome = result.unwrap();

    let (return_status, mut return_data) = match outcome {
        ChainUpdateOutcome::Success(events) => {
            unsafe {
                *block_state_out =
                    Box::into_raw(Box::new(block_state.inner_block_state.savepoint()));
            }
            (0, common::to_bytes(&events))
        }
        ChainUpdateOutcome::Failed(failure_kind) => (1, common::to_bytes(&failure_kind)),
    };

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

    return_status
}
