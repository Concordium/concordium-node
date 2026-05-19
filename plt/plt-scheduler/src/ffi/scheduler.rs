//! This module provides a C ABI for the Rust PLT scheduler query functions.
//!
//! It is only available if the `ffi` feature is enabled.

use crate::ffi::status;
use crate::scheduler;
use concordium_base::base::{AccountIndex, Energy, Nonce};
use concordium_base::contracts_common::{AccountAddress, Timestamp};
use concordium_base::transactions::Payload;
use concordium_base::updates::UpdatePayload;
use concordium_base::{common, contracts_common};
use libc::size_t;
use plt_block_state::block_state::{
    ExecutionTimeBlockStateP9, ExecutionTimeBlockStateP10, ExecutionTimeBlockStateP11,
};
use plt_block_state::entity::accounts::Account;
use plt_block_state::entity::block_state::p9::BlockStateP9;
use plt_block_state::entity::block_state::p10::BlockStateP10;
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::ffi::blob_store_callbacks::LoadCallback;
use plt_block_state::ffi::block_state_callbacks::{
    ExternalBlockStateOperationCallbacks, ExternalBlockStateQueryCallbacks,
    GetAccountIndexByAddressCallback, GetCanonicalAddressByAccountIndexCallback,
    GetTokenAccountStatesCallback, IncrementPltUpdateSequenceNumberCallback,
    ReadTokenAccountBalanceCallback, TouchTokenAccountCallback, UpdateTokenAccountBalanceCallback,
};
use plt_block_state::ffi::memory;
use plt_block_state::persistent::block_state::PersistentBlockState;
use plt_scheduler_types::types::execution::{ChainUpdateOutcome, TransactionOutcome};

/// Context with no external block state (will panic if accessed).
#[derive(Debug)]
pub struct FfiSchedulerBlockStateTypes;

impl EntityContextTypes for FfiSchedulerBlockStateTypes {
    type ExternalBlockState = ExternalBlockStateOperationCallbacks;
    type Loader = LoadCallback;
}

pub type FfiSchedulerEntityContext = EntityContext<FfiSchedulerBlockStateTypes>;

/// C-binding for calling [`scheduler::execute_transaction`].
///
/// Returns a byte representing the result:
///
/// - [`status::FfiStatusCode::Success`]: Transaction execution succeeded and transaction was applied to block state.
/// - [`status::FfiStatusCode::Failed`]: Transaction was rejected with a reject reason. Block state changes applied
///   via callbacks must be rolled back.
/// - [`status::FfiStatusCode::Panic`]: Execution of the transaction resulted in an unrecoverable error or panic.
///
/// # Arguments
///
/// - `load_callback` External function to call for loading bytes a reference from the blob store.
/// - `read_token_account_balance_callback` External function to call reading the token balance of an account.
/// - `update_token_account_balance_callback` External function to call updating the token balance of an account.
/// - `touch_token_account_callback` External function to call to touch token account state.
/// - `increment_plt_update_sequence_number_callback` External function for incrementing the PLT update instruction sequence number.
/// - `get_account_address_by_index_callback` External function for getting account canonical address by account index.
/// - `get_account_index_by_address_callback` External function for getting account index by account address.
/// - `get_token_account_states_callback` External function for getting token account states.
/// - `block_state` Shared pointer to a block state to use as input state to execution.
/// - `payload` Shared pointer to transaction payload bytes.
/// - `payload_len` Byte length of transaction payload.
/// - `sender_account_index` The account index of the account which signed as the sender of the transaction.
/// - `sender_account_address` The account address of the account which signed as the sender of the transaction.
/// - `transaction_sequence_number` The account sequence number (nonce) of the transaction to execute.
/// - `block_timestamp` Timestamp of the block in which the transaction is executed.
/// - `remaining_energy` The remaining energy at the start of the execution.
/// - `block_state_out` Location for writing the pointer of the updated block state.
///   The block state is only written if return value is [`status::FfiStatusCode::Success`].
///   The pointer written is to a uniquely owned instance.
///   The caller must free the written block state using `ffi_free_plt_block_state` when it is no longer used.
/// - `used_energy_out` Location for writing the energy used by the execution.
/// - `return_data_out` Location for writing pointer to array containing return data, which is either serialized events or reject reason.
///   If the return value is [`status::FfiStatusCode::Success`], the data is a list of block item events. If the return value
///   is [`status::FfiStatusCode::Failed`], it is a transaction reject reason.
///   The pointer written is to a uniquely owned array.
///   The caller must free the written array using `free_array_len_2` when it is no longer used.
/// - `return_data_len_out` Location for writing the length of the array whose pointer was written to `return_data_out`.
///
/// # Safety
///
/// - All callback arguments must be a valid function pointers to functions with a signature matching the
///   signature of Rust type of the function pointer.
/// - Argument `block_state` must be a non-null pointer to well-formed [`BlockState`].
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
    touch_token_account_callback: TouchTokenAccountCallback,
    increment_plt_update_sequence_number_callback: IncrementPltUpdateSequenceNumberCallback,
    get_account_index_by_address_callback: GetAccountIndexByAddressCallback,
    get_account_address_by_index_callback: GetCanonicalAddressByAccountIndexCallback,
    get_token_account_states_callback: GetTokenAccountStatesCallback,
    block_state: *const PersistentBlockState,
    payload: *const u8,
    payload_len: size_t,
    sender_account_index: u64,
    sender_account_address: *const u8,
    transaction_sequence_number: Nonce,
    block_timestamp: Timestamp,
    remaining_energy: u64,
    block_state_out: *mut *mut PersistentBlockState,
    used_energy_out: *mut u64,
    return_data_out: *mut *mut u8,
    return_data_len_out: *mut size_t,
) -> status::FfiStatusCode {
    let (return_status, data_out) = status::catch_unwind(|| {
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
        let external = ExternalBlockStateOperationCallbacks {
            queries: ExternalBlockStateQueryCallbacks {
                read_token_account_balance_ptr: read_token_account_balance_callback,
                get_account_address_by_index_ptr: get_account_address_by_index_callback,
                get_account_index_by_address_ptr: get_account_index_by_address_callback,
                get_token_account_states_ptr: get_token_account_states_callback,
            },
            update_token_account_balance_ptr: update_token_account_balance_callback,
            touch_token_account_ptr: touch_token_account_callback,
            increment_plt_update_sequence_number_ptr: increment_plt_update_sequence_number_callback,
        };
        let context = FfiSchedulerEntityContext {
            external,
            loader: load_callback,
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
        let payload: Payload = common::from_bytes_complete(payload_bytes)
            .expect("Failed decoding transaction payload");
        let remaining_energy = Energy::from(remaining_energy);
        let (result, new_block_state) = match unsafe { &*block_state } {
            PersistentBlockState::P9(persistent) => {
                let block_state = BlockStateP9 {
                    persistent: persistent.clone(),
                };
                let mut exec_block_state = ExecutionTimeBlockStateP9 {
                    block_state,
                    context,
                };
                (
                    scheduler::execute_transaction(
                        Account::from_existing_account(sender_account_index),
                        sender_account_address,
                        transaction_sequence_number,
                        block_timestamp,
                        &mut exec_block_state,
                        payload,
                        remaining_energy,
                    ),
                    PersistentBlockState::P9(exec_block_state.block_state.persistent),
                )
            }
            PersistentBlockState::P10(persistent) => {
                let block_state = BlockStateP10 {
                    persistent: persistent.clone(),
                };
                let mut exec_block_state = ExecutionTimeBlockStateP10 {
                    block_state,
                    context,
                };
                (
                    scheduler::execute_transaction(
                        Account::from_existing_account(sender_account_index),
                        sender_account_address,
                        transaction_sequence_number,
                        block_timestamp,
                        &mut exec_block_state,
                        payload,
                        remaining_energy,
                    ),
                    PersistentBlockState::P10(exec_block_state.block_state.persistent),
                )
            }
            PersistentBlockState::P11(persistent) => {
                let block_state = BlockStateP11 {
                    persistent: persistent.clone(),
                };
                let mut exec_block_state = ExecutionTimeBlockStateP11 {
                    block_state,
                    context,
                };
                (
                    scheduler::execute_transaction(
                        Account::from_existing_account(sender_account_index),
                        sender_account_address,
                        transaction_sequence_number,
                        block_timestamp,
                        &mut exec_block_state,
                        payload,
                        remaining_energy,
                    ),
                    PersistentBlockState::P11(exec_block_state.block_state.persistent),
                )
            }
        };
        let summary = result.expect("Unexpected failure during transaction execution");
        unsafe {
            *used_energy_out = summary.energy_used.energy;
        }
        match summary.outcome {
            TransactionOutcome::Success(events) => {
                unsafe {
                    *block_state_out = Box::into_raw(Box::new(new_block_state));
                }
                (status::FfiStatusCode::Success, common::to_bytes(&events))
            }
            TransactionOutcome::Rejected(reject_reason) => (
                status::FfiStatusCode::Failed,
                common::to_bytes(&reject_reason),
            ),
        }
    });

    let array = memory::alloc_array_from_vec(data_out);
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
/// - [`status::FfiStatusCode::Success`]: Chain update execution succeeded and update was applied to block state.
/// - [`status::FfiStatusCode::Failed`]: Chain update failed. Block state changes applied
///   via callbacks must be rolled back.
/// - [`status::FfiStatusCode::Panic`]: Execution of the chain update resulted in an unrecoverable error or panic.
///
/// # Arguments
///
/// - `load_callback` External function to call for loading bytes a reference from the blob store.
/// - `read_token_account_balance_callback` External function to call reading the token balance of an account.
/// - `update_token_account_balance_callback` External function to call updating the token balance of an account.
/// - `touch_token_account_callback` External function to call to touch token account state.
/// - `increment_plt_update_sequence_number_callback` External function for incrementing the PLT update instruction sequence number.
/// - `get_account_address_by_index_callback` External function for getting account canonical address by account index.
/// - `get_account_index_by_address_callback` External function for getting account index by account address.
/// - `get_token_account_states_callback` External function for getting token account states.
/// - `block_state` Shared pointer to a block state to use as input state to execution.
/// - `payload` Shared pointer to chain update payload bytes.
/// - `payload_len` Byte length of chain update payload.
/// - `block_state_out` Location for writing the pointer of the updated block state.
///   The block state is only written if return value is [`status::FfiStatusCode::Success`].
///   The pointer written is to a uniquely owned instance.
///   The caller must free the written block state using `ffi_free_plt_block_state` when it is no longer used.
/// - `return_data_out` Location for writing pointer to array containing return data, which is either serialized events or a failure kind.
///   If the return value is [`status::FfiStatusCode::Success`], the data is a list of block item events. If the return value
///   is [`status::FfiStatusCode::Failed`], it is a failure kind.
///   The pointer written is to a uniquely owned array.
///   The caller must free the written array using `free_array_len_2` when it is no longer used.
/// - `return_data_len_out` Location for writing the length of the array whose pointer was written to `return_data_out`.
///
/// # Safety
///
/// - All callback arguments must be a valid function pointers to functions with a signature matching the
///   signature of Rust type of the function pointer.
/// - Argument `block_state` must be a non-null pointer to well-formed [`plt_block_state::block_state::BlockState`].
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
    touch_token_account_callback: TouchTokenAccountCallback,
    increment_plt_update_sequence_number_callback: IncrementPltUpdateSequenceNumberCallback,
    get_account_index_by_address_callback: GetAccountIndexByAddressCallback,
    get_account_address_by_index_callback: GetCanonicalAddressByAccountIndexCallback,
    get_token_account_states_callback: GetTokenAccountStatesCallback,
    block_state: *const PersistentBlockState,
    payload: *const u8,
    payload_len: size_t,
    block_state_out: *mut *mut PersistentBlockState,
    return_data_out: *mut *mut u8,
    return_data_len_out: *mut size_t,
) -> status::FfiStatusCode {
    let (return_status, return_data) = status::catch_unwind(|| {
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

        let external = ExternalBlockStateOperationCallbacks {
            queries: ExternalBlockStateQueryCallbacks {
                read_token_account_balance_ptr: read_token_account_balance_callback,
                get_account_address_by_index_ptr: get_account_address_by_index_callback,
                get_account_index_by_address_ptr: get_account_index_by_address_callback,
                get_token_account_states_ptr: get_token_account_states_callback,
            },
            update_token_account_balance_ptr: update_token_account_balance_callback,
            touch_token_account_ptr: touch_token_account_callback,
            increment_plt_update_sequence_number_ptr: increment_plt_update_sequence_number_callback,
        };
        let context = FfiSchedulerEntityContext {
            external,
            loader: load_callback,
        };
        let payload_bytes = unsafe { std::slice::from_raw_parts(payload, payload_len) };
        let payload: UpdatePayload = common::from_bytes_complete(payload_bytes)
            .expect("Failed decoding chain update payload");
        let (result, new_block_state) = match unsafe { &*block_state } {
            PersistentBlockState::P9(persistent) => {
                let block_state = BlockStateP9 {
                    persistent: persistent.clone(),
                };
                let mut exec_block_state = ExecutionTimeBlockStateP9 {
                    block_state,
                    context,
                };
                (
                    scheduler::execute_chain_update(&mut exec_block_state, payload),
                    PersistentBlockState::P9(exec_block_state.block_state.persistent),
                )
            }
            PersistentBlockState::P10(persistent) => {
                let block_state = BlockStateP10 {
                    persistent: persistent.clone(),
                };
                let mut exec_block_state = ExecutionTimeBlockStateP10 {
                    block_state,
                    context,
                };
                (
                    scheduler::execute_chain_update(&mut exec_block_state, payload),
                    PersistentBlockState::P10(exec_block_state.block_state.persistent),
                )
            }
            PersistentBlockState::P11(persistent) => {
                let block_state = BlockStateP11 {
                    persistent: persistent.clone(),
                };
                let mut exec_block_state = ExecutionTimeBlockStateP11 {
                    block_state,
                    context,
                };
                (
                    scheduler::execute_chain_update(&mut exec_block_state, payload),
                    PersistentBlockState::P11(exec_block_state.block_state.persistent),
                )
            }
        };

        let outcome = result.expect("Unexpected failure during chain update execution");
        match outcome {
            ChainUpdateOutcome::Success(events) => {
                unsafe {
                    *block_state_out = Box::into_raw(Box::new(new_block_state));
                }
                (status::FfiStatusCode::Success, common::to_bytes(&events))
            }
            ChainUpdateOutcome::Failed(failure_kind) => (
                status::FfiStatusCode::Failed,
                common::to_bytes(&failure_kind),
            ),
        }
    });

    let array = memory::alloc_array_from_vec(return_data);
    unsafe {
        *return_data_len_out = array.length;
        *return_data_out = array.array;
    }
    return_status
}

#[cfg(test)]
mod tests {
    use plt_block_state::ffi::blob_store_callbacks::tests_helpers::UNIMPLEMENTED_LOAD_CALLBACK;
    use plt_block_state::ffi::block_state_callbacks::tests_helpers::{
        UNIMPLEMENTED_GET_ACCOUNT_INDEX_BY_ADDRESS,
        UNIMPLEMENTED_GET_CANONICAL_ADDRESS_BY_ACCOUNT_INDEX,
        UNIMPLEMENTED_GET_TOKEN_ACCOUNT_STATES, UNIMPLEMENTED_INCREMENT_PLT_UPDATE_SEQUENCE_NUMBER,
        UNIMPLEMENTED_READ_TOKEN_ACCOUNT_BALANCE, UNIMPLEMENTED_TOUCH_TOKEN_ACCOUNT,
        UNIMPLEMENTED_UPDATE_TOKEN_ACCOUNT_BALANCE,
    };

    use super::*;
    use std::ptr;

    /// Test ensuring panics are caught and returned properly when providing invalid arguments to `ffi_execute_transaction`.
    #[test]
    fn test_execute_transaction_catches_panic() {
        let data_out = Box::into_raw(Box::new(ptr::null_mut()));
        let data_out_len = Box::into_raw(Box::new(0));

        let status_code = ffi_execute_transaction(
            UNIMPLEMENTED_LOAD_CALLBACK,
            UNIMPLEMENTED_READ_TOKEN_ACCOUNT_BALANCE,
            UNIMPLEMENTED_UPDATE_TOKEN_ACCOUNT_BALANCE,
            UNIMPLEMENTED_TOUCH_TOKEN_ACCOUNT,
            UNIMPLEMENTED_INCREMENT_PLT_UPDATE_SEQUENCE_NUMBER,
            UNIMPLEMENTED_GET_ACCOUNT_INDEX_BY_ADDRESS,
            UNIMPLEMENTED_GET_CANONICAL_ADDRESS_BY_ACCOUNT_INDEX,
            UNIMPLEMENTED_GET_TOKEN_ACCOUNT_STATES,
            ptr::null(),
            ptr::null(),
            0,
            0,
            ptr::null(),
            Nonce::from(1),
            Timestamp::from_timestamp_millis(0),
            0,
            ptr::null_mut(),
            ptr::null_mut(),
            data_out,
            data_out_len,
        );
        assert_eq!(status_code, status::FfiStatusCode::Panic);
        let data = unsafe {
            let data_len = *data_out_len;
            assert!(data_len > 0);
            let data_ptr = *data_out;
            assert!(!data_ptr.is_null());
            std::slice::from_raw_parts(data_ptr, data_len)
        };
        let message = std::str::from_utf8(data).expect("Failed decoding panic message");
        assert_eq!(message, "block_state is a null pointer.");
    }
}
