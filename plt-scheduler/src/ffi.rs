//! This module provides a C ABI for the top-level functions of this library.
//!
//! It is only available if the `ffi` feature is enabled.

use crate::block_state;
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
/// - `sender_account_address` The account address of the account which signed as the sender of the transaction.
/// - `remaining_energy` The remaining energy at the start of the execution.
/// - `block_state_out` Location for writing the pointer of the updated block state.
/// - `remaining_energy_out` Location for writing the remaining energy after the execution.
///
/// # Safety
///
/// - Argument `block_state` must be non-null point to well-formed [`crate::block_state::BlockState`].
/// - Argument `payload` must be non-null and valid for reads for `payload_len` many bytes.
/// - Argument `sender_account_address` must be non-null and valid for reads for 32 bytes.
#[no_mangle]
unsafe extern "C" fn ffi_execute_transaction(
    load_callback: block_state::ffi::LoadCallback,
    update_token_account_balance_callback: block_state::ffi::UpdateTokenAccountBalanceCallback,
    block_state: *const block_state::BlockStateSavepoint,
    payload: *const u8,
    payload_len: size_t,
    sender_account_index: u64,
    sender_account_address: *const u8,
    remaining_energy: u64,
    block_state_out: *mut *const block_state::BlockStateSavepoint,
    remaining_energy_out: *mut u64,
) -> u8 {
    debug_assert!(!block_state.is_null(), "Block state is a null pointer.");
    debug_assert!(!payload.is_null(), "Payload is a null pointer.");
    debug_assert!(
        !sender_account_address.is_null(),
        "Sender account address is a null pointer."
    );
    let payload = std::slice::from_raw_parts(payload, payload_len);
    let sender_account_address = {
        let mut bytes = [0u8; concordium_base::contracts_common::ACCOUNT_ADDRESS_SIZE];
        std::ptr::copy_nonoverlapping(
            sender_account_address,
            bytes.as_mut_ptr(),
            concordium_base::contracts_common::ACCOUNT_ADDRESS_SIZE,
        );
        concordium_base::contracts_common::AccountAddress(bytes)
    };
    let mut scheduler_state = SchedulerState {
        remaining_energy: remaining_energy.into(),
        sender_account_index: sender_account_index.into(),
        sender_account_address,
    };
    let mut block_state = block_state::ffi::ExecutionTimeBlockState {
        inner_block_state: (*block_state).new_generation(),
        load_callback,
        update_token_account_balance_callback,
    };
    let result = crate::execute_transaction(&mut scheduler_state, &mut block_state, payload);
    let block_state = block_state.inner_block_state;
    *remaining_energy_out = scheduler_state.remaining_energy.into();
    match result {
        Ok(()) => {
            *block_state_out = Box::into_raw(Box::new(block_state.savepoint()));
            0
        }
        Err(crate::TransactionRejectReason) => {
            *block_state_out = std::ptr::null();
            1
        }
    }
}

/// Tracks the energy remaining and some context during the execution.
struct SchedulerState {
    /// The remaining energy tracked spent during the execution.
    remaining_energy: concordium_base::base::Energy,
    /// The account index of the account which signed as the sender of the transaction.
    sender_account_index: concordium_base::base::AccountIndex,
    /// The account address of the account which signed as the sender of the transaction.
    sender_account_address: concordium_base::contracts_common::AccountAddress,
}
impl crate::SchedulerOperations for SchedulerState {
    fn sender_account(&self) -> concordium_base::base::AccountIndex {
        self.sender_account_index
    }

    fn sender_account_address(&self) -> concordium_base::contracts_common::AccountAddress {
        self.sender_account_address
    }

    fn get_energy(&self) -> concordium_base::base::Energy {
        self.remaining_energy
    }

    fn tick_energy(
        &mut self,
        energy: concordium_base::base::Energy,
    ) -> Result<(), crate::OutOfEnergyError> {
        if let Some(remaining_energy) = self.remaining_energy.checked_sub(energy) {
            self.remaining_energy = remaining_energy;
            Ok(())
        } else {
            self.remaining_energy = 0.into();
            Err(crate::OutOfEnergyError)
        }
    }
}
