//! This module provides a C ABI for this library.
//! It is only available if the `ffi` feature is enabled.

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
/// - `block_state` Unique pointer to a block state to mutate during execution.
/// - `payload` Pointer to transaction payload bytes.
/// - `payload_len` Byte length of transaction payload.
///
/// # Safety
///
/// - Argument `block_state` must be non-null point to well-formed [`crate::block_state::BlockState`].
/// - Argument `payload` must be non-null and valid for reads for `payload_len` many bytes.
#[no_mangle]
unsafe extern "C" fn ffi_execute_transaction(
    block_state: *mut crate::block_state::BlockState,
    payload: *const u8,
    payload_len: size_t,
) -> u8 {
    debug_assert!(!block_state.is_null(), "Block state is a null pointer.");
    debug_assert!(!payload.is_null(), "Payload is a null pointer.");
    let payload = std::slice::from_raw_parts(payload, payload_len);
    let mut scheduler_state = SchedulerState {};
    match crate::execute_transaction(&mut scheduler_state, &mut *block_state, payload) {
        Ok(()) => 0,
        Err(crate::TransactionRejectReason) => 1,
    }
}

/// Tracks the energy remaining and some context during the execution.
struct SchedulerState {}
impl crate::SchedulerOperations for SchedulerState {
    fn sender_account(&self) -> concordium_base::base::AccountIndex {
        todo!()
    }

    fn sender_account_address(&self) -> concordium_base::contracts_common::AccountAddress {
        todo!()
    }

    fn get_energy(&self) -> concordium_base::base::Energy {
        todo!()
    }

    fn tick_energy(
        &mut self,
        _energy: concordium_base::base::Energy,
    ) -> Result<(), crate::OutOfEnergyError> {
        todo!()
    }
}

// Block state FFI

/// Allocate a new initial PLT block state.
#[no_mangle]
extern "C" fn ffi_initial_plt_block_state() -> *mut crate::block_state::BlockState {
    let block_state = crate::block_state::BlockState::new();
    Box::into_raw(Box::new(block_state))
}

#[no_mangle]
/// Deallocate the PLT block state.
extern "C" fn ffi_free_plt_block_state(block_state: *mut crate::block_state::BlockState) {
    let state = unsafe { Box::from_raw(block_state) };
    drop(state);
}
