//! This module provides a C ABI for this library.
//! It is only available if the `ffi` feature is enabled.

use crate::block_state_interface::{OutOfEnergyError, SchedulerOperations};
use crate::plt_scheduler;
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
/// - `payload` Pointer to transaction payload bytes.
/// - `payload_len` Byte length of transaction payload.
///
/// # Safety
///
/// - Argument `payload` must be non-null and valid for reads for `payload_len` many bytes.
#[unsafe(no_mangle)]
unsafe extern "C" fn ffi_execute_transaction(payload: *const u8, payload_len: size_t) -> u8 {
    debug_assert!(!payload.is_null(), "Payload is a null pointer.");
    let payload = unsafe { std::slice::from_raw_parts(payload, payload_len) };
    let mut scheduler_state = SchedulerState {};
    let mut block_state = crate::block_state::BlockState {};
    match plt_scheduler::execute_transaction(&mut scheduler_state, &mut block_state, payload) {
        Ok(()) => 0,
        Err(_) => 1,
    }
}

/// Trackes the energy remaining and some context during the execution.
struct SchedulerState {}
impl SchedulerOperations for SchedulerState {
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
    ) -> Result<(), OutOfEnergyError> {
        todo!()
    }
}
