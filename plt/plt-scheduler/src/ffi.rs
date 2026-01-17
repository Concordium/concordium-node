//! This module provides a C ABI for this library.
//! It is only available if the `ffi` feature is enabled.

use crate::scheduler;
use crate::scheduler::TransactionOutcome;
use concordium_base::base::{AccountIndex, Energy};
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
    let mut block_state = crate::block_state::BlockState {};
    let account = AccountIndex::from(0);
    let payload: Payload = common::from_bytes(&mut &*payload).unwrap();

    match scheduler::execute_transaction(account, &mut block_state, payload, Energy::from(u64::MAX))
        .unwrap()
        .outcome
    {
        TransactionOutcome::Success(_) => 0,
        TransactionOutcome::Rejected(_) => 1,
    }
}
