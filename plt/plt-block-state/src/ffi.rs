//! This module provides a C ABI for the Rust PLT block state.
//!
//! It is only available if the `ffi` feature is enabled.

pub mod blob_store_callbacks;
pub mod block_state;
pub mod block_state_callbacks;
pub mod memory;
