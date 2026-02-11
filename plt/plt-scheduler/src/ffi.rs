//! This module provides a C ABI for the Rust PLT scheduler library.
//!
//! It is only available if the `ffi` feature is enabled.

mod blob_store_callbacks;
mod block_state;
mod block_state_callbacks;
mod memory;
mod queries;
mod scheduler;
