//! Types that are externally exposed by the PLT Scheduler
//! as part of protocol execution and queries.
//! The types generally follow the same model
//! as the similar types on the Haskell side and implements the same serialization when possible.
//!
//! Notice that protocol types that are exposed outside the node
//! are defined in concordium-smart-contracts-common and concordium_base. As such,
//! the present crate defines types used internally in the node for protocol execution and queries.

pub mod types;
