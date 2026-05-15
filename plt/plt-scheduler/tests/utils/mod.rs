// Allow items in this file to be unused. This is needed because it is imported from multiple
// compile targets (each of the integration tests), and some of the targets may not use all
// items in the file.
#![allow(unused)]

use plt_scheduler_types::types::protocol_version::ProtocolVersion;

mod account;
mod lock;
mod token;

pub use account::*;
pub use lock::*;
pub use token::*;

/// The latest protocol version supported by the scheduler, used as default in tests.
pub const LATEST_PROTOCOL_VERSION: ProtocolVersion = ProtocolVersion::P11;
