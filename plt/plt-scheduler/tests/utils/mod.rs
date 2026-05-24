// Allow items in this file to be unused. This is needed because it is imported from multiple
// compile targets (each of the integration tests), and some of the targets may not use all
// items in the file.
#![allow(unused)]


mod lock;
mod token;
mod scheduler;

pub use lock::*;
pub use token::*;
pub use scheduler::*;

use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_scheduler_types::types::protocol_version::ProtocolVersion;

/// The latest protocol version supported by the scheduler, used as default in tests.
pub const LATEST_PROTOCOL_VERSION: ProtocolVersion = ProtocolVersion::P11;

pub type BlockStateLatest = BlockStateP11;
