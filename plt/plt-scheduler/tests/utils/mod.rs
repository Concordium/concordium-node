// Allow items in this file to be unused. This is needed because it is imported from multiple
// compile targets (each of the integration tests), and some of the targets may not use all
// items in the file.
#![allow(unused)]

pub mod entity_traits;
mod lock;
mod token;

use concordium_base::{base::Energy, contracts_common::AccountAddress, transactions};
pub use lock::*;
pub use token::*;

use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_scheduler_types::types::protocol_version::ProtocolVersion;

/// The latest protocol version supported by the scheduler, used as default in tests.
pub const LATEST_PROTOCOL_VERSION: ProtocolVersion = ProtocolVersion::P11;

pub type BlockStateLatest = BlockStateP11;

/// Creates a [`TransactionContext`] with the given sender account address and
/// transaction sequence number. The energy limit is set to the maximum value,
/// and the block timestamp is set to 0.
pub fn simple_transaction_context_with_nonce(
    sender_account_address: AccountAddress,
    transaction_sequence_number: u64,
) -> plt_scheduler::TransactionContext {
    plt_scheduler::TransactionContext {
        energy_limit: Energy::from(u64::MAX),
        sender_account_address,
        transaction_sequence_number: transaction_sequence_number.into(),
        block_timestamp: 0.into(),
    }
}

/// Creates a [`TransactionContext`] with the given sender account address.
/// The energy limit is set to the maximum value, and the block timestamp is set
/// to 0. The transaction sequence number is set to 1.
pub fn simple_transaction_context(
    sender_account_address: AccountAddress,
) -> plt_scheduler::TransactionContext {
    simple_transaction_context_with_nonce(sender_account_address, 1)
}
