//! Types used specifically in the block state.

// todo ar restructure
pub mod blob_reference;
mod protocol_level_tokens;

pub use protocol_level_tokens::*;

use concordium_base::common::Serialize;
use concordium_base::contracts_common::AccountAddress;

/// Account representing (read-only) account state.
///
/// The account is guaranteed to exist on chain, when holding an instance of this type.
#[derive(Debug)]
pub struct AccountWithCanonicalAddress<Account> {
    /// Opaque type that represents an account on chain.
    pub account: Account,
    /// The canonical account address of the account, i.e. the address used as part of the
    /// credential deployment and not an alias.
    pub canonical_account_address: AccountAddress,
}
