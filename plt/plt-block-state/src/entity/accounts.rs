//! Protocol-level token types used in the block state.

use crate::block_state::external::ExternalBlockStateOperations;
use concordium_base::base::AccountIndex;
use concordium_base::common::Serialize;
use concordium_base::contracts_common::AccountAddress;
use plt_scheduler_types::types::tokens::RawTokenAmount;

/// Account representing (read-only) account state.
///
/// The account is guaranteed to exist on chain, when holding an instance of this type.
#[derive(Debug)]
pub struct AccountWithCanonicalAddress<'a, E> {
    /// Opaque type that represents an account on chain.
    pub account: Account<'a, E>,
    /// The canonical account address of the account, i.e. the address used as part of the
    /// credential deployment and not an alias.
    pub canonical_account_address: AccountAddress,
}

/// Token account state at block state level.
///
/// Corresponding Haskell type: `Concordium.GlobalState.Persistent.Account.ProtocolLevelTokens.TokenAccountState`
#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize)]
pub struct TokenAccountState {
    /// Balance of the account
    pub balance: RawTokenAmount,
}


/// Accounts block state
#[derive(Debug)]
pub struct Accounts<'a, E> {
    /// Part of block state that is managed externally.
    external: &'a E,
}

impl<'a, E> Accounts<'a, E> {
    pub fn new(external: &'a E) -> Self {
        Self {
            external
        }
    }
}

impl<'a, E: ExternalBlockStateOperations> Accounts<'a, E> {

}

/// Block state account
#[derive(Debug)]
pub struct Account<'a, E> {
    /// Account index
    account_index: AccountIndex,
    /// Part of block state that is managed externally.
    external: &'a E,
}

impl<'a, E: ExternalBlockStateOperations> Account<'a, E> {

}
