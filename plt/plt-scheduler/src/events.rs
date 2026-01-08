//! Events produced by transactions executed by the scheduler.
//! Events generally represents observable changes to the chain state.

use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::TokenAmount;
use concordium_base::transactions::Memo;
use plt_token_module::token_kernel_interface::TokenModuleEvent;

/// Token event. This is an observable effect on the token state.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TransactionEvent {
    /// An event emitted by the token module.
    TokenModule(TokenModuleEvent),
    /// An event emitted when a transfer of tokens is performed.
    TokenTransfer(TokenTransferEvent),
    /// An event emitted when the token supply is updated by minting tokens to a
    /// token holder.
    TokenMint(TokenSupplyUpdateEvent),
    /// An event emitted when the token supply is updated by burning tokens from
    /// the balance of a token holder.
    TokenBurn(TokenSupplyUpdateEvent),
}

/// An event emitted when a transfer of tokens from `from` to `to` is performed.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TokenTransferEvent {
    /// The token holder from which the tokens are transferred.
    pub from: AccountAddress,
    /// The token holder to which the tokens are transferred.
    pub to: AccountAddress,
    /// The amount of tokens transferred.
    pub amount: TokenAmount,
    /// An optional memo field that can be used to attach a message to the token
    /// transfer.
    pub memo: Option<Memo>,
}

/// An event emitted when the token supply is updated, i.e. by minting/burning
/// tokens to/from the balance of the `target`.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct TokenSupplyUpdateEvent {
    /// The token holder the balance update is performed on.
    pub target: AccountAddress,
    /// The balance difference to be applied to the target.
    pub amount: TokenAmount,
}
