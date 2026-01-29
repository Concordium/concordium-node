//! Events produced by block items executed by the scheduler.
//! Events generally represents observable changes to the chain state.

use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{
    RawCbor, TokenAmount, TokenId, TokenModuleCborTypeDiscriminator,
};
use concordium_base::transactions::Memo;
use concordium_base::updates::CreatePlt;

/// Block item event. This is an observable effect on the token state.
#[derive(Debug, Clone)]
pub enum BlockItemEvent {
    /// An event emitted by the token module.
    TokenModule(EncodedTokenModuleEvent),
    /// An event emitted when a transfer of tokens is performed.
    TokenTransfer(TokenTransferEvent),
    /// An event emitted when the token supply is updated by minting tokens to a
    /// token holder.
    TokenMint(TokenMintEvent),
    /// An event emitted when the token supply is updated by burning tokens from
    /// the balance of a token holder.
    TokenBurn(TokenBurnEvent),
    /// A new token was created.
    TokenCreated(TokenCreateEvent),
}

/// An event emitted when a transfer of tokens from `from` to `to` is performed.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TokenTransferEvent {
    /// The canonical token id.
    pub token_id: TokenId,
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

/// An event emitted when the token supply is updated by minting tokens to a
/// token holder.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct TokenMintEvent {
    /// The canonical token id.
    pub token_id: TokenId,
    /// The account whose balance the amount is minted to.
    pub target: AccountAddress,
    /// The minted amount
    pub amount: TokenAmount,
}

/// An event emitted when the token supply is updated by burning tokens from
/// the balance of a token holder.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct TokenBurnEvent {
    /// The canonical token id.
    pub token_id: TokenId,
    /// The account whose balance the amount is burned from.
    pub target: AccountAddress,
    /// The burned amount
    pub amount: TokenAmount,
}

/// A new token was created.
#[derive(Debug, Clone)]
pub struct TokenCreateEvent {
    /// The update instruction payload for the token creation.
    pub payload: CreatePlt,
}

/// Event produced from the effect of a token transaction.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct EncodedTokenModuleEvent {
    /// The canonical token id.
    pub token_id: TokenId,
    /// The type of event produced.
    pub event_type: TokenModuleCborTypeDiscriminator,
    /// The details of the event produced, in the raw byte encoded form.
    pub details: RawCbor,
}
