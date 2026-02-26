//! Events produced by block items executed by the scheduler.
//! Events generally represents observable changes to the chain state.

use crate::types::tokens::{TokenAmount, TokenHolder};
use concordium_base::common::{Buffer, Put, Serial};
use concordium_base::protocol_level_tokens::{RawCbor, TokenId, TokenModuleCborTypeDiscriminator};
use concordium_base::transactions::Memo;
use concordium_base::updates::CreatePlt;

/// Block item event. This is an observable effect on the token state.
///
/// Corresponding Haskell type: `Concordium.Types.Execution.Event`
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

impl Serial for BlockItemEvent {
    fn serial<B: Buffer>(&self, out: &mut B) {
        match self {
            BlockItemEvent::TokenModule(token_module_event) => {
                out.put(&38u8);
                out.put(token_module_event);
            }
            BlockItemEvent::TokenTransfer(token_transfer) => {
                out.put(&39u8);
                out.put(token_transfer);
            }
            BlockItemEvent::TokenMint(token_mint) => {
                out.put(&40u8);
                out.put(token_mint);
            }
            BlockItemEvent::TokenBurn(token_burn) => {
                out.put(&41u8);
                out.put(token_burn);
            }
            BlockItemEvent::TokenCreated(token_created) => {
                out.put(&42u8);
                out.put(token_created);
            }
        }
    }
}

/// An event emitted when a transfer of tokens from `from` to `to` is performed.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TokenTransferEvent {
    /// The canonical token id.
    pub token_id: TokenId,
    /// The token holder from which the tokens are transferred.
    pub from: TokenHolder,
    /// The token holder to which the tokens are transferred.
    pub to: TokenHolder,
    /// The amount of tokens transferred.
    pub amount: TokenAmount,
    /// An optional memo field that can be used to attach a message to the token
    /// transfer.
    pub memo: Option<Memo>,
}

/// Serial implementation matching the serialization of `TokenTransfer` in `Event`
/// in the Haskell module `Concordium.Types.Execution`.
impl Serial for TokenTransferEvent {
    fn serial<B: Buffer>(&self, out: &mut B) {
        let set_if = |n, b| if b { 1u16 << n } else { 0 };
        let bitmap: u16 = set_if(0, self.memo.is_some());
        out.put(&bitmap);

        out.put(&self.token_id);
        out.put(&self.from);
        out.put(&self.to);
        out.put(&self.amount);
        if let Some(memo) = &self.memo {
            out.put(memo);
        }
    }
}

/// An event emitted when the token supply is updated by minting tokens to a
/// token holder.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct TokenMintEvent {
    /// The canonical token id.
    pub token_id: TokenId,
    /// The account whose balance the amount is minted to.
    pub target: TokenHolder,
    /// The minted amount
    pub amount: TokenAmount,
}

/// Serial implementation matching the serialization of `TokenMint` in `Event`
/// in the Haskell module `Concordium.Types.Execution`.
impl Serial for TokenMintEvent {
    fn serial<B: Buffer>(&self, out: &mut B) {
        let bitmap: u16 = 0;
        out.put(&bitmap);

        out.put(&self.token_id);
        out.put(&self.target);
        out.put(&self.amount);
    }
}

/// An event emitted when the token supply is updated by burning tokens from
/// the balance of a token holder.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct TokenBurnEvent {
    /// The canonical token id.
    pub token_id: TokenId,
    /// The account whose balance the amount is burned from.
    pub target: TokenHolder,
    /// The burned amount
    pub amount: TokenAmount,
}

/// Serial implementation matching the serialization of `TokenBurn` in `Event`
/// in the Haskell module `Concordium.Types.Execution`.
impl Serial for TokenBurnEvent {
    fn serial<B: Buffer>(&self, out: &mut B) {
        let bitmap: u16 = 0;
        out.put(&bitmap);

        out.put(&self.token_id);
        out.put(&self.target);
        out.put(&self.amount);
    }
}

/// A new token was created.
#[derive(Debug, Clone, Serial)]
pub struct TokenCreateEvent {
    /// The update instruction payload for the token creation.
    pub payload: CreatePlt,
}

/// Event produced from the effect of a token transaction.
#[derive(Debug, Clone, Eq, PartialEq, Hash, Serial)]
pub struct EncodedTokenModuleEvent {
    /// The canonical token id.
    pub token_id: TokenId,
    /// The type of event produced.
    pub event_type: TokenModuleCborTypeDiscriminator,
    /// The details of the event produced, in the raw byte encoded form.
    pub details: RawCbor,
}

#[cfg(test)]
mod test {
    use crate::types::events::{
        BlockItemEvent, EncodedTokenModuleEvent, TokenBurnEvent, TokenCreateEvent, TokenMintEvent,
        TokenTransferEvent,
    };
    use crate::types::tokens::{RawTokenAmount, TokenAmount, TokenHolder};
    use concordium_base::common;
    use concordium_base::contracts_common::AccountAddress;
    use concordium_base::protocol_level_tokens::{RawCbor, TokenModuleRef};
    use concordium_base::transactions::Memo;
    use concordium_base::updates::CreatePlt;

    #[test]
    fn test_token_module_event_serial() {
        let event = BlockItemEvent::TokenModule(EncodedTokenModuleEvent {
            token_id: "tokenid1".parse().unwrap(),
            event_type: "type1".parse().unwrap(),
            details: RawCbor::from(vec![1, 2, 3, 4]),
        });

        let bytes = common::to_bytes(&event);
        assert_eq!(
            hex::encode(&bytes),
            "2608746f6b656e6964310574797065310000000401020304"
        );
    }

    #[test]
    fn test_token_transfer_event_serial() {
        // no memo
        let event = BlockItemEvent::TokenTransfer(TokenTransferEvent {
            token_id: "tokenid1".parse().unwrap(),
            from: TokenHolder::Account(AccountAddress([1; 32])),
            to: TokenHolder::Account(AccountAddress([2; 32])),
            amount: TokenAmount {
                amount: RawTokenAmount(1000),
                decimals: 4,
            },
            memo: None,
        });

        let bytes = common::to_bytes(&event);
        assert_eq!(
            hex::encode(&bytes),
            "27000008746f6b656e696431000101010101010101010101010101010101010101010101010101010101010101000202020202020202020202020202020202020202020202020202020202020202876804"
        );

        // with memo
        let reject_reason = BlockItemEvent::TokenTransfer(TokenTransferEvent {
            token_id: "tokenid1".parse().unwrap(),
            from: TokenHolder::Account(AccountAddress([1; 32])),
            to: TokenHolder::Account(AccountAddress([2; 32])),
            amount: TokenAmount {
                amount: RawTokenAmount(1000),
                decimals: 4,
            },
            memo: Some(Memo::try_from(vec![1, 2, 3]).unwrap()),
        });

        let bytes = common::to_bytes(&reject_reason);
        assert_eq!(
            hex::encode(&bytes),
            "27000108746f6b656e6964310001010101010101010101010101010101010101010101010101010101010101010002020202020202020202020202020202020202020202020202020202020202028768040003010203"
        );
    }

    #[test]
    fn test_token_mint_event_serial() {
        let event = BlockItemEvent::TokenMint(TokenMintEvent {
            token_id: "tokenid1".parse().unwrap(),
            target: TokenHolder::Account(AccountAddress([1; 32])),
            amount: TokenAmount {
                amount: RawTokenAmount(1000),
                decimals: 4,
            },
        });

        let bytes = common::to_bytes(&event);
        assert_eq!(
            hex::encode(&bytes),
            "28000008746f6b656e696431000101010101010101010101010101010101010101010101010101010101010101876804"
        );
    }

    #[test]
    fn test_token_burn_event_serial() {
        let event = BlockItemEvent::TokenBurn(TokenBurnEvent {
            token_id: "tokenid1".parse().unwrap(),
            target: TokenHolder::Account(AccountAddress([1; 32])),
            amount: TokenAmount {
                amount: RawTokenAmount(1000),
                decimals: 4,
            },
        });

        let bytes = common::to_bytes(&event);
        assert_eq!(
            hex::encode(&bytes),
            "29000008746f6b656e696431000101010101010101010101010101010101010101010101010101010101010101876804"
        );
    }

    #[test]
    fn test_token_created_event_serial() {
        let event = BlockItemEvent::TokenCreated(TokenCreateEvent {
            payload: CreatePlt {
                token_id: "tokenid1".parse().unwrap(),
                token_module: TokenModuleRef::from([5; 32]),
                decimals: 2,
                initialization_parameters: RawCbor::from(vec![1, 2, 3, 4]),
            },
        });

        let bytes = common::to_bytes(&event);
        assert_eq!(
            hex::encode(&bytes),
            "2a08746f6b656e6964310505050505050505050505050505050505050505050505050505050505050505020000000401020304"
        );
    }
}
