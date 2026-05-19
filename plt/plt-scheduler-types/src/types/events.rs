//! Events produced by block items executed by the scheduler.
//! Events generally represents observable changes to the chain state.

use crate::types::tokens::{TokenAmount, TokenHolder};
use concordium_base::common::{Buffer, Put, Serial};
use concordium_base::protocol_level_locks::LockId;
use concordium_base::protocol_level_tokens::{RawCbor, TokenId, TokenModuleCborTypeDiscriminator};
use concordium_base::transactions::Memo;
use concordium_base::updates::CreatePlt;

/// Block item event. This is an observable effect on the token state.
///
/// Corresponding Haskell type: `Concordium.Types.Execution.Event`
#[derive(Debug, Clone, PartialEq)]
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
    /// A protocol-level lock was created.
    LockCreated(LockCreateEvent),
    /// A protocol-level lock was destroyed
    LockDestroyed(LockDestroyEvent),
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
            BlockItemEvent::LockCreated(lock_created) => {
                out.put(&43u8);
                out.put(lock_created);
            }
            BlockItemEvent::LockDestroyed(lock_destroyed) => {
                out.put(&44u8);
                out.put(lock_destroyed);
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
    /// When the funds originate on the locked balance of an account, the
    /// identity of the lock controlling the funds. Absent when the funds
    /// are not on the locked balance of the originating account.
    pub from_lock: Option<LockId>,
    /// When the funds are transferred into the control of a lock, the
    /// identity of the lock assuming control of the funds. Absent when the
    /// funds are sent to the available balance of the receiving account.
    pub to_lock: Option<LockId>,
}

/// Serial implementation matching the serialization of `TokenTransfer` in `Event`
/// in the Haskell module `Concordium.Types.Execution`.
impl Serial for TokenTransferEvent {
    fn serial<B: Buffer>(&self, out: &mut B) {
        let set_if = |n, b| if b { 1u16 << n } else { 0 };
        let bitmap: u16 = set_if(0, self.memo.is_some())
            | set_if(1, self.from_lock.is_some())
            | set_if(2, self.to_lock.is_some());
        out.put(&bitmap);

        out.put(&self.token_id);
        out.put(&self.from);
        out.put(&self.to);
        out.put(&self.amount);
        if let Some(memo) = &self.memo {
            out.put(memo);
        }
        if let Some(from_lock) = &self.from_lock {
            out.put(from_lock);
        }
        if let Some(to_lock) = &self.to_lock {
            out.put(to_lock);
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
#[derive(Debug, Clone, PartialEq, Serial)]
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

#[derive(Debug, Clone, PartialEq, Serial)]
pub struct LockCreateEvent {
    /// The Lock ID of the newly-created lock.
    pub lock_id: LockId,
    /// The CBOR-encoded lock configuration.
    pub lock_config: RawCbor,
}

#[derive(Debug, Clone, PartialEq, Serial)]
pub struct LockDestroyEvent {
    /// The Lock ID of the destroyed lock.
    pub lock_id: LockId,
}

#[cfg(test)]
mod test {
    use crate::types::events::{
        BlockItemEvent, EncodedTokenModuleEvent, LockCreateEvent, LockDestroyEvent, TokenBurnEvent,
        TokenCreateEvent, TokenMintEvent, TokenTransferEvent,
    };
    use crate::types::tokens::{RawTokenAmount, TokenAmount, TokenHolder};
    use concordium_base::common;
    use concordium_base::contracts_common::AccountAddress;
    use concordium_base::protocol_level_locks::LockId;
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
            from_lock: None,
            to_lock: None,
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
            from_lock: None,
            to_lock: None,
        });

        let bytes = common::to_bytes(&reject_reason);
        assert_eq!(
            hex::encode(&bytes),
            "27000108746f6b656e6964310001010101010101010101010101010101010101010101010101010101010101010002020202020202020202020202020202020202020202020202020202020202028768040003010203"
        );

        // with from lock
        let event = BlockItemEvent::TokenTransfer(TokenTransferEvent {
            token_id: "tokenid1".parse().unwrap(),
            from: TokenHolder::Account(AccountAddress([1; 32])),
            to: TokenHolder::Account(AccountAddress([2; 32])),
            amount: TokenAmount {
                amount: RawTokenAmount(1000),
                decimals: 4,
            },
            memo: None,
            from_lock: Some(LockId::new(
                0x0f0e0d0c0b0a0908,
                0x1122334455667788,
                0x99aabbccddeeff00,
            )),
            to_lock: None,
        });
        let bytes = common::to_bytes(&event);
        assert_eq!(
            hex::encode(&bytes),
            "27000208746f6b656e6964310001010101010101010101010101010101010101010101010101010101010101010002020202020202020202020202020202020202020202020202020202020202028768040f0e0d0c0b0a0908112233445566778899aabbccddeeff00"
        );

        // with to lock
        let event = BlockItemEvent::TokenTransfer(TokenTransferEvent {
            token_id: "tokenid1".parse().unwrap(),
            from: TokenHolder::Account(AccountAddress([1; 32])),
            to: TokenHolder::Account(AccountAddress([2; 32])),
            amount: TokenAmount {
                amount: RawTokenAmount(1000),
                decimals: 4,
            },
            memo: None,
            from_lock: None,
            to_lock: Some(LockId::new(
                0x99aabbccddeeff00,
                0x0f0e0d0c0b0a0908,
                0x1122334455667788,
            )),
        });
        let bytes = common::to_bytes(&event);
        assert_eq!(
            hex::encode(&bytes),
            "27000408746f6b656e69643100010101010101010101010101010101010101010101010101010101010101010100020202020202020202020202020202020202020202020202020202020202020287680499aabbccddeeff000f0e0d0c0b0a09081122334455667788"
        );

        // with everything
        let event = BlockItemEvent::TokenTransfer(TokenTransferEvent {
            token_id: "TestTT".parse().unwrap(),
            from: TokenHolder::Account(AccountAddress([13; 32])),
            to: TokenHolder::Account(AccountAddress([64; 32])),
            amount: TokenAmount {
                amount: RawTokenAmount(u64::MAX),
                decimals: 255,
            },
            memo: Some(Memo::try_from((0x00..=0xff).collect::<Vec<u8>>()).unwrap()),
            from_lock: Some(LockId::new(
                0x0f0e0d0c0b0a0908,
                0x1122334455667788,
                0x99aabbccddeeff00,
            )),
            to_lock: Some(LockId::new(
                0x7071727374757677,
                0x88898a8b8c8d8e8f,
                0x9f9e9d9c9b9a9998,
            )),
        });
        let bytes = common::to_bytes(&event);
        assert_eq!(
            hex::encode(&bytes),
            concat!(
                "27",
                "000706546573745454",
                "000d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d",
                "004040404040404040404040404040404040404040404040404040404040404040",
                "81ffffffffffffffff7fff",
                "0100000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeafb0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfeff",
                "0f0e0d0c0b0a0908112233445566778899aabbccddeeff00",
                "707172737475767788898a8b8c8d8e8f9f9e9d9c9b9a9998"
            )
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

    #[test]
    fn test_lock_created_event_serial() {
        let event = BlockItemEvent::LockCreated(LockCreateEvent {
            lock_id: LockId::new(12, 10001, 17),
            lock_config: RawCbor::from(vec![5, 6, 7, 8]),
        });

        let bytes = common::to_bytes(&event);
        assert_eq!(
            hex::encode(&bytes),
            "2b000000000000000c000000000000271100000000000000110000000405060708"
        );
    }

    #[test]
    fn test_lock_destroyed_event_serial() {
        let event = BlockItemEvent::LockDestroyed(LockDestroyEvent {
            lock_id: LockId::new(12, 10001, 17),
        });

        let bytes = common::to_bytes(&event);
        assert_eq!(
            hex::encode(&bytes),
            "2c000000000000000c00000000000027110000000000000011"
        );
    }
}
