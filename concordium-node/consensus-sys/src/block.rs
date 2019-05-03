// https://gitlab.com/Concordium/consensus/globalstate-mockup/blob/master/globalstate/src/Concordium/GlobalState/Block.hs

use byteorder::{ByteOrder, NetworkEndian, ReadBytesExt};
use chrono::prelude::Utc;

use crate::{common::*, parameters::*, transaction::*};

const SLOT: usize = 8;
pub const BLOCK_HASH: usize = SHA256;
const POINTER: usize = BLOCK_HASH;
const BAKER_ID: usize = 8;
const NONCE: usize = BLOCK_HASH + PROOF_LENGTH; // should soon be shorter
const LAST_FINALIZED: usize = BLOCK_HASH;
const PAYLOAD_TYPE: usize = 1;
const UNDEFINED: usize = 8;
const PAYLOAD_SIZE: usize = 2;
const TIMESTAMP: usize = 8;
const SLOT_DURATION: usize = 8;
const BLOCK_BODY: usize = 8;
const SIGNATURE: usize = 64;
pub const BLOCK_HEIGHT: usize = 8;

#[derive(Debug)]
pub struct Block {
    slot: Slot,
    data: BlockData,
}

#[derive(Debug)]
pub enum BlockData {
    GenesisData(GenesisData),
    RegularData(RegularData),
}

#[derive(Debug)]
pub struct GenesisData {
    timestamp:               Timestamp,
    slot_duration:           Duration,
    birk_parameters:         BirkParameters,
    finalization_parameters: FinalizationParameters,
}

#[derive(Debug)]
pub struct RegularData {
    pointer:        BlockHash,
    baker_id:       BakerId,
    proof:          Encoded,
    nonce:          Encoded,
    last_finalized: BlockHash,
    transactions:   Transactions,
    signature:      Encoded,
}

macro_rules! get_block_content {
    ($method_name:ident, $content_type:ty, $content_ident:ident, $content_name:expr) => {
        pub fn $method_name(&self) -> $content_type {
            if let BlockData::RegularData(ref data) = self.data {
                data.$content_ident.clone()
            } else {
                panic!("Genesis block has no {}", $content_name)
            }
        }
    }
}

macro_rules! get_block_content_ref {
    ($method_name:ident, $content_type:ty, $content_ident:ident, $content_name:expr) => {
        pub fn $method_name(&self) -> &$content_type {
            if let BlockData::RegularData(ref data) = self.data {
                &data.$content_ident
            } else {
                panic!("Genesis block has no {}", $content_name)
            }
        }
    }
}

impl Block {
    get_block_content!(pointer, BlockHash, pointer, "block pointer");

    get_block_content_ref!(pointer_ref, BlockHash, pointer, "block pointer");

    get_block_content!(baker_id, BakerId, baker_id, "baker");

    get_block_content!(proof, Encoded, proof, "proof");

    get_block_content_ref!(proof_ref, Encoded, proof, "proof");

    get_block_content!(nonce, Encoded, nonce, "nonce");

    get_block_content_ref!(nonce_ref, Encoded, nonce, "nonce");

    get_block_content!(
        last_finalized,
        BlockHash,
        last_finalized,
        "last finalized pointer"
    );

    get_block_content_ref!(
        last_finalized_ref,
        BlockHash,
        last_finalized,
        "last finalized pointer"
    );

    get_block_content_ref!(
        transactions_ref,
        Transactions,
        transactions,
        "transactions"
    );

    get_block_content!(signature, Encoded, signature, "signature");

    get_block_content_ref!(signature_ref, Encoded, signature, "signature");

    // FIXME: only works for regular blocks for now
    // FIXME: use UCursor (for all deserialization) when it's available outside of
    // client
    pub fn deserialize(bytes: &[u8]) -> Option<Self> {
        let mut curr_pos = 0;

        let slot = (&bytes[curr_pos..][..SLOT])
            .read_u64::<NetworkEndian>()
            .ok()?;
        curr_pos += SLOT;

        let pointer = HashBytes::new(&bytes[curr_pos..][..POINTER]);
        curr_pos += POINTER;

        let baker_id = (&bytes[curr_pos..][..BAKER_ID])
            .read_u64::<NetworkEndian>()
            .ok()?;
        curr_pos += BAKER_ID;

        let proof = Encoded::new(&bytes[curr_pos..][..PROOF_LENGTH]);
        curr_pos += PROOF_LENGTH;

        let nonce = Encoded::new(&bytes[curr_pos..][..NONCE]);
        curr_pos += NONCE;

        let last_finalized = HashBytes::new(&bytes[curr_pos..][..SHA256]);
        curr_pos += SHA256;

        let transactions = Transactions::deserialize(&bytes[curr_pos..bytes.len() - SIGNATURE])?;

        let signature = Encoded::new(&bytes[bytes.len() - SIGNATURE..]);

        let block = Block {
            slot,
            data: BlockData::RegularData(RegularData {
                pointer,
                baker_id,
                proof,
                nonce,
                last_finalized,
                transactions,
                signature,
            }),
        };

        debug_assert_eq!(block.serialize().as_slice(), bytes);

        Some(block)
    }

    // FIXME: only works for regular blocks for now
    pub fn serialize(&self) -> Vec<u8> {
        let mut ret = Vec::new(); // FIXME: estimate capacity

        let mut slot = [0u8; SLOT];
        NetworkEndian::write_u64(&mut slot, self.slot);
        ret.extend_from_slice(&slot);

        ret.extend_from_slice(&self.pointer());

        let mut baker_id = [0u8; BAKER_ID];
        NetworkEndian::write_u64(&mut baker_id, self.baker_id());
        ret.extend_from_slice(&baker_id);

        ret.extend_from_slice(&self.proof());

        ret.extend_from_slice(&self.nonce());

        ret.extend_from_slice(&self.last_finalized()); // check

        ret.extend_from_slice(&Transactions::serialize(self.transactions_ref()));

        ret.extend_from_slice(&[0, 0, 0, 0, 0, 0, 0, 64]); // FIXME: superfluous signature length

        ret.extend_from_slice(&self.signature());

        ret
    }

    pub fn slot_id(&self) -> Slot { self.slot }

    pub fn is_genesis(&self) -> bool { self.slot_id() == 0 }
}

pub type BakerId = u64;

pub type Timestamp = u64;

pub type Duration = u64;

pub type BlockHeight = u64;

pub type BlockHash = HashBytes;

pub struct PendingBlock {
    block:    Block,
    hash:     BlockHash,
    received: Utc,
}

pub struct BlockPointer {
    block:  Block,
    hash:   BlockHash,
    parent: Option<Box<BlockPointer>>,
    height: BlockHeight,
    //    state: BlockState,
    received:          Utc,
    arrived:           Utc,
    transaction_count: u64,
}
