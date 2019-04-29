// https://gitlab.com/Concordium/consensus/globalstate-mockup/blob/master/globalstate/src/Concordium/GlobalState/Block.hs

use byteorder::{NetworkEndian, ReadBytesExt};
use chrono::prelude::Utc;
use std::str;

use crate::common::*;
use crate::parameters::{BakerId, GenesisData};
use crate::transaction::*;

const SLOT: usize = 8;
const POINTER: usize = SHA256;
const BAKER_ID: usize = 8;
const NONCE: usize = SHA256 + PROOF_LENGTH;
const LAST_FINALIZED: usize = SHA256;

#[derive(Debug)]
pub enum Block {
    Genesis(GenesisBlock),
    Normal(NormalBlock),
}

#[derive(Debug)]
pub struct GenesisBlock {
    slot: Slot,
    data: GenesisData,
}

#[derive(Debug)]
pub struct NormalBlock {
    slot: Slot,
    pointer: BlockHash,
    baker_id: BakerId,
    proof: Encoded,
    nonce: Encoded,
    last_finalized: BlockHash,
    transactions: Vec<Transaction>,
    signature: Encoded,
    data: Vec<u8>, // FIXME: to be removed
}

impl Block {
    pub fn deserialize(data: &[u8]) -> Option<Self> {
        let mut curr_pos = 0;
        let slot = (&data[curr_pos..][..SLOT]).read_u64::<NetworkEndian>().ok()?;
        curr_pos += SLOT;

        let mut pointer_bytes = [0u8; POINTER];
        pointer_bytes.copy_from_slice(&data[curr_pos..][..POINTER]);
        let pointer = Box::new(pointer_bytes);
        curr_pos += POINTER;

        let baker_id = (&data[curr_pos..][..BAKER_ID]).read_u64::<NetworkEndian>().ok()?;
        curr_pos += BAKER_ID;

        let mut proof_bytes = [0u8; PROOF_LENGTH];
        proof_bytes.copy_from_slice(&data[curr_pos..][..PROOF_LENGTH]);
        let proof = Box::new(proof_bytes);
        curr_pos += PROOF_LENGTH;

        let mut nonce_bytes = [0u8; NONCE];
        nonce_bytes.copy_from_slice(&data[curr_pos..][..SHA256]);
        let nonce = Box::new(nonce_bytes);
        curr_pos += NONCE;

        let mut last_finalized_bytes = [0u8; SHA256];
        last_finalized_bytes.copy_from_slice(&data[curr_pos..][..SHA256]);
        let last_finalized = Box::new(last_finalized_bytes);
        curr_pos += SHA256;

        let transactions = vec![];

        let signature = Box::new([]);

        Some(Block::Normal(NormalBlock {
            slot,
            pointer,
            baker_id,
            proof,
            nonce,
            last_finalized,
            transactions,
            signature,
            data: data.to_owned(),
        }))
    }

    pub fn serialize(&self) -> Result<Vec<u8>, &'static str> {
        match self {
            Block::Genesis(_block) => unimplemented!(),
            Block::Normal(block) => Ok(block.data.clone()),
        }
    }

    pub fn slot_id(&self) -> Slot {
        match self {
            Block::Genesis(block) => block.slot,
            Block::Normal(block) => block.slot,
        }
    }

    pub fn pointer(&self) -> BlockHash {
        match self {
            Block::Genesis(_) => panic!("Genesis block has no block pointer"),
            Block::Normal(block) => block.pointer.clone(),
        }
    }

    pub fn baker_id(&self) -> BakerId {
        match self {
            Block::Genesis(_) => panic!("Genesis block has no baker"),
            Block::Normal(block) => block.baker_id,
        }
    }

    pub fn last_finalized(&self) -> BlockHash {
        match self {
            Block::Genesis(_) => panic!("Genesis block has no last finalized pointer"),
            Block::Normal(block) => block.last_finalized.clone(),
        }
    }

    pub fn block_transactions(&self) -> &[Transaction] {
        match self {
            Block::Genesis(_) => &[],
            Block::Normal(block) => &block.transactions,
        }
    }

}

pub type BlockHeight = u64;

pub type BlockHash = Encoded;

pub struct PendingBlock {
    block: Block,
    hash: BlockHash,
    received: Utc,
}

pub struct BlockPointer {
    block: Block,
    hash: BlockHash,
    parent: Option<Box<BlockPointer>>,
    height: BlockHeight,
//    state: BlockState,
    received: Utc,
    arrived: Utc,
    transaction_count: u64,
}
