// https://gitlab.com/Concordium/consensus/globalstate-mockup/blob/master/globalstate/src/Concordium/GlobalState/Block.hs

use byteorder::{NetworkEndian, ReadBytesExt};
use chrono::prelude::Utc;
use std::{
    mem,
    str,
};

use crate::common::*;
use crate::parameters::{BakerId, GenesisData};
use crate::transaction::*;

const SLOT: usize = 8;
const POINTER: usize = SHA256;
const BAKER_ID: usize = 8;
const NONCE: usize = SHA256 + PROOF_LENGTH;
const LAST_FINALIZED: usize = SHA256;

pub enum Block {
    Genesis(GenesisBlock),
    Normal(NormalBlock),
}

struct GenesisBlock {
    slot: Slot,
    data: GenesisData,
}

struct NormalBlock {
    slot: Slot,
    pointer: BlockHash,
    baker_id: BakerId,
    proof: BlockProof,
    nonce: BlockNonce,
    last_finalized: BlockHash,
    signature: BlockSignature,
    transactions: Vec<Transaction>,
    data: Vec<u8>, // FIXME: to be removed
}

unsafe fn sha_from_bytes(bytes: [u8; SHA256]) -> Sha256 {
    mem::transmute::<[u8; SHA256], Sha256>(bytes)
}

impl Block {
    pub fn deserialize(data: &[u8]) -> Option<Self> {
        let mut curr_pos = 0;
        let slot = (&data[curr_pos..][..SLOT]).read_u64::<NetworkEndian>().ok()?;
        curr_pos += SLOT;
        
        let mut pointer_bytes = [0u8; POINTER];
        pointer_bytes.copy_from_slice(&data[curr_pos..][..POINTER]);
        let pointer = unsafe { mem::transmute::<[u8; POINTER], Sha256>(pointer_bytes) };
        curr_pos += POINTER;
        
        let baker_id = (&data[curr_pos..][..BAKER_ID]).read_u64::<NetworkEndian>().ok()?;
        curr_pos += BAKER_ID;
        
        let mut proof_bytes = [0u8; PROOF_LENGTH];
        proof_bytes.copy_from_slice(&data[curr_pos..][..PROOF_LENGTH]);
        let proof = Proof::from_bytes(&proof_bytes).ok()?;
        curr_pos += PROOF_LENGTH;
        
        let mut nonce_sha_bytes = [0u8; SHA256];
        nonce_sha_bytes.copy_from_slice(&data[curr_pos..][..SHA256]);
        let nonce_sha = unsafe { mem::transmute::<[u8; SHA256], Sha256>(nonce_sha_bytes) };
        curr_pos += SHA256;
        let mut nonce_proof_bytes = [0u8; PROOF_LENGTH];
        nonce_proof_bytes.copy_from_slice(&data[curr_pos..][..PROOF_LENGTH]);
        let nonce_proof = Proof::from_bytes(&nonce_proof_bytes).ok()?;
        let nonce = (nonce_sha, nonce_proof);
        curr_pos += NONCE;
        
        let mut last_finalized_bytes = [0u8; SHA256];
        last_finalized_bytes.copy_from_slice(&data[curr_pos..][..SHA256]);
        let last_finalized = unsafe { mem::transmute::<[u8; SHA256], Sha256>(last_finalized_bytes) };
        curr_pos += SHA256;
        
        Some(Block::Normal(NormalBlock {
            slot,
            pointer,
            baker_id,
            proof,
            nonce,
            last_finalized,
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

    pub fn proof(&self) -> BlockProof {
        match self {
            Block::Genesis(_) => panic!("Genesis block has no proof"),
            Block::Normal(block) => block.proof,
        }
    }

    pub fn nonce(&self) -> BlockNonce {
        match self {
            Block::Genesis(_) => panic!("Genesis block has no block nonce"),
            Block::Normal(block) => block.nonce,
        }
    }

    pub fn last_finalized(&self) -> BlockHash {
        match self {
            Block::Genesis(_) => panic!("Genesis block has no last finalized pointer"),
            Block::Normal(block) => block.last_finalized.clone(),
        }
    }

    pub fn verify_signature(&self, _sig: VerifyKey) -> bool {
        match self {
            Block::Genesis(_) => true,
            Block::Normal(_block) => unimplemented!(),
        }
    }

    pub fn block_transactions(&self) -> &[Transaction] {
        match self {
            Block::Genesis(_) => &[],
            Block::Normal(block) => &block.transactions,
        }
    }

}

pub type BlockNonce = (Sha256, Proof); // the hash will be removed

pub type BlockHeight = u64;

pub type BlockProof = Proof;

pub type BlockSignature = sig::Signature;

pub type BlockHash = Sha256;

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
