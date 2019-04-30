// https://gitlab.com/Concordium/consensus/globalstate-mockup/blob/master/globalstate/src/Concordium/GlobalState/Block.hs

use byteorder::{ByteOrder, NetworkEndian, ReadBytesExt};
use chrono::prelude::Utc;

use std::fmt;

use crate::{common::*, parameters::*, transaction::*};

const SLOT: usize = 8;
const POINTER: usize = SHA256;
const BAKER_ID: usize = 8;
const NONCE: usize = SHA256 + PROOF_LENGTH; // should soon be shorter
const LAST_FINALIZED: usize = SHA256;
const PAYLOAD_TYPE: usize = 1;
const UNDEFINED: usize = 8;
const PAYLOAD_SIZE: usize = 2;
const TRANSACTION_COUNT: usize = 2;
const TIMESTAMP: usize = 8;
const SLOT_DURATION: usize = 8;
const BLOCK_BODY: usize = 8;
const BLOCK_SIGNATURE: usize = 64;

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

pub struct RegularData {
    pointer:        BlockHash,
    baker_id:       BakerId,
    proof:          Encoded,
    nonce:          Encoded,
    last_finalized: BlockHash,
    transactions:   Vec<Transaction>,
    signature:      Encoded,
}

impl fmt::Debug for RegularData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "\n  ptr: {:0x}\n  baker_id: {}\n  proof: <{}B>\n  nonce: <{}B>\n  last_finalized: \
             {:0x}\n  transactions: {:?}\n  signature: <{}B>\n",
            (&*self.pointer).read_u64::<NetworkEndian>().unwrap(),
            self.baker_id,
            self.proof.len(),
            self.nonce.len(),
            (&*self.last_finalized).read_u64::<NetworkEndian>().unwrap(),
            self.transactions,
            self.signature.len(),
        )
    }
}

impl Block {
    // FIXME: only works for regular blocks for now
    pub fn deserialize(bytes: &[u8]) -> Option<Self> {
        let mut curr_pos = 0;

        let slot = (&bytes[curr_pos..][..SLOT])
            .read_u64::<NetworkEndian>()
            .ok()?;
        curr_pos += SLOT;

        let mut pointer_bytes = [0u8; POINTER];
        pointer_bytes.copy_from_slice(&bytes[curr_pos..][..POINTER]);
        let pointer = Box::new(pointer_bytes);
        curr_pos += POINTER;

        let baker_id = (&bytes[curr_pos..][..BAKER_ID])
            .read_u64::<NetworkEndian>()
            .ok()?;
        curr_pos += BAKER_ID;

        let mut proof_bytes = [0u8; PROOF_LENGTH];
        proof_bytes.copy_from_slice(&bytes[curr_pos..][..PROOF_LENGTH]);
        let proof = Box::new(proof_bytes);
        curr_pos += PROOF_LENGTH;

        let mut nonce_bytes = [0u8; NONCE];
        nonce_bytes.copy_from_slice(&bytes[curr_pos..][..NONCE]);
        let nonce = Box::new(nonce_bytes);
        curr_pos += NONCE;

        let mut last_finalized_bytes = [0u8; SHA256];
        last_finalized_bytes.copy_from_slice(&bytes[curr_pos..][..SHA256]);
        let last_finalized = Box::new(last_finalized_bytes);
        curr_pos += SHA256;

        let transactions =
            deserialize_transactions(&bytes[curr_pos..bytes.len() - BLOCK_SIGNATURE])?;

        let mut signature_bytes = [0u8; BLOCK_SIGNATURE];
        signature_bytes.copy_from_slice(&bytes[bytes.len() - BLOCK_SIGNATURE..]);
        let signature = Box::new(signature_bytes);

        Some(Block {
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
        })
    }

    // FIXME: only works for regular blocks for now
    pub fn serialize(&self) -> Result<Vec<u8>, &'static str> {
        let mut ret = Vec::new();

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

        ret.extend_from_slice(&serialize_transactions(self.transactions()));

        ret.extend_from_slice(&self.signature());

        Ok(ret)
    }

    pub fn slot_id(&self) -> Slot { self.slot }

    pub fn is_genesis(&self) -> bool { self.slot_id() == 0 }

    pub fn pointer(&self) -> BlockHash {
        if let BlockData::RegularData(ref data) = self.data {
            data.pointer.clone()
        } else {
            panic!("Genesis block has no block pointer")
        }
    }

    pub fn baker_id(&self) -> BakerId {
        if let BlockData::RegularData(ref data) = self.data {
            data.baker_id
        } else {
            panic!("Genesis block has no baker")
        }
    }

    pub fn proof(&self) -> Encoded {
        if let BlockData::RegularData(ref data) = self.data {
            data.proof.clone()
        } else {
            panic!("Genesis block has no proof")
        }
    }

    pub fn nonce(&self) -> Encoded {
        if let BlockData::RegularData(ref data) = self.data {
            data.nonce.clone()
        } else {
            panic!("Genesis block has no nonce")
        }
    }

    pub fn last_finalized(&self) -> BlockHash {
        if let BlockData::RegularData(ref data) = self.data {
            data.last_finalized.clone()
        } else {
            panic!("Genesis block has no last finalized pointer")
        }
    }

    pub fn transactions(&self) -> &[Transaction] {
        if let BlockData::RegularData(ref data) = self.data {
            &data.transactions
        } else {
            panic!("Genesis block has no transactions")
        }
    }

    pub fn signature(&self) -> Encoded {
        if let BlockData::RegularData(ref data) = self.data {
            data.signature.clone()
        } else {
            panic!("Genesis block has no signature")
        }
    }
}

// FIXME: move to its own impl in transaction.rs
fn deserialize_transactions(bytes: &[u8]) -> Option<Vec<Transaction>> {
    let mut curr_pos = 0;

    let transaction_count = (&bytes[curr_pos..][..TRANSACTION_COUNT])
        .read_u16::<NetworkEndian>()
        .ok()?;
    curr_pos += TRANSACTION_COUNT;

    if transaction_count > 0 {
        let mut transactions = Vec::with_capacity(transaction_count as usize);

        while let Some((transaction, size)) = Transaction::deserialize(&bytes[curr_pos..]) {
            transactions.push(transaction);
            curr_pos += size;
        }

        Some(transactions)
    } else {
        Some(vec![])
    }
}

// FIXME: move to its own impl in transaction.rs
fn serialize_transactions(transactions: &[Transaction]) -> Vec<u8> {
    if !transactions.is_empty() {
        let mut transaction_bytes = Vec::new(); // TODO: estimate capacity

        for transaction in transactions {
            transaction_bytes.extend_from_slice(&transaction.serialize());
        }

        transaction_bytes
    } else {
        let mut ret = [0u8; 16];
        ret[15] = 64;

        ret.to_vec()
    }
}

#[derive(Debug)]
pub struct GenesisData {
    timestamp:               Timestamp,
    slot_duration:           Duration,
    birk_parameters:         BirkParameters,
    finalization_parameters: FinalizationParameters,
}

pub type BakerId = u64;

pub type Timestamp = u64;

pub type Duration = u64;

pub type BlockHeight = u64;

pub type BlockHash = Encoded;

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
