// https://gitlab.com/Concordium/consensus/globalstate-mockup/blob/master/globalstate/src/Concordium/GlobalState/Block.hs

use byteorder::{ByteOrder, NetworkEndian, WriteBytesExt};
use chrono::prelude::{DateTime, Utc};
use failure::Fallible;

use std::{
    io::{Cursor, Read, Write},
    mem::size_of,
};

use crate::{common::*, parameters::*, transaction::*};

pub const BLOCK_HASH: u8 = SHA256;
const POINTER: u8 = BLOCK_HASH;
const NONCE: u8 = BLOCK_HASH + PROOF_LENGTH as u8; // should soon be shorter
const LAST_FINALIZED: u8 = BLOCK_HASH;
const SIGNATURE: u8 = 8 + 64; // FIXME: unnecessary 8B prefix

#[derive(Debug, Clone)]
pub enum Block {
    Genesis(GenesisData),
    Regular(BakedBlock),
}

impl Block {
    pub fn is_genesis(&self) -> bool {
        match self {
            Block::Genesis(_) => true,
            Block::Regular(_) => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BakedBlock {
    pub slot:       Slot,
    pointer:        BlockHash,
    baker_id:       BakerId,
    proof:          Encoded,
    nonce:          Encoded,
    last_finalized: BlockHash,
    transactions:   Transactions,
    signature:      ByteString,
}

impl BakedBlock {
    pub fn deserialize(bytes: &[u8]) -> Fallible<Self> {
        // debug_deserialization!("Block", bytes);

        let mut cursor = Cursor::new(bytes);

        let slot = NetworkEndian::read_u64(&read_const_sized!(&mut cursor, 8));
        let pointer = HashBytes::new(&read_const_sized!(&mut cursor, POINTER));
        let baker_id = NetworkEndian::read_u64(&read_const_sized!(&mut cursor, 8));
        let proof = Encoded::new(&read_const_sized!(&mut cursor, PROOF_LENGTH));
        let nonce = Encoded::new(&read_const_sized!(&mut cursor, NONCE));
        let last_finalized = HashBytes::new(&read_const_sized!(&mut cursor, SHA256));
        let payload_size = bytes.len() - cursor.position() as usize - SIGNATURE as usize;
        let transactions = Transactions::deserialize(&read_sized!(&mut cursor, payload_size))?;
        let signature = ByteString::new(&read_const_sized!(&mut cursor, SIGNATURE));

        let block = BakedBlock {
            slot,
            pointer,
            baker_id,
            proof,
            nonce,
            last_finalized,
            transactions,
            signature,
        };

        check_serialization!(block, cursor);

        Ok(block)
    }

    pub fn baker_id(&self) -> BakerId { self.baker_id }

    pub fn pointer_ref(&self) -> &HashBytes { &self.pointer }

    pub fn last_finalized_ref(&self) -> &HashBytes { &self.last_finalized }

    pub fn slot_id(&self) -> Slot { self.slot }

    pub fn is_genesis(&self) -> bool { self.slot_id() == 0 }
}

impl SerializeToBytes for BakedBlock {
    fn serialize(&self) -> Box<[u8]> {
        let transactions = Transactions::serialize(&self.transactions);
        let consts = size_of::<Slot>()
            + POINTER as usize
            + size_of::<BakerId>()
            + PROOF_LENGTH
            + NONCE as usize
            + LAST_FINALIZED as usize
            + SIGNATURE as usize;
        let mut cursor = create_serialization_cursor(consts + transactions.len());

        let _ = cursor.write_u64::<NetworkEndian>(self.slot);
        let _ = cursor.write_all(&self.pointer);
        let _ = cursor.write_u64::<NetworkEndian>(self.baker_id);
        let _ = cursor.write_all(&self.proof);
        let _ = cursor.write_all(&self.nonce);
        let _ = cursor.write_all(&self.last_finalized);
        let _ = cursor.write_all(&transactions);
        let _ = cursor.write_all(&self.signature);

        cursor.into_inner()
    }
}

#[derive(Debug, Clone)]
pub struct GenesisData {
    timestamp:               Timestamp,
    slot_duration:           Duration,
    birk_parameters:         BirkParameters,
    finalization_parameters: FinalizationParameters,
}

impl GenesisData {
    pub fn deserialize(bytes: &[u8]) -> Fallible<Self> {
        let mut cursor = Cursor::new(bytes);

        let timestamp = NetworkEndian::read_u64(&read_const_sized!(&mut cursor, 8));
        let slot_duration = NetworkEndian::read_u64(&read_const_sized!(&mut cursor, 8));
        let birk_parameters = BirkParameters::deserialize(&mut cursor)?;
        let finalization_parameters = FinalizationParameters::deserialize(&mut cursor)?;

        let data = GenesisData {
            timestamp,
            slot_duration,
            birk_parameters,
            finalization_parameters,
        };

        check_serialization!(data, cursor);

        Ok(data)
    }

    pub fn serialize(&self) -> Box<[u8]> {
        let birk_params = BirkParameters::serialize(&self.birk_parameters);
        let finalization_params = FinalizationParameters::serialize(&self.finalization_parameters);

        let size = size_of::<Timestamp>()
            + size_of::<Duration>()
            + birk_params.len()
            + finalization_params.len();
        let mut cursor = create_serialization_cursor(size);

        let _ = cursor.write_u64::<NetworkEndian>(self.timestamp);
        let _ = cursor.write_u64::<NetworkEndian>(self.slot_duration);
        let _ = cursor.write_all(&birk_params);
        let _ = cursor.write_all(&finalization_params);

        cursor.into_inner()
    }
}

pub type BakerId = u64;

pub type Timestamp = u64;

pub type Duration = u64;

pub type BlockHeight = u64;

pub type BlockHash = HashBytes;

#[derive(Debug)]
pub struct PendingBlock {
    pub hash:     BlockHash,
    pub block:    BakedBlock,
    pub received: DateTime<Utc>,
}

impl PendingBlock {
    pub fn new(bytes: &[u8]) -> Fallible<Self> {
        let received = Utc::now();
        let block = BakedBlock::deserialize(bytes)?;
        let hash = sha256(bytes);

        Ok(Self {
            hash,
            block,
            received,
        })
    }
}

#[derive(Debug, Clone)]
pub struct BlockPtr {
    pub hash:           BlockHash,
    pub block:          Block,
    pub parent:         Option<Box<BlockPtr>>,
    pub last_finalized: Option<Box<BlockPtr>>,
    pub height:         BlockHeight,
    // state:       BlockState,
    pub received:  DateTime<Utc>,
    pub validated: DateTime<Utc>,
}

impl BlockPtr {
    pub fn genesis(genesis_bytes: &[u8]) -> Self {
        let genesis_data = GenesisData::deserialize(genesis_bytes).expect("Invalid genesis data");
        let genesis_block = Block::Genesis(genesis_data);
        let timestamp = Utc::now(); // TODO: be more precise when Kontrol is there

        BlockPtr {
            hash:           sha256(genesis_bytes),
            block:          genesis_block,
            parent:         None,
            last_finalized: None,
            height:         0,
            received:       timestamp,
            validated:      timestamp,
        }
    }

    pub fn new(
        pb: PendingBlock,
        parent: Self,
        last_finalized: Self,
        validated: DateTime<Utc>,
    ) -> Self {
        assert_eq!(parent.hash, *pb.block.pointer_ref());
        assert_eq!(last_finalized.hash, *pb.block.last_finalized_ref());

        let height = parent.height + 1;

        Self {
            hash: pb.hash,
            block: Block::Regular(pb.block),
            parent: Some(Box::new(parent)),
            last_finalized: Some(Box::new(last_finalized)),
            height,
            received: pb.received,
            validated,
        }
    }
}

impl PartialEq for BlockPtr {
    fn eq(&self, other: &Self) -> bool { self.hash == other.hash }
}

impl Eq for BlockPtr {}
