// https://gitlab.com/Concordium/consensus/globalstate-mockup/blob/master/globalstate/src/Concordium/GlobalState/Block.hs

use byteorder::{ByteOrder, NetworkEndian, WriteBytesExt};
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

#[derive(Debug)]
pub enum Block {
    Genesis(GenesisData),
    Regular(BakedBlock),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct BakedBlock {
    pub slot: Slot,
    pub data: BlockData,
}

impl BakedBlock {
    pub fn deserialize(bytes: &[u8]) -> Fallible<Self> {
        // debug_deserialization!("Block", bytes);

        let mut cursor = Cursor::new(bytes);

        let slot = NetworkEndian::read_u64(&read_const_sized!(&mut cursor, 8));

        let data = BlockData::deserialize(&read_all(&mut cursor)?)?;

        let block = BakedBlock { slot, data };

        check_serialization!(block, cursor);

        Ok(block)
    }

    pub fn serialize(&self) -> Box<[u8]> {
        let data = self.data.serialize();

        let mut cursor = create_serialization_cursor(size_of::<Slot>() + data.len());

        let _ = cursor.write_u64::<NetworkEndian>(self.slot);
        let _ = cursor.write_all(&data);

        cursor.into_inner()
    }

    pub fn baker_id(&self) -> BakerId { self.data.baker_id }

    pub fn pointer_ref(&self) -> &HashBytes { &self.data.pointer }

    pub fn last_finalized_ref(&self) -> &HashBytes { &self.data.last_finalized }

    pub fn slot_id(&self) -> Slot { self.slot }

    pub fn is_genesis(&self) -> bool { self.slot_id() == 0 }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct BlockData {
    pointer:        BlockHash,
    baker_id:       BakerId,
    proof:          Encoded,
    nonce:          Encoded,
    last_finalized: BlockHash,
    transactions:   Transactions,
    signature:      ByteString,
}

impl BlockData {
    pub fn deserialize(bytes: &[u8]) -> Fallible<Self> {
        let mut cursor = Cursor::new(bytes);

        let pointer = HashBytes::new(&read_const_sized!(&mut cursor, POINTER));
        let baker_id = NetworkEndian::read_u64(&read_const_sized!(&mut cursor, 8));
        let proof = Encoded::new(&read_const_sized!(&mut cursor, PROOF_LENGTH));
        let nonce = Encoded::new(&read_const_sized!(&mut cursor, NONCE));
        let last_finalized = HashBytes::new(&read_const_sized!(&mut cursor, SHA256));
        let payload_size = bytes.len() - cursor.position() as usize - SIGNATURE as usize;
        let transactions = Transactions::deserialize(&read_sized!(&mut cursor, payload_size))?;
        let signature = ByteString::new(&read_const_sized!(&mut cursor, SIGNATURE));

        let data = Self {
            pointer,
            baker_id,
            proof,
            nonce,
            last_finalized,
            transactions,
            signature,
        };

        check_serialization!(data, cursor);

        Ok(data)
    }

    pub fn serialize(&self) -> Box<[u8]> {
        let transactions = Transactions::serialize(&self.transactions);
        let consts = POINTER as usize
            + size_of::<BakerId>()
            + PROOF_LENGTH
            + NONCE as usize
            + LAST_FINALIZED as usize
            + SIGNATURE as usize;
        let mut cursor = create_serialization_cursor(consts + transactions.len());

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

#[derive(Debug)]
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
