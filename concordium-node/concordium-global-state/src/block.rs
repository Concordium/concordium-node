// https://gitlab.com/Concordium/consensus/globalstate-mockup/blob/master/globalstate/src/Concordium/GlobalState/Block.hs

use byteorder::{ByteOrder, NetworkEndian, ReadBytesExt, WriteBytesExt};
use failure::{bail, Fallible};

use std::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    io::{Cursor, Read, Write},
    mem::size_of,
    ops::Deref,
    sync::Arc,
};

use crate::{common::*, parameters::*, transaction::*};

const NONCE: u8 = PROOF_LENGTH as u8;
const TX_ALLOC_LIMIT: usize = 256 * 1024;

pub struct Block {
    pub slot: Slot,
    pub data: BlockData,
}

impl PartialEq for Block {
    fn eq(&self, other: &Self) -> bool { self.pointer() == other.pointer() }
}

impl Eq for Block {}

impl PartialOrd for Block {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { Some(self.slot().cmp(&other.slot())) }
}

impl Ord for Block {
    fn cmp(&self, other: &Self) -> Ordering { self.slot().cmp(&other.slot()) }
}

impl Block {
    pub fn genesis_data(&self) -> &Encoded {
        match self.data {
            BlockData::Genesis(ref data) => data,
            BlockData::Regular(_) => unreachable!(), // the genesis block is unmistakeable
        }
    }

    pub fn block_data(&self) -> &BakedBlock {
        match self.data {
            BlockData::Genesis(_) => unreachable!(), // the genesis block is unmistakeable
            BlockData::Regular(ref data) => data,
        }
    }

    pub fn pointer(&self) -> Option<&BlockHash> {
        match &self.data {
            BlockData::Genesis(_) => None,
            BlockData::Regular(ref block) => Some(&block.fields.pointer),
        }
    }

    pub fn last_finalized(&self) -> Option<&BlockHash> {
        match &self.data {
            BlockData::Genesis(_) => None,
            BlockData::Regular(ref block) => Some(&block.fields.last_finalized),
        }
    }

    pub fn slot(&self) -> Slot { self.slot }
}

impl Serial for Block {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        let slot = Slot::deserial(source)?;
        let data = BlockData::deserial_with_param(source, slot)?;

        let block = Block { slot, data };

        Ok(block)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        Slot::serial(&self.slot, target)?;
        BlockData::serial(&self.data, target)?;
        Ok(())
    }
}

impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut serialized = Vec::new();
        self.serial(&mut serialized).unwrap_or(());

        let hash = if self.slot != 0 {
            format!(
                "block {:?} by baker {}",
                sha256(&serialized),
                self.block_data().fields.baker_id
            )
        } else {
            format!("genesis {:?}", sha256(&serialized))
        };

        write!(f, "{}", hash)
    }
}

#[derive(Debug)]
pub enum BlockData {
    Genesis(Encoded),
    Regular(BakedBlock),
}

impl Serial for BlockData {
    type Param = Slot;

    fn deserial_with_param<R: ReadBytesExt>(source: &mut R, slot: Self::Param) -> Fallible<Self> {
        if slot == 0 {
            let mut buffer = Vec::new();
            source.read_to_end(&mut buffer)?;
            Ok(BlockData::Genesis(Encoded::new(&buffer.into_boxed_slice())))
        } else {
            let pointer = HashBytes::from(read_ty!(source, BlockHash));
            let baker_id = BakerId::deserial(source)?;
            let proof = Encoded::new(&read_const_sized!(source, PROOF_LENGTH));
            let nonce = Encoded::new(&read_const_sized!(source, NONCE));
            let last_finalized = HashBytes::from(read_ty!(source, BlockHash));
            let transactions = read_multiple!(
                source,
                FullTransaction::deserial(source)?,
                8,
                TX_ALLOC_LIMIT
            );
            let signature = read_bytestring_short_length(source)?;
            let txs = serialize_list(&transactions)?;
            let mut transactions = vec![];
            write_multiple!(&mut transactions, txs, Write::write_all);
            let data = BlockData::Regular(BakedBlock {
                fields: Arc::new(BlockFields {
                    pointer,
                    baker_id,
                    proof,
                    nonce,
                    last_finalized,
                }),
                transactions: Encoded::new(&transactions.into_boxed_slice()),
                signature,
            });

            Ok(data)
        }
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        match self {
            BlockData::Genesis(ref data) => {
                target.write_all(data)?;
            }
            BlockData::Regular(ref data) => {
                target.write_all(&data.fields.pointer)?;
                target.write_u64::<NetworkEndian>(data.fields.baker_id)?;
                target.write_all(&data.fields.proof)?;
                target.write_all(&data.fields.nonce)?;
                target.write_all(&data.fields.last_finalized)?;
                target.write_all(data.transactions.deref())?;
                write_bytestring_short_length(target, &data.signature)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct BakedBlock {
    pub fields:       Arc<BlockFields>,
    pub transactions: Encoded,
    pub signature:    Encoded,
}

#[derive(Debug)]
pub struct BlockFields {
    pub pointer:        BlockHash,
    pub baker_id:       BakerId,
    pub proof:          Encoded,
    pub nonce:          Encoded,
    pub last_finalized: BlockHash,
}

#[derive(Debug)]
pub struct GenesisData {
    timestamp:                   Timestamp,
    slot_duration:               Duration,
    birk_parameters:             BirkParameters,
    baker_accounts:              Box<[Account]>,
    pub finalization_parameters: Box<[VoterInfo]>,
    finalization_minimum_skip:   BlockHeight,
    cryptographic_parameters:    CryptographicParameters,
    identity_providers:          Encoded,
}

pub type Timestamp = u64;

pub type Duration = u64;

pub type BlockHeight = u64;

pub type Delta = u64;

#[derive(Debug)]
pub struct PendingBlock {
    pub hash:  BlockHash,
    pub block: Arc<Block>,
}

fn hash_without_timestamps(block: &Block) -> Fallible<BlockHash> {
    let mut target = Vec::new();

    target.write_u64::<NetworkEndian>(block.slot)?;

    match block.data {
        BlockData::Regular(ref data) => {
            fn transform_txs(source: &[u8]) -> Fallible<Vec<Box<[u8]>>> {
                let mut cursor_txs = Cursor::new(source);
                let txs = read_multiple!(
                    &mut cursor_txs,
                    FullTransaction::deserial(&mut cursor_txs)?,
                    8,
                    TX_ALLOC_LIMIT
                );

                let mut ret = Vec::new();
                for tx in txs.iter() {
                    let mut bare_tx = Vec::new();
                    tx.bare_transaction.serial(&mut bare_tx)?;
                    ret.push(bare_tx.into_boxed_slice());
                }

                Ok(ret)
            }

            let transactions = transform_txs(data.transactions.deref())?;

            target.write_all(&data.fields.pointer)?;
            target.write_u64::<NetworkEndian>(data.fields.baker_id)?;
            target.write_all(&data.fields.proof)?;
            target.write_all(&data.fields.nonce)?;
            target.write_all(&data.fields.last_finalized)?;
            write_multiple!(&mut target, transactions, Write::write_all);
            write_bytestring_short_length(&mut target, &data.signature)?;
        }
        _ => unreachable!("GenesisData will never be transformed into a Pending Block"),
    };

    Ok(sha256(&target))
}

impl PendingBlock {
    pub fn new(bytes: &[u8]) -> Fallible<Self> {
        let block = Block::deserial(&mut Cursor::new(bytes))?;
        Ok(Self {
            hash:  hash_without_timestamps(&block)?,
            block: Arc::new(block),
        })
    }
}

impl PartialEq for PendingBlock {
    fn eq(&self, other: &Self) -> bool { self.hash == other.hash }
}

impl Eq for PendingBlock {}

impl Hash for PendingBlock {
    fn hash<H: Hasher>(&self, state: &mut H) { self.hash.hash(state) }
}

pub struct BlockPtr {
    pub hash:                    BlockHash,
    pub block:                   Arc<Block>,
    pub height:                  BlockHeight,
    pub transaction_count:       u64,
    pub transaction_energy_cost: u64,
    pub transaction_size:        u64,
}

impl BlockPtr {
    pub fn genesis(genesis_bytes: &[u8]) -> Fallible<Self> {
        let mut cursor = Cursor::new(genesis_bytes);
        let genesis_data =
            BlockData::deserial_with_param(&mut cursor, 0).expect("Invalid genesis data");
        let genesis_block = Block {
            slot: 0,
            data: genesis_data,
        };

        let mut genesis_block_hash = Vec::new();
        genesis_block.serial(&mut genesis_block_hash)?;
        let genesis_block_hash = sha256(&genesis_block_hash);

        Ok(Self {
            hash:                    genesis_block_hash,
            block:                   Arc::new(genesis_block),
            height:                  0,
            transaction_count:       0,
            transaction_energy_cost: 0,
            transaction_size:        0,
        })
    }

    pub fn new(pb: &PendingBlock, height: BlockHeight) -> Fallible<Self> {
        fn get_tx_count_energy(mut cursor: Cursor<&[u8]>) -> Fallible<(u64, u64)> {
            let txs = read_multiple!(
                cursor,
                FullTransaction::deserial(&mut cursor)?,
                8,
                TX_ALLOC_LIMIT
            );

            Ok((
                txs.len() as u64,
                txs.iter()
                    .map(|x| x.bare_transaction.header.gas_amount)
                    .sum(),
            ))
        }

        if let BlockData::Regular(ref bblock) = pb.block.data {
            let cursor = Cursor::new(bblock.transactions.deref());
            if let Ok((tx_count, energy)) = get_tx_count_energy(cursor) {
                Ok(Self {
                    hash: pb.hash.clone(),
                    block: pb.block.clone(),
                    height,
                    transaction_count: tx_count,
                    transaction_energy_cost: energy,
                    transaction_size: bblock.transactions.deref().len() as u64
                        - (size_of::<u64>() as u64 + size_of::<u64>() as u64 * tx_count as u64),
                })
            } else {
                bail!(
                    "Couldn't read txs of a pending block {:?} when creating a block pointer",
                    pb
                )
            }
        } else {
            bail!("Tried to create block pointer from a pending block containing genesis data")
        }
    }

    pub fn serialize_to_disk_format(&self) -> Fallible<Vec<u8>> {
        let mut buffer = Vec::new();

        self.block.serial(&mut buffer)?;
        buffer.write_u64::<NetworkEndian>(self.height)?;

        Ok(buffer)
    }
}

impl PartialEq for BlockPtr {
    fn eq(&self, other: &Self) -> bool { self.hash == other.hash }
}

impl Eq for BlockPtr {}

impl PartialOrd for BlockPtr {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { Some(self.height.cmp(&other.height)) }
}

impl Ord for BlockPtr {
    fn cmp(&self, other: &Self) -> Ordering { self.height.cmp(&other.height) }
}

impl fmt::Debug for BlockPtr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{:?}", self.hash) }
}

impl Serial for BlockPtr {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(_source: &mut R) -> Fallible<Self> {
        unimplemented!("BlockPtr is not to be deserialized directly")
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> { self.block.serial(target) }
}

#[cfg(test)]
mod tests {
    use super::PendingBlock;
    use failure::Fallible;
    #[test]
    fn test_block() -> Fallible<()> {
        let b: Vec<u8> = vec![
            0, 0, 0, 0, 0, 0, 0, 2, 147, 19, 214, 189, 117, 174, 39, 250, 195, 74, 229, 190, 97,
            47, 180, 201, 169, 93, 30, 103, 21, 197, 140, 175, 45, 112, 84, 56, 122, 211, 72, 144,
            0, 0, 0, 0, 0, 0, 0, 6, 31, 65, 241, 41, 54, 93, 130, 42, 225, 254, 12, 122, 163, 82,
            85, 7, 173, 41, 190, 235, 198, 117, 122, 186, 176, 145, 207, 64, 148, 17, 99, 8, 110,
            158, 67, 42, 58, 202, 203, 53, 65, 91, 154, 255, 247, 125, 175, 28, 124, 105, 68, 218,
            187, 252, 147, 232, 247, 8, 235, 76, 95, 141, 241, 9, 37, 117, 89, 2, 52, 161, 26, 194,
            69, 227, 4, 183, 162, 215, 214, 1, 227, 193, 164, 74, 246, 31, 13, 46, 215, 165, 64,
            67, 138, 202, 222, 104, 179, 56, 134, 96, 250, 55, 176, 161, 209, 114, 40, 1, 188, 130,
            122, 5, 148, 228, 171, 192, 217, 54, 173, 133, 236, 228, 136, 157, 49, 4, 218, 147,
            250, 237, 104, 79, 143, 43, 192, 5, 120, 44, 72, 131, 135, 237, 0, 28, 48, 65, 57, 37,
            195, 125, 209, 220, 159, 77, 169, 131, 76, 18, 182, 8, 147, 19, 214, 189, 117, 174, 39,
            250, 195, 74, 229, 190, 97, 47, 180, 201, 169, 93, 30, 103, 21, 197, 140, 175, 45, 112,
            84, 56, 122, 211, 72, 144, 0, 0, 0, 0, 0, 0, 0, 22, 0, 64, 194, 254, 12, 37, 245, 70,
            19, 140, 142, 207, 85, 36, 91, 132, 187, 104, 133, 134, 116, 74, 182, 121, 139, 99,
            195, 150, 91, 86, 90, 149, 8, 194, 15, 198, 12, 226, 88, 2, 203, 46, 141, 7, 176, 139,
            160, 71, 58, 239, 136, 38, 219, 49, 117, 13, 64, 137, 238, 240, 153, 238, 204, 80, 109,
            14, 0, 0, 32, 62, 79, 17, 184, 164, 63, 91, 76, 99, 249, 216, 90, 233, 222, 54, 80, 87,
            201, 188, 232, 197, 124, 175, 132, 227, 79, 16, 64, 229, 245, 158, 205, 0, 0, 0, 0, 0,
            0, 0, 1, 0, 0, 0, 0, 0, 15, 66, 64, 0, 0, 0, 45, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 3, 2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0, 0,
            0, 0, 10, 0, 0, 0, 0, 93, 159, 35, 82, 0, 64, 143, 223, 245, 120, 96, 83, 116, 184, 28,
            224, 53, 242, 10, 92, 89, 49, 106, 137, 247, 68, 25, 192, 93, 70, 183, 205, 129, 22,
            14, 52, 18, 12, 110, 245, 97, 64, 25, 124, 249, 114, 116, 8, 215, 98, 235, 147, 0, 39,
            209, 44, 45, 21, 118, 40, 0, 103, 34, 117, 206, 162, 65, 47, 250, 13, 0, 0, 32, 62, 79,
            17, 184, 164, 63, 91, 76, 99, 249, 216, 90, 233, 222, 54, 80, 87, 201, 188, 232, 197,
            124, 175, 132, 227, 79, 16, 64, 229, 245, 158, 205, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0,
            0, 15, 66, 64, 0, 0, 0, 45, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
            0, 0, 0, 0, 0, 3, 2, 0, 0, 0, 1, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0,
            93, 159, 35, 82, 0, 64, 24, 109, 1, 140, 28, 45, 17, 211, 123, 17, 191, 109, 77, 139,
            33, 98, 219, 38, 121, 59, 75, 167, 76, 122, 147, 59, 117, 151, 72, 159, 227, 83, 99,
            17, 88, 16, 242, 67, 11, 255, 14, 180, 101, 203, 245, 72, 35, 124, 151, 213, 216, 78,
            62, 51, 205, 127, 237, 84, 83, 213, 93, 81, 152, 12, 0, 0, 32, 62, 79, 17, 184, 164,
            63, 91, 76, 99, 249, 216, 90, 233, 222, 54, 80, 87, 201, 188, 232, 197, 124, 175, 132,
            227, 79, 16, 64, 229, 245, 158, 205, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 15, 66, 64,
            0, 0, 0, 45, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            3, 2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 93, 159, 35,
            83, 0, 64, 221, 239, 113, 126, 120, 108, 116, 202, 88, 195, 248, 233, 186, 50, 46, 221,
            109, 172, 14, 248, 253, 220, 227, 16, 220, 136, 184, 4, 248, 34, 71, 48, 96, 174, 9,
            106, 78, 1, 176, 66, 141, 80, 22, 42, 220, 25, 42, 226, 203, 154, 254, 248, 20, 45,
            122, 108, 211, 43, 94, 98, 36, 145, 117, 1, 0, 0, 32, 62, 79, 17, 184, 164, 63, 91, 76,
            99, 249, 216, 90, 233, 222, 54, 80, 87, 201, 188, 232, 197, 124, 175, 132, 227, 79, 16,
            64, 229, 245, 158, 205, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 15, 66, 64, 0, 0, 0, 45,
            2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 3, 2, 0, 0,
            0, 0, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 93, 159, 35, 83, 0, 64,
            240, 160, 231, 100, 199, 116, 73, 106, 169, 205, 189, 214, 128, 248, 38, 10, 18, 127,
            124, 243, 205, 125, 68, 154, 227, 27, 91, 165, 201, 0, 213, 74, 186, 7, 93, 34, 100,
            43, 47, 218, 146, 110, 72, 153, 220, 206, 237, 113, 219, 239, 92, 82, 220, 88, 223, 32,
            79, 43, 37, 181, 227, 121, 131, 0, 0, 0, 32, 62, 79, 17, 184, 164, 63, 91, 76, 99, 249,
            216, 90, 233, 222, 54, 80, 87, 201, 188, 232, 197, 124, 175, 132, 227, 79, 16, 64, 229,
            245, 158, 205, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 15, 66, 64, 0, 0, 0, 45, 2, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 3, 2, 0, 0, 0, 1, 0,
            0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 93, 159, 35, 84, 0, 64, 78, 88,
            197, 48, 136, 227, 171, 247, 24, 16, 138, 101, 186, 1, 200, 75, 192, 4, 175, 174, 52,
            33, 62, 83, 122, 101, 220, 90, 184, 104, 159, 95, 17, 249, 51, 147, 3, 164, 60, 29,
            139, 78, 89, 121, 214, 100, 176, 145, 9, 207, 113, 251, 136, 75, 67, 80, 255, 76, 176,
            131, 204, 130, 82, 5, 0, 0, 32, 62, 79, 17, 184, 164, 63, 91, 76, 99, 249, 216, 90,
            233, 222, 54, 80, 87, 201, 188, 232, 197, 124, 175, 132, 227, 79, 16, 64, 229, 245,
            158, 205, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 15, 66, 64, 0, 0, 0, 45, 2, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 3, 2, 0, 0, 0, 0, 0, 0,
            0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 93, 159, 35, 84, 0, 64, 122, 175, 156,
            87, 164, 42, 34, 144, 229, 96, 248, 82, 149, 176, 65, 244, 163, 243, 70, 177, 132, 116,
            82, 92, 198, 47, 230, 141, 8, 167, 154, 13, 107, 110, 76, 154, 149, 149, 167, 1, 67,
            229, 39, 38, 110, 147, 41, 218, 237, 74, 181, 19, 120, 207, 173, 178, 238, 91, 71, 145,
            118, 225, 27, 7, 0, 0, 32, 62, 79, 17, 184, 164, 63, 91, 76, 99, 249, 216, 90, 233,
            222, 54, 80, 87, 201, 188, 232, 197, 124, 175, 132, 227, 79, 16, 64, 229, 245, 158,
            205, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 15, 66, 64, 0, 0, 0, 45, 2, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 2, 0, 0, 0, 0, 0, 0, 0, 1,
            0, 2, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 93, 159, 35, 85, 0, 64, 246, 76, 187, 195,
            117, 32, 243, 37, 220, 211, 94, 81, 42, 190, 255, 26, 152, 64, 206, 210, 76, 238, 0,
            124, 12, 187, 108, 49, 127, 184, 118, 63, 190, 68, 157, 36, 36, 102, 103, 4, 193, 154,
            131, 249, 80, 114, 172, 105, 210, 111, 161, 132, 174, 137, 22, 142, 252, 65, 64, 146,
            195, 106, 238, 12, 0, 0, 32, 62, 79, 17, 184, 164, 63, 91, 76, 99, 249, 216, 90, 233,
            222, 54, 80, 87, 201, 188, 232, 197, 124, 175, 132, 227, 79, 16, 64, 229, 245, 158,
            205, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 15, 66, 64, 0, 0, 0, 45, 2, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 3, 2, 0, 0, 0, 0, 0, 0, 0, 1,
            0, 2, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 93, 159, 35, 85, 0, 64, 117, 243, 153, 118,
            173, 212, 187, 246, 161, 125, 167, 46, 199, 178, 138, 163, 154, 251, 115, 238, 213, 31,
            200, 118, 201, 96, 139, 147, 2, 5, 20, 229, 25, 80, 141, 141, 71, 58, 173, 95, 235, 54,
            188, 105, 204, 175, 87, 233, 42, 157, 7, 114, 51, 192, 169, 237, 184, 236, 94, 23, 112,
            194, 44, 12, 0, 0, 32, 62, 79, 17, 184, 164, 63, 91, 76, 99, 249, 216, 90, 233, 222,
            54, 80, 87, 201, 188, 232, 197, 124, 175, 132, 227, 79, 16, 64, 229, 245, 158, 205, 0,
            0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 15, 66, 64, 0, 0, 0, 45, 2, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 2, 0,
            0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 93, 159, 35, 86, 0, 64, 130, 119, 19, 197, 83, 110,
            109, 249, 74, 177, 27, 45, 4, 152, 20, 15, 131, 148, 68, 126, 247, 49, 84, 193, 231,
            41, 150, 104, 40, 75, 147, 243, 66, 38, 238, 194, 56, 128, 207, 245, 133, 107, 212, 94,
            63, 50, 36, 209, 84, 4, 15, 0, 232, 113, 188, 79, 187, 103, 130, 127, 103, 222, 83, 14,
            0, 0, 32, 62, 79, 17, 184, 164, 63, 91, 76, 99, 249, 216, 90, 233, 222, 54, 80, 87,
            201, 188, 232, 197, 124, 175, 132, 227, 79, 16, 64, 229, 245, 158, 205, 0, 0, 0, 0, 0,
            0, 0, 10, 0, 0, 0, 0, 0, 15, 66, 64, 0, 0, 0, 45, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 3, 2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0,
            0, 0, 0, 10, 0, 0, 0, 0, 93, 159, 35, 86, 0, 64, 109, 61, 88, 32, 189, 177, 86, 122,
            145, 22, 130, 65, 125, 199, 226, 5, 153, 214, 247, 70, 129, 241, 22, 139, 91, 162, 1,
            155, 87, 163, 245, 159, 233, 180, 169, 57, 190, 133, 119, 209, 48, 183, 185, 27, 34,
            128, 43, 152, 122, 53, 155, 184, 244, 107, 114, 107, 21, 167, 89, 197, 98, 1, 30, 13,
            0, 0, 32, 62, 79, 17, 184, 164, 63, 91, 76, 99, 249, 216, 90, 233, 222, 54, 80, 87,
            201, 188, 232, 197, 124, 175, 132, 227, 79, 16, 64, 229, 245, 158, 205, 0, 0, 0, 0, 0,
            0, 0, 11, 0, 0, 0, 0, 0, 15, 66, 64, 0, 0, 0, 45, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0,
            0, 0, 0, 10, 0, 0, 0, 0, 93, 159, 35, 87, 0, 64, 164, 152, 141, 176, 33, 235, 128, 77,
            86, 150, 82, 126, 216, 42, 66, 118, 178, 115, 147, 219, 253, 221, 219, 243, 199, 252,
            246, 50, 9, 245, 239, 252, 13, 6, 19, 11, 215, 183, 136, 203, 83, 144, 122, 105, 51,
            201, 120, 141, 250, 158, 1, 217, 30, 22, 122, 196, 41, 167, 43, 139, 247, 84, 59, 10,
            0, 0, 32, 62, 79, 17, 184, 164, 63, 91, 76, 99, 249, 216, 90, 233, 222, 54, 80, 87,
            201, 188, 232, 197, 124, 175, 132, 227, 79, 16, 64, 229, 245, 158, 205, 0, 0, 0, 0, 0,
            0, 0, 12, 0, 0, 0, 0, 0, 15, 66, 64, 0, 0, 0, 45, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0,
            0, 0, 0, 10, 0, 0, 0, 0, 93, 159, 35, 87, 0, 64, 191, 204, 142, 58, 61, 159, 237, 69,
            6, 50, 108, 139, 51, 151, 142, 24, 105, 225, 35, 79, 109, 158, 69, 34, 236, 144, 135,
            149, 166, 3, 92, 26, 27, 84, 21, 31, 43, 17, 155, 75, 35, 70, 168, 35, 36, 161, 250,
            115, 225, 15, 104, 15, 166, 233, 208, 162, 174, 109, 109, 32, 114, 69, 50, 14, 0, 0,
            32, 62, 79, 17, 184, 164, 63, 91, 76, 99, 249, 216, 90, 233, 222, 54, 80, 87, 201, 188,
            232, 197, 124, 175, 132, 227, 79, 16, 64, 229, 245, 158, 205, 0, 0, 0, 0, 0, 0, 0, 13,
            0, 0, 0, 0, 0, 15, 66, 64, 0, 0, 0, 45, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            1, 0, 0, 0, 0, 0, 0, 0, 0, 3, 2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 10,
            0, 0, 0, 0, 93, 159, 35, 88, 0, 64, 162, 198, 26, 223, 56, 1, 176, 162, 233, 127, 177,
            191, 245, 119, 45, 21, 21, 30, 28, 197, 42, 122, 171, 77, 226, 100, 44, 140, 40, 40,
            147, 85, 67, 86, 100, 83, 5, 20, 226, 205, 140, 39, 151, 65, 16, 87, 225, 180, 186,
            254, 108, 216, 208, 157, 225, 209, 98, 217, 1, 81, 194, 27, 174, 12, 0, 0, 32, 62, 79,
            17, 184, 164, 63, 91, 76, 99, 249, 216, 90, 233, 222, 54, 80, 87, 201, 188, 232, 197,
            124, 175, 132, 227, 79, 16, 64, 229, 245, 158, 205, 0, 0, 0, 0, 0, 0, 0, 14, 0, 0, 0,
            0, 0, 15, 66, 64, 0, 0, 0, 45, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
            0, 0, 0, 0, 0, 0, 3, 2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0,
            0, 93, 159, 35, 88, 0, 64, 42, 104, 47, 146, 233, 45, 52, 40, 20, 208, 107, 161, 160,
            210, 222, 115, 149, 144, 48, 84, 63, 61, 61, 227, 174, 216, 89, 23, 73, 166, 131, 104,
            164, 227, 161, 254, 210, 155, 161, 64, 209, 226, 77, 9, 21, 129, 131, 102, 11, 165, 41,
            235, 85, 76, 232, 140, 150, 171, 150, 178, 5, 38, 254, 1, 0, 0, 32, 62, 79, 17, 184,
            164, 63, 91, 76, 99, 249, 216, 90, 233, 222, 54, 80, 87, 201, 188, 232, 197, 124, 175,
            132, 227, 79, 16, 64, 229, 245, 158, 205, 0, 0, 0, 0, 0, 0, 0, 15, 0, 0, 0, 0, 0, 15,
            66, 64, 0, 0, 0, 45, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 3, 2, 0, 0, 0, 1, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 93,
            159, 35, 89, 0, 64, 88, 51, 223, 62, 223, 210, 63, 250, 107, 116, 110, 62, 209, 2, 116,
            173, 185, 159, 244, 67, 200, 203, 216, 239, 238, 141, 231, 52, 235, 52, 58, 113, 190,
            207, 92, 42, 54, 48, 122, 115, 242, 66, 24, 139, 16, 72, 88, 140, 24, 112, 246, 175,
            30, 64, 213, 75, 62, 147, 53, 0, 112, 206, 73, 9, 0, 0, 32, 62, 79, 17, 184, 164, 63,
            91, 76, 99, 249, 216, 90, 233, 222, 54, 80, 87, 201, 188, 232, 197, 124, 175, 132, 227,
            79, 16, 64, 229, 245, 158, 205, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 0, 0, 0, 15, 66, 64, 0,
            0, 0, 45, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 3,
            2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 93, 159, 35, 89,
            0, 64, 202, 93, 100, 148, 62, 177, 28, 69, 237, 104, 39, 223, 27, 182, 156, 130, 50,
            128, 131, 161, 59, 159, 101, 3, 174, 188, 132, 151, 167, 57, 130, 246, 162, 53, 92, 24,
            158, 72, 219, 46, 56, 22, 182, 55, 104, 212, 190, 160, 43, 8, 76, 210, 119, 125, 224,
            13, 126, 64, 15, 197, 220, 202, 128, 11, 0, 0, 32, 62, 79, 17, 184, 164, 63, 91, 76,
            99, 249, 216, 90, 233, 222, 54, 80, 87, 201, 188, 232, 197, 124, 175, 132, 227, 79, 16,
            64, 229, 245, 158, 205, 0, 0, 0, 0, 0, 0, 0, 17, 0, 0, 0, 0, 0, 15, 66, 64, 0, 0, 0,
            45, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 2, 0,
            0, 0, 0, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 93, 159, 35, 90, 0, 64,
            144, 146, 84, 184, 146, 156, 70, 112, 51, 140, 5, 83, 71, 19, 159, 239, 23, 92, 22,
            174, 133, 231, 47, 141, 210, 32, 186, 50, 95, 17, 136, 152, 102, 6, 242, 53, 82, 29,
            91, 217, 150, 127, 236, 172, 200, 135, 71, 214, 23, 65, 164, 248, 251, 154, 200, 48,
            240, 222, 180, 6, 205, 79, 171, 14, 0, 0, 32, 62, 79, 17, 184, 164, 63, 91, 76, 99,
            249, 216, 90, 233, 222, 54, 80, 87, 201, 188, 232, 197, 124, 175, 132, 227, 79, 16, 64,
            229, 245, 158, 205, 0, 0, 0, 0, 0, 0, 0, 18, 0, 0, 0, 0, 0, 15, 66, 64, 0, 0, 0, 45, 2,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 3, 2, 0, 0, 0,
            0, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 93, 159, 35, 90, 0, 64, 134,
            35, 234, 209, 121, 144, 177, 25, 193, 218, 157, 64, 224, 23, 68, 237, 230, 116, 143,
            41, 87, 187, 247, 3, 224, 161, 3, 102, 230, 196, 128, 67, 54, 255, 148, 227, 210, 255,
            1, 167, 108, 144, 109, 145, 18, 215, 28, 232, 232, 5, 182, 78, 118, 155, 189, 210, 160,
            118, 10, 180, 152, 0, 26, 10, 0, 0, 32, 62, 79, 17, 184, 164, 63, 91, 76, 99, 249, 216,
            90, 233, 222, 54, 80, 87, 201, 188, 232, 197, 124, 175, 132, 227, 79, 16, 64, 229, 245,
            158, 205, 0, 0, 0, 0, 0, 0, 0, 19, 0, 0, 0, 0, 0, 15, 66, 64, 0, 0, 0, 45, 2, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 3, 2, 0, 0, 0, 0, 0, 0,
            0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 93, 159, 35, 91, 0, 64, 192, 134, 93,
            138, 126, 164, 42, 56, 107, 95, 233, 19, 68, 115, 111, 195, 128, 53, 13, 197, 142, 18,
            33, 93, 111, 139, 15, 145, 167, 175, 205, 171, 181, 249, 124, 28, 107, 64, 21, 133, 14,
            228, 140, 210, 254, 81, 11, 93, 196, 181, 70, 219, 101, 233, 68, 172, 30, 173, 188,
            135, 141, 110, 169, 0, 0, 0, 32, 62, 79, 17, 184, 164, 63, 91, 76, 99, 249, 216, 90,
            233, 222, 54, 80, 87, 201, 188, 232, 197, 124, 175, 132, 227, 79, 16, 64, 229, 245,
            158, 205, 0, 0, 0, 0, 0, 0, 0, 20, 0, 0, 0, 0, 0, 15, 66, 64, 0, 0, 0, 45, 2, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 2, 0, 0, 0, 0, 0, 0,
            0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 93, 159, 35, 91, 0, 64, 17, 182, 36,
            151, 173, 7, 108, 231, 252, 132, 140, 119, 71, 254, 93, 49, 194, 77, 18, 71, 140, 25,
            165, 10, 17, 115, 77, 41, 138, 171, 11, 79, 178, 38, 35, 27, 191, 140, 122, 4, 29, 157,
            79, 233, 170, 56, 5, 232, 126, 19, 161, 126, 187, 174, 216, 69, 97, 46, 98, 160, 2,
            191, 181, 0, 0, 0, 32, 62, 79, 17, 184, 164, 63, 91, 76, 99, 249, 216, 90, 233, 222,
            54, 80, 87, 201, 188, 232, 197, 124, 175, 132, 227, 79, 16, 64, 229, 245, 158, 205, 0,
            0, 0, 0, 0, 0, 0, 21, 0, 0, 0, 0, 0, 15, 66, 64, 0, 0, 0, 45, 2, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 2,
            0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 93, 159, 35, 92, 0, 64, 159, 107, 124, 140, 59,
            167, 209, 130, 61, 173, 219, 108, 122, 40, 23, 208, 79, 117, 115, 21, 51, 8, 253, 53,
            81, 92, 176, 105, 248, 45, 129, 120, 191, 162, 241, 60, 11, 197, 125, 22, 93, 217, 164,
            228, 128, 171, 79, 75, 245, 87, 236, 19, 182, 234, 133, 101, 79, 208, 66, 112, 83, 73,
            199, 13, 0, 0, 32, 62, 79, 17, 184, 164, 63, 91, 76, 99, 249, 216, 90, 233, 222, 54,
            80, 87, 201, 188, 232, 197, 124, 175, 132, 227, 79, 16, 64, 229, 245, 158, 205, 0, 0,
            0, 0, 0, 0, 0, 22, 0, 0, 0, 0, 0, 15, 66, 64, 0, 0, 0, 45, 2, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 3, 2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 2, 0,
            0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 93, 159, 35, 92, 0, 64, 5, 170, 141, 42, 69, 127, 85,
            174, 153, 224, 207, 142, 35, 3, 222, 189, 179, 185, 226, 34, 204, 84, 16, 84, 60, 249,
            218, 168, 38, 106, 98, 158, 242, 87, 52, 204, 5, 204, 177, 14, 229, 221, 144, 170, 212,
            249, 119, 54, 193, 155, 90, 15, 177, 156, 177, 33, 225, 214, 90, 109, 136, 143, 230, 6,
        ];
        PendingBlock::new(&b).map(|_| ())
    }
}
