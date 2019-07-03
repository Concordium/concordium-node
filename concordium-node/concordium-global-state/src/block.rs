// https://gitlab.com/Concordium/consensus/globalstate-mockup/blob/master/globalstate/src/Concordium/GlobalState/Block.hs

use byteorder::{ByteOrder, NetworkEndian, WriteBytesExt};
use chrono::prelude::{DateTime, Utc};
use failure::Fallible;

use std::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    io::{Cursor, Read, Write},
    mem::size_of,
    rc::Rc,
};

use crate::{common::*, parameters::*, transaction::*};

const NONCE: u8 = size_of::<BlockHash>() as u8 + PROOF_LENGTH as u8; // should soon be shorter
const SIGNATURE: u8 = 8 + 64; // FIXME: unnecessary 8B prefix
const CHRONO_DATE_TIME_LEN: u8 = 12;

#[derive(Debug)]
pub enum Block {
    Genesis(GenesisData),
    Regular(BakedBlock),
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
    pub fn baker_id(&self) -> BakerId { self.block_data().baker_id }

    pub fn block_data(&self) -> &BakedBlock {
        match self {
            Block::Genesis(_) => unreachable!(), // the genesis block is unmistakeable
            Block::Regular(data) => data,
        }
    }

    pub fn genesis_data(&self) -> &GenesisData {
        match self {
            Block::Genesis(ref data) => data,
            Block::Regular(_) => unreachable!(), // the genesis block is unmistakeable
        }
    }

    pub fn pointer(&self) -> Option<&BlockHash> {
        match self {
            Block::Genesis(_) => None,
            Block::Regular(block) => Some(&block.pointer),
        }
    }

    pub fn slot(&self) -> Slot {
        match self {
            Block::Genesis(_) => 0,
            Block::Regular(block) => block.slot,
        }
    }
}

impl<'a, 'b> SerializeToBytes<'a, 'b> for Block {
    type Source = &'a [u8];

    fn deserialize(_bytes: &[u8]) -> Fallible<Self> {
        unimplemented!() // not used directly
    }

    fn serialize(&self) -> Box<[u8]> {
        match self {
            Block::Genesis(genesis_data) => {
                [
                    &[0u8; 8], // a 0u64 slot id prefix
                    &*genesis_data.serialize(),
                ]
                .concat()
                .into_boxed_slice()
            }
            Block::Regular(block_data) => block_data.serialize(),
        }
    }
}

pub struct BakedBlock {
    pub slot:           Slot,
    pub pointer:        BlockHash,
    pub baker_id:       BakerId,
    proof:              Encoded,
    nonce:              Encoded,
    pub last_finalized: BlockHash,
    transactions:       Box<[Transaction]>,
    signature:          Encoded,
}

// this is a very debug method used only by the Display impl of ConsensusMessage
impl fmt::Debug for BakedBlock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "block {:?} -> {:?}",
            sha256(&self.serialize()),
            self.pointer
        )
    }
}

impl<'a, 'b> SerializeToBytes<'a, 'b> for BakedBlock {
    type Source = &'a [u8];

    fn deserialize(bytes: &[u8]) -> Fallible<Self> {
        let mut cursor = Cursor::new(bytes);

        let slot = NetworkEndian::read_u64(&read_ty!(&mut cursor, Slot));
        let pointer = HashBytes::from(read_ty!(&mut cursor, BlockHash));
        let baker_id = NetworkEndian::read_u64(&read_ty!(&mut cursor, BakerId));
        let proof = Encoded::new(&read_const_sized!(&mut cursor, PROOF_LENGTH));
        let nonce = Encoded::new(&read_const_sized!(&mut cursor, NONCE));
        let last_finalized = HashBytes::from(read_ty!(&mut cursor, BlockHash));
        let transactions = read_multiple!(
            &mut cursor,
            "transactions",
            Transaction::deserialize(&mut cursor)?
        );
        let signature = Encoded::new(&read_const_sized!(&mut cursor, SIGNATURE));

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

    fn serialize(&self) -> Box<[u8]> {
        let transactions = serialize_list(&self.transactions);

        let mut cursor = create_serialization_cursor(
            size_of::<Slot>()
                + size_of::<BlockHash>()
                + size_of::<BakerId>()
                + PROOF_LENGTH
                + NONCE as usize
                + size_of::<BlockHash>()
                + size_of::<u64>()
                + list_len(&transactions)
                + SIGNATURE as usize,
        );

        let _ = cursor.write_u64::<NetworkEndian>(self.slot);
        let _ = cursor.write_all(&self.pointer);
        let _ = cursor.write_u64::<NetworkEndian>(self.baker_id);
        let _ = cursor.write_all(&self.proof);
        let _ = cursor.write_all(&self.nonce);
        let _ = cursor.write_all(&self.last_finalized);
        write_multiple!(&mut cursor, transactions, Write::write_all);
        let _ = cursor.write_all(&self.signature);

        cursor.into_inner()
    }
}

#[derive(Debug)]
pub struct GenesisData {
    timestamp:                   Timestamp,
    slot_duration:               Duration,
    birk_parameters:             BirkParameters,
    baker_accounts:              Box<[Account]>,
    pub finalization_parameters: Box<[VoterInfo]>,
}

impl<'a, 'b> SerializeToBytes<'a, 'b> for GenesisData {
    type Source = &'a [u8];

    fn deserialize(bytes: &[u8]) -> Fallible<Self> {
        let mut cursor = Cursor::new(bytes);

        let timestamp = NetworkEndian::read_u64(&read_ty!(&mut cursor, Timestamp));
        let slot_duration = NetworkEndian::read_u64(&read_ty!(&mut cursor, Duration));
        let birk_parameters = BirkParameters::deserialize(&mut cursor)?;
        let baker_accounts =
            read_multiple!(cursor, "baker accounts", Account::deserialize(&mut cursor)?);
        let finalization_parameters = read_multiple!(
            cursor,
            "finalization parameters",
            VoterInfo::deserialize(&read_const_sized!(&mut cursor, VOTER_INFO))?
        );

        let data = GenesisData {
            timestamp,
            slot_duration,
            birk_parameters,
            baker_accounts,
            finalization_parameters,
        };

        check_serialization!(data, cursor);

        Ok(data)
    }

    fn serialize(&self) -> Box<[u8]> {
        let birk_params = BirkParameters::serialize(&self.birk_parameters);
        let baker_accounts = serialize_list(&self.baker_accounts);
        let finalization_params = serialize_list(&self.finalization_parameters);

        let size = size_of::<Timestamp>()
            + size_of::<Duration>()
            + birk_params.len()
            + size_of::<u64>()
            + list_len(&baker_accounts)
            + size_of::<u64>()
            + list_len(&finalization_params);
        let mut cursor = create_serialization_cursor(size);

        let _ = cursor.write_u64::<NetworkEndian>(self.timestamp);
        let _ = cursor.write_u64::<NetworkEndian>(self.slot_duration);
        let _ = cursor.write_all(&birk_params);
        write_multiple!(&mut cursor, baker_accounts, Write::write_all);
        write_multiple!(&mut cursor, finalization_params, Write::write_all);

        cursor.into_inner()
    }
}

pub type BakerId = u64;

pub type Timestamp = u64;

pub type Duration = u64;

pub type BlockHeight = u64;

pub type Delta = u64;

pub type BlockHash = HashBytes;

#[derive(Debug)]
pub struct PendingBlock {
    pub hash:     BlockHash,
    pub block:    BakedBlock,
    pub received: DateTime<Utc>,
}

impl PendingBlock {
    pub fn new(bytes: &[u8]) -> Fallible<Self> {
        Ok(Self {
            hash:     sha256(bytes),
            block:    BakedBlock::deserialize(bytes)?,
            received: Utc::now(),
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

impl From<BakedBlock> for PendingBlock {
    fn from(block: BakedBlock) -> Self {
        Self {
            hash: sha256(&block.serialize()),
            block,
            received: Utc::now(),
        }
    }
}

pub struct BlockPtr {
    pub hash:           BlockHash,
    pub block:          Block,
    pub parent:         Option<Rc<BlockPtr>>,
    pub last_finalized: Option<Rc<BlockPtr>>,
    pub height:         BlockHeight,
    // state:       BlockState,
    pub received:  DateTime<Utc>,
    pub validated: DateTime<Utc>,
}

impl BlockPtr {
    pub fn genesis(genesis_bytes: &[u8]) -> Self {
        let genesis_data = GenesisData::deserialize(genesis_bytes).expect("Invalid genesis data");
        let genesis_block = Block::Genesis(genesis_data);
        // the genesis block byte representation is the genesis data prefixed with a
        // 0u64 slot id
        let genesis_block_hash = sha256(&[&[0u8; 8], genesis_bytes].concat());
        let timestamp = Utc::now(); // TODO: be more precise when Kontrol is there

        BlockPtr {
            hash:           genesis_block_hash,
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
        parent: Rc<Self>,
        last_finalized: Rc<Self>,
        validated: DateTime<Utc>,
    ) -> Self {
        let height = parent.height + 1;

        Self {
            hash: pb.hash,
            block: Block::Regular(pb.block),
            parent: Some(parent),
            last_finalized: Some(last_finalized),
            height,
            received: pb.received,
            validated,
        }
    }

    pub fn is_ancestor_of(&self, candidate: &Self) -> bool {
        match self.cmp(candidate) {
            Ordering::Greater => false,
            Ordering::Equal => self == candidate,
            Ordering::Less => {
                let next_candidate = if let Some(ref candidate) = candidate.parent {
                    candidate
                } else {
                    return false;
                };

                self.is_ancestor_of(next_candidate)
            }
        }
    }

    // possibly change to SerializeToBytes::serialize when implementing caching
    pub fn serialize_to_disk_format(&self) -> Box<[u8]> {
        fn serialize_date(date: DateTime<Utc>) -> [u8; CHRONO_DATE_TIME_LEN as usize] {
            unsafe {
                std::mem::transmute::<DateTime<Utc>, [u8; CHRONO_DATE_TIME_LEN as usize]>(date)
            }
        }

        let block = self.block.serialize();

        let mut cursor = create_serialization_cursor(
            block.len() + size_of::<BlockHeight>() + 2 * CHRONO_DATE_TIME_LEN as usize,
        );

        let _ = cursor.write_all(&block);
        let _ = cursor.write_u64::<NetworkEndian>(self.height);
        let _ = cursor.write_all(&serialize_date(self.received));
        let _ = cursor.write_all(&serialize_date(self.validated));

        cursor.into_inner()
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

impl<'a, 'b> SerializeToBytes<'a, 'b> for BlockPtr {
    type Source = &'a [u8];

    fn serialize(&self) -> Box<[u8]> { self.block.serialize() }

    fn deserialize(_source: Self::Source) -> Fallible<Self> {
        unimplemented!("BlockPtr is not to be deserialized directly")
    }
}
