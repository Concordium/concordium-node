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
const CHRONO_DATE_TIME_LEN: u8 = 12;

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
    pub fn genesis_data(&self) -> &GenesisData {
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
            BlockData::Regular(ref block) => Some(&block.pointer),
        }
    }

    pub fn last_finalized(&self) -> Option<&BlockHash> {
        match &self.data {
            BlockData::Genesis(_) => None,
            BlockData::Regular(ref block) => Some(&block.last_finalized),
        }
    }

    pub fn slot(&self) -> Slot { self.slot }
}

impl<'a, 'b> SerializeToBytes<'a, 'b> for Block {
    type Source = &'a [u8];

    fn deserialize(bytes: &[u8]) -> Fallible<Self> {
        let mut cursor = Cursor::new(bytes);

        let slot = NetworkEndian::read_u64(&read_ty!(&mut cursor, Slot));
        let data = BlockData::deserialize((&mut cursor, slot))?;

        let block = Block { slot, data };

        check_serialization!(block, cursor);

        Ok(block)
    }

    fn serialize(&self) -> Box<[u8]> {
        let block_data = BlockData::serialize(&self.data);

        let mut cursor = create_serialization_cursor(size_of::<Slot>() + block_data.len());

        let _ = cursor.write_u64::<NetworkEndian>(self.slot);
        let _ = cursor.write_all(&block_data);

        cursor.into_inner()
    }
}

impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let hash = if self.slot != 0 {
            format!(
                "block {:?} by baker {}",
                sha256(&self.serialize()),
                self.block_data().baker_id
            )
        } else {
            format!("genesis {:?}", sha256(&self.serialize()))
        };

        write!(f, "{}", hash)
    }
}

pub enum BlockData {
    Genesis(GenesisData),
    Regular(BakedBlock),
}

impl<'a, 'b: 'a> SerializeToBytes<'a, 'b> for BlockData {
    type Source = (&'a mut Cursor<&'b [u8]>, Slot);

    fn deserialize((cursor, slot): Self::Source) -> Fallible<Self> {
        if slot == 0 {
            let timestamp = NetworkEndian::read_u64(&read_ty!(cursor, Timestamp));
            let slot_duration = NetworkEndian::read_u64(&read_ty!(cursor, Duration));
            let birk_parameters = BirkParameters::deserialize(cursor)?;
            let baker_accounts =
                read_multiple!(cursor, "baker accounts", Account::deserialize(cursor)?, 8);
            let finalization_parameters = read_multiple!(
                cursor,
                "finalization parameters",
                VoterInfo::deserialize(&read_const_sized!(cursor, VOTER_INFO))?,
                8
            );
            let finalization_minimum_skip = NetworkEndian::read_u64(&read_ty!(cursor, BlockHeight));
            let cryptographic_parameters = CryptographicParameters::deserialize(cursor)?;
            let mut buffer = Vec::new();
            cursor.read_to_end(&mut buffer)?;
            let identity_providers = Encoded::new(&buffer);

            let data = BlockData::Genesis(GenesisData {
                timestamp,
                slot_duration,
                birk_parameters,
                baker_accounts,
                finalization_parameters,
                finalization_minimum_skip,
                cryptographic_parameters,
                identity_providers,
            });

            Ok(data)
        } else {
            let pointer = HashBytes::from(read_ty!(cursor, BlockHash));
            let baker_id = NetworkEndian::read_u64(&read_ty!(cursor, BakerId));
            let proof = Encoded::new(&read_const_sized!(cursor, PROOF_LENGTH));
            let nonce = Encoded::new(&read_const_sized!(cursor, NONCE));
            let last_finalized = HashBytes::from(read_ty!(cursor, BlockHash));
            let transactions =
                read_multiple!(cursor, "transactions", Transaction::deserialize(cursor)?, 8);
            let signature = read_bytestring_short_length(cursor, "block signature")?;

            let data = BlockData::Regular(BakedBlock {
                pointer,
                baker_id,
                proof,
                nonce,
                last_finalized,
                transactions,
                signature,
            });

            Ok(data)
        }
    }

    fn serialize(&self) -> Box<[u8]> {
        match self {
            BlockData::Genesis(ref data) => {
                let birk_params = BirkParameters::serialize(&data.birk_parameters);
                let cryptographic_parameters =
                    CryptographicParameters::serialize(&data.cryptographic_parameters);
                let baker_accounts = serialize_list(&data.baker_accounts);
                let finalization_params = serialize_list(&data.finalization_parameters);

                let size = size_of::<Timestamp>()
                    + size_of::<Duration>()
                    + birk_params.len()
                    + size_of::<u64>()
                    + list_len(&baker_accounts)
                    + size_of::<u64>()
                    + list_len(&finalization_params)
                    + size_of::<u64>()
                    + size_of::<u64>()
                    + data.cryptographic_parameters.elgamal_generator.len()
                    + size_of::<u64>()
                    + data.cryptographic_parameters.attribute_commitment_key.len()
                    + data.identity_providers.len();
                let mut cursor = create_serialization_cursor(size);

                let _ = cursor.write_u64::<NetworkEndian>(data.timestamp);
                let _ = cursor.write_u64::<NetworkEndian>(data.slot_duration);
                let _ = cursor.write_all(&birk_params);
                write_multiple!(&mut cursor, baker_accounts, Write::write_all);
                write_multiple!(&mut cursor, finalization_params, Write::write_all);
                let _ = cursor.write_u64::<NetworkEndian>(data.finalization_minimum_skip);
                let _ = cursor.write_all(&cryptographic_parameters);
                let _ = cursor.write_all(&data.identity_providers);

                cursor.into_inner()
            }
            BlockData::Regular(ref data) => {
                let transactions = serialize_list(&data.transactions);

                let mut cursor = create_serialization_cursor(
                    size_of::<BlockHash>()
                        + size_of::<BakerId>()
                        + PROOF_LENGTH
                        + NONCE as usize
                        + size_of::<BlockHash>()
                        + size_of::<u64>()
                        + list_len(&transactions)
                        + size_of::<u16>()
                        + data.signature.len() as usize,
                );

                let _ = cursor.write_all(&data.pointer);
                let _ = cursor.write_u64::<NetworkEndian>(data.baker_id);
                let _ = cursor.write_all(&data.proof);
                let _ = cursor.write_all(&data.nonce);
                let _ = cursor.write_all(&data.last_finalized);
                write_multiple!(&mut cursor, transactions, Write::write_all);
                write_bytestring_short_length(&mut cursor, &data.signature);

                cursor.into_inner()
            }
        }
    }
}

#[derive(Debug)]
pub struct BakedBlock {
    pub pointer:        BlockHash,
    pub baker_id:       BakerId,
    proof:              Encoded,
    nonce:              Encoded,
    pub last_finalized: BlockHash,
    transactions:       Box<[Transaction]>,
    signature:          Encoded,
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

pub type BakerId = u64;

pub type Timestamp = u64;

pub type Duration = u64;

pub type BlockHeight = u64;

pub type Delta = u64;

pub type BlockHash = HashBytes;

#[derive(Debug)]
pub struct PendingBlock {
    pub hash:     BlockHash,
    pub block:    Block,
    pub received: DateTime<Utc>,
}

impl PendingBlock {
    pub fn new(bytes: &[u8]) -> Fallible<Self> {
        Ok(Self {
            hash:     sha256(bytes),
            block:    Block::deserialize(bytes)?,
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
        let mut cursor = Cursor::new(genesis_bytes);
        let genesis_data = BlockData::deserialize((&mut cursor, 0)).expect("Invalid genesis data");
        let genesis_block = Block {
            slot: 0,
            data: genesis_data,
        };
        // the genesis block byte representation is the genesis data prefixed with a
        // 0u64 slot id
        let genesis_block_hash = sha256(&genesis_block.serialize());
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
            block: pb.block,
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
