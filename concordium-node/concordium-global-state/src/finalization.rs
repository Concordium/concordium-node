use byteorder::{ByteOrder, NetworkEndian, ReadBytesExt, WriteBytesExt};
use failure::Fallible;

use std::{cmp::Ordering, fmt, mem::size_of};

use crate::{block::*, common::*};

const HEADER: u8 = size_of::<SessionId>() as u8
    + size_of::<FinalizationIndex>() as u8
    + size_of::<BlockHeight>() as u8
    + size_of::<Party>() as u8;

#[derive(PartialEq, Eq, Hash)]
pub struct FinalizationMessage {
    header:  FinalizationMessageHeader,
    payload: Encoded,
}

impl fmt::Debug for FinalizationMessage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "finalization message (sess {}, idx {}, party {}",
            self.header.session_id, self.header.index, self.header.sender /* , self.message */
        )
    }
}

impl Serial for FinalizationMessage {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        let header =
            FinalizationMessageHeader::deserial(&mut &read_const_sized!(source, HEADER)[..])?;
        let mut payload = Vec::new();
        source.read_to_end(&mut payload)?;
        let payload = Encoded::from(payload.into_boxed_slice());

        let msg = FinalizationMessage { header, payload };

        Ok(msg)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        self.header.serial(target)?;
        target.write_all(&*self.payload)?;

        Ok(())
    }
}

pub type FinalizationIndex = u64;

type Party = u32;

#[derive(Debug, PartialEq, Eq, Hash)]
struct FinalizationMessageHeader {
    session_id: SessionId,
    index:      FinalizationIndex,
    delta:      Delta,
    sender:     Party,
}

impl Serial for FinalizationMessageHeader {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        let session_id = SessionId::deserial(&mut &read_ty!(source, SessionId)[..])?;
        let index = FinalizationIndex::deserial(source)?;
        let delta = Delta::deserial(source)?;
        let sender = Party::deserial(source)?;

        let header = FinalizationMessageHeader {
            session_id,
            index,
            delta,
            sender,
        };

        Ok(header)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        self.session_id.serial(target)?;
        FinalizationIndex::serial(&self.index, target)?;
        Delta::serial(&self.delta, target)?;
        Party::serial(&self.sender, target)?;

        Ok(())
    }
}

pub struct FinalizationRecord {
    pub index:         FinalizationIndex,
    pub block_pointer: BlockHash,
    pub proof:         Box<[(Party, Encoded)]>,
    pub delay:         BlockHeight,
}

impl PartialEq for FinalizationRecord {
    fn eq(&self, other: &Self) -> bool { self.block_pointer == other.block_pointer }
}

impl Eq for FinalizationRecord {}

impl PartialOrd for FinalizationRecord {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { Some(self.index.cmp(&other.index)) }
}

impl Ord for FinalizationRecord {
    fn cmp(&self, other: &Self) -> Ordering { self.index.cmp(&other.index) }
}

impl fmt::Debug for FinalizationRecord {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "finalization record for block {:?}", self.block_pointer)
    }
}

impl FinalizationRecord {
    pub fn genesis(genesis_block_ptr: &BlockPtr) -> Self {
        let proof: Box<[(u32, Encoded)]> = Vec::new().into_boxed_slice();

        Self {
            index: 0,
            block_pointer: genesis_block_ptr.hash.to_owned(),
            proof,
            delay: 0,
        }
    }
}

impl Serial for FinalizationRecord {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        let index = FinalizationIndex::deserial(source)?;
        let block_pointer = HashBytes::from(read_ty!(source, BlockHash));

        let proof = read_multiple!(
            source,
            (
                source.read_u32::<NetworkEndian>()?,
                read_bytestring_short_length(source)?
            ),
            4,
            1024
        );

        let delay = BlockHeight::deserial(source)?;

        let rec = FinalizationRecord {
            index,
            block_pointer,
            proof,
            delay,
        };

        Ok(rec)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        FinalizationIndex::serial(&self.index, target)?;
        target.write_all(&self.block_pointer)?;

        target.write_u32::<NetworkEndian>(self.proof.len() as u32)?;
        for (party, signature) in &*self.proof {
            target.write_u32::<NetworkEndian>(*party)?;
            target.write_u16::<NetworkEndian>(signature.len() as u16)?;
            target.write_all(signature)?;
        }

        BlockHeight::serial(&self.index, target)?;

        Ok(())
    }
}
