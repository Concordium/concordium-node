use byteorder::{NetworkEndian, ReadBytesExt};

use crate::block::*;
use crate::common::*;

const HEADER: usize = 60;
const FINALIZATION_INDEX: usize = 8;
const DELTA: usize = 8;
const SENDER: usize = 4;
const SIGNATURE: usize = 64;
const WMVBA_TYPE: usize = 1;
const VAL: usize = BLOCK_HASH;
const PHASE: usize = 4;
const TICKET: usize = 80;
const ABBA_INPUT: usize = PHASE + TICKET;
const PARTY: usize = 4;

#[derive(Debug)]
pub struct FinalizationMessage {
    header: FinalizationMessageHeader,
    message: WmvbaMessage,
    signature: Encoded,
}

impl FinalizationMessage {
    pub fn deserialize(bytes: &[u8]) -> Option<Self> {
        let mut curr_pos = 0;

        let header = FinalizationMessageHeader::deserialize(&bytes[curr_pos..][..HEADER])?;
        curr_pos += HEADER;

        let message = WmvbaMessage::deserialize(&bytes[curr_pos..bytes.len() - SIGNATURE])?;

        let signature = Encoded::new(&bytes[bytes.len() - SIGNATURE..]);

        Some(FinalizationMessage {
            header,
            message,
            signature,
        })
    }

    pub fn serialize(&self) -> Vec<u8> {
        unimplemented!()
    }
}

type FinalizationIndex = u64;

type Party = u32;

#[derive(Debug)]
struct FinalizationMessageHeader {
    session_id: SessionId,
    finalization_index: FinalizationIndex,
    delta: BlockHeight,
    sender: Party,
}

impl FinalizationMessageHeader {
    pub fn deserialize(bytes: &[u8]) -> Option<Self> {
        let mut curr_pos = 0;

        let session_id = SessionId::deserialize(&bytes[curr_pos..][..SESSION_ID])?;
        curr_pos += SESSION_ID;

        let finalization_index = (&bytes[curr_pos..][..FINALIZATION_INDEX])
            .read_u64::<NetworkEndian>()
            .ok()?;
        curr_pos += FINALIZATION_INDEX;

        let delta = (&bytes[curr_pos..][..DELTA])
            .read_u64::<NetworkEndian>()
            .ok()?;
        curr_pos += DELTA;

        let sender = (&bytes[curr_pos..][..SENDER])
            .read_u32::<NetworkEndian>()
            .ok()?;

        Some(FinalizationMessageHeader {
            session_id,
            finalization_index,
            delta,
            sender,
        })
    }
}

type Val = BlockHash;

#[derive(Debug)]
// FIXME: this one needs a better name
enum WmvbaMessage {
    Proposal(Val),
    Vote(Option<Val>),
    AbbaInput(AbbaInput),
    CssSeen(CssSeen),
    CssDoneReporting(CssDoneReporting),
    AreWeDone(bool),
    WitnessCreator(Val),
}

impl WmvbaMessage {
    pub fn deserialize(bytes: &[u8]) -> Option<Self> {
        let mut curr_pos = 0;

        let message_type = (&bytes[curr_pos..][..WMVBA_TYPE])
            .read_u8()
            .ok()?;
        curr_pos += WMVBA_TYPE;

        match message_type {
            0 => {
                let val = HashBytes::new(&bytes[curr_pos..][..VAL]);

                Some(WmvbaMessage::Proposal(val))
            },
            1 => Some(WmvbaMessage::Vote(None)),
            2 => {
                let val = HashBytes::new(&bytes[curr_pos..][..VAL]);

                Some(WmvbaMessage::Vote(Some(val)))
            },
            3 => Some(WmvbaMessage::AbbaInput(AbbaInput::deserialize(&bytes[curr_pos..], false)?)),
            4 => Some(WmvbaMessage::AbbaInput(AbbaInput::deserialize(&bytes[curr_pos..], true)?)),
            5 => Some(WmvbaMessage::CssSeen(CssSeen::deserialize(&bytes[curr_pos..], false)?)),
            6 => Some(WmvbaMessage::CssSeen(CssSeen::deserialize(&bytes[curr_pos..], true)?)),
            7 => Some(WmvbaMessage::CssDoneReporting(CssDoneReporting::deserialize(&bytes[curr_pos..])?)),
            8 => Some(WmvbaMessage::AreWeDone(false)),
            10 => Some(WmvbaMessage::AreWeDone(true)),
            11 => {
                let val = HashBytes::new(&bytes[curr_pos..][..VAL]);

                Some(WmvbaMessage::WitnessCreator(val))
            },
            n => panic!("FIXME: WMVBA message No {} is not specified!", n),
        }
    }

    pub fn serialize(&self) -> Vec<u8> {
        unimplemented!()
    }
}

type Phase = u32;

#[derive(Debug)]
struct AbbaInput {
    phase: Phase,
    ticket: Encoded,
    justified: bool, // FIXME: verify that this is what True/False means here
}

impl AbbaInput {
    pub fn deserialize(bytes: &[u8], justified: bool) -> Option<Self> {
        let mut curr_pos = 0;

        let phase = (&bytes[curr_pos..][..PHASE])
            .read_u32::<NetworkEndian>()
            .ok()?;
        curr_pos += PHASE;

        let ticket = Encoded::new(&bytes[curr_pos..][..TICKET]);

        Some(AbbaInput {
            phase,
            ticket,
            justified,
        })
    }
}

#[derive(Debug)]
struct CssSeen {
    phase: Phase,
    party: Party,
    saw: bool,
}

impl CssSeen {
    pub fn deserialize(bytes: &[u8], saw: bool) -> Option<Self> {
        let mut curr_pos = 0;

        let phase = (&bytes[curr_pos..][..PHASE])
            .read_u32::<NetworkEndian>()
            .ok()?;
        curr_pos += PHASE;

        let party = (&bytes[curr_pos..][..PARTY])
            .read_u32::<NetworkEndian>()
            .ok()?;

        Some(CssSeen {
            phase,
            party,
            saw,
        })
    }
}

#[derive(Debug)]
struct CssDoneReporting {
    phase: Phase,
    rest: Encoded, // FIXME
    /*
    nominatedFalseCount: u64,
    nominatedFalse: u64, // FIXME: it's actually u64 * 4, which seems excessive
    nominatedTrueCount: u64,
    nominatedTrue: u64, // FIXME: it's actually u64 * 4, which seems excessive
    */
}

impl CssDoneReporting {
    pub fn deserialize(bytes: &[u8]) -> Option<Self> {
        let mut curr_pos = 0;

        let phase = (&bytes[curr_pos..][..PHASE])
            .read_u32::<NetworkEndian>()
            .ok()?;
        curr_pos += PHASE;

        let rest = Encoded::new(&bytes[curr_pos..]);

        Some(CssDoneReporting {
            phase,
            rest,
        })
    }
}
