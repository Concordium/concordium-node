use byteorder::{ByteOrder, NetworkEndian, ReadBytesExt};

use std::io::Write;

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
const FINALIZATION_DELAY: usize = BLOCK_HEIGHT;
const SIGNATURE_COUNT: usize = 8;

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
        [
            self.header.serialize().as_slice(),
            self.message.serialize().as_slice(),
            &[0, 0, 0, 0, 0, 0, 0, 64], // FIXME: what is this pad??
            self.signature.as_ref()
        ].concat()
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

        let delta = (&bytes[curr_pos..][..DELTA]).read_u64::<NetworkEndian>().ok()?;
        curr_pos += DELTA;

        let sender = (&bytes[curr_pos..][..SENDER]).read_u32::<NetworkEndian>().ok()?;

        Some(FinalizationMessageHeader {
            session_id,
            finalization_index,
            delta,
            sender,
        })
    }

    pub fn serialize(&self) -> Vec<u8> {
        let mut bytes = [0u8; SESSION_ID + FINALIZATION_INDEX + DELTA + SENDER];
        let mut curr_pos = 0;

        let _ = (&mut bytes[curr_pos..][..SESSION_ID]).write(&self.session_id.serialize());
        curr_pos += SESSION_ID;

        NetworkEndian::write_u64(&mut bytes[curr_pos..][..FINALIZATION_INDEX], self.finalization_index);
        curr_pos += FINALIZATION_INDEX;

        NetworkEndian::write_u64(&mut bytes[curr_pos..][..DELTA], self.delta);
        curr_pos += DELTA;

        NetworkEndian::write_u32(&mut bytes[curr_pos..], self.sender);

        bytes.to_vec()
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

        let message_type = (&bytes[curr_pos..][..WMVBA_TYPE]).read_u8().ok()?;
        curr_pos += WMVBA_TYPE;

        let message = match message_type {
            0 => WmvbaMessage::Proposal(HashBytes::new(&bytes[curr_pos..][..VAL])),
            1 => WmvbaMessage::Vote(None),
            2 => WmvbaMessage::Vote(Some(HashBytes::new(&bytes[curr_pos..][..VAL]))),
            3 => WmvbaMessage::AbbaInput(AbbaInput::deserialize(&bytes[curr_pos..], false)?),
            4 => WmvbaMessage::AbbaInput(AbbaInput::deserialize(&bytes[curr_pos..], true)?),
            5 => WmvbaMessage::CssSeen(CssSeen::deserialize(&bytes[curr_pos..], false)?),
            6 => WmvbaMessage::CssSeen(CssSeen::deserialize(&bytes[curr_pos..], true)?),
            7 => WmvbaMessage::CssDoneReporting(CssDoneReporting::deserialize(&bytes[curr_pos..])?),
            8 => WmvbaMessage::AreWeDone(false),
            9 => WmvbaMessage::AreWeDone(true),
            10 => WmvbaMessage::WitnessCreator(HashBytes::new(&bytes[curr_pos..][..VAL])),
            n => panic!("Deserialization of WMVBA message type No {} is not implemented!", n),
        };

        Some(message)
    }

    pub fn serialize(&self) -> Vec<u8> {
        match self {
            WmvbaMessage::Proposal(ref val) => [&[0], val.as_ref()].concat(),
            WmvbaMessage::Vote(vote) => match vote {
                None => vec![1],
                Some(val) => [&[2], val.as_ref()].concat(),
            },
            WmvbaMessage::AbbaInput(abba) => match abba.justified {
                false => [&[3], abba.serialize().as_slice()].concat(),
                true => [&[4], abba.serialize().as_slice()].concat(),
            },
            WmvbaMessage::CssSeen(css) => match css.saw {
                false => [&[5], css.serialize().as_slice()].concat(),
                true => [&[6], css.serialize().as_slice()].concat(),
            },
            WmvbaMessage::CssDoneReporting(cdr) => [&[7], cdr.serialize().as_slice()].concat(),
            WmvbaMessage::AreWeDone(arewe) => match arewe {
                false => vec![8],
                true => vec![9],
            },
            WmvbaMessage::WitnessCreator(val) => [&[10], val.as_ref()].concat(),
        }
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

        let phase = (&bytes[curr_pos..][..PHASE]).read_u32::<NetworkEndian>().ok()?;
        curr_pos += PHASE;

        let ticket = Encoded::new(&bytes[curr_pos..][..TICKET]);

        Some(AbbaInput {
            phase,
            ticket,
            justified,
        })
    }

    pub fn serialize(&self) -> Vec<u8> {
        let mut bytes = [0u8; PHASE + TICKET];

        NetworkEndian::write_u32(&mut bytes[..PHASE], self.phase);
        let _ = (&mut bytes[PHASE..][..TICKET]).write(&self.ticket);

        bytes.to_vec()
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
        let phase = (&bytes[..PHASE]).read_u32::<NetworkEndian>().ok()?;

        let party = (&bytes[PHASE..][..PARTY]).read_u32::<NetworkEndian>().ok()?;

        Some(CssSeen {
            phase,
            party,
            saw,
        })
    }

    pub fn serialize(&self) -> Vec<u8> {
        let mut bytes = [0u8; PHASE + PARTY];

        NetworkEndian::write_u32(&mut bytes[..PHASE], self.phase);
        NetworkEndian::write_u32(&mut bytes[PHASE..][..PARTY], self.party);

        bytes.to_vec()
    }
}

#[derive(Debug)]
struct CssDoneReporting {
    phase: Phase,
    rest: Encoded, // TODO when specs improve
    /*
    nominatedFalseCount: u64,
    nominatedFalse: u64, // FIXME: it's actually u64 * 4, which seems excessive
    nominatedTrueCount: u64,
    nominatedTrue: u64, // FIXME: it's actually u64 * 4, which seems excessive
    */
}

impl CssDoneReporting {
    pub fn deserialize(bytes: &[u8]) -> Option<Self> {
        let phase = (&bytes[..PHASE]).read_u32::<NetworkEndian>().ok()?;
        let rest = Encoded::new(&bytes[PHASE..]);

        Some(CssDoneReporting { phase, rest })
    }

    pub fn serialize(&self) -> Vec<u8> {
        let mut phase_bytes = [0u8; PHASE];

        NetworkEndian::write_u32(&mut phase_bytes, self.phase);

        [&phase_bytes, self.rest.as_ref()].concat()
    }
}

#[derive(Debug)]
pub struct FinalizationRecord {
    finalization_index: FinalizationIndex,
    block_pointer: BlockHash,
    proof: FinalizationProof,
    delay: BlockHeight,
}

impl FinalizationRecord {
    pub fn deserialize(bytes: &[u8]) -> Option<Self> {
        let mut curr_pos = 0;

        let finalization_index = (&bytes[curr_pos..][..FINALIZATION_INDEX])
            .read_u64::<NetworkEndian>()
            .ok()?;
        curr_pos += FINALIZATION_INDEX;


        let block_pointer = HashBytes::new(&bytes[curr_pos..][..BLOCK_HASH]);
        curr_pos += BLOCK_HASH;

        let proof = FinalizationProof::deserialize(&bytes[curr_pos..bytes.len() - FINALIZATION_DELAY])?;

        let delay = (&bytes[bytes.len() - FINALIZATION_DELAY..])
            .read_u64::<NetworkEndian>()
            .ok()?;

        Some(FinalizationRecord {
            finalization_index,
            block_pointer,
            proof,
            delay,
        })
    }

    pub fn serialize(&self) -> Vec<u8> {
        unimplemented!()
    }
}

#[derive(Debug)]
struct FinalizationProof {
    signature_count: u64,
    signatures: Vec<Encoded>,
}

impl FinalizationProof {
    pub fn deserialize(bytes: &[u8]) -> Option<Self> {
        let mut curr_pos = 0;

        let signature_count = (&bytes[curr_pos..SIGNATURE_COUNT])
            .read_u64::<NetworkEndian>()
            .ok()?;
        curr_pos += SIGNATURE_COUNT;

        let mut signatures = Vec::with_capacity(signature_count as usize);

        for _ in 0..signature_count {
            signatures.push(Encoded::new(&bytes[curr_pos..SIGNATURE]));
            curr_pos += SIGNATURE;
        }

        Some(FinalizationProof {
            signature_count,
            signatures,
        })
    }
}
