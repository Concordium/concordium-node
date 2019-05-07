use byteorder::{ByteOrder, NetworkEndian};
use failure::Fallible;

use std::io::{Cursor, Read, Write};

use crate::{block::*, common::*};

const HEADER: usize = 60;
const INDEX: usize = 8;
const DELTA: usize = 8;
const SENDER: usize = 4;
const SIGNATURE: usize = 64 + 8; // FIXME: unknown 8B prefix
const WMVBA_TYPE: usize = 1;
const VAL: usize = BLOCK_HASH;
const PHASE: usize = 4;
const TICKET: usize = 80;
const ABBA_INPUT: usize = PHASE + TICKET;
const PARTY: usize = 4;
const DELAY: usize = BLOCK_HEIGHT;
const SIGNATURE_COUNT: usize = 8;

#[derive(Debug)]
pub struct FinalizationMessage {
    header:    FinalizationMessageHeader,
    message:   WmvbaMessage,
    signature: Encoded,
}

impl FinalizationMessage {
    pub fn deserialize(bytes: &[u8]) -> Fallible<Self> {
        debug_deserialization!("FinalizationMessage", bytes);

        let mut cursor = Cursor::new(bytes);

        let header = FinalizationMessageHeader::deserialize(&read_const_sized!(&mut cursor, HEADER))?;
        let message_size = bytes.len() - cursor.position() as usize - SIGNATURE;
        let message = WmvbaMessage::deserialize(&read_sized!(&mut cursor, message_size))?;
        let signature = Encoded::new(&read_const_sized!(&mut cursor, SIGNATURE));

        let msg = FinalizationMessage {
            header,
            message,
            signature,
        };

        check_serialization!(msg, bytes);

        Ok(msg)
    }

    pub fn serialize(&self) -> Vec<u8> {
        debug_serialization!(self);

        [
            self.header.serialize().as_slice(),
            self.message.serialize().as_slice(),
            self.signature.as_ref(),
        ]
        .concat()
    }
}

type FinalizationIndex = u64;

type Party = u32;

#[derive(Debug)]
struct FinalizationMessageHeader {
    session_id: SessionId,
    index:      FinalizationIndex,
    delta:      BlockHeight,
    sender:     Party,
}

impl FinalizationMessageHeader {
    pub fn deserialize(bytes: &[u8]) -> Fallible<Self> {
        // debug_deserialization!("FinalizationMessageHeader", bytes);

        let mut cursor = Cursor::new(bytes);

        let session_id = SessionId::deserialize(&read_const_sized!(&mut cursor, SESSION_ID))?;
        let index = NetworkEndian::read_u64(&read_const_sized!(&mut cursor, INDEX));
        let delta = NetworkEndian::read_u64(&read_const_sized!(&mut cursor, DELTA));
        let sender = NetworkEndian::read_u32(&read_const_sized!(&mut cursor, SENDER));

        let header = FinalizationMessageHeader {
            session_id,
            index,
            delta,
            sender,
        };

        check_serialization!(header, bytes);

        Ok(header)
    }

    pub fn serialize(&self) -> Vec<u8> {
        // debug_serialization!(self);

        let mut bytes = [0u8; SESSION_ID + INDEX + DELTA + SENDER];
        let mut curr_pos = 0;

        let _ = (&mut bytes[curr_pos..][..SESSION_ID]).write(&self.session_id.serialize());
        curr_pos += SESSION_ID;

        NetworkEndian::write_u64(
            &mut bytes[curr_pos..][..INDEX],
            self.index,
        );
        curr_pos += INDEX;

        NetworkEndian::write_u64(&mut bytes[curr_pos..][..DELTA], self.delta);
        curr_pos += DELTA;

        NetworkEndian::write_u32(&mut bytes[curr_pos..][..SENDER], self.sender);

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
    pub fn deserialize(bytes: &[u8]) -> Fallible<Self> {
        debug_deserialization!("WmvbaMessage", bytes);

        let mut cursor = Cursor::new(bytes);

        let message_type = &read_const_sized!(&mut cursor, WMVBA_TYPE)[0];

        let msg = match message_type {
            0 => WmvbaMessage::Proposal(HashBytes::new(&read_const_sized!(&mut cursor, VAL))),
            1 => WmvbaMessage::Vote(None),
            2 => WmvbaMessage::Vote(Some(HashBytes::new(&read_const_sized!(&mut cursor, VAL)))),
            3 => WmvbaMessage::AbbaInput(AbbaInput::deserialize(&read_all!(&mut cursor), false)?),
            4 => WmvbaMessage::AbbaInput(AbbaInput::deserialize(&read_all!(&mut cursor), true)?),
            5 => WmvbaMessage::CssSeen(CssSeen::deserialize(&read_all!(&mut cursor), false)?),
            6 => WmvbaMessage::CssSeen(CssSeen::deserialize(&read_all!(&mut cursor), true)?),
            7 => WmvbaMessage::CssDoneReporting(CssDoneReporting::deserialize(&read_all!(&mut cursor))?),
            8 => WmvbaMessage::AreWeDone(false),
            9 => WmvbaMessage::AreWeDone(true),
            10 => WmvbaMessage::WitnessCreator(HashBytes::new(&read_const_sized!(&mut cursor, VAL))),
            n => panic!(
                "Deserialization of WMVBA message type No {} is not implemented!",
                n
            ),
        };

        check_serialization!(msg, bytes);

        Ok(msg)
    }

    pub fn serialize(&self) -> Vec<u8> {
        debug_serialization!(self);

        match self {
            WmvbaMessage::Proposal(ref val) => [&[0], val.as_ref()].concat(),
            WmvbaMessage::Vote(vote) => match vote {
                None => vec![1],
                Some(val) => [&[2], val.as_ref()].concat(),
            },
            WmvbaMessage::AbbaInput(abba) => {
                if !abba.justified {
                    [&[3], abba.serialize().as_slice()].concat()
                } else {
                    [&[4], abba.serialize().as_slice()].concat()
                }
            }
            WmvbaMessage::CssSeen(css) => {
                if !css.saw {
                    [&[5], css.serialize().as_slice()].concat()
                } else {
                    [&[6], css.serialize().as_slice()].concat()
                }
            }
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
    phase:     Phase,
    ticket:    Encoded,
    justified: bool, // FIXME: verify that this is what True/False means here
}

impl AbbaInput {
    pub fn deserialize(bytes: &[u8], justified: bool) -> Fallible<Self> {
        debug_deserialization!("AbbaInput", bytes);

        let mut cursor = Cursor::new(bytes);

        let phase = NetworkEndian::read_u32(&read_const_sized!(&mut cursor, PHASE));
        let ticket = Encoded::new(&read_const_sized!(&mut cursor, TICKET));

        let abba = AbbaInput {
            phase,
            ticket,
            justified,
        };

        check_serialization!(abba, bytes);

        Ok(abba)
    }

    pub fn serialize(&self) -> Vec<u8> {
        debug_serialization!(self);

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
    saw:   bool,
}

impl CssSeen {
    pub fn deserialize(bytes: &[u8], saw: bool) -> Fallible<Self> {
        debug_deserialization!("CssSeen", bytes);

        let mut cursor = Cursor::new(bytes);

        let phase = NetworkEndian::read_u32(&read_const_sized!(&mut cursor, PHASE));
        let party = NetworkEndian::read_u32(&read_const_sized!(&mut cursor, PARTY));

        let css = CssSeen { phase, party, saw };

        check_serialization!(css, bytes);

        Ok(css)
    }

    pub fn serialize(&self) -> Vec<u8> {
        debug_serialization!(self);

        let mut bytes = [0u8; PHASE + PARTY];

        NetworkEndian::write_u32(&mut bytes[..PHASE], self.phase);
        NetworkEndian::write_u32(&mut bytes[PHASE..][..PARTY], self.party);

        bytes.to_vec()
    }
}

#[derive(Debug)]
struct CssDoneReporting {
    phase: Phase,
    rest:  Encoded, /* TODO when specs improve
                     * nominatedFalseCount: u64,
                     * nominatedFalse: u64, // FIXME: it's actually u64 * 4, which seems
                     * excessive nominatedTrueCount: u64,
                     * nominatedTrue: u64, // FIXME: it's actually u64 * 4, which seems
                     * excessive
                     */
}

impl CssDoneReporting {
    pub fn deserialize(bytes: &[u8]) -> Fallible<Self> {
        debug_deserialization!("CssDoneReporting", bytes);

        let mut cursor = Cursor::new(bytes);

        let phase = NetworkEndian::read_u32(&read_const_sized!(&mut cursor, PHASE));
        let rest = Encoded::new(&read_all!(&mut cursor));

        let cssr = CssDoneReporting { phase, rest };

        check_serialization!(cssr, bytes);

        Ok(cssr)
    }

    pub fn serialize(&self) -> Vec<u8> {
        debug_serialization!(self);

        let mut phase_bytes = [0u8; PHASE];

        NetworkEndian::write_u32(&mut phase_bytes, self.phase);

        [&phase_bytes, self.rest.as_ref()].concat()
    }
}

#[derive(Debug)]
pub struct FinalizationRecord {
    index:         FinalizationIndex,
    block_pointer: BlockHash,
    proof:         FinalizationProof,
    delay:         BlockHeight,
}

impl FinalizationRecord {
    pub fn deserialize(bytes: &[u8]) -> Fallible<Self> {
        debug_deserialization!("FinalizationRecord", bytes);

        let mut cursor = Cursor::new(bytes);

        let index = NetworkEndian::read_u64(&read_const_sized!(&mut cursor, INDEX));
        let block_pointer = HashBytes::new(&read_const_sized!(&mut cursor, BLOCK_HASH));
        let proof_size = bytes.len() - cursor.position() as usize - DELAY;
        let proof = FinalizationProof::deserialize(&read_sized!(&mut cursor, proof_size))?;
        let delay = NetworkEndian::read_u64(&read_const_sized!(&mut cursor, DELAY));

        let rec = FinalizationRecord {
            index,
            block_pointer,
            proof,
            delay,
        };

        check_serialization!(rec, bytes);

        Ok(rec)
    }

    pub fn serialize(&self) -> Vec<u8> {
        debug_serialization!(self);

        let mut index_bytes = [0u8; INDEX];
        NetworkEndian::write_u64(&mut index_bytes, self.index);

        let mut delay_bytes = [0u8; DELAY];
        NetworkEndian::write_u64(&mut delay_bytes, self.delay);

        [
            &index_bytes,
            self.block_pointer.as_ref(),
            self.proof.serialize().as_slice(),
            &delay_bytes,
        ]
        .concat()
    }
}

#[derive(Debug)]
struct FinalizationProof(Vec<(u32, Encoded)>);

impl FinalizationProof {
    pub fn deserialize(bytes: &[u8]) -> Fallible<Self> {
        debug_deserialization!("FinalizationProof", bytes);

        let mut cursor = Cursor::new(bytes);

        let signature_count = NetworkEndian::read_u64(&read_const_sized!(&mut cursor, SIGNATURE_COUNT));

        let mut signatures = Vec::with_capacity(signature_count as usize);

        for _ in 0..signature_count {
            // FIXME: determine the use and apply a more informative name
            let tbd = NetworkEndian::read_u32(&read_const_sized!(&mut cursor, 4));
            let signature = Encoded::new(&read_const_sized!(&mut cursor, SIGNATURE));

            signatures.push((tbd, signature));
        }

        let proof = FinalizationProof(signatures);

        check_serialization!(proof, bytes);

        Ok(proof)
    }

    pub fn serialize(&self) -> Vec<u8> {
        debug_serialization!(self);

        let mut signature_count_bytes = [0u8; SIGNATURE_COUNT];
        NetworkEndian::write_u64(&mut signature_count_bytes, self.0.len() as u64);

        let mut bytes = Vec::with_capacity(self.0.len() as usize * (4 + SIGNATURE));
        bytes.extend_from_slice(&signature_count_bytes);

        for (tbd, signature) in &self.0 {
            let mut tbd_bytes = [0u8; 4];
            NetworkEndian::write_u32(&mut tbd_bytes, *tbd);

            bytes.extend_from_slice(&tbd_bytes);
            bytes.extend_from_slice(signature);
        }

        bytes
    }
}
