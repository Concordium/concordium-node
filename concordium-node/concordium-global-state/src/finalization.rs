use byteorder::{ByteOrder, NetworkEndian, WriteBytesExt};
use failure::Fallible;

use std::{
    cmp::Ordering,
    fmt,
    io::{Cursor, Read, Write},
    mem::size_of,
};

use crate::{block::*, common::*};

const HEADER: u8 = size_of::<SessionId>() as u8
    + size_of::<FinalizationIndex>() as u8
    + size_of::<BlockHeight>() as u8
    + size_of::<Party>() as u8;
const WMVBA_TYPE: u8 = 1;
const VAL: u8 = size_of::<BlockHash>() as u8;
const TICKET: u8 = 80;

#[derive(PartialEq, Eq, Hash)]
pub struct FinalizationMessage {
    header:    FinalizationMessageHeader,
    message:   WmvbaMessage,
    signature: ByteString,
}

impl fmt::Debug for FinalizationMessage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "finalization message (sess {}, idx {}, party {}, {})",
            self.header.session_id, self.header.index, self.header.sender, self.message
        )
    }
}

impl<'a, 'b: 'a> SerializeToBytes<'a, 'b> for FinalizationMessage {
    type Source = &'a [u8];

    fn deserialize(bytes: &[u8]) -> Fallible<Self> {
        let mut cursor = Cursor::new(bytes);

        let header =
            FinalizationMessageHeader::deserialize(&read_const_sized!(&mut cursor, HEADER))?;
        let message = WmvbaMessage::deserialize(&mut cursor)?;
        let signature = read_bytestring_short_length(&mut cursor, "finalization signature")?;

        let msg = FinalizationMessage {
            header,
            message,
            signature,
        };

        check_serialization!(msg, cursor);

        Ok(msg)
    }

    fn serialize(&self) -> Box<[u8]> {
        let mut buffer = [0; 2];
        NetworkEndian::write_u16(&mut buffer, self.signature.len() as u16);
        [
            &self.header.serialize(),
            &self.message.serialize(),
            &buffer[..],
            self.signature.as_ref(),
        ]
        .concat()
        .into_boxed_slice()
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

impl<'a, 'b: 'a> SerializeToBytes<'a, 'b> for FinalizationMessageHeader {
    type Source = &'a [u8];

    fn deserialize(bytes: Self::Source) -> Fallible<Self> {
        let mut cursor = Cursor::new(bytes);

        let session_id = SessionId::deserialize(&read_ty!(&mut cursor, SessionId))?;
        let index = NetworkEndian::read_u64(&read_ty!(&mut cursor, FinalizationIndex));
        let delta = NetworkEndian::read_u64(&read_ty!(&mut cursor, Delta));
        let sender = NetworkEndian::read_u32(&read_ty!(&mut cursor, Party));

        let header = FinalizationMessageHeader {
            session_id,
            index,
            delta,
            sender,
        };

        check_serialization!(header, cursor);

        Ok(header)
    }

    fn serialize(&self) -> Box<[u8]> {
        let mut cursor = create_serialization_cursor(
            size_of::<SessionId>()
                + size_of::<FinalizationIndex>()
                + size_of::<BlockHeight>()
                + size_of::<Party>(),
        );

        let _ = cursor.write_all(&self.session_id.serialize());
        let _ = cursor.write_u64::<NetworkEndian>(self.index);
        let _ = cursor.write_u64::<NetworkEndian>(self.delta);
        let _ = cursor.write_u32::<NetworkEndian>(self.sender);

        cursor.into_inner()
    }
}

type Val = BlockHash;

#[derive(Debug, PartialEq, Eq, Hash)]
// Weak Multi-Valued Byzantine Agreement
enum WmvbaMessage {
    Proposal(Val),
    Vote(Option<Val>),
    Abba(Abba),
    Css(Css),
    AreWeDone(bool),
    WitnessCreator(Val),
}

impl fmt::Display for WmvbaMessage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let output = match self {
            WmvbaMessage::Proposal(prop) => format!("Proposal: {:?}", prop),
            WmvbaMessage::Vote(vote) => format!(
                "Vote: {}",
                if let Some(v) = vote {
                    format!("{:?}", v)
                } else {
                    "blank".to_owned()
                }
            ),
            WmvbaMessage::Abba(abba) => format!("ABBA phase {}", abba.phase),
            WmvbaMessage::Css(css) => format!("Css{:?} phase {}", css.variant, css.phase),
            WmvbaMessage::AreWeDone(arewe) => if *arewe {
                "We're done"
            } else {
                "We're not done"
            }
            .to_owned(),
            WmvbaMessage::WitnessCreator(_) => "WitnessCreator".to_owned(),
        };

        write!(f, "{}", output)
    }
}

impl<'a, 'b: 'a> SerializeToBytes<'a, 'b> for WmvbaMessage {
    type Source = &'a mut Cursor<&'b [u8]>;

    fn deserialize(cursor: Self::Source) -> Fallible<Self> {
        use CssVariant::*;
        use NominationTag::*;

        let message_type = &read_const_sized!(cursor, WMVBA_TYPE)[0];

        let msg = match message_type {
            0 => WmvbaMessage::Proposal(HashBytes::from(read_const_sized!(cursor, VAL))),
            1 => WmvbaMessage::Vote(None),
            2 => WmvbaMessage::Vote(Some(HashBytes::from(read_const_sized!(cursor, VAL)))),
            3 => WmvbaMessage::Abba(Abba::deserialize((cursor, false))?),
            4 => WmvbaMessage::Abba(Abba::deserialize((cursor, true))?),
            5 => WmvbaMessage::Css(Css::deserialize((cursor, Seen, Top))?),
            6 => WmvbaMessage::Css(Css::deserialize((cursor, Seen, Bottom))?),
            7 => WmvbaMessage::Css(Css::deserialize((cursor, Seen, Both))?),
            8 => WmvbaMessage::Css(Css::deserialize((cursor, DoneReporting, Top))?),
            9 => WmvbaMessage::Css(Css::deserialize((cursor, DoneReporting, Bottom))?),
            10 => WmvbaMessage::Css(Css::deserialize((cursor, DoneReporting, Both))?),
            11 => WmvbaMessage::AreWeDone(false),
            12 => WmvbaMessage::AreWeDone(true),
            13 => WmvbaMessage::WitnessCreator(HashBytes::from(read_const_sized!(cursor, VAL))),
            n => panic!(
                "Deserialization of WMVBA message type No {} is not implemented!",
                n
            ),
        };

        Ok(msg)
    }

    fn serialize(&self) -> Box<[u8]> {
        let vec = match self {
            WmvbaMessage::Proposal(ref val) => [&[0], val.as_ref()].concat(),
            WmvbaMessage::Vote(vote) => match vote {
                None => vec![1],
                Some(val) => [&[2], val.as_ref()].concat(),
            },
            WmvbaMessage::Abba(abba) => {
                if !abba.justified {
                    [&[3], &*abba.serialize()].concat()
                } else {
                    [&[4], &*abba.serialize()].concat()
                }
            }
            WmvbaMessage::Css(css) => {
                let nt = css.nomination_tag() as u8;
                match css.variant {
                    CssVariant::Seen => [&[nt], &*css.serialize()].concat(),
                    CssVariant::DoneReporting => [&[nt + 3], &*css.serialize()].concat(),
                }
            }
            WmvbaMessage::AreWeDone(arewe) => match arewe {
                false => vec![11],
                true => vec![12],
            },
            WmvbaMessage::WitnessCreator(val) => [&[13], val.as_ref()].concat(),
        };

        vec.into_boxed_slice()
    }
}

type Phase = u32;

#[derive(Debug, PartialEq, Eq, Hash)]
struct Abba {
    phase:     Phase,
    ticket:    Encoded,
    justified: bool, // FIXME: verify that this is what True/False means here
}

impl<'a, 'b: 'a> SerializeToBytes<'a, 'b> for Abba {
    type Source = (&'a mut Cursor<&'b [u8]>, bool);

    fn deserialize((cursor, justified): Self::Source) -> Fallible<Self> {
        let phase = NetworkEndian::read_u32(&read_const_sized!(cursor, 4));
        let ticket = Encoded::new(&read_const_sized!(cursor, TICKET));

        let abba = Abba {
            phase,
            ticket,
            justified,
        };

        Ok(abba)
    }

    fn serialize(&self) -> Box<[u8]> {
        let mut cursor = create_serialization_cursor(size_of::<Phase>() + TICKET as usize);

        let _ = cursor.write_u32::<NetworkEndian>(self.phase);
        let _ = cursor.write_all(&self.ticket);

        cursor.into_inner()
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum CssVariant {
    Seen,
    DoneReporting,
}

#[derive(Debug, PartialEq, Eq, Hash, Default)]
struct BitString(Box<[u32]>);

#[derive(Debug, PartialEq, Eq, Hash)]
struct NominationSet {
    max_party: Party,
    top:       BitString,
    bottom:    BitString,
}

impl<'a, 'b: 'a> SerializeToBytes<'a, 'b> for NominationSet {
    type Source = (&'a mut Cursor<&'b [u8]>, NominationTag);

    fn deserialize((cursor, tag): Self::Source) -> Fallible<Self> {
        fn check_bit(bit: u32, number: u32) -> bool {
            if bit < 32 {
                number & (1 << bit) != 0
            } else {
                panic!("The bit index is too high ({})!", bit);
            }
        }

        fn get_parties(
            cursor: &mut Cursor<&[u8]>,
            bitstring: &mut Vec<u32>,
            party: Party,
            max_party: Party,
        ) -> Fallible<()> {
            if party <= max_party {
                let byte = u32::from(read_const_sized!(cursor, 1)[0]);
                let bgn = ((0u32..8).filter(|&n| check_bit(n, byte)).map(|n| n + party))
                    .collect::<Vec<_>>();
                let bitstring2 = [bgn.as_slice(), bitstring].concat();
                *bitstring = bitstring2;
                get_parties(cursor, bitstring, party + 8, max_party)
            } else {
                bitstring.sort();
                bitstring.dedup();
                Ok(())
            }
        }

        let max_party = NetworkEndian::read_u32(&read_ty!(cursor, Party));

        let bitstr_len = (max_party as usize + 1) / 8;
        let (mut top, mut bottom) = (
            Vec::with_capacity(bitstr_len),
            Vec::with_capacity(bitstr_len),
        );

        if tag == NominationTag::Top || tag == NominationTag::Both {
            get_parties(cursor, &mut top, Party::min_value(), max_party)?;
        }

        if tag == NominationTag::Bottom || tag == NominationTag::Both {
            get_parties(cursor, &mut bottom, Party::min_value(), max_party)?;
        }

        let set = NominationSet {
            max_party,
            top: BitString(top.into_boxed_slice()),
            bottom: BitString(bottom.into_boxed_slice()),
        };

        Ok(set)
    }

    fn serialize(&self) -> Box<[u8]> {
        fn set_bit(bit: u32, number: u32) -> u32 {
            if bit < 32 {
                number | (1 << bit)
            } else {
                panic!("The bit index is too high ({})!", bit);
            }
        }

        fn unpack_party(
            unpacked: &mut Cursor<Box<[u8]>>,
            packed: &[u32],
            party: Party,
            max_party: Party,
        ) {
            fn party_byte(bits: &[u32], party: Party) -> u8 {
                bits.iter().fold(0, |acc, byte| set_bit(byte - party, acc)) as u8
            }

            if party <= max_party {
                let (curr, rest): (Vec<_>, Vec<_>) = packed.iter().partition(|&&b| b < party + 8);
                let _ = unpacked.write(&[party_byte(&curr, party)]);
                unpack_party(unpacked, &rest, party + 8, max_party);
            }
        }

        let bitstr_len = ((f64::from(self.max_party) + 1.0) / 8.0).ceil() as usize;
        let set_size = if !self.top.0.is_empty() && !self.bottom.0.is_empty() {
            2 * bitstr_len
        } else {
            bitstr_len
        };
        let mut cursor = create_serialization_cursor(size_of::<Party>() + set_size);

        let _ = cursor.write_u32::<NetworkEndian>(self.max_party);
        if !self.top.0.is_empty() {
            unpack_party(&mut cursor, &self.top.0, Party::min_value(), self.max_party);
        }
        if !self.bottom.0.is_empty() {
            unpack_party(
                &mut cursor,
                &self.bottom.0,
                Party::min_value(),
                self.max_party,
            );
        }

        cursor.into_inner()
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct Css {
    variant:        CssVariant,
    phase:          Phase,
    nomination_set: NominationSet,
}

#[derive(Debug, PartialEq, Eq)]
enum NominationTag {
    Empty,
    Top    = 5,
    Bottom = 6,
    Both   = 7,
}

impl Css {
    fn nomination_tag(&self) -> NominationTag {
        let set = &self.nomination_set;

        if set.top.0.is_empty() && set.bottom.0.is_empty() {
            NominationTag::Empty
        } else if !set.top.0.is_empty() && set.bottom.0.is_empty() {
            NominationTag::Top
        } else if set.top.0.is_empty() && !set.bottom.0.is_empty() {
            NominationTag::Bottom
        } else {
            NominationTag::Both
        }
    }
}

impl<'a, 'b: 'a> SerializeToBytes<'a, 'b> for Css {
    type Source = (&'a mut Cursor<&'b [u8]>, CssVariant, NominationTag);

    fn deserialize((cursor, variant, tag): Self::Source) -> Fallible<Self> {
        let phase = NetworkEndian::read_u32(&read_ty!(cursor, Phase));
        let nomination_set = NominationSet::deserialize((cursor, tag))?;

        let css = Css {
            variant,
            phase,
            nomination_set,
        };

        Ok(css)
    }

    fn serialize(&self) -> Box<[u8]> {
        let nomination_set = self.nomination_set.serialize();

        let mut cursor = create_serialization_cursor(size_of::<Phase>() + nomination_set.len());

        let _ = cursor.write_u32::<NetworkEndian>(self.phase);
        let _ = cursor.write_all(&nomination_set);

        cursor.into_inner()
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
        // TODO: verify it's the desired content
        // let proof = genesis_block_ptr
        //     .block
        //     .genesis_data()
        //     .finalization_parameters
        //     .iter()
        //     .enumerate()
        //     .map(|(n, info)| (n as u32, info.signature_verify_key.clone()))
        //     .collect::<Vec<_>>()
        //     .into_boxed_slice();
        let proof: Box<[(u32, Encoded)]> = Vec::new().into_boxed_slice();

        Self {
            index: 0,
            block_pointer: genesis_block_ptr.hash.to_owned(),
            proof,
            delay: 0,
        }
    }
}

impl<'a, 'b> SerializeToBytes<'a, 'b> for FinalizationRecord {
    type Source = &'a [u8];

    fn deserialize(bytes: &[u8]) -> Fallible<Self> {
        let mut cursor = Cursor::new(bytes);

        let index = NetworkEndian::read_u64(&read_ty!(&mut cursor, FinalizationIndex));
        let block_pointer = HashBytes::from(read_ty!(&mut cursor, BlockHash));

        let proof = read_multiple!(
            cursor,
            "finalization proof",
            (
                NetworkEndian::read_u32(&read_const_sized!(&mut cursor, 4)),
                read_bytestring_short_length(&mut cursor, "finalization proof signature")?
            ),
            4
        );

        let delay = NetworkEndian::read_u64(&read_ty!(&mut cursor, BlockHeight));

        let rec = FinalizationRecord {
            index,
            block_pointer,
            proof,
            delay,
        };

        check_serialization!(rec, cursor);

        Ok(rec)
    }

    fn serialize(&self) -> Box<[u8]> {
        let proof_len = self
            .proof
            .iter()
            .map(|(_, sig)| size_of::<Party>() + size_of::<u16>() + sig.len())
            .sum::<usize>();

        let mut cursor = create_serialization_cursor(
            size_of::<FinalizationIndex>()
                + self.block_pointer.len()
                + size_of::<u32>()
                + proof_len
                + size_of::<BlockHeight>(),
        );

        let _ = cursor.write_u64::<NetworkEndian>(self.index);
        let _ = cursor.write_all(&self.block_pointer);

        let _ = cursor.write_u32::<NetworkEndian>(self.proof.len() as u32);
        for (party, signature) in &*self.proof {
            let _ = cursor.write_u32::<NetworkEndian>(*party);
            let _ = cursor.write_u16::<NetworkEndian>(signature.len() as u16);
            let _ = cursor.write_all(signature);
        }

        let _ = cursor.write_u64::<NetworkEndian>(self.delay);

        cursor.into_inner()
    }
}
