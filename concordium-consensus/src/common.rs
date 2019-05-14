use byteorder::{ByteOrder, NetworkEndian, ReadBytesExt, WriteBytesExt};
use digest::Digest;
use failure::Fallible;

use std::{
    fmt,
    io::{Cursor, Read, Write},
    num::NonZeroU64,
    ops::Deref,
};

pub use ec_vrf_ed25519 as vrf;
pub use ec_vrf_ed25519::{Proof, Sha256, PROOF_LENGTH};
pub use eddsa_ed25519 as sig;

pub const SHA256: u8 = 32;
pub const INCARNATION: u8 = 8;
pub const SESSION_ID: u8 = SHA256 + INCARNATION;

use crate::block::{BlockHash, BLOCK_HASH};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct HashBytes([u8; BLOCK_HASH as usize]);

impl HashBytes {
    pub fn new(bytes: &[u8]) -> Self {
        let mut buf = [0u8; BLOCK_HASH as usize];
        buf.copy_from_slice(bytes);

        HashBytes(buf)
    }
}

impl Deref for HashBytes {
    type Target = [u8];

    fn deref(&self) -> &Self::Target { &self.0 }
}

impl fmt::Debug for HashBytes {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{:08x}",
            (&self.0[..]).read_u32::<NetworkEndian>().unwrap(),
        )
    }
}

#[allow(dead_code)]
pub struct ContractAddress {
    index:    u64,
    subindex: u64,
}

pub enum Address {
    Account(Encoded),
    Contract(ContractAddress),
}

pub type Amount = u64;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Nonce(NonZeroU64);

pub type Slot = u64;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct SessionId {
    genesis_block: BlockHash,
    incarnation:   u64,
}

impl SessionId {
    pub fn deserialize(bytes: &[u8]) -> Fallible<Self> {
        let mut cursor = Cursor::new(bytes);

        let genesis_block = HashBytes::new(&read_const_sized!(&mut cursor, BLOCK_HASH));
        let incarnation = NetworkEndian::read_u64(&read_const_sized!(&mut cursor, 8));

        let sess = SessionId {
            genesis_block,
            incarnation,
        };

        check_serialization!(sess, cursor);

        Ok(sess)
    }

    pub fn serialize(&self) -> Box<[u8]> {
        let mut cursor = create_serialization_cursor(BLOCK_HASH as usize + INCARNATION as usize);

        let _ = cursor.write_all(&self.genesis_block);
        let _ = cursor.write_u64::<NetworkEndian>(self.incarnation);

        cursor.into_inner()
    }
}

// a type used for objects we only need to store, but not handle
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Encoded(Box<[u8]>);

impl Encoded {
    pub fn new(bytes: &[u8]) -> Self {
        let boxed = Box::from(bytes);

        Encoded(boxed)
    }
}

impl Deref for Encoded {
    type Target = [u8];

    fn deref(&self) -> &Self::Target { &self.0 }
}

impl fmt::Debug for Encoded {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "<{}B>", self.0.len()) }
}

// we don't need to handle it in any special way for now, but we might like to
// know that it's prefixed with a u64 length of the rest of it
pub type ByteString = Encoded;

pub fn create_serialization_cursor(size: usize) -> Cursor<Box<[u8]>> {
    let buf = vec![0; size];

    Cursor::new(buf.into_boxed_slice())
}

pub fn read_all(cursor: &mut Cursor<&[u8]>) -> Fallible<Box<[u8]>> {
    let size = cursor.get_ref().len() - cursor.position() as usize;
    let mut buf = vec![0u8; size];
    cursor.read_exact(&mut buf)?;

    Ok(buf.into_boxed_slice())
}

pub fn read_bytestring(input: &mut Cursor<&[u8]>) -> Fallible<Box<[u8]>> {
    let value_size = NetworkEndian::read_u64(&read_const_sized!(input, 8)) as usize;
    let mut buf = Cursor::new(vec![0u8; 8 + value_size]);

    buf.write_u64::<NetworkEndian>(value_size as u64)?;
    buf.write_all(&read_sized!(input, value_size))?;

    Ok(buf.into_inner().into_boxed_slice())
}

pub fn sha256(bytes: &[u8]) -> HashBytes { HashBytes::new(&Sha256::digest(bytes)) }

pub trait SerializeToBytes {
    fn serialize(&self) -> Box<[u8]>;
}
