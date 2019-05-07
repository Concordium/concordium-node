use byteorder::{ByteOrder, NetworkEndian, ReadBytesExt, WriteBytesExt};
use failure::Fallible;

use std::{fmt, hash::Hash, io::{Cursor, Read, Write}, num::NonZeroU64, ops::Deref};

pub use ec_vrf_ed25519 as vrf;
pub use ec_vrf_ed25519::{Proof, Sha256, PROOF_LENGTH};
pub use eddsa_ed25519 as sig;

pub const SHA256: usize = 32;
pub const INCARNATION: usize = 8;
pub const SESSION_ID: usize = SHA256 + INCARNATION;

use crate::block::{BlockHash, BLOCK_HASH};

#[derive(Clone)]
pub struct HashBytes(Box<[u8]>);

impl HashBytes {
    pub fn new(bytes: &[u8]) -> Self { HashBytes(Box::from(bytes)) }
}

impl Deref for HashBytes {
    type Target = [u8];

    fn deref(&self) -> &Self::Target { &self.0 }
}

impl fmt::Debug for HashBytes {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:0x}", (&*self.0).read_u64::<NetworkEndian>().unwrap(),)
    }
}

pub struct Hashed<T: Hash> {
    unhashed: T,
    hashed:   Sha256,
}

pub struct ContractAddress {
    index:    u64,
    subindex: u64,
}

pub enum Address {
    Account(AccountAddress),
    Contract(ContractAddress),
}

pub type Amount = u64;

pub struct Nonce(NonZeroU64);

pub type Slot = u64;

#[derive(Debug)]
pub struct SessionId {
    genesis_block: BlockHash,
    incarnation:   u64,
}

impl SessionId {
    pub fn deserialize(bytes: &[u8]) -> Fallible<Self> {
        debug_deserialization!("SessionId", bytes);

        let mut cursor = Cursor::new(bytes);

        let genesis_block = HashBytes::new(&read_const_sized!(&mut cursor, BLOCK_HASH));
        let incarnation = NetworkEndian::read_u64(&read_const_sized!(&mut cursor, INCARNATION));

        let sess = SessionId {
            genesis_block,
            incarnation,
        };

        check_serialization!(sess, bytes);

        Ok(sess)
    }

    pub fn serialize(&self) -> Vec<u8> {
        debug_serialization!(self);

        let mut cursor = create_serialization_cursor(BLOCK_HASH + INCARNATION);

        let _ = cursor.write_all(&self.genesis_block);
        let _ = cursor.write_u64::<NetworkEndian>(self.incarnation);

        cursor.into_inner().to_vec()
    }
}

// a type used for objects we only need to store, but not handle
#[derive(Clone)]
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

pub fn create_serialization_cursor(size: usize) -> Cursor<Box<[u8]>> {
    let mut buf = Vec::with_capacity(size);
    buf.resize(size, 0);

    Cursor::new(buf.into_boxed_slice())
}

// temporary type placeholders
pub type AccountAddress = usize;
