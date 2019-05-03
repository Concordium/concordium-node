use byteorder::{ByteOrder, NetworkEndian, ReadBytesExt};

use std::{fmt, hash::Hash, io::Write, num::NonZeroU64, ops::Deref};

pub use ec_vrf_ed25519 as vrf;
pub use ec_vrf_ed25519::{Proof, Sha256, PROOF_LENGTH};
pub use eddsa_ed25519 as sig;

use crate::block::*;

pub const SHA256: usize = 32;
pub const INCARNATION: usize = 8;
pub const SESSION_ID: usize = SHA256 + INCARNATION;

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
    pub fn deserialize(bytes: &[u8]) -> Option<Self> {
        let mut curr_pos = 0;

        let genesis_block = HashBytes::new(&bytes[curr_pos..][..BLOCK_HASH]);
        curr_pos += BLOCK_HASH;

        let incarnation = (&bytes[curr_pos..][..INCARNATION])
            .read_u64::<NetworkEndian>()
            .ok()?;

        Some(SessionId {
            genesis_block,
            incarnation,
        })
    }

    pub fn serialize(&self) -> Vec<u8> {
        let mut bytes = [0u8; BLOCK_HASH + INCARNATION];

        let _ = (&mut bytes[..BLOCK_HASH]).write(&self.genesis_block);

        NetworkEndian::write_u64(&mut bytes[BLOCK_HASH..], self.incarnation);

        bytes.to_vec()
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "<{}B>", self.0.len(),) }
}

// temporary type placeholders
pub type AccountAddress = usize;
