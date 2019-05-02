use byteorder::{NetworkEndian, ReadBytesExt};

use std::{hash::Hash, num::NonZeroU64};

pub use ec_vrf_ed25519 as vrf;
pub use ec_vrf_ed25519::{Proof, Sha256, PROOF_LENGTH};
pub use eddsa_ed25519 as sig;

use crate::block::BlockHash;

pub const SHA256: usize = 32;
pub const INCARNATION: usize = 8;
pub const SESSION_ID: usize = SHA256 + INCARNATION;

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
    incarnation: u64,
}

impl SessionId {
    pub fn deserialize(bytes: &[u8]) -> Option<Self> {
        let mut curr_pos = 0;

        let mut genesis_block_bytes = [0u8; SHA256];
        genesis_block_bytes.copy_from_slice(&bytes[curr_pos..][..SHA256]);
        let genesis_block = Box::new(genesis_block_bytes);
        curr_pos += SHA256;

        let incarnation = (&bytes[curr_pos..][..INCARNATION])
            .read_u64::<NetworkEndian>()
            .ok()?;

        Some(SessionId { genesis_block, incarnation })
    }
}

// a type used for objects we only need to store, but not handle
pub type Encoded = Box<[u8]>;

// temporary type placeholders
pub type AccountAddress = usize;
pub type FinalizationRecord = usize;
