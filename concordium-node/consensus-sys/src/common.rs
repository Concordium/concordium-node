use std::hash::Hash;
use std::num::NonZeroU64;

pub use ec_vrf_ed25519::{Proof, Sha256, PROOF_LENGTH};
pub use ec_vrf_ed25519 as vrf;
pub use eddsa_ed25519 as sig;

pub const SHA256: usize = 32;

pub struct Hashed<T: Hash> {
    unhashed: T,
    hashed: Sha256,
}

pub struct ContractAddress {
    index: u64,
    subindex: u64,
}

pub enum Address {
    Account(AccountAddress),
    Contract(ContractAddress),
}

pub type Amount = u64;

pub struct Nonce(NonZeroU64);

pub type Slot = u64;

pub type EncodedPayload = Box<[u8]>;

// a type used for objects we only need to store, but not handle
pub type Encoded = Box<[u8]>;

// temporary type placeholders
pub type AccountAddress = usize;
