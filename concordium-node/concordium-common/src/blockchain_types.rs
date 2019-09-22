use crate::{read_ty, HashBytes, SerializeToBytes};
use base58::ToBase58;
use byteorder::{ByteOrder, NetworkEndian, WriteBytesExt};
use digest::Digest;
use failure::{format_err, Fallible};
use sha2::Sha224;
use std::{
    convert::TryFrom,
    fmt,
    io::{Cursor, Read},
    mem::size_of,
};

pub type ContractIndex = u64;
pub type ContractSubIndex = u64;
pub type BlockHash = HashBytes;
pub type BakerId = u64;
pub type Amount = u64;
pub type Slot = u64;
pub type Energy = u64;
pub type TransactionHash = HashBytes;

#[derive(Debug, Clone, Copy)]
pub struct ContractAddress {
    index:    ContractIndex,
    subindex: ContractSubIndex,
}

impl<'a, 'b: 'a> SerializeToBytes<'a, 'b> for ContractAddress {
    type Source = &'a mut Cursor<&'b [u8]>;

    fn deserialize(cursor: Self::Source) -> Fallible<Self> {
        let index = NetworkEndian::read_u64(&read_ty!(cursor, ContractIndex));
        let subindex = NetworkEndian::read_u64(&read_ty!(cursor, ContractSubIndex));

        let contract_address = ContractAddress { index, subindex };

        Ok(contract_address)
    }

    fn serialize(&self) -> Box<[u8]> {
        let mut cursor = create_serialization_cursor(size_of::<ContractAddress>());

        let _ = cursor.write_u64::<NetworkEndian>(self.index);
        let _ = cursor.write_u64::<NetworkEndian>(self.subindex);

        cursor.into_inner()
    }
}

impl std::fmt::Display for ContractAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{},{}>", self.index, self.subindex)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum SchemeId {
    Cl = 0,
    Ed25519,
}

impl TryFrom<u8> for SchemeId {
    type Error = failure::Error;

    fn try_from(id: u8) -> Fallible<Self> {
        match id {
            0 => Ok(SchemeId::Cl),
            1 => Ok(SchemeId::Ed25519),
            _ => Err(format_err!("Unsupported SchemeId ({})!", id)),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct AccountAddress(pub [u8; 21]);

impl AccountAddress {
    pub fn new(bytes: &[u8]) -> Self {
        let mut buf = [0u8; size_of::<AccountAddress>()];
        buf.copy_from_slice(bytes);
        AccountAddress(buf)
    }
}

impl From<(&[u8], SchemeId)> for AccountAddress {
    fn from((verification_key, scheme_id): (&[u8], SchemeId)) -> Self {
        let mut buf = [0u8; size_of::<AccountAddress>()];
        let hash_len = size_of::<AccountAddress>() - 1;

        buf[0] = scheme_id as u8;
        buf[1..].copy_from_slice(&Sha224::digest(verification_key)[0..hash_len]);

        Self(buf)
    }
}

impl fmt::Debug for AccountAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", ToBase58::to_base58(&self.0[..]))
    }
}

impl fmt::Display for AccountAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{:?}", self) }
}

pub fn create_serialization_cursor(size: usize) -> Cursor<Box<[u8]>> {
    let buf = vec![0; size];

    Cursor::new(buf.into_boxed_slice())
}
