use base58::ToBase58;
use base58check::ToBase58Check;
use byteorder::{ReadBytesExt, WriteBytesExt};
use digest::Digest;
use failure::{format_err, Fallible};
use sha2::Sha224;

use crate::{
    serial::{NoParam, Serial},
    HashBytes,
};

use std::{convert::TryFrom, fmt, io::Cursor, mem::size_of};

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

impl Serial for ContractAddress {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        let index = ContractIndex::deserial(source)?;
        let subindex = ContractSubIndex::deserial(source)?;

        let contract_address = ContractAddress { index, subindex };

        Ok(contract_address)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        self.index.serial(target)?;
        self.subindex.serial(target)?;
        Ok(())
    }
}

impl std::fmt::Display for ContractAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{},{}>", self.index, self.subindex)
    }
}

// Until we have more than one scheme identifier this will have an unused
// placeholder in it
#[derive(Debug, Clone, Copy)]
pub enum SchemeId {
    Ed25519 = 0,
    PlaceHolder,
}

impl SchemeId {
    pub fn verify_key_length(&self) -> u32 {
        match self {
            SchemeId::Ed25519 => 32,
        }
    }
}

impl TryFrom<u8> for SchemeId {
    type Error = failure::Error;

    fn try_from(id: u8) -> Fallible<Self> {
        match id {
            0 => Ok(SchemeId::Ed25519),
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
        let scheme_id_encoded = &self.0[..1].to_base58();
        let scheme_part = if scheme_id_encoded.len() < 2 {
            format!("1{}", scheme_id_encoded)
        } else {
            scheme_id_encoded.to_owned()
        };
        write!(f, "{}{}", scheme_part, &self.0[1..].to_base58check(1))
    }
}

impl fmt::Display for AccountAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{:?}", self) }
}

pub fn create_serialization_cursor(size: usize) -> Cursor<Box<[u8]>> {
    let buf = vec![0; size];

    Cursor::new(buf.into_boxed_slice())
}

#[cfg(test)]
mod tests {
    use crate::blockchain_types::AccountAddress;
    #[test]
    fn check_encoding_of_address() {
        let expected_result = &"11gXqUxCA425wahmjy9RfFZAJpCMsRXi6BZ";
        let bytes = [
            0, 177, 161, 218, 231, 97, 52, 140, 166, 47, 87, 0, 117, 36, 195, 102, 196, 50, 70,
            223, 23,
        ];
        assert_eq!(expected_result, &AccountAddress::new(&bytes).to_string());
    }
}
