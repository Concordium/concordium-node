use crate::consensus_ffi::helpers::HashBytes;
use anyhow::anyhow;
use base58check::ToBase58Check;
use byteorder::{ReadBytesExt, WriteBytesExt};
use crypto_common::{Buffer, Deserial, Serial};
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
    fn serial<W: Buffer + WriteBytesExt>(&self, target: &mut W) {
        self.index.serial(target);
        self.subindex.serial(target);
    }
}

impl Deserial for ContractAddress {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> anyhow::Result<Self> {
        let index = ContractIndex::deserial(source)?;
        let subindex = ContractSubIndex::deserial(source)?;

        let contract_address = ContractAddress {
            index,
            subindex,
        };

        Ok(contract_address)
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
    pub fn verify_key_length(self) -> u32 {
        match self {
            SchemeId::Ed25519 => 32,
            SchemeId::PlaceHolder => unreachable!(),
        }
    }
}

impl TryFrom<u8> for SchemeId {
    type Error = anyhow::Error;

    fn try_from(id: u8) -> anyhow::Result<Self> {
        match id {
            0 => Ok(SchemeId::Ed25519),
            _ => Err(anyhow!("Unsupported SchemeId ({})!", id)),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct AccountAddress(pub [u8; 32]);

impl AccountAddress {
    pub fn new(bytes: &[u8]) -> Self {
        let mut buf = [0u8; size_of::<AccountAddress>()];
        buf.copy_from_slice(bytes);
        AccountAddress(buf)
    }
}

impl fmt::Debug for AccountAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", &self.0.to_base58check(1))
    }
}

impl fmt::Display for AccountAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{:?}", self) }
}

impl Serial for AccountAddress {
    fn serial<W: WriteBytesExt>(&self, target: &mut W) {
        target.write_all(&self.0).expect("Writing to buffer is safe.");
    }
}

impl Deserial for AccountAddress {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> anyhow::Result<Self> {
        let mut buf = [0; 32];
        source.read_exact(&mut buf)?;
        Ok(AccountAddress(buf))
    }
}

pub fn create_serialization_cursor(size: usize) -> Cursor<Box<[u8]>> {
    let buf = vec![0; size];

    Cursor::new(buf.into_boxed_slice())
}

#[cfg(test)]
mod tests {
    use crate::consensus_ffi::blockchain_types::AccountAddress;
    #[test]
    fn check_encoding_of_address() {
        let expected_result = &"2xBvQb4QFBzCDcRdyuGzPDcWSMvDDisfMUnXeRnNJFdWqBBmK7";
        let bytes = (1..=32).collect::<Vec<_>>();
        assert_eq!(expected_result, &AccountAddress::new(&bytes).to_string());
    }
}
