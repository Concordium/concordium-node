use byteorder::{ByteOrder, NetworkEndian, WriteBytesExt};
use digest::Digest;
use failure::{ensure, format_err, Fallible};

use std::{
    convert::TryFrom,
    fmt,
    io::{Cursor, Read, Write},
    mem::size_of,
    num::NonZeroU64,
    ops::Deref,
};

pub use concordium_common::{HashBytes, SHA256};
pub use ec_vrf_ed25519 as vrf;
pub use ec_vrf_ed25519::{Proof, Sha256, PROOF_LENGTH};
pub use eddsa_ed25519 as sig;

pub const ALLOCATION_LIMIT: usize = 4096;

use crate::block::{BlockHash, BLOCK_HASH};

#[allow(dead_code)]
pub struct ContractAddress {
    index:    u64,
    subindex: u64,
}

pub enum Address {
    Account(Encoded),
    Contract(ContractAddress),
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

#[derive(Debug)]
pub struct AccountAddress(pub [u8; 21]);

#[derive(Debug)]
pub struct Account {
    address:           AccountAddress,
    nonce:             Nonce,
    amount:            Amount,
    encrypted_amounts: Box<[ByteString]>,
    encryption_key:    Option<ByteString>,
    verification_key:  ByteString,
    signature_scheme:  SchemeId,
    credentials:       Box<[Encoded]>,
}

impl<'a, 'b: 'a> SerializeToBytes<'a, 'b> for Account {
    type Source = &'a mut Cursor<&'b [u8]>;

    fn deserialize(cursor: Self::Source) -> Fallible<Self> {
        let address = AccountAddress(read_const_sized!(cursor, size_of::<AccountAddress>()));

        let nonce_raw = NetworkEndian::read_u64(&read_const_sized!(cursor, size_of::<Nonce>()));
        let nonce = Nonce::new(nonce_raw)?;

        let amount = NetworkEndian::read_u64(&read_const_sized!(cursor, size_of::<Amount>()));

        let encrypted_amounts = read_multiple!(cursor, "encrypted amounts", read_bytestring(cursor, "encrypted amount's length")?);

        let has_encryption_key = read_const_sized!(cursor, 1)[0] == 1;
        let encryption_key = if has_encryption_key {
            Some(read_bytestring(cursor, "encrypted key's length")?)
        } else {
            None
        };

        let verification_key = read_bytestring(cursor, "verification key's length")?;

        let signature_scheme = SchemeId::try_from(read_const_sized!(cursor, 1)[0])?;

        let credentials = read_multiple!(cursor, "credentials", read_bytestring(cursor, "encrypted amount's length")?);

        let account = Account {
            address,
            nonce,
            amount,
            encrypted_amounts,
            encryption_key,
            verification_key,
            signature_scheme,
            credentials,
        };

        Ok(account)
    }

    fn serialize(&self) -> Box<[u8]> {
        fn serialized_bs_list_len(bs_list: &[ByteString]) -> usize {
            bs_list.iter().map(|bs| size_of::<u64>() + bs.len()).sum::<usize>()
        }

        let mut cursor = create_serialization_cursor(
            size_of::<AccountAddress>()
                + size_of::<Nonce>()
                + size_of::<Amount>()
                + size_of::<u64>()
                + serialized_bs_list_len(&self.encrypted_amounts)
                + size_of::<u8>()
                + self.encryption_key.iter().next().map(|k| k.len()).unwrap_or(0)
                + size_of::<u64>()
                + self.verification_key.len()
                + size_of::<SchemeId>()
                + size_of::<u64>()
                + serialized_bs_list_len(&self.credentials)
        );

        let _ = cursor.write_all(&self.address.0);
        let _ = cursor.write_u64::<NetworkEndian>(self.nonce.0.get());
        let _ = cursor.write_u64::<NetworkEndian>(self.amount);
        write_multiple!(&mut cursor, self.encrypted_amounts, write_bytestring);
        write_maybe!(&mut cursor, self.encryption_key, write_bytestring);
        write_bytestring(&mut cursor, &self.verification_key);
        let _ = cursor.write(&[self.signature_scheme as u8]);
        write_multiple!(&mut cursor, self.credentials, write_bytestring);

        cursor.into_inner()
    }
}

pub type Amount = u64;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Nonce(NonZeroU64);

impl Nonce {
    fn new(raw: u64) -> Fallible<Self> {
        Ok(Nonce(NonZeroU64::new(raw).ok_or_else(|| format_err!("A zero nonce was received!"))?))
    }
}

pub type Slot = u64;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct SessionId {
    genesis_block: BlockHash,
    incarnation:   u64,
}

impl fmt::Display for SessionId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.incarnation) }
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
        let mut cursor = create_serialization_cursor(BLOCK_HASH as usize + size_of::<u64>());

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
        Encoded(Box::from(bytes))
    }
}

impl Deref for Encoded {
    type Target = [u8];

    fn deref(&self) -> &Self::Target { &self.0 }
}

impl fmt::Debug for Encoded {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} ({}B)", sha256(&self.0), self.0.len())
    }
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
    ensure!(size <= ALLOCATION_LIMIT, "The size of a variable-length object ({}) exceeds the safety limit!", size);
    let mut buf = vec![0u8; size];
    cursor.read_exact(&mut buf)?;

    Ok(buf.into_boxed_slice())
}

pub fn read_bytestring(input: &mut Cursor<&[u8]>, object_name: &str) -> Fallible<ByteString> {
    let object_length = safe_get_len!(input, object_name);

    Ok(Encoded(read_sized!(input, object_length)))
}

pub fn write_bytestring(target: &mut Cursor<Box<[u8]>>, bytes: &[u8]) {
    let _ = target.write_u64::<NetworkEndian>(bytes.len() as u64);
    let _ = target.write_all(&bytes);
}

pub fn sha256(bytes: &[u8]) -> HashBytes { HashBytes::new(&Sha256::digest(bytes)) }

pub trait SerializeToBytes<'a, 'b>
where
    Self: Sized, {
    type Source; // either a byte slice or a mutable cursor (when total size is unknown)

    fn deserialize(source: Self::Source) -> Fallible<Self>;
    fn serialize(&self) -> Box<[u8]>;
}
