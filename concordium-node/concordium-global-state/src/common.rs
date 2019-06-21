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
    address: AccountAddress,
    nonce: Nonce,
    amount: Amount,
    encrypted_amounts: Box<[ByteString]>,
    encryption_key: Option<ByteString>,
    verification_key: ByteString,
    signature_scheme: SchemeId,
    credentials: Box<[Encoded]>,
}

impl<'a, 'b: 'a> SerializeToBytes<'a, 'b> for Account {
    type Source = &'a mut Cursor<&'b [u8]>;

    fn deserialize(cursor: Self::Source) -> Fallible<Self> {
        let address = AccountAddress(read_const_sized!(cursor, size_of::<AccountAddress>()));

        let nonce_raw = NetworkEndian::read_u64(&read_const_sized!(cursor, size_of::<Nonce>()));
        ensure!(nonce_raw != 0, "A zero nonce was received!");
        let nonce = Nonce(unsafe { NonZeroU64::new_unchecked(nonce_raw) }); // safe, just checked

        let amount = NetworkEndian::read_u64(&read_const_sized!(cursor, size_of::<Amount>()));

        let n_encrypted_amounts = safe_get_len!(cursor, "encrypted amount count");
        let mut encrypted_amounts = Vec::with_capacity(n_encrypted_amounts as usize);
        for _ in 0..n_encrypted_amounts {
            let amount_len = safe_get_len!(cursor, "encrypted amount's length");
            encrypted_amounts.push(Encoded::new(&read_sized!(cursor, amount_len)));
        }
        let encrypted_amounts = encrypted_amounts.into_boxed_slice();

        let has_encryption_key = read_const_sized!(cursor, 1)[0] == 1;
        let encryption_key = if has_encryption_key {
            let encryption_key_len = safe_get_len!(cursor, "encrypted key's length");
            Some(Encoded::new(&read_sized!(cursor, encryption_key_len)))
        } else {
            None
        };

        let verification_key_len = safe_get_len!(cursor, "verification key's length");
        let verification_key = Encoded::new(&read_sized!(cursor, verification_key_len));

        let signature_scheme = SchemeId::try_from(read_const_sized!(cursor, 1)[0])?;

        let n_credentials = safe_get_len!(cursor, "credential count");
        let mut credentials = Vec::with_capacity(n_credentials as usize);
        for _ in 0..n_credentials {
            let length = safe_get_len!(cursor, "credential length");
            credentials.push(Encoded::new(&read_sized!(cursor, length)));
        }
        let credentials = credentials.into_boxed_slice();

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
        let mut cursor = create_serialization_cursor(
            size_of::<AccountAddress>()
            + size_of::<Nonce>()
            + size_of::<Amount>()
            + size_of::<u64>()
            + self.encrypted_amounts.iter().map(|ea| size_of::<u64>() + ea.len()).sum::<usize>()
            + size_of::<u8>()
            + if let Some(ref key) = self.encryption_key { key.len() } else { 0 }
            + size_of::<u64>()
            + self.verification_key.len()
            + size_of::<SchemeId>()
            + size_of::<u64>()
            + self.credentials.iter().map(|cred| size_of::<u64>() + cred.len()).sum::<usize>()
        );

        let _ = cursor.write_all(&self.address.0);
        let _ = cursor.write_u64::<NetworkEndian>(self.nonce.0.get());
        let _ = cursor.write_u64::<NetworkEndian>(self.amount);

        let _ = cursor.write_u64::<NetworkEndian>(self.encrypted_amounts.len() as u64);
        for ea in &*self.encrypted_amounts {
            let _ = cursor.write_u64::<NetworkEndian>(ea.len() as u64);
            let _ = cursor.write_all(ea);
        }

        if let Some(ref key) = self.encryption_key {
            let _ = cursor.write(&[1]);
            let _ = cursor.write_u64::<NetworkEndian>(key.len() as u64);
            let _ = cursor.write_all(key);
        } else {
            let _ = cursor.write(&[0]);
        };

        let _ = cursor.write_u64::<NetworkEndian>(self.verification_key.len() as u64);
        let _ = cursor.write_all(&self.verification_key);

        let _ = cursor.write(&[self.signature_scheme as u8]);

        let _ = cursor.write_u64::<NetworkEndian>(self.credentials.len() as u64);
        for cred in &*self.credentials {
            let _ = cursor.write_u64::<NetworkEndian>(cred.len() as u64);
            let _ = cursor.write_all(cred);
        }

        cursor.into_inner()
    }
}

pub type Amount = u64;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Nonce(NonZeroU64);

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
        let boxed = Box::from(bytes);

        Encoded(boxed)
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

pub trait SerializeToBytes<'a, 'b>
where
    Self: Sized, {
    type Source; // either a byte slice or a mutable cursor (when total size is unknown)

    fn deserialize(source: Self::Source) -> Fallible<Self>;
    fn serialize(&self) -> Box<[u8]>;
}
