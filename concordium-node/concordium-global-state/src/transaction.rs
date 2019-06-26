// https://gitlab.com/Concordium/consensus/globalstate-mockup/blob/master/globalstate/src/Concordium/GlobalState/Transactions.hs

use byteorder::{ByteOrder, NetworkEndian, WriteBytesExt};
use failure::Fallible;

use std::{
    collections::HashMap,
    convert::TryFrom,
    io::{Cursor, Read, Write},
    mem::size_of,
};

use crate::{block::*, common::*};

pub type TransactionHash = HashBytes;

#[derive(Debug)]
pub struct TransactionHeader {
    scheme_id:      SchemeId,
    sender_key:     ByteString,
    nonce:          Nonce,
    gas_amount:     Energy,
    finalized_ptr:  BlockHash,
    sender_account: AccountAddress,
}

impl<'a, 'b: 'a> SerializeToBytes<'a, 'b> for TransactionHeader {
    type Source = &'a mut Cursor<&'b [u8]>;

    fn deserialize(cursor: Self::Source) -> Fallible<Self> {
        let scheme_id = SchemeId::try_from(read_const_sized!(cursor, 1)[0])?;
        let sender_key = read_bytestring(cursor, "sender key's length")?;

        let nonce_raw = NetworkEndian::read_u64(&read_const_sized!(cursor, size_of::<Nonce>()));
        let nonce = Nonce::try_from(nonce_raw)?;

        let gas_amount = NetworkEndian::read_u64(&read_const_sized!(cursor, size_of::<Energy>()));
        let finalized_ptr = HashBytes::from(read_const_sized!(cursor, size_of::<HashBytes>()));
        let sender_account = AccountAddress::from((&*sender_key, scheme_id));

        let transaction_header = TransactionHeader {
            scheme_id,
            sender_key,
            nonce,
            gas_amount,
            finalized_ptr,
            sender_account,
        };

        Ok(transaction_header)
    }

    fn serialize(&self) -> Box<[u8]> {
        let mut cursor = create_serialization_cursor(
            size_of::<SchemeId>()
                + size_of::<u64>()
                + self.sender_key.len()
                + size_of::<Nonce>()
                + size_of::<Energy>()
                + size_of::<BlockHash>(),
        );

        let _ = cursor.write(&[self.scheme_id as u8]);
        let _ = cursor.write_u64::<NetworkEndian>(self.sender_key.len() as u64);
        let _ = cursor.write_all(&self.sender_key);
        let _ = cursor.write_u64::<NetworkEndian>(self.nonce.0.get());
        let _ = cursor.write_u64::<NetworkEndian>(self.gas_amount);
        let _ = cursor.write_all(&self.finalized_ptr);

        cursor.into_inner()
    }
}

#[derive(Debug)]
pub struct Transaction {
    signature: ByteString,
    header:    TransactionHeader,
    payload:   ByteString,
    hash:      TransactionHash,
}

impl<'a, 'b: 'a> SerializeToBytes<'a, 'b> for Transaction {
    type Source = &'a mut Cursor<&'b [u8]>;

    fn deserialize(cursor: Self::Source) -> Fallible<Self> {
        let initial_pos = cursor.position() as usize;
        let signature = read_bytestring(cursor, "transaction signature")?;
        let header = TransactionHeader::deserialize(cursor)?;
        let payload = read_bytestring_short(cursor)?;
        let hash = sha256(&cursor.get_ref()[initial_pos..cursor.position() as usize]);

        let transaction = Transaction {
            signature,
            header,
            payload,
            hash,
        };

        check_serialization!(transaction, cursor);

        Ok(transaction)
    }

    fn serialize(&self) -> Box<[u8]> {
        let header = self.header.serialize();

        let mut cursor = create_serialization_cursor(
            size_of::<u64>()
                + self.signature.len()
                + header.len()
                + size_of::<u32>()
                + self.payload.len(),
        );

        let _ = cursor.write_u64::<NetworkEndian>(self.signature.len() as u64);
        let _ = cursor.write_all(&self.signature);
        let _ = cursor.write_all(&header);
        let _ = cursor.write_u32::<NetworkEndian>(self.payload.len() as u32);
        let _ = cursor.write_all(&self.payload);

        cursor.into_inner()
    }
}

#[derive(Debug)]
pub enum TransactionType {
    DeployLibrary,
    DeploySmartContract,
    ContractCall,
    SimpleTransfer,
    UpgradeAccount,
    UpdateCredentials,
    UpgradeSmartContract,
}

impl TryFrom<u8> for TransactionType {
    type Error = String;

    fn try_from(id: u8) -> Result<Self, Self::Error> {
        match id {
            0 => Ok(TransactionType::DeployLibrary),
            1 => Ok(TransactionType::DeploySmartContract),
            2 => Ok(TransactionType::ContractCall),
            3 => Ok(TransactionType::SimpleTransfer),
            4 => Ok(TransactionType::UpgradeAccount),
            5 => Ok(TransactionType::UpdateCredentials),
            6 => Ok(TransactionType::UpgradeSmartContract),
            n => Err(format!("Unsupported TransactionType ({})!", n)),
        }
    }
}

#[derive(Debug)]
pub struct AccountNonFinalizedTransactions {
    map:        Vec<Vec<Transaction>>, // indexed by Nonce
    next_nonce: Nonce,
}

#[derive(Debug, Default)]
pub struct TransactionTable {
    map: HashMap<TransactionHash, (Transaction, Slot)>,
    non_finalized_transactions: HashMap<AccountAddress, AccountNonFinalizedTransactions>,
}

pub type PendingTransactionTable = HashMap<AccountAddress, (Nonce, Nonce)>;
