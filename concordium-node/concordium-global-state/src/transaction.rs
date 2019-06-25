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
        let sender_account = AccountAddress(read_const_sized!(cursor, size_of::<AccountAddress>()));

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
            + size_of::<BlockHash>()
            + size_of::<AccountAddress>()
        );

        let _ = cursor.write(&[self.scheme_id as u8]);
        let _ = cursor.write_u64::<NetworkEndian>(self.sender_key.len() as u64);
        let _ = cursor.write_all(&self.sender_key);
        let _ = cursor.write_u64::<NetworkEndian>(self.nonce.0.get());
        let _ = cursor.write_u64::<NetworkEndian>(self.gas_amount);
        let _ = cursor.write_all(&self.finalized_ptr);
        let _ = cursor.write_all(&self.sender_account.0);

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

impl<'a, 'b> SerializeToBytes<'a, 'b> for Transaction {
    type Source = &'a [u8];

    fn deserialize(bytes: &[u8]) -> Fallible<Self> {
        let mut cursor = Cursor::new(bytes);

        let signature = read_bytestring(&mut cursor, "transaction signature")?;
        let header = TransactionHeader::deserialize(&mut cursor)?;
        let payload = read_bytestring(&mut cursor, "transaction payload")?;
        let hash = sha256(&bytes[..cursor.position() as usize]);

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
            + self.payload.len()
        );

        let _ = cursor.write_u64::<NetworkEndian>(self.signature.len() as u64);
        let _ = cursor.write_all(&self.signature);
        let _ = cursor.write_all(&self.payload);

        cursor.into_inner()
    }
}

pub type TransactionCount = u64;

#[derive(Debug)]
pub struct Transactions(Vec<Transaction>);

impl<'a, 'b> SerializeToBytes<'a, 'b> for Transactions {
    type Source = &'a [u8];

    fn deserialize(bytes: &[u8]) -> Fallible<Self> {
        let mut cursor = Cursor::new(bytes);

        let transaction_count = safe_get_len!(&mut cursor, "transaction count");
        let transactions = Transactions(Vec::with_capacity(transaction_count as usize));

        if transaction_count > 0 {
            // FIXME: determine how to read each transaction
        }

        check_serialization!(transactions, cursor);

        Ok(transactions)
    }

    fn serialize(&self) -> Box<[u8]> {
        // FIXME: add an estimated size of all Transactions
        let mut cursor = create_serialization_cursor(size_of::<TransactionCount>());

        let _ = cursor.write_u64::<NetworkEndian>(self.0.len() as u64);
        for transaction in &self.0 {
            let _ = cursor.write_all(&transaction.serialize());
        }

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
    map:        Vec<(Nonce, Vec<Transaction>)>,
    next_nonce: Nonce,
}

#[derive(Debug, Default)]
pub struct TransactionTable {
    map: HashMap<TransactionHash, (Transaction, Slot)>,
    non_finalized_transactions: HashMap<Encoded, AccountNonFinalizedTransactions>,
}

pub type PendingTransactionTable = HashMap<Encoded, (Nonce, Nonce)>;
