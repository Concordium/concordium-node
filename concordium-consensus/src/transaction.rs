// https://gitlab.com/Concordium/consensus/globalstate-mockup/blob/master/globalstate/src/Concordium/GlobalState/Transactions.hs

use byteorder::{ByteOrder, NetworkEndian, WriteBytesExt};
use failure::Fallible;

use std::{
    collections::{HashMap, HashSet},
    convert::TryFrom,
    io::{Cursor, Read, Write},
    mem::size_of,
};

use crate::{block::*, common::*};

#[derive(Debug)]
pub struct TransactionHeader {
    scheme_id:         Encoded,
    sender_key:        Encoded,
    nonce:             Encoded,
    gas_amount:        Amount,
    finalized_pointer: BlockHash,
    // sender_account: AccountAddress,
}

pub type TransactionHash = Sha256;

#[derive(Debug)]
pub struct Transaction {
    // transaction_type: TransactionType,
    payload: Encoded,
}

impl Transaction {
    // FIXME: finish
    pub fn deserialize(bytes: &[u8]) -> Option<Self> {
        debug_deserialization!("Transaction", bytes);

        unimplemented!()
    }

    pub fn serialize(&self) -> Box<[u8]> {
        debug_serialization!(self);

        vec![].into_boxed_slice() // TODO
    }
}

type TransactionCount = u64;

#[derive(Debug)]
pub struct Transactions(Vec<Transaction>);

impl Transactions {
    pub fn deserialize(bytes: &[u8]) -> Fallible<Self> {
        let mut cursor = Cursor::new(bytes);

        let transaction_count = NetworkEndian::read_u64(&read_const_sized!(&mut cursor, size_of::<TransactionCount>()));

        let mut transactions = Transactions(Vec::with_capacity(transaction_count as usize));

        if transaction_count > 0 {
            // FIXME: determine how to read each transaction
            while let Some(transaction) = Transaction::deserialize(&read_all(&mut cursor)?) {
                transactions.0.push(transaction);
            }
        }

        check_serialization!(transactions, cursor);

        Ok(transactions)
    }

    pub fn serialize(&self) -> Box<[u8]> {
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

#[allow(dead_code)]
pub struct AccountNonFinalizedTransactions {
    map:        Vec<(Nonce, HashSet<Transaction>)>,
    next_nonce: Nonce,
}

#[allow(dead_code)]
pub struct TransactionTable {
    map: HashMap<TransactionHash, (Transaction, Slot)>,
    non_finalized_transactions: HashMap<AccountAddress, AccountNonFinalizedTransactions>,
}

pub type PendingTransactionTable = HashMap<AccountAddress, (Nonce, Nonce)>;
