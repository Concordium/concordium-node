// https://gitlab.com/Concordium/consensus/globalstate-mockup/blob/master/globalstate/src/Concordium/GlobalState/Transactions.hs

use byteorder::{ByteOrder, NetworkEndian, ReadBytesExt};
use failure::Fallible;

use std::{
    collections::{HashMap, HashSet},
    convert::TryFrom,
};

use crate::{block::*, common::*};

const TRANSACTION_SIZE: usize = 2;
const TRANSACTION_TYPE: usize = 1;
const TRANSACTION_COUNT: usize = 8;

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
    pub fn deserialize(bytes: &[u8]) -> Option<(Self, usize)> {
        debug_deserialization!("Transaction", bytes);

        unimplemented!()
    }

    pub fn serialize(&self) -> Vec<u8> {
        debug_serialization!(self);

        vec![] // TODO
    }
}

#[derive(Debug)]
pub struct Transactions(Vec<Transaction>);

impl Transactions {
    pub fn deserialize(bytes: &[u8]) -> Fallible<Self> {
        debug_deserialization!("Transactions", bytes);

        let mut curr_pos = 0;

        let transaction_count =
            (&bytes[curr_pos..][..TRANSACTION_COUNT]).read_u64::<NetworkEndian>()?;
        curr_pos += TRANSACTION_COUNT;

        let mut transactions = Transactions(Vec::with_capacity(transaction_count as usize));

        if transaction_count > 0 {
            while let Some((transaction, size)) = Transaction::deserialize(&bytes[curr_pos..]) {
                transactions.0.push(transaction);
                curr_pos += size;
            }
        }

        check_serialization!(transactions, bytes);

        Ok(transactions)
    }

    pub fn serialize(&self) -> Vec<u8> {
        debug_serialization!(self);

        let mut transaction_count = [0u8; 8];
        NetworkEndian::write_u64(&mut transaction_count, self.0.len() as u64);

        let mut transactions_bytes = Vec::new(); // TODO: estimate capacity

        for transaction in &self.0 {
            transactions_bytes.extend_from_slice(&transaction.serialize());
        }

        [&transaction_count, transactions_bytes.as_slice()].concat()
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

pub struct AccountNonFinalizedTransactions {
    map:        HashMap<Nonce, HashSet<Transaction>>,
    next_nonce: Nonce,
}

pub struct TransactionTable {
    map: HashMap<TransactionHash, (Transaction, Slot)>,
    non_finalized_transactions: HashMap<AccountAddress, AccountNonFinalizedTransactions>,
}

pub type PendingTransactionTable = HashMap<AccountAddress, (Nonce, Nonce)>;
