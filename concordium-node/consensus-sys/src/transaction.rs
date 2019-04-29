// https://gitlab.com/Concordium/consensus/globalstate-mockup/blob/master/globalstate/src/Concordium/GlobalState/Transactions.hs

use byteorder::{NetworkEndian, ReadBytesExt};

use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;

use crate::common::*;
use crate::block::*;

const TRANSACTION_SIZE: usize = 2;
const TRANSACTION_TYPE: usize = 1;

#[derive(Debug)]
pub struct TransactionHeader {
    scheme_id: Encoded,
    sender_key: Encoded,
    nonce: Encoded,
    gas_amount: Amount,
    finalized_pointer: BlockHash,
    //sender_account: AccountAddress,
}

pub type TransactionHash = Sha256;

#[derive(Debug)]
pub struct Transaction {
    transaction_type: TransactionType,
    payload: Encoded,
}

impl Transaction {
    // FIXME: finish
    pub fn deserialize(bytes: &[u8]) -> Option<(Self, usize)> {
        let mut curr_pos = 0;

        let transaction_size = (&bytes[curr_pos..][..TRANSACTION_SIZE]).read_u16::<NetworkEndian>().ok()? as usize;
        curr_pos += TRANSACTION_SIZE;

        let transaction_type = TransactionType::try_from(bytes[curr_pos]).expect("Unknown payload type!");
        curr_pos += TRANSACTION_TYPE;

        let mut payload_bytes = Vec::with_capacity(transaction_size);
        payload_bytes.copy_from_slice(&bytes[curr_pos..][..transaction_size]);
        let payload = payload_bytes.into_boxed_slice();

        Some((Transaction { transaction_type, payload }, TRANSACTION_SIZE + TRANSACTION_TYPE + transaction_size))
    }

    pub fn serialize(&self) -> Vec<u8> {
        unimplemented!()
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
            n => Err(format!("Unsupported TransactionType ({})!", n))
        }
    }
}

pub struct AccountNonFinalizedTransactions {
    map: HashMap<Nonce, HashSet<Transaction>>,
    next_nonce: Nonce,
}

pub struct TransactionTable {
    map: HashMap<TransactionHash, (Transaction, Slot)>,
    non_finalized_transactions: HashMap<AccountAddress, AccountNonFinalizedTransactions>,
}

pub type PendingTransactionTable = HashMap<AccountAddress, (Nonce, Nonce)>;
