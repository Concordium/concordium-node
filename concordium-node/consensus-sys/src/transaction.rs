// https://gitlab.com/Concordium/consensus/globalstate-mockup/blob/master/globalstate/src/Concordium/GlobalState/Transactions.hs

use std::collections::{HashMap, HashSet};

use crate::common::*;
use crate::block::*;

const TYPE: usize = 1;
const TIMESTAMP: usize = 8;
const DEDICATED_GAS: usize = 8;
const DEDICATED_GTU: usize = 8;
const PAYLOAD_SIZE: usize = 2;

pub type TransactionSignature = sig::Signature;

pub struct TransactionHeader {
    scheme: SchemeId,
    sender_key: AccountVerificationKey,
    nonce: Nonce,
    gas_amount: Amount,
    finalized_pointer: BlockHash,
    sender_account: AccountAddress,
}

pub type TransactionHash = Sha256;

pub struct Transaction {
    signature: TransactionSignature,
    header: TransactionHeader,
    payload: EncodedPayload,
    hash: TransactionHash,
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
