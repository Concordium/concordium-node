use concordium_common::{
    blockchain_types::{
        AccountAddress, Amount, BakerId, BlockHash, ContractAddress, Slot, TransactionHash,
    },
    into_err, RelayOrStopReceiver, RelayOrStopSenderHelper, RelayOrStopSyncSender,
};
use failure::{format_err, Fallible};

use std::{
    convert::TryFrom,
    sync::{mpsc, Mutex},
};

const TRANSACTION_LOG_QUEUE_DEPTH: usize = 4096;

pub struct TransactionLogQueue {
    pub receiver: Mutex<RelayOrStopReceiver<TransactionLogMessage>>,
    pub sender:   RelayOrStopSyncSender<TransactionLogMessage>,
}

impl Default for TransactionLogQueue {
    fn default() -> Self {
        let (sender, receiver) = mpsc::sync_channel(TRANSACTION_LOG_QUEUE_DEPTH);
        Self {
            receiver: Mutex::new(receiver),
            sender,
        }
    }
}

impl TransactionLogQueue {
    pub fn send_message(&self, message: TransactionLogMessage) -> Fallible<()> {
        into_err!(self.sender.send_msg(message))
    }

    pub fn stop(&self) -> Fallible<()> {
        into_err!(self.sender.send_stop())?;
        Ok(())
    }
}

lazy_static! {
    pub static ref TRANSACTION_LOG_QUEUE: TransactionLogQueue = { TransactionLogQueue::default() };
}

#[derive(Debug)]
pub enum TransactionLogMessage {
    DirectTransfer(
        BlockHash,
        Slot,
        TransactionHash,
        Amount,
        AccountAddress,
        AccountAddress,
    ),
    TransferFromAccountToContract(
        BlockHash,
        Slot,
        TransactionHash,
        Amount,
        AccountAddress,
        ContractAddress,
    ),
    TransferFromContractToAccount(
        BlockHash,
        Slot,
        TransactionHash,
        Amount,
        ContractAddress,
        AccountAddress,
    ),
    ExecutionCost(
        BlockHash,
        Slot,
        TransactionHash,
        Amount,
        AccountAddress,
        BakerId,
    ),
    BlockReward(BlockHash, Slot, Amount, BakerId, AccountAddress),
}

impl std::fmt::Display for TransactionLogMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::DirectTransfer(block_hash, slot, tx_hash, amount, from_account, to_account) => {
                write!(
                    f,
                    "DirectTransfer occured in {}/{}/{} for {} from {} to {}",
                    block_hash, slot, tx_hash, amount, from_account, to_account
                )
            }
            Self::TransferFromAccountToContract(
                block_hash,
                slot,
                tx_hash,
                amount,
                from_account,
                to_contract,
            ) => write!(
                f,
                "TransferFromAccountToContract occured in {}/{}/{} for {} from {} to {}",
                block_hash, slot, tx_hash, amount, from_account, to_contract
            ),
            Self::TransferFromContractToAccount(
                block_hash,
                slot,
                tx_hash,
                amount,
                from_contract,
                to_account,
            ) => write!(
                f,
                "TransferFromContractToAccount occured in {}/{}/{} for {} from {} to {}",
                block_hash, slot, tx_hash, amount, from_contract, to_account
            ),
            Self::ExecutionCost(block_hash, slot, tx_hash, amount, from_account, baker_id) => {
                write!(
                    f,
                    "ExecutionCost occured in {}/{}/{} for {} from {} to {}",
                    block_hash, slot, tx_hash, amount, from_account, baker_id
                )
            }
            Self::BlockReward(block_hash, slot, amount, baker_id, baker_account) => write!(
                f,
                "BlockReward occured in {}/{} for {} to {}/{}",
                block_hash, slot, amount, baker_id, baker_account
            ),
        }
    }
}

pub enum TransferLogType {
    DirectTransfer = 0,
    TransferFromAccountToContract,
    TransferFromContractToAccount,
    ExecutionCost,
    BlockReward,
}

impl TryFrom<u8> for TransferLogType {
    type Error = failure::Error;

    fn try_from(byte: u8) -> Fallible<Self> {
        match byte as u8 {
            0 => Ok(Self::DirectTransfer),
            1 => Ok(Self::TransferFromAccountToContract),
            2 => Ok(Self::TransferFromContractToAccount),
            3 => Ok(Self::ExecutionCost),
            4 => Ok(Self::BlockReward),
            _ => Err(format_err!("Received invalid transfer log type: {}", byte)),
        }
    }
}

impl std::fmt::Display for TransferLogType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::DirectTransfer => write!(f, "DirectTransfer"),
            Self::TransferFromAccountToContract => write!(f, "TransferFromAccountToContract"),
            Self::TransferFromContractToAccount => write!(f, "TransferFromContractToAccount"),
            Self::ExecutionCost => write!(f, "ExecutionCost"),
            Self::BlockReward => write!(f, "BlockReward"),
        }
    }
}
