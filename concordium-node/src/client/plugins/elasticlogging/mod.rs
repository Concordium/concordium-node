use concordium_consensus::transferlog::{TransactionLogMessage, TransferLogType};
use elastic::prelude::*;
use serde::{Deserialize, Serialize};

// pub enum TransactionLogMessage {
// DirectTransfer(
// BlockHash,
// Slot,
// TransactionHash,
// Amount,
// AccountAddress,
// AccountAddress,
// ),
// TransferFromAccountToContract(
// BlockHash,
// Slot,
// TransactionHash,
// Amount,
// AccountAddress,
// ContractAddress,
// ),
// TransferFromContractToAccount(
// BlockHash,
// Slot,
// TransactionHash,
// Amount,
// ContractAddress,
// AccountAddress,
// ),
// ExecutionCost(
// BlockHash,
// Slot,
// TransactionHash,
// Amount,
// AccountAddress,
// BakerId,
// ),
// BlockReward(BlockHash, Slot, Amount, BakerId, AccountAddress),
// }

#[derive(ElasticType, Serialize, Deserialize, Debug)]
#[elastic(index = "index_transfer_log")]
struct TransferLogEvent {
    #[elastic(id)]
    pub id: String,
    pub message_type: String,
    pub timestamp: Date<DefaultDateMapping<EpochMillis>>,
    pub block_hash: String,
    pub slot: String,
    pub transaction_hash: Option<String>,
    pub amount: String,
    pub from_account: Option<String>,
    pub to_account: Option<String>,
    pub from_contract: Option<String>,
    pub to_contract: Option<String>,
    pub baker_id: Option<String>,
}

#[allow(unused_variables)]
pub fn log_transfer_event(host: &str, port: u16, msg: TransactionLogMessage) {
    if let Ok(client) = create_client {
        let doc = match msg {
            TransactionLogMessage::DirectTransfer(
                block_hash,
                slot,
                transaction_hash,
                amount,
                from_account,
                to_account,
            ) => TransferLogEvent {},
        };
    } else {
        error!("Can't create synchronous client to Elastic Search");
    }
}

pub fn create_transfer_index(host: &str, port: u16) {
    if let Ok(client) = create_client(host, port) {
        if !client
            .index(TransferLogEvent::static_index())
            .exists()
            .send()?
            .exists()
        {
            match client
                .index(TransferLogEvent::static_index())
                .create()
                .send()
            {
                Ok(_) => info!("Elastic Seach index created"),
                Err(e) => error!("Could not create Elastic Search index"),
            }
        }
    } else {
        error!("Can't create synchronous client to Elastic Search");
    }
}

fn create_client(host: &str, port: u16) {
    SyncClientBuilder::new()
        .static_node(format!("http://{}:{}", host, port).to_string())
        .build()
}
