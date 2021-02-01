use serde::{Deserialize, Serialize};

/// Contains all the node information used by the collector.
#[allow(non_snake_case)]
#[derive(Serialize, Deserialize)]
pub struct NodeInfo {
    pub nodeName: String,
    pub nodeId: String,
    pub peerType: String,
    pub uptime: u64,
    pub client: String,
    pub averagePing: Option<f64>,
    pub peersCount: u64,
    pub peersList: Vec<String>,
    pub bestBlock: String,
    pub bestBlockHeight: u64,
    pub bestBlockBakerId: Option<u64>,
    pub bestArrivedTime: Option<String>,
    pub blockArrivePeriodEMA: Option<f64>,
    pub blockArrivePeriodEMSD: Option<f64>,
    pub blockArriveLatencyEMA: Option<f64>,
    pub blockArriveLatencyEMSD: Option<f64>,
    pub blockReceivePeriodEMA: Option<f64>,
    pub blockReceivePeriodEMSD: Option<f64>,
    pub blockReceiveLatencyEMA: Option<f64>,
    pub blockReceiveLatencyEMSD: Option<f64>,
    pub finalizedBlock: String,
    pub finalizedBlockHeight: u64,
    pub finalizedTime: Option<String>,
    pub finalizationPeriodEMA: Option<f64>,
    pub finalizationPeriodEMSD: Option<f64>,
    pub packetsSent: u64,
    pub packetsReceived: u64,
    pub consensusRunning: bool,
    pub bakingCommitteeMember: i32,
    pub consensusBakerId: Option<u64>,
    pub finalizationCommitteeMember: bool,
    pub ancestorsSinceBestBlock: Option<Vec<String>>,
    pub stagingNetUsername: Option<String>,
    pub transactionsPerBlockEMA: Option<f64>,
    pub transactionsPerBlockEMSD: Option<f64>,
    pub bestBlockTransactionsSize: Option<u64>,
    pub bestBlockTotalEncryptedAmount: Option<u64>,
    pub bestBlockTotalAmount: Option<u64>,
    pub bestBlockTransactionCount: Option<u64>,
    pub bestBlockTransactionEnergyCost: Option<u64>,
    pub bestBlockExecutionCost: Option<u64>,
    pub bestBlockCentralBankAmount: Option<u64>,
    pub blocksReceivedCount: Option<u64>,
    pub blocksVerifiedCount: Option<u64>,
    pub genesisBlock: String,
    pub finalizationCount: Option<u64>,
    pub finalizedBlockParent: String,
    #[serde(skip)]
    pub last_updated: u64,
    pub averageBytesPerSecondIn: u64,
    pub averageBytesPerSecondOut: u64,
}

/// Contains node details available in `nodes_summary`.
#[allow(non_snake_case)]
#[derive(Serialize)]
pub struct NodeInfoDashboard<'a> {
    pub nodeName: &'a str,
    pub nodeId: &'a str,
    pub peerType: &'a str,
    pub uptime: u64,
    pub client: &'a str,
    pub averagePing: Option<f64>,
    pub peersCount: u64,
    pub peersList: &'a [String],
    pub bestBlock: &'a str,
    pub bestBlockHeight: u64,
    pub bestBlockBakerId: Option<u64>,
    pub bestArrivedTime: Option<&'a str>,
    pub blockArrivePeriodEMA: Option<f64>,
    pub blockArrivePeriodEMSD: Option<f64>,
    pub blockArriveLatencyEMA: Option<f64>,
    pub blockArriveLatencyEMSD: Option<f64>,
    pub blockReceivePeriodEMA: Option<f64>,
    pub blockReceivePeriodEMSD: Option<f64>,
    pub blockReceiveLatencyEMA: Option<f64>,
    pub blockReceiveLatencyEMSD: Option<f64>,
    pub finalizedBlock: &'a str,
    pub finalizedBlockHeight: u64,
    pub finalizedTime: Option<&'a str>,
    pub finalizationPeriodEMA: Option<f64>,
    pub finalizationPeriodEMSD: Option<f64>,
    pub packetsSent: u64,
    pub packetsReceived: u64,
    pub consensusRunning: bool,
    pub bakingCommitteeMember: i32,
    pub consensusBakerId: Option<u64>,
    pub finalizationCommitteeMember: bool,
    pub transactionsPerBlockEMA: Option<f64>,
    pub transactionsPerBlockEMSD: Option<f64>,
    pub bestBlockTransactionsSize: Option<u64>,
    pub bestBlockTotalEncryptedAmount: Option<u64>,
    pub bestBlockTotalAmount: Option<u64>,
    pub bestBlockTransactionCount: Option<u64>,
    pub bestBlockTransactionEnergyCost: Option<u64>,
    pub bestBlockExecutionCost: Option<u64>,
    pub bestBlockCentralBankAmount: Option<u64>,
    pub blocksReceivedCount: Option<u64>,
    pub blocksVerifiedCount: Option<u64>,
    pub genesisBlock: &'a str,
    pub finalizationCount: Option<u64>,
    pub finalizedBlockParent: &'a str,
    pub averageBytesPerSecondIn: f64,
    pub averageBytesPerSecondOut: f64,
}

impl<'a> From<&'a NodeInfo> for NodeInfoDashboard<'a> {
    fn from(other: &'a NodeInfo) -> Self {
        Self {
            nodeName: &other.nodeName,
            nodeId: &other.nodeId,
            peerType: &other.peerType,
            uptime: other.uptime,
            client: &other.client,
            averagePing: other.averagePing,
            peersCount: other.peersCount,
            peersList: &other.peersList,
            bestBlock: &other.bestBlock,
            bestBlockHeight: other.bestBlockHeight,
            bestBlockBakerId: other.bestBlockBakerId,
            bestArrivedTime: other.bestArrivedTime.as_deref(),
            blockArrivePeriodEMA: other.blockArrivePeriodEMA,
            blockArrivePeriodEMSD: other.blockArrivePeriodEMSD,
            blockArriveLatencyEMA: other.blockArriveLatencyEMA,
            blockArriveLatencyEMSD: other.blockArriveLatencyEMSD,
            blockReceivePeriodEMA: other.blockReceivePeriodEMA,
            blockReceivePeriodEMSD: other.blockReceivePeriodEMSD,
            blockReceiveLatencyEMA: other.blockReceiveLatencyEMA,
            blockReceiveLatencyEMSD: other.blockReceiveLatencyEMSD,
            finalizedBlock: &other.finalizedBlock,
            finalizedBlockHeight: other.finalizedBlockHeight,
            finalizedTime: other.finalizedTime.as_deref(),
            finalizationPeriodEMA: other.finalizationPeriodEMA,
            finalizationPeriodEMSD: other.finalizationPeriodEMSD,
            packetsSent: other.packetsSent,
            packetsReceived: other.packetsReceived,
            consensusRunning: other.consensusRunning,
            bakingCommitteeMember: other.bakingCommitteeMember,
            consensusBakerId: other.consensusBakerId,
            finalizationCommitteeMember: other.finalizationCommitteeMember,
            transactionsPerBlockEMA: other.transactionsPerBlockEMA,
            transactionsPerBlockEMSD: other.transactionsPerBlockEMSD,
            bestBlockTransactionsSize: other.bestBlockTransactionsSize,
            bestBlockTotalEncryptedAmount: other.bestBlockTotalEncryptedAmount,
            bestBlockTotalAmount: other.bestBlockTotalAmount,
            bestBlockTransactionCount: other.bestBlockTransactionCount,
            bestBlockTransactionEnergyCost: other.bestBlockTransactionEnergyCost,
            bestBlockExecutionCost: other.bestBlockExecutionCost,
            bestBlockCentralBankAmount: other.bestBlockCentralBankAmount,
            blocksReceivedCount: other.blocksReceivedCount,
            blocksVerifiedCount: other.blocksVerifiedCount,
            genesisBlock: &other.genesisBlock,
            finalizationCount: other.finalizationCount,
            finalizedBlockParent: &other.finalizedBlockParent,
            averageBytesPerSecondIn: other.averageBytesPerSecondIn as f64,
            averageBytesPerSecondOut: other.averageBytesPerSecondOut as f64,
        }
    }
}

/// Contains node details available in `nodes_block_info`.
#[allow(non_snake_case)]
#[derive(Serialize)]
pub struct NodeInfoChainViz<'a> {
    pub nodeName:                &'a str,
    pub nodeId:                  &'a str,
    pub bestBlock:               &'a str,
    pub bestBlockHeight:         u64,
    pub finalizedBlock:          &'a str,
    pub finalizedBlockHeight:    u64,
    pub finalizedBlockParent:    &'a str,
    pub ancestorsSinceBestBlock: Option<&'a [String]>,
}

impl<'a> From<&'a NodeInfo> for NodeInfoChainViz<'a> {
    fn from(other: &'a NodeInfo) -> Self {
        Self {
            nodeName:                &other.nodeName,
            nodeId:                  &other.nodeId,
            bestBlock:               &other.bestBlock,
            bestBlockHeight:         other.bestBlockHeight,
            finalizedBlock:          &other.finalizedBlock,
            finalizedBlockHeight:    other.finalizedBlockHeight,
            finalizedBlockParent:    &other.finalizedBlockParent,
            ancestorsSinceBestBlock: other.ancestorsSinceBestBlock.as_deref(),
        }
    }
}

/// Contains node details available in `nodes_staging_net_users_info`.
#[allow(non_snake_case)]
#[derive(Serialize)]
pub struct NodeInfoStagingNetUsers<'a> {
    pub nodeName:           &'a str,
    pub nodeId:             &'a str,
    pub stagingNetUsername: Option<&'a str>,
}

impl<'a> From<&'a NodeInfo> for NodeInfoStagingNetUsers<'a> {
    fn from(other: &'a NodeInfo) -> Self {
        Self {
            nodeName:           &other.nodeName,
            nodeId:             &other.nodeId,
            stagingNetUsername: other.stagingNetUsername.as_deref(),
        }
    }
}
