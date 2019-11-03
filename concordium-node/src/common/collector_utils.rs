use serde::{Deserialize, Serialize};

#[allow(non_snake_case)]
#[derive(Serialize, Deserialize)]
pub struct NodeInfo {
    pub nodeName: String,
    pub nodeId: String,
    pub peerType: String,
    pub uptime: f64,
    pub client: String,
    pub averagePing: Option<f64>,
    pub peersCount: f64,
    pub peersList: Vec<String>,
    pub bestBlock: String,
    pub bestBlockHeight: f64,
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
    pub finalizedBlockHeight: f64,
    pub finalizedTime: Option<String>,
    pub finalizationPeriodEMA: Option<f64>,
    pub finalizationPeriodEMSD: Option<f64>,
    pub packetsSent: f64,
    pub packetsReceived: f64,
    pub consensusRunning: bool,
    pub bakingCommitteeMember: bool,
    pub finalizationCommitteeMember: bool,
    pub ancestorsSinceBestBlock: Option<Vec<String>>,
    pub betaUsername: Option<String>,
    pub transactionsPerBlockEMA: Option<f64>,
    pub transactionsPerBlockEMSD: Option<f64>,
    pub bestBlockTransactionsSize: Option<f64>,
    pub bestBlockTotalEncryptedAmount: Option<f64>,
    pub bestBlockTotalAmount: Option<f64>,
    pub bestBlockTransactionCount: Option<f64>,
    pub bestBlockTransactionEnergyCost: Option<f64>,
    pub bestBlockExecutionCost: Option<f64>,
    pub bestBlockCentralBankAmount: Option<f64>,
    pub blocksReceivedCount: Option<f64>,
    pub blocksVerifiedCount: Option<f64>,
    pub genesisBlock: String,
    pub finalizationCount: Option<f64>,
    #[serde(skip)]
    pub last_updated: u64,
}

#[allow(non_snake_case)]
#[derive(Serialize)]
pub struct NodeInfoDashboard<'a> {
    pub nodeName: &'a str,
    pub nodeId: &'a str,
    pub peerType: &'a str,
    pub uptime: f64,
    pub client: &'a str,
    pub averagePing: Option<f64>,
    pub peersCount: f64,
    pub peersList: &'a [String],
    pub bestBlock: &'a str,
    pub bestBlockHeight: f64,
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
    pub finalizedBlockHeight: f64,
    pub finalizedTime: Option<&'a str>,
    pub finalizationPeriodEMA: Option<f64>,
    pub finalizationPeriodEMSD: Option<f64>,
    pub packetsSent: f64,
    pub packetsReceived: f64,
    pub consensusRunning: bool,
    pub bakingCommitteeMember: bool,
    pub finalizationCommitteeMember: bool,
    pub transactionsPerBlockEMA: Option<f64>,
    pub transactionsPerBlockEMSD: Option<f64>,
    pub bestBlockTransactionsSize: Option<f64>,
    pub bestBlockTotalEncryptedAmount: Option<f64>,
    pub bestBlockTotalAmount: Option<f64>,
    pub bestBlockTransactionCount: Option<f64>,
    pub bestBlockTransactionEnergyCost: Option<f64>,
    pub bestBlockExecutionCost: Option<f64>,
    pub bestBlockCentralBankAmount: Option<f64>,
    pub blocksReceivedCount: Option<f64>,
    pub blocksVerifiedCount: Option<f64>,
    pub genesisBlock: &'a str,
    pub finalizationCount: Option<f64>,
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
            bestArrivedTime: other.bestArrivedTime.as_ref().map(|s| s.as_str()),
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
            finalizedTime: other.finalizedTime.as_ref().map(|s| s.as_str()),
            finalizationPeriodEMA: other.finalizationPeriodEMA,
            finalizationPeriodEMSD: other.finalizationPeriodEMSD,
            packetsSent: other.packetsSent,
            packetsReceived: other.packetsReceived,
            consensusRunning: other.consensusRunning,
            bakingCommitteeMember: other.bakingCommitteeMember,
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
        }
    }
}

#[allow(non_snake_case)]
#[derive(Serialize)]
pub struct NodeInfoChainViz<'a> {
    pub nodeName:                &'a str,
    pub nodeId:                  &'a str,
    pub bestBlock:               &'a str,
    pub bestBlockHeight:         f64,
    pub finalizedBlock:          &'a str,
    pub finalizedBlockHeight:    f64,
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
            ancestorsSinceBestBlock: other.ancestorsSinceBestBlock.as_ref().map(|s| s.as_slice()),
        }
    }
}

#[allow(non_snake_case)]
#[derive(Serialize)]
pub struct NodeInfoBetaUsers<'a> {
    pub nodeName:     &'a str,
    pub nodeId:       &'a str,
    pub betaUsername: Option<&'a str>,
}

impl<'a> From<&'a NodeInfo> for NodeInfoBetaUsers<'a> {
    fn from(other: &'a NodeInfo) -> Self {
        Self {
            nodeName:     &other.nodeName,
            nodeId:       &other.nodeId,
            betaUsername: other.betaUsername.as_ref().map(|s| s.as_str()),
        }
    }
}
