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
    #[serde(skip)]
    pub last_updated: u64,
}

#[allow(non_snake_case)]
#[derive(Serialize)]
pub struct NodeInfoDashboard<'a> {
    pub nodeName: &'a String,
    pub nodeId: &'a String,
    pub peerType: &'a String,
    pub uptime: &'a f64,
    pub client: &'a String,
    pub averagePing: &'a Option<f64>,
    pub peersCount: &'a f64,
    pub peersList: &'a Vec<String>,
    pub bestBlock: &'a String,
    pub bestBlockHeight: &'a f64,
    pub bestArrivedTime: &'a Option<String>,
    pub blockArrivePeriodEMA: &'a Option<f64>,
    pub blockArrivePeriodEMSD: &'a Option<f64>,
    pub finalizedBlock: &'a String,
    pub finalizedBlockHeight: &'a f64,
    pub finalizedTime: &'a Option<String>,
    pub finalizationPeriodEMA: &'a Option<f64>,
    pub finalizationPeriodEMSD: &'a Option<f64>,
    pub packetsSent: &'a f64,
    pub packetsReceived: &'a f64,
    pub consensusRunning: &'a bool,
    pub bakingCommitteeMember: &'a bool,
    pub finalizationCommitteeMember: &'a bool,
}

impl<'a> From<&'a NodeInfo> for NodeInfoDashboard<'a> {
    fn from(other: &'a NodeInfo) -> Self {
        Self {
            nodeName: &other.nodeName,
            nodeId: &other.nodeId,
            peerType: &other.peerType,
            uptime: &other.uptime,
            client: &other.client,
            averagePing: &other.averagePing,
            peersCount: &other.peersCount,
            peersList: &other.peersList,
            bestBlock: &other.bestBlock,
            bestBlockHeight: &other.bestBlockHeight,
            bestArrivedTime: &other.bestArrivedTime,
            blockArrivePeriodEMA: &other.blockArrivePeriodEMA,
            blockArrivePeriodEMSD: &other.blockArrivePeriodEMSD,
            finalizedBlock: &other.finalizedBlock,
            finalizedBlockHeight: &other.finalizedBlockHeight,
            finalizedTime: &other.finalizedTime,
            finalizationPeriodEMA: &other.finalizationPeriodEMA,
            finalizationPeriodEMSD: &other.finalizationPeriodEMSD,
            packetsSent: &other.packetsSent,
            packetsReceived: &other.packetsReceived,
            consensusRunning: &other.consensusRunning,
            bakingCommitteeMember: &other.bakingCommitteeMember,
            finalizationCommitteeMember: &other.finalizationCommitteeMember,
        }
    }
}

#[allow(non_snake_case)]
#[derive(Serialize)]
pub struct NodeInfoChainViz<'a> {
    pub nodeName:                &'a String,
    pub nodeId:                  &'a String,
    pub bestBlock:               &'a String,
    pub bestBlockHeight:         &'a f64,
    pub finalizedBlock:          &'a String,
    pub finalizedBlockHeight:    &'a f64,
    pub ancestorsSinceBestBlock: &'a Option<Vec<String>>,
}

impl<'a> From<&'a NodeInfo> for NodeInfoChainViz<'a> {
    fn from(other: &'a NodeInfo) -> Self {
        Self {
            nodeName:                &other.nodeName,
            nodeId:                  &other.nodeId,
            bestBlock:               &other.bestBlock,
            bestBlockHeight:         &other.bestBlockHeight,
            finalizedBlock:          &other.finalizedBlock,
            finalizedBlockHeight:    &other.finalizedBlockHeight,
            ancestorsSinceBestBlock: &other.ancestorsSinceBestBlock,
        }
    }
}
