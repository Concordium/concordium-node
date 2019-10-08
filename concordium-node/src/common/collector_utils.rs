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
    pub bestArrivedTime: &'a Option<String>,
    pub blockArrivePeriodEMA: Option<f64>,
    pub blockArrivePeriodEMSD: Option<f64>,
    pub finalizedBlock: &'a str,
    pub finalizedBlockHeight: f64,
    pub finalizedTime: &'a Option<String>,
    pub finalizationPeriodEMA: Option<f64>,
    pub finalizationPeriodEMSD: Option<f64>,
    pub packetsSent: f64,
    pub packetsReceived: f64,
    pub consensusRunning: bool,
    pub bakingCommitteeMember: bool,
    pub finalizationCommitteeMember: bool,
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
            bestArrivedTime: &other.bestArrivedTime,
            blockArrivePeriodEMA: other.blockArrivePeriodEMA,
            blockArrivePeriodEMSD: other.blockArrivePeriodEMSD,
            finalizedBlock: &other.finalizedBlock,
            finalizedBlockHeight: other.finalizedBlockHeight,
            finalizedTime: &other.finalizedTime,
            finalizationPeriodEMA: other.finalizationPeriodEMA,
            finalizationPeriodEMSD: other.finalizationPeriodEMSD,
            packetsSent: other.packetsSent,
            packetsReceived: other.packetsReceived,
            consensusRunning: other.consensusRunning,
            bakingCommitteeMember: other.bakingCommitteeMember,
            finalizationCommitteeMember: other.finalizationCommitteeMember,
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
    pub ancestorsSinceBestBlock: &'a Option<Vec<String>>,
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
            ancestorsSinceBestBlock: &other.ancestorsSinceBestBlock,
        }
    }
}
