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
}