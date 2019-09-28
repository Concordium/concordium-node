use serde::{Deserialize, Serialize};

#[allow(non_snake_case)]
#[derive(Serialize, Deserialize)]
pub struct NodeInfo {
    nodeName: String,
    nodeId: String,
    peerType: String,
    uptime: f64,
    client: String,
    averagePing: Option<f64>,
    peersCount: f64,
    peersList: Vec<String>,
    bestBlock: String,
    bestBlockHeight: f64,
    bestArrivedTime: Option<String>,
    blockArrivePeriodEMA: Option<f64>,
    blockArrivePeriodEMSD: Option<f64>,
    finalizedBlock: String,
    finalizedBlockHeight: f64,
    finalizedTime: Option<String>,
    finalizationPeriodEMA: Option<f64>,
    finalizationPeriodEMSD: Option<f64>,
    packetsSent: f64,
    packetsReceived: f64,
    consensusRunning: bool,
    bakingCommitteeMember: bool,
    finalizationCommitteeMember: bool,
}