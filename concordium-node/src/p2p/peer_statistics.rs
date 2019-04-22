use crate::common::PeerType;
use std::net::SocketAddr;

#[derive(Debug)]
pub struct PeerStatistic {
    pub id:               String,
    pub addr:             SocketAddr,
    pub peer_type:        PeerType,
    pub sent:             u64,
    pub received:         u64,
    pub measured_latency: Option<u64>,
}

impl PeerStatistic {
    pub fn new(
        id: String,
        addr: SocketAddr,
        peer_type: PeerType,
        sent: u64,
        received: u64,
        measured_latency: Option<u64>,
    ) -> PeerStatistic {
        PeerStatistic {
            id,
            addr,
            peer_type,
            sent,
            received,
            measured_latency,
        }
    }
}
