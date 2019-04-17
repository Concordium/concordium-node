use crate::common::PeerType;
use std::net::IpAddr;

#[derive(Debug)]
pub struct PeerStatistic {
    pub id:               String,
    pub ip:               IpAddr,
    pub port:             u16,
    pub peer_type:        PeerType,
    pub sent:             u64,
    pub received:         u64,
    pub measured_latency: Option<u64>,
}

impl PeerStatistic {
    pub fn new(
        id: String,
        ip: IpAddr,
        port: u16,
        peer_type: PeerType,
        sent: u64,
        received: u64,
        measured_latency: Option<u64>,
    ) -> PeerStatistic {
        PeerStatistic {
            id,
            ip,
            port,
            peer_type,
            sent,
            received,
            measured_latency,
        }
    }
}
