use std::net::IpAddr;

#[derive(Debug)]
pub struct PeerStatistic {
    pub id:               String,
    pub ip:               IpAddr,
    pub port:             u16,
    pub sent:             u64,
    pub received:         u64,
    pub measured_latency: Option<u64>,
}

impl PeerStatistic {
    pub fn new(
        id: String,
        ip: IpAddr,
        port: u16,
        sent: u64,
        received: u64,
        measured_latency: Option<u64>,
    ) -> PeerStatistic {
        PeerStatistic {
            id,
            ip,
            port,
            sent,
            received,
            measured_latency,
        }
    }

    pub fn id(&self) -> String { self.id.clone() }

    pub fn sent(&self) -> u64 { self.sent }

    pub fn received(&self) -> u64 { self.received }

    pub fn measured_latency(&self) -> Option<u64> { self.measured_latency.clone() }

    pub fn ip(&self) -> IpAddr { self.ip.clone() }

    pub fn port(&self) -> u16 { self.port }
}
