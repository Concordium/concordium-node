use super::{P2PNodeId, PeerType};
use failure::Fail;
use num_bigint::ParseBigIntError;
use std::net::IpAddr;

#[derive(Debug, Fail)]
#[fail(display = "Empty IP or Port on P2PPeer building")]
pub struct EmptyIpPortError;

#[derive(Debug, Fail)]
#[fail(
    display = "Missing fields on P2PPeer build: type<{:?}>, id<{:?}>, ip<{:?}>, port<{:?}>",
    peer_type, id, ip, port
)]
pub struct MissingFieldsError {
    peer_type: Option<PeerType>,
    id:        Option<P2PNodeId>,
    ip:        Option<IpAddr>,
    port:      Option<u16>,
}

impl MissingFieldsError {
    pub fn new(
        peer_type: Option<PeerType>,
        id: Option<P2PNodeId>,
        ip: Option<IpAddr>,
        port: Option<u16>,
    ) -> MissingFieldsError {
        MissingFieldsError {
            peer_type,
            id,
            ip,
            port,
        }
    }
}

#[derive(Debug, Fail)]
#[fail(display = "Invalid length for specified IP type: type<{}>", ip_type)]
pub struct InvalidLengthForIP {
    ip_type: String,
}

impl InvalidLengthForIP {
    pub fn new(ip_type: String) -> InvalidLengthForIP { InvalidLengthForIP { ip_type } }
}

#[derive(Debug, Fail)]
#[fail(display = "Invalid IP type specified: type<{}>", ip_type)]
pub struct InvalidIpType {
    ip_type: String,
}

impl InvalidIpType {
    pub fn new(ip_type: String) -> InvalidIpType { InvalidIpType { ip_type } }
}

#[derive(Debug, Fail)]
#[fail(display = "Invalid length for serialized P2PPeer")]
pub struct InvalidLength;

#[derive(Debug, Fail)]
#[fail(display = "Error while parsing P2PNodeId")]
pub struct P2PNodeIdError;

impl From<ParseBigIntError> for P2PNodeIdError {
    fn from(_: ParseBigIntError) -> Self { P2PNodeIdError }
}
