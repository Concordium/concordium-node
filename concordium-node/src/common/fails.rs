use super::{P2PNodeId, PeerType};
use failure::Fail;
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
#[fail(
    display = "Remote peer already promoted to post-handshake <{}>/<{}>/<{}>",
    id, ip, port
)]
pub struct RemotePeerAlreadyPromoted {
    id:   P2PNodeId,
    ip:   IpAddr,
    port: u16,
}

impl RemotePeerAlreadyPromoted {
    pub fn new(id: P2PNodeId, ip: IpAddr, port: u16) -> Self { Self { id, ip, port } }
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
