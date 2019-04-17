use super::{P2PNodeId, PeerType};
use failure::Fail;
use std::net::SocketAddr;

#[derive(Debug, Fail)]
#[fail(display = "Empty IP or Port on P2PPeer building")]
pub struct EmptyIpPortError;

#[derive(Debug, Fail)]
#[fail(
    display = "Missing fields on P2PPeer build: type<{:?}>, id<{:?}>, addr<{:?}>",
    peer_type, id, addr
)]
pub struct MissingFieldsError {
    peer_type: Option<PeerType>,
    id:        Option<P2PNodeId>,
    addr:      Option<SocketAddr>,
}

impl MissingFieldsError {
    pub fn new(
        peer_type: Option<PeerType>,
        id: Option<P2PNodeId>,
        addr: Option<SocketAddr>,
    ) -> MissingFieldsError {
        MissingFieldsError {
            peer_type,
            id,
            addr,
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
    display = "Remote peer already promoted to post-handshake <{}>/<{}>",
    id, addr
)]
pub struct RemotePeerAlreadyPromoted {
    id:   P2PNodeId,
    addr: SocketAddr,
}

impl RemotePeerAlreadyPromoted {
    pub fn new(id: P2PNodeId, addr: SocketAddr) -> Self { Self { id, addr } }
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
