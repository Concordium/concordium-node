use crate::{
    common::P2PNodeId,
    network::{AsProtocolRequestType, NetworkId, ProtocolRequestType},
    p2p::banned_nodes::BannedNode,
};

use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum RequestedElementType {
    Transaction,
    Unknown,
}

impl From<u8> for RequestedElementType {
    fn from(elem: u8) -> Self {
        match elem {
            0 => RequestedElementType::Transaction,
            _ => RequestedElementType::Unknown,
        }
    }
}

pub type RequestedSince = u64;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum NetworkRequest {
    Ping,
    GetPeers(HashSet<NetworkId>),
    Handshake(P2PNodeId, u16, HashSet<NetworkId>, Vec<u8>),
    BanNode(BannedNode),
    UnbanNode(BannedNode),
    JoinNetwork(NetworkId),
    LeaveNetwork(NetworkId),
    Retransmit(RequestedElementType, RequestedSince, NetworkId),
}

impl AsProtocolRequestType for NetworkRequest {
    fn protocol_request_type(&self) -> ProtocolRequestType {
        match self {
            NetworkRequest::Ping => ProtocolRequestType::Ping,
            NetworkRequest::BanNode(..) => ProtocolRequestType::BanNode,
            NetworkRequest::Handshake(..) => ProtocolRequestType::Handshake,
            NetworkRequest::GetPeers(..) => ProtocolRequestType::GetPeers,
            NetworkRequest::UnbanNode(..) => ProtocolRequestType::UnbanNode,
            NetworkRequest::JoinNetwork(..) => ProtocolRequestType::JoinNetwork,
            NetworkRequest::LeaveNetwork(..) => ProtocolRequestType::LeaveNetwork,
            NetworkRequest::Retransmit(..) => ProtocolRequestType::Retransmit,
        }
    }
}
