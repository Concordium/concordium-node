use crate::{
    common::{P2PNodeId, P2PPeer},
    network::{AsProtocolResponseType, NetworkId, ProtocolResponseType},
};

use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum NetworkResponse {
    Pong,
    PeerList(Vec<P2PPeer>),
    Handshake(P2PNodeId, u16, HashSet<NetworkId>, Vec<u8>),
}

impl AsProtocolResponseType for NetworkResponse {
    fn protocol_response_type(&self) -> ProtocolResponseType {
        match self {
            NetworkResponse::Pong => ProtocolResponseType::Pong,
            NetworkResponse::PeerList(..) => ProtocolResponseType::PeerList,
            NetworkResponse::Handshake(..) => ProtocolResponseType::Handshake,
        }
    }
}
