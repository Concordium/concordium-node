use crate::{
    common::P2PPeer,
    network::{NetworkId, ProtocolMessageType, AsProtocolMessageType, serialization::{ Serializable, Archive }},
};

use failure::Fallible;
use std::{collections::HashSet };

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum NetworkResponse {
    Pong(P2PPeer),
    FindNode(P2PPeer, Vec<P2PPeer>),
    PeerList(P2PPeer, Vec<P2PPeer>),
    Handshake(P2PPeer, HashSet<NetworkId>, Vec<u8>),
}

impl AsProtocolMessageType for NetworkResponse {
    fn protocol_type(&self) -> ProtocolMessageType {
        match self {
            NetworkResponse::Pong(..) => ProtocolMessageType::ResponsePong,
            NetworkResponse::FindNode(..) => ProtocolMessageType::ResponseFindNode,
            NetworkResponse::PeerList(..) => ProtocolMessageType::ResponsePeersList,
            NetworkResponse::Handshake(..) => ProtocolMessageType::ResponseHandshake,
        }
    }
}

impl Serializable for NetworkResponse {
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()> where A: Archive {
        match self {
            NetworkResponse::Pong(..) => Ok(()),
            NetworkResponse::FindNode(.., ref peers) |
            NetworkResponse::PeerList(.., ref peers) => peers.serialize( archive),
            NetworkResponse::Handshake(me, networks, zk) => {
                me.serialize( archive)?;
                networks.serialize( archive)?;
                zk.serialize( archive)
            }
        }
    }
}
