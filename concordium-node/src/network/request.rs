use crate::{
    common::{P2PNodeId, P2PPeer},
    network::{NetworkId, ProtocolMessageType, AsProtocolMessageType, serialization::{ Serializable, Archive }},
    p2p::banned_nodes::BannedNode,
};
use failure::Fallible;
use std::{collections::HashSet};

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum NetworkRequest {
    Ping(P2PPeer),
    FindNode(P2PPeer, P2PNodeId),
    BanNode(P2PPeer, BannedNode),
    Handshake(P2PPeer, HashSet<NetworkId>, Vec<u8>),
    GetPeers(P2PPeer, HashSet<NetworkId>),
    UnbanNode(P2PPeer, BannedNode),
    JoinNetwork(P2PPeer, NetworkId),
    LeaveNetwork(P2PPeer, NetworkId),
    Retransmit(P2PPeer, u64, NetworkId),
}

impl AsProtocolMessageType for NetworkRequest {
    fn protocol_type(&self) -> ProtocolMessageType {
        match self {
            NetworkRequest::Ping(..) => ProtocolMessageType::RequestPing,
            NetworkRequest::FindNode(..) => ProtocolMessageType::RequestFindNode,
            NetworkRequest::BanNode(..) => ProtocolMessageType::RequestBanNode,
            NetworkRequest::Handshake(..) => ProtocolMessageType::RequestHandshake,
            NetworkRequest::GetPeers(..) => ProtocolMessageType::RequestGetPeers,
            NetworkRequest::UnbanNode(..) => ProtocolMessageType::RequestUnbanNode,
            NetworkRequest::JoinNetwork(..) => ProtocolMessageType::RequestJoinNetwork,
            NetworkRequest::LeaveNetwork(..) => ProtocolMessageType::RequestLeaveNetwork,
            NetworkRequest::Retransmit(..) => ProtocolMessageType::RequestRetransmit
        }
    }
}

impl Serializable for NetworkRequest {
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()> where A: Archive {
        match self {
            NetworkRequest::Ping(..) => Ok(()),
            NetworkRequest::FindNode(.., id) => id.serialize(archive),
            NetworkRequest::JoinNetwork(.., network) |
            NetworkRequest::LeaveNetwork(.., network) => network.serialize( archive),
            NetworkRequest::BanNode(.., node_data) |
            NetworkRequest::UnbanNode(.., node_data) => node_data.serialize( archive),
            NetworkRequest::GetPeers(.., ref networks) => networks.serialize( archive),
            NetworkRequest::Handshake(me, ref networks, ref zk) => {
                me.serialize( archive)?;
                networks.serialize( archive)?;
                zk.serialize( archive)
            }
            NetworkRequest::Retransmit(_, since_stamp, network_id) => {
                archive.write_u64( *since_stamp)?;
                network_id.serialize( archive)
            },
        }
    }

}
