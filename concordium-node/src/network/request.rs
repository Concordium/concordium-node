use crate::{
    common::{P2PNodeId, P2PPeer},
    network::{NetworkId, ProtocolMessageType},
    p2p::banned_nodes::BannedNode,
};
use std::{collections::HashSet, string::ToString};

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

impl NetworkRequest {
    pub fn serialize(&self) -> Vec<u8> {
        match self {
            NetworkRequest::Ping(_) => serialize_message!(ProtocolMessageType::RequestPing, ""),
            NetworkRequest::JoinNetwork(_, network) => {
                serialize_message!(ProtocolMessageType::RequestJoinNetwork, network)
            }
            NetworkRequest::LeaveNetwork(_, network) => {
                serialize_message!(ProtocolMessageType::RequestLeaveNetwork, network)
            }
            NetworkRequest::FindNode(_, id) => {
                serialize_message!(ProtocolMessageType::RequestFindNode, id)
            }
            NetworkRequest::BanNode(_, node_data) => {
                serialize_message!(ProtocolMessageType::RequestBanNode, node_data.serialize())
            }
            NetworkRequest::UnbanNode(_, node_data) => {
                serialize_message!(ProtocolMessageType::RequestUnbanNode, node_data.serialize())
            }
            NetworkRequest::GetPeers(_, networks) => serialize_message!(
                ProtocolMessageType::RequestGetPeers,
                format!(
                    "{:05}{}",
                    networks.len(),
                    networks.iter().map(ToString::to_string).collect::<String>()
                )
            ),
            NetworkRequest::Handshake(me, networks, zk) => {
                let mut pkt = serialize_message!(
                    ProtocolMessageType::RequestHandshake,
                    format!(
                        "{}{:05}{:05}{}{:010}",
                        me.id(),
                        me.port(),
                        networks.len(),
                        networks.iter().map(ToString::to_string).collect::<String>(),
                        zk.len()
                    )
                );
                pkt.extend_from_slice(zk.as_slice());
                pkt
            }
            NetworkRequest::Retransmit(_, since_stamp, network_id) => serialize_message!(
                ProtocolMessageType::RequestRetransmit,
                format!("{:016x}{}", since_stamp, network_id)
            ),
        }
    }
}
