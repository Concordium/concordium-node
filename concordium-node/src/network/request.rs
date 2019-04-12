use crate::{
    common::{P2PNodeId, P2PPeer},
    network::{ make_header, ProtocolMessageType },
};
use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum NetworkRequest {
    Ping(P2PPeer),
    FindNode(P2PPeer, P2PNodeId),
    BanNode(P2PPeer, P2PPeer),
    Handshake(P2PPeer, HashSet<u16>, Vec<u8>),
    GetPeers(P2PPeer, HashSet<u16>),
    UnbanNode(P2PPeer, P2PPeer),
    JoinNetwork(P2PPeer, u16),
    LeaveNetwork(P2PPeer, u16),
}

impl NetworkRequest {
    pub fn serialize(&self) -> Vec<u8> {
        match self {
            NetworkRequest::Ping(_) => {
                format!("{}{}", make_header(), ProtocolMessageType::RequestPing).into_bytes()
            }
            NetworkRequest::JoinNetwork(_, nid) => format!(
                "{}{}{:05}",
                make_header(),
                ProtocolMessageType::RequestJoinNetwork,
                nid
            )
            .into_bytes(),
            NetworkRequest::LeaveNetwork(_, nid) => format!(
                "{}{}{:05}",
                make_header(),
                ProtocolMessageType::RequestLeaveNetwork,
                nid
            )
            .into_bytes(),
            NetworkRequest::FindNode(_, id) => format!(
                "{}{}{}",
                make_header(),
                ProtocolMessageType::RequestFindNode,
                id
            )
            .into_bytes(),
            NetworkRequest::BanNode(_, node_data) => format!(
                "{}{}{}",
                make_header(),
                ProtocolMessageType::RequestBannode,
                node_data.serialize()
            )
            .into_bytes(),
            NetworkRequest::UnbanNode(_, node_data) => format!(
                "{}{}{}",
                make_header(),
                ProtocolMessageType::RequestUnbannode,
                node_data.serialize()
            )
            .into_bytes(),
            NetworkRequest::Handshake(me, nids, zk) => {
                let id = me.id();
                let mut pkt = format!(
                    "{}{}{}{:05}{:05}{}{:010}",
                    make_header(),
                    ProtocolMessageType::RequestHandshake,
                    id,
                    me.port(),
                    nids.len(),
                    nids.iter().map(|x| format!("{:05}", x)).collect::<String>(),
                    zk.len()
                )
                .into_bytes();
                pkt.extend_from_slice(zk.as_slice());
                pkt
            }
            NetworkRequest::GetPeers(_, networks) => format!(
                "{}{}{:05}{}",
                make_header(),
                ProtocolMessageType::RequestGetPeers,
                networks.len(),
                networks
                    .iter()
                    .map(|x| format!("{:05}", x))
                    .collect::<String>()
            )
            .into_bytes(),
        }
    }
}
