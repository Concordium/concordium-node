use crate::{
    common::{P2PNodeId, P2PPeer},
    network::{
        make_header, NetworkId, PROTOCOL_MESSAGE_TYPE_REQUEST_BANNODE,
        PROTOCOL_MESSAGE_TYPE_REQUEST_FINDNODE, PROTOCOL_MESSAGE_TYPE_REQUEST_GET_PEERS,
        PROTOCOL_MESSAGE_TYPE_REQUEST_HANDSHAKE, PROTOCOL_MESSAGE_TYPE_REQUEST_JOINNETWORK,
        PROTOCOL_MESSAGE_TYPE_REQUEST_LEAVENETWORK, PROTOCOL_MESSAGE_TYPE_REQUEST_PING,
        PROTOCOL_MESSAGE_TYPE_REQUEST_UNBANNODE,
    },
};
use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum NetworkRequest {
    Ping(P2PPeer),
    FindNode(P2PPeer, P2PNodeId),
    BanNode(P2PPeer, P2PPeer),
    Handshake(P2PPeer, HashSet<NetworkId>, Vec<u8>),
    GetPeers(P2PPeer, HashSet<NetworkId>),
    UnbanNode(P2PPeer, P2PPeer),
    JoinNetwork(P2PPeer, NetworkId),
    LeaveNetwork(P2PPeer, NetworkId),
}

impl NetworkRequest {
    pub fn serialize(&self) -> Vec<u8> {
        match self {
            NetworkRequest::Ping(_) => {
                format!("{}{}", make_header(), PROTOCOL_MESSAGE_TYPE_REQUEST_PING).into_bytes()
            }
            NetworkRequest::JoinNetwork(_, network) => format!(
                "{}{}{}",
                make_header(),
                PROTOCOL_MESSAGE_TYPE_REQUEST_JOINNETWORK,
                network
            )
            .into_bytes(),
            NetworkRequest::LeaveNetwork(_, network) => format!(
                "{}{}{}",
                make_header(),
                PROTOCOL_MESSAGE_TYPE_REQUEST_LEAVENETWORK,
                network
            )
            .into_bytes(),
            NetworkRequest::FindNode(_, id) => format!(
                "{}{}{}",
                make_header(),
                PROTOCOL_MESSAGE_TYPE_REQUEST_FINDNODE,
                id
            )
            .into_bytes(),
            NetworkRequest::BanNode(_, node_data) => format!(
                "{}{}{}",
                make_header(),
                PROTOCOL_MESSAGE_TYPE_REQUEST_BANNODE,
                node_data.serialize()
            )
            .into_bytes(),
            NetworkRequest::UnbanNode(_, node_data) => format!(
                "{}{}{}",
                make_header(),
                PROTOCOL_MESSAGE_TYPE_REQUEST_UNBANNODE,
                node_data.serialize()
            )
            .into_bytes(),
            NetworkRequest::Handshake(me, networks, zk) => {
                let id = me.id();
                let mut pkt = format!(
                    "{}{}{}{:05}{:05}{}{:010}",
                    make_header(),
                    PROTOCOL_MESSAGE_TYPE_REQUEST_HANDSHAKE,
                    id,
                    me.port(),
                    networks.len(),
                    networks
                        .iter()
                        .map(|net| net.to_string())
                        .collect::<String>(),
                    zk.len()
                )
                .into_bytes();
                pkt.extend_from_slice(zk.as_slice());
                pkt
            }
            NetworkRequest::GetPeers(_, networks) => format!(
                "{}{}{:05}{}",
                make_header(),
                PROTOCOL_MESSAGE_TYPE_REQUEST_GET_PEERS,
                networks.len(),
                networks
                    .iter()
                    .map(|net| net.to_string())
                    .collect::<String>()
            )
            .into_bytes(),
        }
    }
}
