use crate::{
    common::P2PPeer,
    network::{NetworkId, ProtocolMessageType},
};
use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum NetworkResponse {
    Pong(P2PPeer),
    FindNode(P2PPeer, Vec<P2PPeer>),
    PeerList(P2PPeer, Vec<P2PPeer>),
    Handshake(P2PPeer, HashSet<NetworkId>, Vec<u8>),
}

impl NetworkResponse {
    pub fn serialize(&self) -> Vec<u8> {
        match self {
            NetworkResponse::Pong(_) => serialize_message!(ProtocolMessageType::ResponsePong, ""),
            NetworkResponse::FindNode(_, peers) => serialize_message!(
                ProtocolMessageType::ResponseFindNode,
                format!(
                    "{:03}{}",
                    peers.len(),
                    peers
                        .iter()
                        .map(|peer| peer.serialize())
                        .collect::<String>()
                )
            ),
            NetworkResponse::PeerList(_, peers) => serialize_message!(
                ProtocolMessageType::ResponsePeersList,
                format!(
                    "{:03}{}",
                    peers.len(),
                    peers
                        .iter()
                        .map(|peer| peer.serialize())
                        .collect::<String>()
                )
            ),
            NetworkResponse::Handshake(me, networks, zk) => {
                let mut pkt = serialize_message!(
                    ProtocolMessageType::ResponseHandshake,
                    format!(
                        "{}{:05}{:05}{}{:010}",
                        me.id(),
                        me.port(),
                        networks.len(),
                        networks
                            .iter()
                            .map(|net| net.to_string())
                            .collect::<String>(),
                        zk.len()
                    )
                );
                pkt.extend_from_slice(zk.as_slice());
                pkt
            }
        }
    }
}
