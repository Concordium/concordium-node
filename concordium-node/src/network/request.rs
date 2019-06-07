use crate::{
    common::{
        serialization::{Deserializable, ReadArchive, Serializable, WriteArchive},
        P2PNodeId, P2PPeer,
    },
    network::{AsProtocolRequestType, NetworkId, ProtocolRequestType},
    p2p::banned_nodes::BannedNode,
};
use failure::Fallible;
use std::{collections::HashSet, convert::TryFrom};

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
    Ping(P2PPeer),
    FindNode(P2PPeer, P2PNodeId),
    BanNode(P2PPeer, BannedNode),
    Handshake(P2PPeer, HashSet<NetworkId>, Vec<u8>),
    GetPeers(P2PPeer, HashSet<NetworkId>),
    UnbanNode(P2PPeer, BannedNode),
    JoinNetwork(P2PPeer, NetworkId),
    LeaveNetwork(P2PPeer, NetworkId),
    Retransmit(P2PPeer, RequestedElementType, RequestedSince, NetworkId),
}

impl AsProtocolRequestType for NetworkRequest {
    fn protocol_request_type(&self) -> ProtocolRequestType {
        match self {
            NetworkRequest::Ping(..) => ProtocolRequestType::Ping,
            NetworkRequest::FindNode(..) => ProtocolRequestType::FindNode,
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

impl Serializable for NetworkRequest {
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        (self.protocol_request_type() as u8).serialize(archive)?;
        match self {
            NetworkRequest::Ping(..) => Ok(()),
            NetworkRequest::FindNode(.., id) => id.serialize(archive),
            NetworkRequest::JoinNetwork(.., network)
            | NetworkRequest::LeaveNetwork(.., network) => network.serialize(archive),
            NetworkRequest::BanNode(.., node_data) | NetworkRequest::UnbanNode(.., node_data) => {
                node_data.serialize(archive)
            }
            NetworkRequest::GetPeers(.., ref networks) => networks.serialize(archive),
            NetworkRequest::Handshake(ref me, ref networks, ref zk) => {
                me.serialize(archive)?;
                networks.serialize(archive)?;
                zk.serialize(archive)
            }
            NetworkRequest::Retransmit(_, element_type, since_stamp, network_id) => {
                (*element_type as u8).serialize(archive)?;
                (*since_stamp).serialize(archive)?;
                network_id.serialize(archive)
            }
        }
    }
}

impl Deserializable for NetworkRequest {
    fn deserialize<A>(archive: &mut A) -> Fallible<NetworkRequest>
    where
        A: ReadArchive, {
        let protocol_type = ProtocolRequestType::try_from(u8::deserialize(archive)?)?;
        let remote_peer = archive.post_handshake_peer();
        let request = match protocol_type {
            ProtocolRequestType::Ping => NetworkRequest::Ping(remote_peer?),
            ProtocolRequestType::FindNode => {
                NetworkRequest::FindNode(remote_peer?, P2PNodeId::deserialize(archive)?)
            }
            ProtocolRequestType::BanNode => {
                NetworkRequest::BanNode(remote_peer?, BannedNode::deserialize(archive)?)
            }
            ProtocolRequestType::UnbanNode => {
                NetworkRequest::UnbanNode(remote_peer?, BannedNode::deserialize(archive)?)
            }
            ProtocolRequestType::Handshake => NetworkRequest::Handshake(
                P2PPeer::deserialize(archive)?,
                HashSet::<NetworkId>::deserialize(archive)?,
                Vec::<u8>::deserialize(archive)?,
            ),
            ProtocolRequestType::GetPeers => {
                NetworkRequest::GetPeers(remote_peer?, HashSet::<NetworkId>::deserialize(archive)?)
            }
            ProtocolRequestType::JoinNetwork => {
                NetworkRequest::JoinNetwork(remote_peer?, NetworkId::deserialize(archive)?)
            }
            ProtocolRequestType::LeaveNetwork => {
                NetworkRequest::LeaveNetwork(remote_peer?, NetworkId::deserialize(archive)?)
            }
            ProtocolRequestType::Retransmit => NetworkRequest::Retransmit(
                remote_peer?,
                RequestedElementType::from(u8::deserialize(archive)?),
                u64::deserialize(archive)?,
                NetworkId::deserialize(archive)?,
            ),
        };

        Ok(request)
    }
}
