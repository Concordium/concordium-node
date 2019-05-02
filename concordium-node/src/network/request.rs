use crate::{
    common::{P2PNodeId, P2PPeer},
    network::{
        serialization::{Deserializable, ReadArchive, Serializable, WriteArchive},
        AsProtocolMessageType, NetworkId, ProtocolMessageType,
    },
    p2p::banned_nodes::BannedNode,
};
use failure::Fallible;
use std::collections::HashSet;

use std::convert::TryFrom;

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
            NetworkRequest::Retransmit(..) => ProtocolMessageType::RequestRetransmit,
        }
    }
}

impl Serializable for NetworkRequest {
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        archive.write_u8(self.protocol_type() as u8)?;
        match self {
            NetworkRequest::Ping(..) => Ok(()),
            NetworkRequest::FindNode(.., id) => id.serialize(archive),
            NetworkRequest::JoinNetwork(.., network)
            | NetworkRequest::LeaveNetwork(.., network) => network.serialize(archive),
            NetworkRequest::BanNode(.., node_data) | NetworkRequest::UnbanNode(.., node_data) => {
                node_data.serialize(archive)
            }
            NetworkRequest::GetPeers(.., ref networks) => networks.serialize(archive),
            NetworkRequest::Handshake(me, ref networks, ref zk) => {
                me.serialize(archive)?;
                networks.serialize(archive)?;
                zk.serialize(archive)
            }
            NetworkRequest::Retransmit(_, since_stamp, network_id) => {
                archive.write_u64(*since_stamp)?;
                network_id.serialize(archive)
            }
        }
    }
}

impl Deserializable for NetworkRequest {
    fn deserialize<A>(archive: &mut A) -> Fallible<NetworkRequest>
    where
        A: ReadArchive, {
        let protocol_type: ProtocolMessageType = ProtocolMessageType::try_from(archive.read_u8()?)?;
        let remote_peer = archive.remote_peer().clone();
        let request = match protocol_type {
            ProtocolMessageType::RequestPing => NetworkRequest::Ping(remote_peer),
            ProtocolMessageType::RequestFindNode => {
                NetworkRequest::FindNode(remote_peer, P2PNodeId::deserialize(archive)?)
            }
            ProtocolMessageType::RequestBanNode => {
                NetworkRequest::BanNode(remote_peer, BannedNode::deserialize(archive)?)
            }
            ProtocolMessageType::RequestUnbanNode => {
                NetworkRequest::UnbanNode(remote_peer, BannedNode::deserialize(archive)?)
            }
            ProtocolMessageType::RequestHandshake => NetworkRequest::Handshake(
                remote_peer,
                HashSet::<NetworkId>::deserialize(archive)?,
                Vec::<u8>::deserialize(archive)?,
            ),
            ProtocolMessageType::RequestGetPeers => {
                NetworkRequest::GetPeers(remote_peer, HashSet::<NetworkId>::deserialize(archive)?)
            }
            ProtocolMessageType::RequestJoinNetwork => {
                NetworkRequest::JoinNetwork(remote_peer, NetworkId::deserialize(archive)?)
            }
            ProtocolMessageType::RequestLeaveNetwork => {
                NetworkRequest::LeaveNetwork(remote_peer, NetworkId::deserialize(archive)?)
            }
            ProtocolMessageType::RequestRetransmit => NetworkRequest::Retransmit(
                remote_peer,
                archive.read_u64()?,
                NetworkId::deserialize(archive)?,
            ),
            _ => bail!("Unsupported Network Request type"),
        };

        Ok(request)
    }
}
