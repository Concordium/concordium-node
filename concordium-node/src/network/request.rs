use byteorder::{ReadBytesExt, WriteBytesExt};
use failure::Fallible;

use crate::{
    common::{P2PNodeId, P2PPeer},
    network::{AsProtocolRequestType, NetworkId, ProtocolRequestType},
    p2p::banned_nodes::BannedNode,
};
use concordium_common::Serial;

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
    FindNode(P2PPeer, P2PNodeId), // we no longer need the id - always provide all the nodes
    BanNode(P2PPeer, BannedNode),
    Handshake(P2PNodeId, u16, HashSet<NetworkId>, Vec<u8>),
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

impl Serial for NetworkRequest {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        let protocol_type = ProtocolRequestType::try_from(source.read_u8()?)?;

        let request = match protocol_type {
            ProtocolRequestType::Ping => NetworkRequest::Ping(P2PPeer::deserial(source)?),
            ProtocolRequestType::FindNode => {
                NetworkRequest::FindNode(P2PPeer::deserial(source)?, P2PNodeId::deserial(source)?)
            }
            ProtocolRequestType::BanNode => {
                NetworkRequest::BanNode(P2PPeer::deserial(source)?, BannedNode::deserial(source)?)
            }
            ProtocolRequestType::UnbanNode => {
                NetworkRequest::UnbanNode(P2PPeer::deserial(source)?, BannedNode::deserial(source)?)
            }
            ProtocolRequestType::Handshake => {
                let id = P2PNodeId::deserial(source)?;
                let port = u16::deserial(source)?;
                let nets = HashSet::<NetworkId>::deserial(source)?;
                let vec = Vec::<u8>::deserial(source)?;

                NetworkRequest::Handshake(id, port, nets, vec)
            }
            ProtocolRequestType::GetPeers => NetworkRequest::GetPeers(
                P2PPeer::deserial(source)?,
                HashSet::<NetworkId>::deserial(source)?,
            ),
            ProtocolRequestType::JoinNetwork => NetworkRequest::JoinNetwork(
                P2PPeer::deserial(source)?,
                NetworkId::deserial(source)?,
            ),
            ProtocolRequestType::LeaveNetwork => NetworkRequest::LeaveNetwork(
                P2PPeer::deserial(source)?,
                NetworkId::deserial(source)?,
            ),
            ProtocolRequestType::Retransmit => NetworkRequest::Retransmit(
                P2PPeer::deserial(source)?,
                RequestedElementType::from(source.read_u8()?),
                u64::deserial(source)?,
                NetworkId::deserial(source)?,
            ),
        };

        Ok(request)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        (self.protocol_request_type() as u8).serial(target)?;
        match self {
            NetworkRequest::Ping(peer) => peer.serial(target),
            NetworkRequest::FindNode(peer, id) => {
                peer.serial(target)?;
                id.serial(target)
            }
            NetworkRequest::JoinNetwork(peer, network)
            | NetworkRequest::LeaveNetwork(peer, network) => {
                peer.serial(target)?;
                network.serial(target)
            }
            NetworkRequest::BanNode(peer, node_data)
            | NetworkRequest::UnbanNode(peer, node_data) => {
                peer.serial(target)?;
                node_data.serial(target)
            }
            NetworkRequest::GetPeers(peer, ref networks) => {
                peer.serial(target)?;
                networks.serial(target)
            }
            NetworkRequest::Handshake(ref my_node_id, ref my_port, ref networks, ref zk) => {
                my_node_id.serial(target)?;
                my_port.serial(target)?;
                networks.serial(target)?;
                zk.serial(target)
            }
            NetworkRequest::Retransmit(peer, element_type, since_stamp, network_id) => {
                peer.serial(target)?;
                (*element_type as u8).serial(target)?;
                (*since_stamp).serial(target)?;
                network_id.serial(target)
            }
        }
    }
}
