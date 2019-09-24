use byteorder::{ReadBytesExt, WriteBytesExt};
use failure::Fallible;

use crate::{
    common::{P2PNodeId, P2PPeer},
    network::{AsProtocolResponseType, NetworkId, ProtocolResponseType},
};
use concordium_common::Serial;

use std::{collections::HashSet, convert::TryFrom};

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum NetworkResponse {
    Pong(P2PPeer),
    FindNode(P2PPeer, Vec<P2PPeer>), /* we no longer need this one - we always provide all the
                                      * nodes */
    PeerList(P2PPeer, Vec<P2PPeer>),
    Handshake(P2PNodeId, u16, HashSet<NetworkId>, Vec<u8>),
}

impl AsProtocolResponseType for NetworkResponse {
    fn protocol_response_type(&self) -> ProtocolResponseType {
        match self {
            NetworkResponse::Pong(..) => ProtocolResponseType::Pong,
            NetworkResponse::FindNode(..) => ProtocolResponseType::FindNode,
            NetworkResponse::PeerList(..) => ProtocolResponseType::PeersList,
            NetworkResponse::Handshake(..) => ProtocolResponseType::Handshake,
        }
    }
}

impl Serial for NetworkResponse {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        let protocol_type = ProtocolResponseType::try_from(source.read_u8()?)?;
        let response = match protocol_type {
            ProtocolResponseType::Pong => NetworkResponse::Pong(P2PPeer::deserial(source)?),
            ProtocolResponseType::FindNode => NetworkResponse::FindNode(
                P2PPeer::deserial(source)?,
                Vec::<P2PPeer>::deserial(source)?,
            ),
            ProtocolResponseType::PeersList => NetworkResponse::PeerList(
                P2PPeer::deserial(source)?,
                Vec::<P2PPeer>::deserial(source)?,
            ),
            ProtocolResponseType::Handshake => NetworkResponse::Handshake(
                P2PNodeId::deserial(source)?,
                u16::deserial(source)?,
                HashSet::<NetworkId>::deserial(source)?,
                Vec::<u8>::deserial(source)?,
            ),
        };
        Ok(response)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        (self.protocol_response_type() as u8).serial(target)?;
        match self {
            NetworkResponse::Pong(..) => Ok(()),
            NetworkResponse::FindNode(.., ref peers) | NetworkResponse::PeerList(.., ref peers) => {
                peers.serial(target)
            }
            NetworkResponse::Handshake(my_node_id, my_port, networks, zk) => {
                my_node_id.serial(target)?;
                my_port.serial(target)?;
                networks.serial(target)?;
                zk.serial(target)
            }
        }
    }
}
