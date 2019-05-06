use crate::{
    common::{
        serialization::{Deserializable, ReadArchive, Serializable, WriteArchive},
        P2PPeer,
    },
    network::{AsProtocolMessageType, NetworkId, ProtocolMessageType},
};

use failure::Fallible;
use std::{collections::HashSet, convert::TryFrom};

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
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        archive.write_u8(self.protocol_type() as u8)?;
        match self {
            NetworkResponse::Pong(..) => Ok(()),
            NetworkResponse::FindNode(.., ref peers) | NetworkResponse::PeerList(.., ref peers) => {
                peers.serialize(archive)
            }
            NetworkResponse::Handshake(_, networks, zk) => {
                networks.serialize(archive)?;
                zk.serialize(archive)
            }
        }
    }
}

impl Deserializable for NetworkResponse {
    fn deserialize<A>(archive: &mut A) -> Fallible<NetworkResponse>
    where
        A: ReadArchive, {
        let remote_peer = archive.post_handshake_peer()?;
        let protocol_type: ProtocolMessageType = ProtocolMessageType::try_from(archive.read_u8()?)?;
        let response = match protocol_type {
            ProtocolMessageType::ResponsePong => NetworkResponse::Pong(remote_peer),
            ProtocolMessageType::ResponseFindNode => {
                NetworkResponse::FindNode(remote_peer, Vec::<P2PPeer>::deserialize(archive)?)
            }
            ProtocolMessageType::ResponsePeersList => {
                NetworkResponse::PeerList(remote_peer, Vec::<P2PPeer>::deserialize(archive)?)
            }
            ProtocolMessageType::ResponseHandshake => NetworkResponse::Handshake(
                remote_peer,
                HashSet::<NetworkId>::deserialize(archive)?,
                Vec::<u8>::deserialize(archive)?,
            ),
            _ => bail!("Unsupported protocol type for Network response"),
        };
        Ok(response)
    }
}
