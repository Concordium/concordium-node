use crate::{
    common::{
        serialization::{Deserializable, ReadArchive, Serializable, WriteArchive},
        P2PPeer,
    },
    network::{AsProtocolResponseType, NetworkId, ProtocolResponseType},
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

impl Serializable for NetworkResponse {
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        (self.protocol_response_type() as u8).serialize(archive)?;
        match self {
            NetworkResponse::Pong(..) => Ok(()),
            NetworkResponse::FindNode(.., ref peers) | NetworkResponse::PeerList(.., ref peers) => {
                peers.serialize(archive)
            }
            NetworkResponse::Handshake(me, networks, zk) => {
                me.serialize(archive)?;
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
        let remote_peer = archive.post_handshake_peer();
        let protocol_type = ProtocolResponseType::try_from(u8::deserialize(archive)?)?;
        let response = match protocol_type {
            ProtocolResponseType::Pong => NetworkResponse::Pong(remote_peer?),
            ProtocolResponseType::FindNode => {
                NetworkResponse::FindNode(remote_peer?, Vec::<P2PPeer>::deserialize(archive)?)
            }
            ProtocolResponseType::PeersList => {
                NetworkResponse::PeerList(remote_peer?, Vec::<P2PPeer>::deserialize(archive)?)
            }
            ProtocolResponseType::Handshake => NetworkResponse::Handshake(
                P2PPeer::deserialize(archive)?,
                HashSet::<NetworkId>::deserialize(archive)?,
                Vec::<u8>::deserialize(archive)?,
            ),
        };
        Ok(response)
    }
}
