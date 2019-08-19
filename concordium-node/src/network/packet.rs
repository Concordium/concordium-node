use crate::{
    common::{
        serialization::{Deserializable, ReadArchive, Serializable, WriteArchive},
        P2PNodeId, P2PPeer,
    },
    network::{AsProtocolPacketType, NetworkId, ProtocolPacketType},
};
use concordium_common::{HashBytes, UCursor};

use crate::{failure::Fallible, utils};
use rand::RngCore;
use std::convert::TryFrom;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum NetworkPacketType {
    DirectMessage(P2PNodeId),
    BroadcastedMessage(Vec<P2PNodeId>),
}

impl AsProtocolPacketType for NetworkPacketType {
    fn protocol_packet_type(&self) -> ProtocolPacketType {
        match self {
            NetworkPacketType::DirectMessage(..) => ProtocolPacketType::Direct,
            NetworkPacketType::BroadcastedMessage(..) => ProtocolPacketType::Broadcast,
        }
    }
}

impl Serializable for NetworkPacketType {
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        (self.protocol_packet_type() as u8).serialize(archive)?;

        match self {
            NetworkPacketType::DirectMessage(ref receiver) => receiver.serialize(archive),
            NetworkPacketType::BroadcastedMessage(..) => Ok(()),
        }
    }
}

impl Deserializable for NetworkPacketType {
    fn deserialize<A>(archive: &mut A) -> Fallible<NetworkPacketType>
    where
        A: ReadArchive, {
        let protocol_type = ProtocolPacketType::try_from(u8::deserialize(archive)?)?;

        match protocol_type {
            ProtocolPacketType::Direct => Ok(NetworkPacketType::DirectMessage(
                P2PNodeId::deserialize(archive)?,
            )),
            ProtocolPacketType::Broadcast => Ok(NetworkPacketType::BroadcastedMessage(vec![])),
        }
    }
}

pub type MessageId = HashBytes;

/// This is not *thread-safe* but this ensures it temporarily
#[derive(Clone, Builder, Debug, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub struct NetworkPacket {
    pub packet_type: NetworkPacketType,
    pub peer:        P2PPeer,
    pub message_id:  MessageId,
    pub network_id:  NetworkId,
    pub message:     UCursor,
}

impl NetworkPacket {
    pub fn generate_message_id() -> MessageId {
        let mut secure_bytes = vec![0u8; 256];
        let mut rng = rand::thread_rng();

        rng.fill_bytes(&mut secure_bytes);

        MessageId::new(&utils::sha256_bytes(&secure_bytes))
    }
}

impl Serializable for NetworkPacket {
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        self.packet_type.serialize(archive)?;
        self.message_id.serialize(archive)?;
        self.network_id.serialize(archive)?;
        self.message.serialize(archive)
    }
}

impl Deserializable for NetworkPacket {
    fn deserialize<A>(archive: &mut A) -> Fallible<NetworkPacket>
    where
        A: ReadArchive, {
        let packet = NetworkPacket {
            packet_type: NetworkPacketType::deserialize(archive)?,
            peer:        archive.post_handshake_peer()?,
            message_id:  MessageId::deserialize(archive)?,
            network_id:  NetworkId::deserialize(archive)?,
            message:     UCursor::deserialize(archive)?,
        };
        Ok(packet)
    }
}
