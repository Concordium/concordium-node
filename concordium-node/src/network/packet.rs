use crate::{
    common::{
        serialization::{Deserializable, ReadArchive, Serializable, WriteArchive},
        P2PNodeId, P2PPeer,
    },
    network::{AsProtocolPacketType, NetworkId, ProtocolPacketType},
};
use concordium_common::{HashBytes, UCursor};

use crate::{
    failure::{err_msg, Fallible},
    utils,
};
use rand::RngCore;
use std::convert::TryFrom;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum NetworkPacketType {
    DirectMessage(P2PNodeId),
    BroadcastedMessage(),
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
            NetworkPacketType::BroadcastedMessage() => Ok(()),
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
            ProtocolPacketType::Broadcast => Ok(NetworkPacketType::BroadcastedMessage()),
        }
    }
}

pub type MessageId = HashBytes;

/// # BUG
/// It is not *thread-safe* but I've forced it temporary
#[derive(Clone, Builder, Debug, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
#[builder(build_fn(skip))]
pub struct NetworkPacket {
    #[builder(setter(skip))]
    pub packet_type: NetworkPacketType,
    pub peer: P2PPeer,
    pub message_id: MessageId,
    pub network_id: NetworkId,

    pub message: UCursor,
}

impl NetworkPacketBuilder {
    #[inline]
    pub fn build_broadcast(&mut self) -> Fallible<NetworkPacket> {
        self.build(NetworkPacketType::BroadcastedMessage())
    }

    #[inline]
    pub fn build_direct(&mut self, receiver: P2PNodeId) -> Fallible<NetworkPacket> {
        self.build(NetworkPacketType::DirectMessage(receiver))
    }

    pub fn build(&mut self, packet_type: NetworkPacketType) -> Fallible<NetworkPacket> {
        Ok(NetworkPacket {
            packet_type,
            peer: self
                .peer
                .take()
                .ok_or_else(|| err_msg("Peer is a mandatory field"))?,
            message_id: self
                .message_id
                .take()
                .ok_or_else(|| err_msg("Message Id is a mandatory field"))?,
            network_id: self
                .network_id
                .ok_or_else(|| err_msg("Network Id is a mandatory field"))?,
            message: self
                .message
                .take()
                .ok_or_else(|| err_msg("Message payload is a mandatory field"))?,
        })
    }
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
