use crate::{
    common::{P2PNodeId, P2PPeer, UCursor},
    network::{
        serialization::{ Serializable, Archive },
        NetworkId, ProtocolMessageType, AsProtocolMessageType, PROTOCOL_MESSAGE_ID_LENGTH, PROTOCOL_MESSAGE_TYPE_LENGTH,
        PROTOCOL_NAME, PROTOCOL_NETWORK_CONTENT_SIZE_LENGTH, PROTOCOL_NETWORK_ID_LENGTH,
        PROTOCOL_NODE_ID_LENGTH, PROTOCOL_SENT_TIMESTAMP_LENGTH, PROTOCOL_VERSION,
    },
};

use crate::{
    failure::{err_msg, Fallible},
    utils,
};
use rand::{rngs::OsRng, RngCore};
use std::{
    io::{Chain, Cursor, Read},
    sync::RwLock,
};

lazy_static! {
    static ref RNG: RwLock<OsRng> = { RwLock::new(OsRng::new().unwrap()) };
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum NetworkPacketType {
    DirectMessage(P2PNodeId),
    BroadcastedMessage,
}

impl Serializable for NetworkPacketType {
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()> where A: Archive {
        match self{
            NetworkPacketType::DirectMessage(ref receiver) => {
                archive.write_u8(0)?;
                receiver.serialize( archive)
            }
            NetworkPacketType::BroadcastedMessage => archive.write_u8(1)
        }
    }
}

/// # BUG
/// It is not *thread-safe* but I've forced it temporary
#[derive(Clone, Builder, Debug, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
#[builder(build_fn(skip))]
pub struct NetworkPacket {
    #[builder(setter(skip))]
    pub packet_type: NetworkPacketType,
    pub peer: P2PPeer,
    pub message_id: String,
    pub network_id: NetworkId,

    pub message: Box<UCursor>,
}

impl NetworkPacketBuilder {
    #[inline]
    pub fn build_broadcast(&mut self) -> Fallible<NetworkPacket> {
        self.build(NetworkPacketType::BroadcastedMessage)
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
    fn direct_header_as_vec(&self, receiver: P2PNodeId) -> Vec<u8> {
        serialize_message!(
            ProtocolMessageType::DirectMessage,
            format!(
                "{}{}{}{:010}",
                receiver,
                self.message_id,
                self.network_id,
                self.message.len()
            )
        )
    }

    fn broadcast_header_as_vec(&self) -> Vec<u8> {
        serialize_message!(
            ProtocolMessageType::BroadcastedMessage,
            format!(
                "{}{}{:010}",
                self.message_id,
                self.network_id,
                self.message.len()
            )
        )
    }

    fn header_as_vec(&self) -> Vec<u8> {
        match self.packet_type {
            NetworkPacketType::DirectMessage(receiver) => self.direct_header_as_vec(receiver),
            NetworkPacketType::BroadcastedMessage => self.broadcast_header_as_vec(),
        }
    }

    fn expected_serialize_message_len(&self) -> usize {
        let common_part_len = PROTOCOL_NAME.len()
            + PROTOCOL_VERSION.len()
            + PROTOCOL_SENT_TIMESTAMP_LENGTH
            + PROTOCOL_MESSAGE_TYPE_LENGTH
            + PROTOCOL_MESSAGE_ID_LENGTH
            + PROTOCOL_NETWORK_ID_LENGTH
            + PROTOCOL_NETWORK_CONTENT_SIZE_LENGTH
            + self.message.len() as usize;

        if let NetworkPacketType::DirectMessage(..) = self.packet_type {
            common_part_len + PROTOCOL_NODE_ID_LENGTH
        } else {
            common_part_len
        }
    }

    pub fn reader(&self) -> Chain<Cursor<Vec<u8>>, UCursor> {
        let header_reader = Cursor::new(self.header_as_vec());
        header_reader.chain((*self.message).clone())
    }

    pub fn generate_message_id() -> String {
        let mut secure_bytes = vec![0u8; 256];
        match safe_write!(RNG) {
            Ok(mut l) => l.fill_bytes(&mut secure_bytes),
            Err(_) => return String::new(),
        }
        utils::to_hex_string(&utils::sha256_bytes(&secure_bytes))
    }
}

impl AsProtocolMessageType for NetworkPacket{
    fn protocol_type(&self) -> ProtocolMessageType {
        match self.packet_type {
            NetworkPacketType::DirectMessage(..) => ProtocolMessageType::DirectMessage,
            NetworkPacketType::BroadcastedMessage => ProtocolMessageType::BroadcastedMessage
        }
    }
}

impl Serializable for NetworkPacket {
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()> where A: Archive {
        self.packet_type.serialize( archive)?;
        self.message_id.serialize( archive)?;
        self.network_id.serialize( archive)?;
        self.message.serialize( archive)
    }
}
