pub mod buckets;
pub mod message;
pub mod packet;
pub mod protocol_message_type;
pub mod request;
pub mod response;
pub mod serialization;

pub use self::{
    buckets::Buckets,
    message::{NetworkMessage, NetworkMessagePayload},
    packet::{NetworkPacket, NetworkPacketBuilder, NetworkPacketType},
    protocol_message_type::{
        AsProtocolMessageType, AsProtocolPacketType, AsProtocolRequestType, AsProtocolResponseType,
        ProtocolMessageType, ProtocolPacketType, ProtocolRequestType, ProtocolResponseType,
    },
    request::NetworkRequest,
    response::NetworkResponse,
};

use std::fmt;

pub const PROTOCOL_NAME: &str = "CP2P";
pub const PROTOCOL_VERSION: u8 = 1;
pub const PROTOCOL_HEADER_LEN: usize = 4 + 1 + 8; // name + version + timestamp; FIXME: shouldn't include the stamp
pub const PROTOCOL_MAX_MESSAGE_SIZE: u32 = 20_971_520;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub struct NetworkId {
    pub id: u16,
}

impl From<u16> for NetworkId {
    fn from(id: u16) -> Self { NetworkId { id } }
}

impl fmt::Display for NetworkId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{:05}", self.id) }
}
