use crate::common::get_current_stamp_b64;
use std::fmt;

pub mod buckets;
pub mod message;
pub mod packet;
pub mod protocol_message_type;
pub mod request;
pub mod response;
pub mod serialization;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NetworkId {
    pub id: u16,
}

impl From<u16> for NetworkId {
    fn from(id: u16) -> Self { NetworkId { id } }
}

impl fmt::Display for NetworkId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{:05}", self.id) }
}

pub use self::{
    buckets::Buckets,
    message::NetworkMessage,
    packet::{NetworkPacket, NetworkPacketBuilder, NetworkPacketType},
    protocol_message_type::{ProtocolMessageType, PROTOCOL_MESSAGE_TYPE_LENGTH},
    request::NetworkRequest,
    response::NetworkResponse,
};

pub const PROTOCOL_NAME: &'static str = "CONCORDIUMP2P";
pub const PROTOCOL_VERSION: &'static str = "001";
pub const PROTOCOL_HEADER_LENGTH: usize = 13 + 3 + PROTOCOL_SENT_TIMESTAMP_LENGTH;
pub const PROTOCOL_NODE_ID_LENGTH: usize = 16;
pub const PROTOCOL_PORT_LENGTH: usize = 5;
pub const PROTOCOL_MESSAGE_ID_LENGTH: usize = 64;
pub const PROTOCOL_NETWORK_ID_LENGTH: usize = 5;
pub const PROTOCOL_NETWORK_CONTENT_SIZE_LENGTH: usize = 10;
pub const PROTOCOL_SENT_TIMESTAMP_LENGTH: usize = 12;
pub const PROTOCOL_MESSAGE_LENGTH: usize = PROTOCOL_HEADER_LENGTH
    + PROTOCOL_NODE_ID_LENGTH
    + PROTOCOL_PORT_LENGTH
    + PROTOCOL_MESSAGE_ID_LENGTH
    + PROTOCOL_NETWORK_ID_LENGTH
    + PROTOCOL_NETWORK_CONTENT_SIZE_LENGTH
    + PROTOCOL_MESSAGE_TYPE_LENGTH;

pub fn make_header() -> String {
    format!(
        "{}{}{}",
        PROTOCOL_NAME,
        PROTOCOL_VERSION,
        get_current_stamp_b64()
    )
}
