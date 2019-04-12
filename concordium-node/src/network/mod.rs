use crate::common::get_current_stamp_b64;

pub mod protocol_message_type;
pub mod buckets;
pub mod message;
pub mod packet;
pub mod request;
pub mod response;
pub mod serialization;

pub use self::{
    buckets::Buckets,
    message::NetworkMessage,
    packet::{NetworkPacket, NetworkPacketBuilder, NetworkPacketType},
    request::NetworkRequest,
    response::NetworkResponse,
    protocol_message_type::{ ProtocolMessageType, PROTOCOL_MESSAGE_TYPE_LENGTH }
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

