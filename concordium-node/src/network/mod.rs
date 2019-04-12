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
pub const PROTOCOL_NODE_ID_LENGTH: usize = 16;
pub const PROTOCOL_PORT_LENGTH: usize = 5;
pub const PROTOCOL_MESSAGE_ID_LENGTH: usize = 44;
pub const PROTOCOL_NETWORK_ID_LENGTH: usize = 5;
pub const PROTOCOL_NETWORK_CONTENT_SIZE_LENGTH: usize = 10;
pub const PROTOCOL_SENT_TIMESTAMP_LENGTH: usize = 12;

pub fn make_header() -> String {
    format!(
        "{}{}{}",
        PROTOCOL_NAME,
        PROTOCOL_VERSION,
        get_current_stamp_b64()
    )
}

