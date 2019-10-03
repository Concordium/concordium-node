pub mod buckets;
pub mod message;
pub mod network_id;
pub mod packet;
pub mod protocol_message_type;
pub mod request;
pub mod response;
pub mod serialization;

pub use self::{
    buckets::Buckets,
    message::{NetworkMessage, NETWORK_MESSAGE_PROTOCOL_TYPE_IDX},
    network_id::NetworkId,
    packet::{NetworkPacket, NetworkPacketBuilder, NetworkPacketType},
    protocol_message_type::{
        AsProtocolMessageType, AsProtocolPacketType, AsProtocolRequestType, AsProtocolResponseType,
        ProtocolMessageType, ProtocolPacketType, ProtocolRequestType, ProtocolResponseType,
    },
    request::NetworkRequest,
    response::NetworkResponse,
};

pub const PROTOCOL_NAME: &str = "CP2P";
pub const PROTOCOL_VERSION: u8 = 1;
pub const PROTOCOL_PORT_LENGTH: usize = 5;
pub const PROTOCOL_MAX_MESSAGE_SIZE: u32 = 20_971_520;
