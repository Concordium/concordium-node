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
    message::{NetworkMessage, NetworkMessagePayload},
    network_id::NetworkId,
    packet::{NetworkPacket, NetworkPacketBuilder, NetworkPacketType},
    protocol_message_type::{
        AsProtocolMessageType, AsProtocolPacketType, AsProtocolRequestType, AsProtocolResponseType,
        ProtocolMessageType, ProtocolPacketType, ProtocolRequestType, ProtocolResponseType,
    },
    request::NetworkRequest,
    response::NetworkResponse,
    serialization::fbs::{deserialize, serialize},
};

pub const PROTOCOL_NAME: &str = "CP2P";
pub const PROTOCOL_VERSION: u8 = 1;
pub const PROTOCOL_HEADER_LEN: usize = 4 + 1 + 8; // name + version + timestamp; FIXME: shouldn't include the stamp

pub const PROTOCOL_MAX_MESSAGE_SIZE: u32 = 20_971_520;
