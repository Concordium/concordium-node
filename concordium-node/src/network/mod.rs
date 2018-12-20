pub mod packet;
pub mod request;
pub mod message; 
pub mod buckets;
pub mod response;

pub use self::packet::NetworkPacket;
pub use self::message::NetworkMessage;
pub use self::buckets::Buckets;
pub use self::request::NetworkRequest;
pub use self::response::NetworkResponse;

pub const PROTOCOL_NAME: &'static str = "CONCORDIUMP2P";
pub const PROTOCOL_VERSION: &'static str = "001";
pub const PROTOCOL_NODE_ID_LENGTH: usize = 64;
pub const PROTOCOL_SENT_TIMESTAMP_LENGTH: usize = 16;

pub const PROTOCOL_MESSAGE_TYPE_REQUEST_PING: &'static str = "0001";
pub const PROTOCOL_MESSAGE_TYPE_REQUEST_FINDNODE: &'static str = "0002";
pub const PROTOCOL_MESSAGE_TYPE_REQUEST_HANDSHAKE: &'static str = "0003";
pub const PROTOCOL_MESSAGE_TYPE_REQUEST_GET_PEERS: &'static str = "0004";
pub const PROTOCOL_MESSAGE_TYPE_REQUEST_BANNODE: &'static str = "0005";
pub const PROTOCOL_MESSAGE_TYPE_REQUEST_UNBANNODE: &'static str = "0006";
pub const PROTOCOL_MESSAGE_TYPE_REQUEST_JOINNETWORK: &'static str = "0007";
pub const PROTOCOL_MESSAGE_TYPE_REQUEST_LEAVENETWORK: &'static str = "0008";
pub const PROTOCOL_MESSAGE_TYPE_RESPONSE_PONG: &'static str = "1001";
pub const PROTOCOL_MESSAGE_TYPE_RESPONSE_FINDNODE: &'static str = "1002";
pub const PROTOCOL_MESSAGE_TYPE_RESPONSE_PEERSLIST: &'static str = "1003";
pub const PROTOCOL_MESSAGE_TYPE_RESPONSE_HANDSHAKE: &'static str = "1004";
pub const PROTOCOL_MESSAGE_TYPE_DIRECT_MESSAGE: &'static str = "2001";
pub const PROTOCOL_MESSAGE_TYPE_BROADCASTED_MESSAGE: &'static str = "2002";

