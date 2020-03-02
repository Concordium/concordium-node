pub mod buckets;
pub mod serialization;

use semver::Version;

pub use self::buckets::Buckets;

use crate::{
    common::{p2p_peer::P2PPeer, P2PNodeId},
    p2p::bans::BanId,
};

use std::{collections::HashSet, fmt};

pub const PROTOCOL_MAX_MESSAGE_SIZE: u32 = 20_971_520; // 20 MIB

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub struct NetworkId {
    pub id: u16,
}

impl From<u16> for NetworkId {
    fn from(id: u16) -> Self {
        NetworkId {
            id,
        }
    }
}

impl fmt::Display for NetworkId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{:05}", self.id) }
}

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub struct NetworkMessage {
    /// The creation timestamp.
    pub created: u64,
    /// The receipt timestamp (if received from the network).
    pub received: Option<u64>,
    /// The message's payload.
    pub payload: NetworkMessagePayload,
}

#[macro_export]
macro_rules! netmsg {
    ($payload_type:ident, $payload:expr) => {{
        NetworkMessage {
            created:  get_current_stamp(),
            received: None,
            payload:  NetworkMessagePayload::$payload_type($payload),
        }
    }};
}

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum NetworkMessagePayload {
    NetworkRequest(NetworkRequest),
    NetworkResponse(NetworkResponse),
    NetworkPacket(NetworkPacket),
}

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub struct Handshake {
    pub remote_id:   P2PNodeId,
    pub remote_port: u16,
    pub networks:    HashSet<NetworkId>,
    pub version:     Version,
    pub proof:       Vec<u8>,
}

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum NetworkRequest {
    Ping,
    GetPeers(HashSet<NetworkId>),
    Handshake(Handshake),
    BanNode(BanId),
    UnbanNode(BanId),
    JoinNetwork(NetworkId),
    LeaveNetwork(NetworkId),
}

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum NetworkResponse {
    Pong,
    PeerList(Vec<P2PPeer>),
}

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub struct NetworkPacket {
    pub packet_type: NetworkPacketType,
    pub network_id:  NetworkId,
    pub message:     Vec<u8>,
}

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum NetworkPacketType {
    DirectMessage(P2PNodeId),
    BroadcastedMessage(Vec<P2PNodeId>),
}
