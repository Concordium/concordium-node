pub mod buckets;
pub mod serialization;

use semver::Version;

pub use self::buckets::Buckets;

use crate::{
    common::{p2p_peer::P2PPeer, P2PNodeId},
    p2p::bans::BanId,
};

use std::collections::HashSet;

/// Identifies a network.
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

/// The main object used to transmit data over the network.
#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub struct NetworkMessage {
    /// The creation timestamp.
    pub created: u64,
    /// The receipt timestamp (if received from the network).
    pub received: Option<u64>,
    /// The message's payload.
    pub payload: NetworkPayload,
}

#[macro_export]
macro_rules! netmsg {
    ($payload_type:ident, $payload:expr) => {{
        NetworkMessage {
            created:  get_current_stamp(),
            received: None,
            payload:  NetworkPayload::$payload_type($payload),
        }
    }};
}

/// The contents of a network message.
#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum NetworkPayload {
    NetworkRequest(NetworkRequest),
    NetworkResponse(NetworkResponse),
    NetworkPacket(NetworkPacket),
}

/// The "high-level" network handshake.
#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub struct Handshake {
    pub remote_id:   P2PNodeId,
    pub remote_port: u16,
    pub networks:    HashSet<NetworkId>,
    pub version:     Version,
    pub proof:       Vec<u8>,
}

/// A network message serving a specified purpose.
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

/// A network message sent only in response to a network request.
#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum NetworkResponse {
    Pong,
    PeerList(Vec<P2PPeer>),
}

/// A network message carrying any bytes as payload.
#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub struct NetworkPacket {
    pub packet_type: NetworkPacketType,
    pub network_id:  NetworkId,
    pub message:     Vec<u8>,
}

/// The desired target of a network packet.
#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum NetworkPacketType {
    DirectMessage(P2PNodeId),
    BroadcastedMessage(Vec<P2PNodeId>),
}
