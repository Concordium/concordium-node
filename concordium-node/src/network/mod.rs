pub mod buckets;
pub mod serialization;

pub use self::buckets::Buckets;

use crate::{
    common::{p2p_peer::P2PPeer, P2PNodeId},
    p2p::banned_nodes::BannedNode,
};

use std::{collections::HashSet, fmt, sync::Arc};

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
    pub timestamp1: Option<u64>,
    pub timestamp2: Option<u64>,
    pub payload:    NetworkMessagePayload,
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
pub enum NetworkRequest {
    Ping,
    GetPeers(HashSet<NetworkId>),
    Handshake(P2PNodeId, u16, HashSet<NetworkId>, Vec<u8>),
    BanNode(BannedNode),
    UnbanNode(BannedNode),
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
    pub message:     Arc<[u8]>,
}

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum NetworkPacketType {
    DirectMessage(P2PNodeId),
    BroadcastedMessage(Vec<P2PNodeId>),
}
