//! Network-related objects.

pub mod buckets;
pub mod serialization;

use nohash_hasher::BuildNoHashHasher;
use semver::Version;

pub use self::buckets::Buckets;

use crate::{
    common::{
        p2p_peer::{P2PPeer, RemotePeerId},
        P2PNodeId,
    },
    consensus_ffi::blockchain_types::BlockHash,
};

use std::collections::HashSet;

/// Wire protocol version number. Nodes must agree on a common wire protocol in
/// order to communicate. This should be the highest protocol version supported
/// by both nodes.
///
/// Note that the wire protocol version is somewhat independent of the consensus
/// protocol version.
pub type WireProtocolVersion = u8;

/// The current wire protocol version, currently 1.
pub const WIRE_PROTOCOL_CURRENT_VERSION: WireProtocolVersion = 1;

/// The supported write protocol versions in descending order.
pub const WIRE_PROTOCOL_VERSIONS: [WireProtocolVersion; 1] = [WIRE_PROTOCOL_CURRENT_VERSION];

/// Identifies a network.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

/// The collection of networks a node belongs to.
pub type Networks = HashSet<NetworkId, BuildNoHashHasher<u16>>;

/// The main object used to transmit data over the network.
#[derive(Debug, PartialEq)]
pub struct NetworkMessage {
    /// The creation timestamp.
    pub created:  u64,
    /// The receipt timestamp (if received from the network).
    pub received: Option<u64>,
    /// The message's payload.
    pub payload:  NetworkPayload,
}

/// A helper macro used to create a network message with the given payload.
#[macro_export]
macro_rules! netmsg {
    ($payload_type:ident, $payload:expr) => {{
        $crate::network::NetworkMessage {
            created:  get_current_stamp(),
            received: None,
            payload:  $crate::network::NetworkPayload::$payload_type($payload),
        }
    }};
}

/// The contents of a network message.
#[derive(Debug, PartialEq)]
pub enum NetworkPayload {
    NetworkRequest(NetworkRequest),
    NetworkResponse(NetworkResponse),
    NetworkPacket(NetworkPacket),
}

/// The "high-level" network handshake.
#[derive(Debug, PartialEq)]
pub struct Handshake {
    pub remote_id:      P2PNodeId,
    pub remote_port:    u16,
    pub networks:       Networks,
    pub node_version:   Version,
    pub wire_versions:  Vec<WireProtocolVersion>,
    pub genesis_blocks: Vec<BlockHash>,
    pub proof:          Vec<u8>,
}

/// A network message serving a specified purpose.
#[derive(Debug, PartialEq)]
pub enum NetworkRequest {
    /// Used to measure connection liveness and latency.
    Ping,
    /// Used to obtain peers' peers.
    GetPeers(Networks),
    /// Used in the initial exchange of metadata with peers.
    Handshake(Handshake),
    /// Notifies that a node joined a specific network.
    JoinNetwork(NetworkId),
    /// Notifies that a node left a specific network.
    LeaveNetwork(NetworkId),
}

/// A network message sent only in response to a network request.
#[derive(Debug, PartialEq)]
pub enum NetworkResponse {
    /// A response to a Ping request.
    Pong,
    /// A response to a GetPeers request.
    PeerList(Vec<P2PPeer>),
}

/// A network message carrying any bytes as payload.
#[derive(Debug, PartialEq)]
pub struct NetworkPacket {
    pub destination: PacketDestination,
    pub network_id:  NetworkId,
    pub message:     Vec<u8>,
}

/// The desired target of a network packet.
#[derive(Debug, PartialEq)]
pub enum PacketDestination {
    /// A single node.
    Direct(RemotePeerId),
    /// All peers, optionally excluding the ones in the vector.
    Broadcast(Vec<RemotePeerId>),
}
