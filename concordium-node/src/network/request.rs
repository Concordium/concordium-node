use crate::{common::P2PNodeId, network::NetworkId, p2p::banned_nodes::BannedNode};

use std::collections::HashSet;

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
