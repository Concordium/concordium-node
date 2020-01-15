pub mod p2p_node_id;
pub mod p2p_peer;

use chrono::prelude::*;

#[cfg(feature = "collector")]
pub mod collector_utils;

pub fn get_current_stamp() -> u64 { Utc::now().timestamp_millis() as u64 }

pub use self::{
    p2p_node_id::P2PNodeId,
    p2p_peer::{P2PPeer, PeerStats, PeerType, RemotePeer},
};
