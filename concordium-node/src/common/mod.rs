//! Common objects used by the client.

pub mod grpc_api;
pub mod p2p_node_id;
pub mod p2p_peer;
#[macro_use]
pub mod utils;

use chrono::prelude::*;

/// Returns the current timestamp.
pub fn get_current_stamp() -> u64 { Utc::now().timestamp_millis() as u64 }

pub use self::{
    p2p_node_id::{P2PNodeId, PeerId},
    p2p_peer::{P2PPeer, PeerStats, PeerType, RemotePeer},
};
