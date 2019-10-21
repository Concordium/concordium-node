pub mod counter;
pub mod fails;
pub mod p2p_node_id;
pub mod p2p_peer;

use concordium_common::hybrid_buf::HybridBuf;

use chrono::prelude::*;
use mio::Token;

use std::net::{IpAddr, SocketAddr};

/// This data type is used to queue a request from any thread (like tests, RPC,
/// Cli, etc.), into a node. Please note that any access to internal `socket`
/// *must be executed* inside MIO poll-loop thread.
pub struct NetworkRawRequest {
    pub token: Token, // Identifies the connection.
    pub data:  HybridBuf,
}

#[cfg(feature = "collector")]
pub mod collector_utils;

pub fn serialize_ip(ip: IpAddr) -> String {
    match ip {
        IpAddr::V4(ip4) => format!(
            "IP4{:03}{:03}{:03}{:03}",
            ip4.octets()[0],
            ip4.octets()[1],
            ip4.octets()[2],
            ip4.octets()[3],
        ),
        IpAddr::V6(ip6) => format!(
            "IP6{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:\
             02x}{:02x}{:02x}",
            ip6.octets()[0],
            ip6.octets()[1],
            ip6.octets()[2],
            ip6.octets()[3],
            ip6.octets()[4],
            ip6.octets()[5],
            ip6.octets()[6],
            ip6.octets()[7],
            ip6.octets()[8],
            ip6.octets()[9],
            ip6.octets()[10],
            ip6.octets()[11],
            ip6.octets()[12],
            ip6.octets()[13],
            ip6.octets()[14],
            ip6.octets()[15],
        ),
    }
}

pub fn serialize_addr(addr: SocketAddr) -> String {
    format!("{}{:05}", serialize_ip(addr.ip()), addr.port())
}

pub fn get_current_stamp() -> u64 { Utc::now().timestamp_millis() as u64 }

pub use self::{
    p2p_node_id::P2PNodeId,
    p2p_peer::{P2PPeer, P2PPeerBuilder, PeerStats, PeerType, RemotePeer},
};
