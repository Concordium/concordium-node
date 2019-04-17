use std::fmt;
use std::net::SocketAddr;

use crate::{
    common::{P2PNodeId, P2PPeer},
    network::NetworkId,
};

use self::P2PEvent::*;

#[derive(Clone, Debug, PartialEq)]
pub enum P2PEvent {
    ConnectEvent(SocketAddr),
    DisconnectEvent(String),
    ReceivedMessageEvent(P2PNodeId),
    SentMessageEvent(P2PNodeId),
    InitiatingConnection(SocketAddr),
    JoinedNetwork(P2PPeer, NetworkId),
    LeftNetwork(P2PPeer, NetworkId),
}

impl fmt::Display for P2PEvent {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}",
            match self {
                ConnectEvent(addr) => format!("Received a connection from {}", addr),
                DisconnectEvent(msg) => format!("Received a disconnect for {}", msg),
                ReceivedMessageEvent(node_id) => format!("Received a message from {:?}", node_id),
                SentMessageEvent(node_id) => format!("Sent a message to {:?}", node_id),
                InitiatingConnection(addr) => format!("Initiating a connection to {}", addr),
                JoinedNetwork(peer, network_id) => format!("Peer {} joined the network {}", peer.id(), network_id),
                LeftNetwork(peer, network_id) => format!("Peer {} left the network {}", peer.id(), network_id)
            }
        )
    }
}