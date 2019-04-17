use std::net::SocketAddr;

use crate::{
    common::{P2PNodeId, P2PPeer},
    network::NetworkId,
};

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
