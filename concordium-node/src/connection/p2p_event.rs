use std::net::IpAddr;

use crate::{
    common::{P2PNodeId, P2PPeer},
    network::NetworkId,
};

#[derive(Clone, Debug, PartialEq)]
pub enum P2PEvent {
    ConnectEvent(String, u16),
    DisconnectEvent(String),
    ReceivedMessageEvent(P2PNodeId),
    SentMessageEvent(P2PNodeId),
    InitiatingConnection(IpAddr, u16),
    JoinedNetwork(P2PPeer, NetworkId),
    LeftNetwork(P2PPeer, NetworkId),
}
