use std::net::IpAddr;

use crate::common::{P2PNodeId, P2PPeer};

#[derive(Clone, Debug, PartialEq)]
pub enum P2PEvent {
    ConnectEvent(String, u16),
    DisconnectEvent(String),
    ReceivedMessageEvent(P2PNodeId),
    SentMessageEvent(P2PNodeId),
    InitiatingConnection(IpAddr, u16),
    JoinedNetwork(P2PPeer, u16),
    LeftNetwork(P2PPeer, u16),
}
