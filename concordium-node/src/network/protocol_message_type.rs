use failure::{Error, Fallible};

use std::{
    convert::TryFrom,
    fmt::{Display, Formatter, Result},
};

// Utilities for NetworkMessage

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ProtocolMessageType {
    Request = 0,
    Response,
    Packet,
}

pub trait AsProtocolMessageType {
    fn protocol_message_type(&self) -> ProtocolMessageType;
}

impl TryFrom<u8> for ProtocolMessageType {
    type Error = Error;

    fn try_from(value: u8) -> Fallible<ProtocolMessageType> {
        let pmt = match value {
            0 => ProtocolMessageType::Request,
            1 => ProtocolMessageType::Response,
            2 => ProtocolMessageType::Packet,
            _ => bail!("Unsupported Protocol Message type '{}'", value),
        };
        Ok(pmt)
    }
}

// Utilities for NetworkRequest

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ProtocolRequestType {
    Ping = 0,
    Handshake,
    GetPeers,
    BanNode,
    UnbanNode,
    JoinNetwork,
    LeaveNetwork,
    Retransmit,
}

pub trait AsProtocolRequestType {
    fn protocol_request_type(&self) -> ProtocolRequestType;
}

impl TryFrom<u8> for ProtocolRequestType {
    type Error = Error;

    fn try_from(value: u8) -> Fallible<ProtocolRequestType> {
        let prt = match value {
            0 => ProtocolRequestType::Ping,
            1 => ProtocolRequestType::Handshake,
            2 => ProtocolRequestType::GetPeers,
            3 => ProtocolRequestType::BanNode,
            4 => ProtocolRequestType::UnbanNode,
            5 => ProtocolRequestType::JoinNetwork,
            6 => ProtocolRequestType::LeaveNetwork,
            7 => ProtocolRequestType::Retransmit,
            _ => bail!("Unsupported Protocol Request type '{}'", value),
        };
        Ok(prt)
    }
}

// Utilities for NetworkResponse

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ProtocolResponseType {
    Pong = 0,
    PeerList,
    Handshake,
}

impl Display for ProtocolResponseType {
    fn fmt(&self, f: &mut Formatter) -> Result { write!(f, "{:02x}", *self as u8) }
}

pub trait AsProtocolResponseType {
    fn protocol_response_type(&self) -> ProtocolResponseType;
}

impl TryFrom<u8> for ProtocolResponseType {
    type Error = Error;

    fn try_from(value: u8) -> Fallible<ProtocolResponseType> {
        let prt = match value {
            0 => ProtocolResponseType::Pong,
            1 => ProtocolResponseType::PeerList,
            2 => ProtocolResponseType::Handshake,
            _ => bail!("Unsupported Protocol Response type '{}'", value),
        };
        Ok(prt)
    }
}

// Utilities for NetworkPacket

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ProtocolPacketType {
    Direct = 0,
    Broadcast,
}

pub trait AsProtocolPacketType {
    fn protocol_packet_type(&self) -> ProtocolPacketType;
}

impl TryFrom<u8> for ProtocolPacketType {
    type Error = Error;

    fn try_from(value: u8) -> Fallible<ProtocolPacketType> {
        let ppt = match value {
            0 => ProtocolPacketType::Direct,
            1 => ProtocolPacketType::Broadcast,
            _ => bail!("Unsupported Protocol Packet type '{}'", value),
        };
        Ok(ppt)
    }
}
