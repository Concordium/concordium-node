use failure::{Error, Fallible};

use std::{
    convert::TryFrom,
    fmt::{Display, Formatter, Result},
};

// Utility for NetworkMessage
// =================================

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ProtocolMessageType {
    Request = 0,
    Response,
    Packet,
}

impl Display for ProtocolMessageType {
    fn fmt(&self, f: &mut Formatter) -> Result { write!(f, "{:02x}", *self as u8) }
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

impl TryFrom<&str> for ProtocolMessageType {
    type Error = Error;

    fn try_from(value: &str) -> Fallible<ProtocolMessageType> {
        debug_assert_eq!(value.len(), 2);
        ProtocolMessageType::try_from(u8::from_str_radix(value, 16)?)
    }
}

// Utility for Network Request
// =================================

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ProtocolRequestType {
    Ping = 0,
    FindNode,
    Handshake,
    GetPeers,
    BanNode,
    UnbanNode,
    JoinNetwork,
    LeaveNetwork,
    Retransmit,
}

impl Display for ProtocolRequestType {
    fn fmt(&self, f: &mut Formatter) -> Result { write!(f, "{:02x}", *self as u8) }
}

pub trait AsProtocolRequestType {
    fn protocol_request_type(&self) -> ProtocolRequestType;
}

impl TryFrom<u8> for ProtocolRequestType {
    type Error = Error;

    fn try_from(value: u8) -> Fallible<ProtocolRequestType> {
        let prt = match value {
            0 => ProtocolRequestType::Ping,
            1 => ProtocolRequestType::FindNode,
            2 => ProtocolRequestType::Handshake,
            3 => ProtocolRequestType::GetPeers,
            4 => ProtocolRequestType::BanNode,
            5 => ProtocolRequestType::UnbanNode,
            6 => ProtocolRequestType::JoinNetwork,
            7 => ProtocolRequestType::LeaveNetwork,
            8 => ProtocolRequestType::Retransmit,
            _ => bail!("Unsupported Protocol Request type '{}'", value),
        };
        Ok(prt)
    }
}

impl TryFrom<&str> for ProtocolRequestType {
    type Error = Error;

    fn try_from(value: &str) -> Fallible<ProtocolRequestType> {
        debug_assert_eq!(value.len(), 2);
        ProtocolRequestType::try_from(u8::from_str_radix(value, 16)?)
    }
}

// Utility for Network Response
// =================================

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ProtocolResponseType {
    Pong = 0,
    FindNode,
    PeersList,
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
            1 => ProtocolResponseType::FindNode,
            2 => ProtocolResponseType::PeersList,
            3 => ProtocolResponseType::Handshake,
            _ => bail!("Unsupported Protocol Response type '{}'", value),
        };
        Ok(prt)
    }
}

impl TryFrom<&str> for ProtocolResponseType {
    type Error = Error;

    fn try_from(value: &str) -> Fallible<ProtocolResponseType> {
        debug_assert_eq!(value.len(), 2);
        ProtocolResponseType::try_from(u8::from_str_radix(value, 16)?)
    }
}

// Utility for Packet
// =================================

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ProtocolPacketType {
    Direct = 0,
    Broadcast,
}

impl Display for ProtocolPacketType {
    fn fmt(&self, f: &mut Formatter) -> Result { write!(f, "{:02x}", *self as u8) }
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

impl TryFrom<&str> for ProtocolPacketType {
    type Error = Error;

    fn try_from(value: &str) -> Fallible<ProtocolPacketType> {
        debug_assert_eq!(value.len(), 2);
        ProtocolPacketType::try_from(u8::from_str_radix(value, 16)?)
    }
}
