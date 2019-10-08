use byteorder::{ReadBytesExt, WriteBytesExt};

use crate::{
    common::P2PNodeId,
    network::{AsProtocolPacketType, NetworkId, ProtocolPacketType},
};
use concordium_common::{
    hybrid_buf::HybridBuf,
    serial::{NoParam, Serial},
};

use crate::failure::Fallible;
use std::convert::TryFrom;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum NetworkPacketType {
    DirectMessage(P2PNodeId),
    BroadcastedMessage(Vec<P2PNodeId>),
}

impl AsProtocolPacketType for NetworkPacketType {
    fn protocol_packet_type(&self) -> ProtocolPacketType {
        match self {
            NetworkPacketType::DirectMessage(..) => ProtocolPacketType::Direct,
            NetworkPacketType::BroadcastedMessage(..) => ProtocolPacketType::Broadcast,
        }
    }
}

impl Serial for NetworkPacketType {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        let protocol_type = ProtocolPacketType::try_from(source.read_u8()?)?;

        match protocol_type {
            ProtocolPacketType::Direct => Ok(NetworkPacketType::DirectMessage(
                P2PNodeId::deserial(source)?,
            )),
            ProtocolPacketType::Broadcast => Ok(NetworkPacketType::BroadcastedMessage(vec![])),
        }
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        target.write_u8(self.protocol_packet_type() as u8)?;

        match self {
            NetworkPacketType::DirectMessage(ref receiver) => receiver.serial(target),
            NetworkPacketType::BroadcastedMessage(..) => Ok(()),
        }
    }
}

/// This is not *thread-safe* but this ensures it temporarily
#[derive(Clone, Builder, Debug)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub struct NetworkPacket {
    pub packet_type: NetworkPacketType,
    pub network_id:  NetworkId,
    pub message:     HybridBuf,
}

impl Serial for NetworkPacket {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        Ok(NetworkPacket {
            packet_type: NetworkPacketType::deserial(source)?,
            network_id:  NetworkId::deserial(source)?,
            message:     HybridBuf::deserial(source)?,
        })
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        self.packet_type.serial(target)?;
        self.network_id.serial(target)?;
        self.message.serial(target)
    }
}
