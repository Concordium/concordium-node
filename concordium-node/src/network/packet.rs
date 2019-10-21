use crate::{
    common::P2PNodeId,
    network::{AsProtocolPacketType, NetworkId, ProtocolPacketType},
};
use concordium_common::hybrid_buf::HybridBuf;

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

#[derive(Clone, Builder, Debug, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub struct NetworkPacket {
    pub packet_type: NetworkPacketType,
    pub network_id:  NetworkId,
    pub message:     HybridBuf,
}
