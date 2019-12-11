use super::{
    AsProtocolMessageType, NetworkPacket, NetworkRequest, NetworkResponse, ProtocolMessageType,
};

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub struct NetworkMessage {
    pub timestamp1: Option<u64>,
    pub timestamp2: Option<u64>,
    pub payload:    NetworkMessagePayload,
}

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum NetworkMessagePayload {
    NetworkRequest(NetworkRequest),
    NetworkResponse(NetworkResponse),
    NetworkPacket(NetworkPacket),
}

impl AsProtocolMessageType for NetworkMessage {
    fn protocol_message_type(&self) -> ProtocolMessageType {
        match self.payload {
            NetworkMessagePayload::NetworkRequest(..) => ProtocolMessageType::Request,
            NetworkMessagePayload::NetworkResponse(..) => ProtocolMessageType::Response,
            NetworkMessagePayload::NetworkPacket(..) => ProtocolMessageType::Packet,
        }
    }
}
