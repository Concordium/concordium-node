use crate::{
    common::P2PPeer,
    network::{AsProtocolResponseType, ProtocolResponseType},
};

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum NetworkResponse {
    Pong,
    PeerList(Vec<P2PPeer>),
}

impl AsProtocolResponseType for NetworkResponse {
    fn protocol_response_type(&self) -> ProtocolResponseType {
        match self {
            NetworkResponse::Pong => ProtocolResponseType::Pong,
            NetworkResponse::PeerList(..) => ProtocolResponseType::PeerList,
        }
    }
}
