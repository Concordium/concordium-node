use crate::{
    connection::network_handler::NetworkPacketCW,
    network::{NetworkPacket, NetworkPacketType},
};
use concordium_common::functor::{FunctorResult, Functorable, UnitFunctor};

pub struct PacketHandler {
    pub direct_parser:    UnitFunctor<NetworkPacket>,
    pub broadcast_parser: UnitFunctor<NetworkPacket>,
}

impl Default for PacketHandler {
    fn default() -> Self { PacketHandler::new() }
}

impl PacketHandler {
    pub fn new() -> Self {
        PacketHandler {
            direct_parser:    UnitFunctor::new("Network::Packet::Direct"),
            broadcast_parser: UnitFunctor::new("Network::Packet::Broadcast"),
        }
    }

    pub fn add_direct_callback(&mut self, callback: NetworkPacketCW) -> &mut Self {
        self.direct_parser.add_callback(callback);
        self
    }

    pub fn add_broadcast_callback(&mut self, callback: NetworkPacketCW) -> &mut Self {
        self.broadcast_parser.add_callback(callback);
        self
    }

    /// It runs main parser and specific ones for the internal type of msg.
    pub fn process_message(&self, msg: &NetworkPacket) -> FunctorResult<()> {
        match msg.packet_type {
            NetworkPacketType::DirectMessage(_) => self.direct_parser.run_callbacks(&msg),
            NetworkPacketType::BroadcastedMessage(_) => self.broadcast_parser.run_callbacks(&msg),
        }
    }
}
