use crate::common::functor::{ AFunctor, FunctorResult };
use crate::network::{ NetworkPacket };

use crate::connection::network_handler::{ NetworkPacketCW };

pub struct PacketHandler {
    pub direct_parser: AFunctor<NetworkPacket>,
    pub broadcast_parser: AFunctor<NetworkPacket>,
}

impl PacketHandler {
    pub fn new() -> Self {
        PacketHandler {
            direct_parser: AFunctor::<NetworkPacket>::new(
                    "Network::Packet::Direct"),
            broadcast_parser: AFunctor::<NetworkPacket>::new(
                    "Network::Packet::Broadcast"),
        }
    }

    pub fn add_direct_callback( &mut self, callback: NetworkPacketCW) -> &mut Self {
        self.direct_parser.add_callback( callback);
        self
    }

    pub fn add_broadcast_callback( &mut self, callback: NetworkPacketCW) -> &mut Self {
        self.broadcast_parser.add_callback( callback);
        self
    }

    /// It runs main parser and specific ones for the internal type of msg.
    fn process_message(&self, msg: &NetworkPacket) -> FunctorResult {

        let spec_status = match msg {
            NetworkPacket::DirectMessage(_, _, _, _, _) => {
                (&self.direct_parser)( &msg)
            },
            NetworkPacket::BroadcastedMessage( _, _, _, _) => {
                (&self.broadcast_parser)( &msg)
            }
        };

        spec_status
    }
}

impl_all_fns!( PacketHandler, NetworkPacket);
