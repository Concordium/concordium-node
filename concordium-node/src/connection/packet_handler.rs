use std::sync::{ Arc, Mutex };
use connection::parse_handler::{ ParseHandler, ParseCallback, ParseCallbackResult };
use network::{ NetworkPacket };
use common::{ P2PPeer, P2PNodeId };

pub type PacketHandlerDirect = (P2PPeer, String, P2PNodeId, u16, Vec<u8>);
pub type PacketHandlerBroadcast = (P2PPeer, String, u16, Vec<u8>);

pub struct PacketHandler {
    pub direct_parser: ParseHandler<PacketHandlerDirect >,
    pub broadcast_parser: ParseHandler<PacketHandlerBroadcast >
}

impl PacketHandler {
    pub fn new() -> Self {
        PacketHandler {
            direct_parser: ParseHandler::<PacketHandlerDirect>::new( 
                    "Network Packet Direct Handler"), 
            broadcast_parser: ParseHandler::<PacketHandlerBroadcast>::new(
                    "Network Packet Broadcast Handler")
        }
    }

    pub fn add_direct_callback( 
            mut self, 
            callback: Arc< Mutex <Box< ParseCallback<PacketHandlerDirect> > > >) -> Self {
        self.direct_parser = self.direct_parser.add_callback( callback);
        self
    }
    
    pub fn add_broadcast_callback( 
            mut self, 
            callback: Arc< Mutex< Box< ParseCallback<PacketHandlerBroadcast> > > >) -> Self {
        self.broadcast_parser = self.broadcast_parser.add_callback( callback);
        self
    }
   
    /// *todo*: This is making copies. We should try to use `ref`.
    fn process_message(&self, msg: &NetworkPacket) -> ParseCallbackResult {
        match msg {
            NetworkPacket::DirectMessage(peer, msgid, nodeid, nid, inner_msg) => {
                let dm = (peer.clone(), msgid.clone(), nodeid.clone(), nid.clone(), inner_msg.clone());
                (&self.direct_parser)( &dm)
            },
            NetworkPacket::BroadcastedMessage( peer, msgid, nid, inner_msg) => {
                let bm = (peer.clone(), msgid.clone(), nid.clone(), inner_msg.clone()); 
                (&self.broadcast_parser)( &bm)
            }
        }
    }
}

impl_all_fns!( PacketHandler, NetworkPacket);

