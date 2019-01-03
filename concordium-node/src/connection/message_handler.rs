use network::{ NetworkMessage, NetworkRequest, NetworkResponse, NetworkPacket };
use common::{ P2PPeer, P2PNodeId };

macro_rules! run_callbacks{
    ($handlers:expr, $message:expr, $errorMsg: expr) => {
        $handlers.iter()
            .map( |handler| { handler($message) })
            .fold( Ok(()), |status, handler_result|{
                match handler_result {
                    Err(e) => Err(e),
                    Ok(_) => status
                }
            })
            .map_err( |_| $errorMsg.to_string())
    }
}

pub type ParseCallbackResult = Result<(), String>;
pub type ParseCallback<T> = fn(&T) -> ParseCallbackResult;

#[derive(Clone)]
pub struct ParseHandler<T> {
    pub error_msg: String,
    pub callbacks: Vec< ParseCallback<T> >
}

impl<T> ParseHandler<T> {
    pub fn new( error_msg: String ) -> Self {
        ParseHandler::<T> {
            error_msg: error_msg.clone(),
            callbacks: Vec::new() 
        }
    }

    pub fn add_callback(mut self, callback: ParseCallback<T> ) -> Self {
        self.callbacks.push( callback );
        self
    }
}

impl<T> FnOnce<(&T,)> for ParseHandler<T> {
    type Output = ParseCallbackResult;
    extern "rust-call" fn call_once(self, args: (&T,)) -> ParseCallbackResult
    {
        let msg: &T = args.0;
        run_callbacks!( &self.callbacks, msg, &self.error_msg)
    }
}

impl<T> FnMut<(&T,)> for ParseHandler<T> {
    extern "rust-call" fn call_mut(&mut self, args: (&T,)) -> ParseCallbackResult
    {
        let msg: &T = args.0;
        run_callbacks!( &self.callbacks, msg, &self.error_msg)
    }
}

impl<T> Fn<(&T,)> for ParseHandler<T> {
    extern "rust-call" fn call(&self, args: (&T,)) -> ParseCallbackResult
    {
        let msg: &T = args.0;
        run_callbacks!( &self.callbacks, msg, &self.error_msg)
    }
}



#[derive(Clone)]
pub struct MessageHandler {
    request_parser: ParseHandler<NetworkRequest>,
    response_parser: ParseHandler<NetworkResponse>,
    packet_parser: ParseHandler<NetworkPacket> 
}

impl MessageHandler {
    pub fn new() -> Self {
        MessageHandler {
            request_parser : ParseHandler::<NetworkRequest>::new( 
                    "Network Request Handler".to_string()),
            response_parser: ParseHandler::<NetworkResponse>::new( 
                    "Network Response Handler".to_string()),
            packet_parser: ParseHandler::<NetworkPacket>::new( 
                    "Network Package Handler".to_string())
        }
    }

    pub fn add_request_callback(mut self, callback: ParseCallback<NetworkRequest> ) -> Self {
        self.request_parser = self.request_parser.add_callback( callback);
        self
    }

    pub fn add_response_callback(mut self, callback: ParseCallback<NetworkResponse>) -> Self {
        self.response_parser = self.response_parser.add_callback( callback);
        self
    }
    
    pub fn add_packet_callback(mut self, callback: ParseCallback<NetworkPacket>) -> Self {
        self.packet_parser = self.packet_parser.add_callback( callback);
        self
    }

    fn process_message(&self, msg: &NetworkMessage) -> ParseCallbackResult {
        match msg {
            NetworkMessage::NetworkRequest(ref nr, _, _) => {
                (&self.request_parser)( nr)
            },
            NetworkMessage::NetworkResponse(ref nr, _, _) => {
                (&self.response_parser)( nr)
            },
            NetworkMessage::NetworkPacket(ref np, _, _) => {
                (&self.packet_parser)( np)
            }
            NetworkMessage::UnknownMessage | NetworkMessage::InvalidMessage => {
                Ok(())
            }
        }
    }
}

impl FnOnce<(&NetworkMessage,)> for MessageHandler {
    type Output = ParseCallbackResult;

    extern "rust-call" fn call_once(self, args: (&NetworkMessage,)) -> ParseCallbackResult
    {
        let msg: &NetworkMessage = args.0;
        self.process_message( msg)
    }
}

impl FnMut<(&NetworkMessage,)> for MessageHandler {
    extern "rust-call" fn call_mut(&mut self, args: (&NetworkMessage,)) -> ParseCallbackResult
    {
        let msg: &NetworkMessage = args.0;
        self.process_message( msg)
    }
}

impl Fn<(&NetworkMessage,)> for MessageHandler {
    extern "rust-call" fn call(&self, args: (&NetworkMessage,)) -> ParseCallbackResult
    {
        let msg: &NetworkMessage = args.0;
        self.process_message( msg)
    }
}


type PacketHandlerDirect = (P2PPeer, String, P2PNodeId, u16, Vec<u8>);
type PacketHandlerBroadcast = (P2PPeer, String, u16, Vec<u8>);

pub struct PacketHandler {
    pub direct_parser: ParseHandler< PacketHandlerDirect >,
    pub broadcast_parser: ParseHandler< PacketHandlerBroadcast >
}

impl PacketHandler {
    
    pub fn new() -> Self {
        PacketHandler {
            direct_parser: ParseHandler::<PacketHandlerDirect>::new( 
                    "Network Packet Direct Handler".to_string()), 
            broadcast_parser: ParseHandler::<PacketHandlerBroadcast>::new(
                    "Network Packet Broadcast Handler".to_string())
        }
    }

    pub fn add_direct_callback( mut self, callback: ParseCallback<PacketHandlerDirect>) -> Self {
        self.direct_parser = self.direct_parser.add_callback( callback);
        self
    }
    
    pub fn add_broadcast_callback( mut self, callback: ParseCallback<PacketHandlerBroadcast>) -> Self {
        self.broadcast_parser = self.broadcast_parser.add_callback( callback);
        self
    }

    
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

impl FnOnce<(&NetworkPacket,)> for PacketHandler {
    type Output = ParseCallbackResult;

    extern "rust-call" fn call_once(self, args: (&NetworkPacket,)) -> ParseCallbackResult
    {
        let msg = args.0;
        self.process_message( msg)
    }
}

impl FnMut<(&NetworkPacket,)> for PacketHandler {
    extern "rust-call" fn call_mut(&mut self, args: (&NetworkPacket,)) -> ParseCallbackResult
    {
        let msg = args.0;
        self.process_message( msg)
    }
}

impl Fn<(&NetworkPacket,)> for PacketHandler {
    extern "rust-call" fn call(&self, args: (&NetworkPacket,)) -> ParseCallbackResult
    {
        let msg = args.0;
        self.process_message( msg)
    }
}






#[cfg(test)]
mod tests {
    use connection::message_handler::{ MessageHandler, PacketHandler, PacketHandlerDirect, PacketHandlerBroadcast, ParseCallbackResult };
    use common::{ ConnectionType, P2PPeer, P2PNodeId };
    use network::{ NetworkMessage, NetworkRequest, NetworkResponse };
    use network::packet::{ NetworkPacket as NetworkPacketEnum };

    use std::net::{ IpAddr, Ipv4Addr };
    use std::sync::atomic::{ AtomicUsize, Ordering, ATOMIC_USIZE_INIT };

    static NETWORK_REQUEST_COUNTER: AtomicUsize = ATOMIC_USIZE_INIT;
    static NETWORK_RESPONSE_COUNTER: AtomicUsize = ATOMIC_USIZE_INIT;
    static NETWORK_PACKET_COUNTER: AtomicUsize = ATOMIC_USIZE_INIT;
    static NETWORK_PACKET_DIRECT_COUNTER: AtomicUsize = ATOMIC_USIZE_INIT;
    static NETWORK_PACKET_BROADCAST_COUNTER: AtomicUsize = ATOMIC_USIZE_INIT;

    // Test data for `on_network_request_handler`.
    pub fn on_network_request_handler_data() -> Vec<NetworkMessage> {
        let ip = IpAddr::V4(Ipv4Addr::new(127,0,0,1));
        let p2p_peer = P2PPeer::new( ConnectionType::Node, ip, 8080);
        let inner_msg: Vec<u8> = "Message XXX".to_string().as_bytes().to_vec();
        let node_id: P2PNodeId = P2PNodeId::from_ip_port( ip, 8080);

        let data = vec![
            NetworkMessage::NetworkRequest( NetworkRequest::Ping( p2p_peer.clone()), Some(100), Some(42)),
            NetworkMessage::NetworkRequest( NetworkRequest::Ping( p2p_peer.clone()), None, None),
            NetworkMessage::NetworkResponse( NetworkResponse::Pong( p2p_peer.clone()), None, None),
            NetworkMessage::NetworkPacket( 
                NetworkPacketEnum::BroadcastedMessage( p2p_peer.clone(), "MSG-ID-1".to_string(), 
                        100 as u16, inner_msg.clone()),
                None, None),
            NetworkMessage::NetworkPacket(
                NetworkPacketEnum::DirectMessage( p2p_peer.clone(), "MSG-ID-2".to_string(),
                        node_id, 100 as u16, inner_msg.clone()),
                None, None)
        ];

        data
    }

    /// Handler function for `NetworkRequest` elements that does nothing.
    fn network_request_handler_1( _nr: &NetworkRequest) -> ParseCallbackResult {
        Ok(())
    }

    /// Handler function for `NetworkRequest` elements. It only increases its counter.
    fn network_request_handler_2( _nr: &NetworkRequest) -> ParseCallbackResult {
        NETWORK_REQUEST_COUNTER.fetch_add( 1, Ordering::SeqCst);
        Ok(())
    }

    /// Creates message handler for testing.
    fn make_message_handler() -> MessageHandler {
        
        let pkg_handler = PacketHandler::new()
            .add_direct_callback( |pd: &PacketHandlerDirect| {
                NETWORK_PACKET_DIRECT_COUNTER.fetch_add( 1, Ordering::SeqCst);
                Ok(())
            })
            .add_broadcast_callback( |pb: &PacketHandlerBroadcast| {
                NETWORK_PACKET_BROADCAST_COUNTER.fetch_add( 1, Ordering::SeqCst);
                Ok(())
            });
 
        let msg_handler = MessageHandler::new()
            .add_request_callback( network_request_handler_1)
            .add_request_callback( network_request_handler_2)
            .add_request_callback( |_x: &NetworkRequest| { 
                println!( 
                    "Network Request {}", 
                    NETWORK_REQUEST_COUNTER.load(Ordering::Relaxed)); 
                Ok(()) 
            })
            .add_response_callback(|_x: &NetworkResponse| { 
                NETWORK_RESPONSE_COUNTER.fetch_add( 1, Ordering::SeqCst); 
                Ok(())
            })
            .add_packet_callback( |p: &NetworkPacketEnum| {
                NETWORK_PACKET_COUNTER.fetch_add( 1, Ordering::SeqCst);
                // (pkg_handler)(p)
                Ok(())
            });

        msg_handler 
    }


    #[test]
    pub fn on_network_request_handler() {
        let mh: MessageHandler = make_message_handler();

        for message in on_network_request_handler_data() {
            let _status = (&mh)( &message);
        }

        assert_eq!( NETWORK_REQUEST_COUNTER.load(Ordering::Relaxed), 2);
        assert_eq!( NETWORK_RESPONSE_COUNTER.load(Ordering::Relaxed), 1);
        assert_eq!( NETWORK_PACKET_COUNTER.load(Ordering::Relaxed), 2);
        assert_eq!( NETWORK_PACKET_BROADCAST_COUNTER.load(Ordering::Relaxed), 1);
        assert_eq!( NETWORK_PACKET_DIRECT_COUNTER.load(Ordering::Relaxed), 1);

        for message in on_network_request_handler_data() {
            let _status = (&mh)( &message);
        }
        
        assert_eq!( NETWORK_REQUEST_COUNTER.load(Ordering::Relaxed), 4);
        assert_eq!( NETWORK_RESPONSE_COUNTER.load(Ordering::Relaxed), 2);
    }
}


