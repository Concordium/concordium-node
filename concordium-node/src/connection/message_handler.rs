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
    pub fn new( error_msg: &String ) -> Self {
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


#[derive(Clone)]
pub struct MessageHandler {
    request_parser: ParseHandler<NetworkRequest>,
    response_parser: ParseHandler<NetworkResponse>,
    pub packet_parser: ParseHandler<NetworkPacket> 
}

impl MessageHandler {
    pub fn new(
            request_parser: ParseHandler<NetworkRequest>,
            response_parser: ParseHandler<NetworkResponse>,
            packet_parser: ParseHandler<NetworkPacket>,
        ) -> Self {
        MessageHandler {
            request_parser : request_parser,
            response_parser: response_parser,
            packet_parser: packet_parser
        }
    }
}

impl FnOnce<(&NetworkMessage,)> for MessageHandler {
    type Output = ParseCallbackResult;

    extern "rust-call" fn call_once(self, args: (&NetworkMessage,)) -> ParseCallbackResult
    {
        let msg: &NetworkMessage = args.0;
        let status :ParseCallbackResult = match msg {
            NetworkMessage::NetworkRequest(ref nr, _, _) => {
                (self.request_parser)( nr)
            },
            NetworkMessage::NetworkResponse(ref nr, _, _) => {
                (self.response_parser)( nr)
            },
            NetworkMessage::NetworkPacket(ref np, _, _) => {
                (self.packet_parser)( np)
            }
            NetworkMessage::UnknownMessage | NetworkMessage::InvalidMessage => {
                Ok(())
            }
        };

        status
    }
}

/*
struct PacketHandlerDirect(P2PPeer, String, P2PNodeId, u16, Vec<u8>);

pub struct PacketHandler {
    pub direct_parser: ParseHandler< PacketHandlerDirect >,
    // pub broadcast_parser: ParseHandler<(P2PPeer, String, u16, Vec<u8>)>,
}

impl PacketHandler {
    
    pub fn new() -> Self {
        let dp = ParseHandler< PacketHandlerDirect >::new();
        PacketHandler {
            direct_parser: dp
            //broadcast_parser: ParseHandler<(P2PPeer, String, u16, Vec<u8>)>::new()
        }
    }
}

impl FnOnce<(&NetworkPacket,)> for PacketHandler {
    type Output = ParseCallbackResult;


    extern "rust-call" fn call_once(self, args: (&NetworkPacket,)) -> ParseCallbackResult {
        let msg: &NetworkPacket = args.0;
        let status: ParseCallbackResult = match msg {
            NetworkPacket::DirectMessage(peer, msg_id, node_id, nid, inner_msg) => {
                let dm_args = ( peer.clone(), msg_id.clone(), node_id.clone(), nid.clone(), inner_msg.clone());
                (self.direct_parser)(&dm_args)
            },
            NetworkPacket::BroadcastedMessage(peer, msg_id, nid, inner_msg) => {
                let bm_args = ( peer.clone(), msg_id.clone(), nid.clone(), inner_msg.clone());
                (self.broadcast_parser)(&bm_args)
            }
        };

        status
    }
}*/


#[cfg(test)]
mod tests {
    use connection::message_handler::{ MessageHandler, ParseHandler, ParseCallbackResult };
    use common::{ ConnectionType, P2PPeer };
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
        let p2p_peer = P2PPeer::new( ConnectionType::Node, IpAddr::V4(Ipv4Addr::new(127,0,0,1)), 8080);
        let data = vec![
            NetworkMessage::NetworkRequest( NetworkRequest::Ping( p2p_peer.clone()), Some(100), Some(42)),
            NetworkMessage::NetworkRequest( NetworkRequest::Ping( p2p_peer.clone()), None, None),
            NetworkMessage::NetworkResponse( NetworkResponse::Pong( p2p_peer.clone()), None, None)
        ];

        data
    }

    /// Handler function for `NetworkRequest` elements that does nothing.
    fn network_request_handler_1( nr: &NetworkRequest) -> ParseCallbackResult {
        Ok(())
    }

    /// Handler function for `NetworkRequest` elements. It only increases its counter.
    fn network_request_handler_2( nr: &NetworkRequest) -> ParseCallbackResult {
        NETWORK_REQUEST_COUNTER.fetch_add( 1, Ordering::SeqCst);
        Ok(())
    }

    /// Creates message handler for testing.
    fn make_message_handler() -> MessageHandler {

        let req_handlers = vec![
            ParseHandler<NetworkRequest>::new().add_callback( network_request_handler_1),
            ParseHandler<NetworkRequest>::new().add_callback( network_request_handler_2),
            ParseHandler<NetworkRequest>::new().add_callback( 
                |x: &NetworkRequest| { 
                    println!("Network Request {}", NETWORK_REQUEST_COUNTER.load(Ordering::Relaxed)); 
                    Ok(())
            })
        ];
        let response_handlers = vec![
            ParseHandler<NetworkResponse>::new().add_callback( |x: &NetworkResponse| { 
                NETWORK_RESPONSE_COUNTER.fetch_add( 1, Ordering::SeqCst); 
                Ok(())
            })
        ];
        let packet_handlers = vec![
            ParseHandler<NetworkPacket>::new().add_callback( |p: &NetworkPacketEnum| {
                NETWORK_PACKET_COUNTER.fetch_add( 1, Ordering::SeqCst);
                Ok(p)
            }),
        ];
        
        MessageHandler::new( req_handlers, response_handlers, packet_handlers)
    }


    #[test]
    pub fn on_network_request_handler() {
        let mh: MessageHandler = make_message_handler();

        for test_data in on_network_request_handler_data() {
            let( message, exp_result) = test_data;
            let status = mh.call( &message);
            // assert_eq!( status, exp_result); 
        }

        assert_eq!( NETWORK_REQUEST_COUNTER.load(Ordering::Relaxed), 2);
        assert_eq!( NETWORK_RESPONSE_COUNTER.load(Ordering::Relaxed), 1);
    }
}


