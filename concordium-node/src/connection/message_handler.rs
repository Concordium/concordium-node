use std::sync::{ Arc, Mutex };

use connection::parse_handler::{ ParseHandler, ParseCallback, ParseCallbackResult };
use network::{ NetworkMessage, NetworkRequest, NetworkResponse, NetworkPacket };

/// It is a handler for `NetworkMessage`.
#[derive(Clone)]
pub struct MessageHandler {
    request_parser: ParseHandler<NetworkRequest>,
    response_parser: ParseHandler<NetworkResponse>,
    packet_parser: ParseHandler<NetworkPacket>,
}

impl MessageHandler {
    pub fn new() -> Self {

        MessageHandler {
            request_parser : ParseHandler::<NetworkRequest>::new( 
                    "Network Request Handler"),
            response_parser: ParseHandler::<NetworkResponse>::new( 
                    "Network Response Handler"),
            packet_parser: ParseHandler::<NetworkPacket>::new( 
                    "Network Package Handler"),
        }
    }

    pub fn add_request_callback(
            mut self, 
            callback: Arc< Mutex< Box< ParseCallback<NetworkRequest> > > > ) -> Self {
        self.request_parser = self.request_parser.add_callback( callback);
        self
    }

    pub fn add_response_callback(
            mut self, 
            callback: Arc< Mutex< Box< ParseCallback<NetworkResponse> > > > ) -> Self {
        self.response_parser = self.response_parser.add_callback( callback);
        self
    }
    
    pub fn add_packet_callback(
            mut self, 
            callback: Arc< Mutex< Box<ParseCallback<NetworkPacket> > > > ) -> Self {
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

impl_all_fns!( MessageHandler, NetworkMessage);

#[cfg(test)]
mod message_handler_unit_test {
    use connection::message_handler::{ MessageHandler, ParseCallbackResult };
    use network::{ NetworkMessage, NetworkRequest, NetworkResponse, NetworkPacket };
    
    use common::{ ConnectionType, P2PPeer };
    use std::net::{ IpAddr, Ipv4Addr };
    use std::sync::{ Arc, Mutex };

    fn request_handler_func_1( _nr: &NetworkRequest) -> ParseCallbackResult { Ok(()) }
    fn request_handler_func_2( _nr: &NetworkRequest) -> ParseCallbackResult { Ok(()) }
    fn response_handler_func_1( _nr: &NetworkResponse) -> ParseCallbackResult { Ok(()) }
    fn packet_handler_func_1( _np: &NetworkPacket) -> ParseCallbackResult { Ok(()) }

    #[test]
    pub fn test_message_handler_mix() {
        let mh = MessageHandler::new()
            .add_request_callback( make_callback!( request_handler_func_1))
            .add_request_callback( make_callback!( request_handler_func_2))
            .add_request_callback( make_callback!( |_| { Ok(()) })) 
            .add_response_callback( make_callback!( response_handler_func_1))
            .add_response_callback( make_callback!( response_handler_func_1))
            .add_packet_callback( make_callback!( |_| { Ok(()) }))
            .add_packet_callback( make_callback!( packet_handler_func_1));
        let mh_arc = Arc::new( mh);

        let ip = IpAddr::V4(Ipv4Addr::new(127,0,0,1));
        let p2p_peer = P2PPeer::new( ConnectionType::Node, ip, 8080);
        let msg = NetworkMessage::NetworkRequest( NetworkRequest::Ping( p2p_peer), None, None);

        (mh_arc)(&msg).unwrap();
    }
}

#[cfg(test)]
mod integration_test {
    use connection::message_handler::{ MessageHandler };
    use connection::packet_handler::{ PacketHandler, PacketHandlerDirect, PacketHandlerBroadcast };
    use connection::parse_handler::{ ParseCallbackResult };
    use common::{ ConnectionType, P2PPeer, P2PNodeId };
    use network::{ NetworkMessage, NetworkRequest, NetworkResponse };
    use network::packet::{ NetworkPacket as NetworkPacketEnum };

    use std::sync::{ Arc, Mutex };
    use std::net::{ IpAddr, Ipv4Addr };
    use std::sync::atomic::{ AtomicUsize, Ordering, ATOMIC_USIZE_INIT };

    static NETWORK_REQUEST_COUNTER: AtomicUsize = ATOMIC_USIZE_INIT;
    static NETWORK_RESPONSE_COUNTER: AtomicUsize = ATOMIC_USIZE_INIT;
    static NETWORK_PACKET_COUNTER: AtomicUsize = ATOMIC_USIZE_INIT;
    static NETWORK_PACKET_DIRECT_COUNTER: AtomicUsize = ATOMIC_USIZE_INIT;
    static NETWORK_PACKET_BROADCAST_COUNTER: AtomicUsize = ATOMIC_USIZE_INIT;

    // Test data for `on_network_request_handler`.
    pub fn network_request_handler_data() -> Vec<NetworkMessage> {
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
            .add_direct_callback( make_callback!( |_pd: &PacketHandlerDirect| {
                NETWORK_PACKET_DIRECT_COUNTER.fetch_add( 1, Ordering::SeqCst);
                Ok(())
            }))
            .add_broadcast_callback( make_callback!( |_pb: &PacketHandlerBroadcast| {
                NETWORK_PACKET_BROADCAST_COUNTER.fetch_add( 1, Ordering::SeqCst);
                Ok(())
            }));

        let msg_handler = MessageHandler::new()
            .add_request_callback( make_callback!( network_request_handler_1))
            .add_request_callback( make_callback!( network_request_handler_2))
            .add_request_callback( make_callback!( |_x: &NetworkRequest| { 
                println!( 
                    "Network Request {}", 
                    NETWORK_REQUEST_COUNTER.load(Ordering::Relaxed)); 
                Ok(()) 
            }))
            .add_response_callback( make_callback!( |_x: &NetworkResponse| { 
                NETWORK_RESPONSE_COUNTER.fetch_add( 1, Ordering::SeqCst); 
                Ok(())
            }))
            .add_packet_callback( make_callback!( move |p: &NetworkPacketEnum| {
                NETWORK_PACKET_COUNTER.fetch_add( 1, Ordering::SeqCst);
                (pkg_handler)(p)
            }));

        msg_handler 
    }


    #[test]
    pub fn network_request_handler() {
        let mh: MessageHandler = make_message_handler();

        for message in network_request_handler_data() {
            let _status = (&mh)( &message);
        }

        assert_eq!( NETWORK_REQUEST_COUNTER.load(Ordering::Relaxed), 2);
        assert_eq!( NETWORK_RESPONSE_COUNTER.load(Ordering::Relaxed), 1);
        assert_eq!( NETWORK_PACKET_COUNTER.load(Ordering::Relaxed), 2);
        assert_eq!( NETWORK_PACKET_BROADCAST_COUNTER.load(Ordering::Relaxed), 1);
        assert_eq!( NETWORK_PACKET_DIRECT_COUNTER.load(Ordering::Relaxed), 1);

        for message in network_request_handler_data() {
            let _status = (&mh)( &message);
        }
        
        assert_eq!( NETWORK_REQUEST_COUNTER.load(Ordering::Relaxed), 4);
        assert_eq!( NETWORK_RESPONSE_COUNTER.load(Ordering::Relaxed), 2);
        assert_eq!( NETWORK_PACKET_COUNTER.load(Ordering::Relaxed), 4);
        assert_eq!( NETWORK_PACKET_BROADCAST_COUNTER.load(Ordering::Relaxed), 2);
        assert_eq!( NETWORK_PACKET_DIRECT_COUNTER.load(Ordering::Relaxed), 2);
    }
}
