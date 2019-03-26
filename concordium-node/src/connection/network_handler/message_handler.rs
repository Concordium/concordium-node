use std::sync::{ Arc, RwLock };
use crate::network::{ NetworkMessage, NetworkRequest, NetworkResponse, NetworkPacket };
use crate::common::functor::{ AFunctor, AFunctorCW, FunctorResult };

pub type NetworkMessageCW = AFunctorCW<NetworkMessage>;
pub type NetworkRequestCW = AFunctorCW<NetworkRequest>;
pub type NetworkResponseCW = AFunctorCW<NetworkResponse>;
pub type NetworkPacketCW = AFunctorCW<NetworkPacket>;
pub type EmptyCW = AFunctorCW<()>;


/// It is a handler for `NetworkMessage`.
#[derive(Clone)]
pub struct MessageHandler {
    pub request_parser: AFunctor<NetworkRequest>,
    pub response_parser: AFunctor<NetworkResponse>,
    pub packet_parser: AFunctor<NetworkPacket>,
    pub unknown_parser: AFunctor<()>,
    pub invalid_parser: AFunctor<()>,

    pub general_parser: AFunctor<NetworkMessage>
}

impl MessageHandler {
    pub fn new() -> Self {

        MessageHandler {
            request_parser : AFunctor::<NetworkRequest>::new(
                    "Network::Request"),
            response_parser: AFunctor::<NetworkResponse>::new(
                    "Network::Response"),
            packet_parser: AFunctor::<NetworkPacket>::new(
                    "Network::Package"),
            unknown_parser: AFunctor::new(
                    "Network::Unknown"),
            invalid_parser: AFunctor::new(
                    "Network::Invalid"),
            general_parser: AFunctor::<NetworkMessage>::new(
                    "General NetworkMessage")
        }
    }

    pub fn add_request_callback( &mut self, callback: NetworkRequestCW) -> &mut Self {
        self.request_parser.add_callback( callback);
        self
    }

    pub fn add_response_callback( &mut self, callback: NetworkResponseCW) -> &mut Self {
        self.response_parser.add_callback( callback);
        self
    }

    pub fn add_packet_callback( &mut self, callback: NetworkPacketCW) -> &mut Self {
        self.packet_parser.add_callback( callback);
        self
    }

    pub fn add_unknown_callback( &mut self, callback: EmptyCW) -> &mut Self {
        self.unknown_parser.add_callback( callback);
        self
    }

    pub fn add_invalid_callback( &mut self, callback: EmptyCW ) -> &mut Self {
        self.invalid_parser.add_callback( callback);
        self
    }

    pub fn add_callback( &mut self, callback: NetworkMessageCW) -> &mut Self {
        self.general_parser.add_callback( callback);
        self
    }

    /// It merges into `this` all parsers from `other` `MessageHandler`.
    pub fn merge(&mut self, other: &MessageHandler) -> &mut Self
    {
        for  cb in other.general_parser.callbacks().iter() {
            self.add_callback( cb.clone());
        }

        for cb in other.packet_parser.callbacks().iter() {
            self.add_packet_callback( cb.clone());
        }

        for cb in other.response_parser.callbacks().iter() {
            self.add_response_callback( cb.clone());
        }

        for cb in other.request_parser.callbacks().iter() {
            self.add_request_callback( cb.clone());
        }

        for cb in other.unknown_parser.callbacks().iter() {
            self.add_unknown_callback( cb.clone());
        }

        for cb in other.invalid_parser.callbacks().iter() {
            self.add_invalid_callback( cb.clone());
        }
        self
    }

    fn process_message(&self, msg: &NetworkMessage) -> FunctorResult
    {
        // General
        let general_status = (&self.general_parser)(msg);

        // Specific
        let specific_status = match msg {
            NetworkMessage::NetworkRequest(ref nr, _, _) => {
                (&self.request_parser)( nr)
            },
            NetworkMessage::NetworkResponse(ref nr, _, _) => {
                (&self.response_parser)( nr)
            },
            NetworkMessage::NetworkPacket(ref np, _, _) => {
                (&self.packet_parser)( np)
            },
            NetworkMessage::UnknownMessage => {
                (&self.unknown_parser)( &())
            },
            NetworkMessage::InvalidMessage => {
                (&self.invalid_parser)( &())
            }
        };

        general_status.and( specific_status)
    }

}

impl_all_fns!( MessageHandler, NetworkMessage);

pub trait MessageManager {
    fn message_handler(&self) -> Arc< RwLock< MessageHandler>>;
}

#[cfg(test)]
mod message_handler_unit_test {
    use crate::connection::{ MessageHandler };
    use crate::network::{ NetworkMessage, NetworkRequest, NetworkResponse, NetworkPacket };
    use crate::common::functor::{ FunctorResult };

    use crate::common::{ ConnectionType, P2PPeerBuilder };
    use std::net::{ IpAddr, Ipv4Addr };
    use std::sync::{ Arc, Mutex };

    fn request_handler_func_1( _nr: &NetworkRequest) -> FunctorResult { Ok(()) }
    fn request_handler_func_2( _nr: &NetworkRequest) -> FunctorResult { Ok(()) }
    fn response_handler_func_1( _nr: &NetworkResponse) -> FunctorResult { Ok(()) }
    fn packet_handler_func_1( _np: &NetworkPacket) -> FunctorResult { Ok(()) }

    #[test]
    pub fn test_message_handler_mix() {
        let mut mh = MessageHandler::new();

        mh.add_request_callback( make_atomic_callback!( request_handler_func_1))
            .add_request_callback( make_atomic_callback!( request_handler_func_2))
            .add_request_callback( make_atomic_callback!( |_| { Ok(()) }))
            .add_response_callback( make_atomic_callback!( response_handler_func_1))
            .add_response_callback( make_atomic_callback!( response_handler_func_1))
            .add_packet_callback( make_atomic_callback!( |_| { Ok(()) }))
            .add_packet_callback( make_atomic_callback!( packet_handler_func_1));
        let mh_arc = Arc::new( mh);

        let ip = IpAddr::V4(Ipv4Addr::new(127,0,0,1));
        let p2p_peer = P2PPeerBuilder::default().connection_type(ConnectionType::Node).ip(ip).port(8080).build().unwrap();
        let msg = NetworkMessage::NetworkRequest( NetworkRequest::Ping( p2p_peer), None, None);

        (mh_arc)(&msg).unwrap();
    }
}

#[cfg(test)]
mod integration_test {
    use crate::connection::{ MessageHandler,  PacketHandler };
    use crate::common::{ ConnectionType, P2PPeerBuilder, P2PNodeId };
    use crate::network::{ NetworkMessage, NetworkRequest, NetworkResponse };
    use crate::network::packet::{ NetworkPacket as NetworkPacketEnum };
    use crate::common::functor::{ FunctorResult };

    use std::sync::{ Arc, Mutex };
    use std::net::{ IpAddr, Ipv4Addr };
    use std::sync::atomic::{ AtomicUsize, Ordering };

    static NETWORK_REQUEST_COUNTER: AtomicUsize = AtomicUsize::new(0);
    static NETWORK_RESPONSE_COUNTER: AtomicUsize = AtomicUsize::new(0);
    static NETWORK_PACKET_COUNTER: AtomicUsize = AtomicUsize::new(0);
    static NETWORK_PACKET_DIRECT_COUNTER: AtomicUsize = AtomicUsize::new(0);
    static NETWORK_PACKET_BROADCAST_COUNTER: AtomicUsize = AtomicUsize::new(0);

    // Test data for `on_network_request_handler`.
    pub fn network_request_handler_data() -> Vec<NetworkMessage> {
        let ip = IpAddr::V4(Ipv4Addr::new(127,0,0,1));
        let p2p_peer = P2PPeerBuilder::default().connection_type(ConnectionType::Node).ip(ip).port(8080).build().unwrap();
        let inner_msg: Vec<u8> = "Message XXX".to_string().as_bytes().to_vec();
        let node_id: P2PNodeId = P2PNodeId::from_ip_port( ip, 8080).unwrap();

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
    fn network_request_handler_1( _nr: &NetworkRequest) -> FunctorResult {
        Ok(())
    }

    /// Handler function for `NetworkRequest` elements. It only increases its counter.
    fn network_request_handler_2( _nr: &NetworkRequest) -> FunctorResult {
        NETWORK_REQUEST_COUNTER.fetch_add( 1, Ordering::SeqCst);
        Ok(())
    }

    /// Creates message handler for testing.
    fn make_message_handler() -> MessageHandler {

        let mut pkg_handler = PacketHandler::new();

        pkg_handler.add_direct_callback( make_atomic_callback!( |_pd: &NetworkPacketEnum| {
                NETWORK_PACKET_DIRECT_COUNTER.fetch_add( 1, Ordering::SeqCst);
                Ok(())
            }))
            .add_broadcast_callback( make_atomic_callback!( |_pb: &NetworkPacketEnum| {
                NETWORK_PACKET_BROADCAST_COUNTER.fetch_add( 1, Ordering::SeqCst);
                Ok(())
            }));

        let mut msg_handler = MessageHandler::new();

        msg_handler.add_request_callback( make_atomic_callback!( network_request_handler_1))
            .add_request_callback( make_atomic_callback!( network_request_handler_2))
            .add_request_callback( make_atomic_callback!( |_x: &NetworkRequest| {
                println!(
                    "Network Request {}",
                    NETWORK_REQUEST_COUNTER.load(Ordering::Relaxed));
                Ok(())
            }))
            .add_response_callback( make_atomic_callback!( |_x: &NetworkResponse| {
                NETWORK_RESPONSE_COUNTER.fetch_add( 1, Ordering::SeqCst);
                Ok(())
            }))
            .add_packet_callback( make_atomic_callback!( move |p: &NetworkPacketEnum| {
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
