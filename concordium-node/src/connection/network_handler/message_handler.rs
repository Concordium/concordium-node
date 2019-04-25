use crate::{
    common::functor::{AFunctor, AFunctorCW, FunctorResult},
    network::{NetworkMessage, NetworkPacket, NetworkRequest, NetworkResponse},
};
use std::{
    rc::Rc,
    sync::{Arc, RwLock},
};

pub type NetworkMessageCW = AFunctorCW<NetworkMessage>;
pub type NetworkRequestCW = AFunctorCW<NetworkRequest>;
pub type NetworkResponseCW = AFunctorCW<NetworkResponse>;
pub type NetworkPacketCW = AFunctorCW<NetworkPacket>;
pub type EmptyCW = AFunctorCW<()>;

/// It is a handler for `NetworkMessage`.
#[derive(Clone)]
pub struct MessageHandler {
    request_parser:  AFunctor<NetworkRequest>,
    response_parser: AFunctor<NetworkResponse>,
    packet_parser:   AFunctor<NetworkPacket>,
    invalid_handler: Rc<(Fn() -> FunctorResult)>,
    unknown_handler: Rc<(Fn() -> FunctorResult)>,

    general_parser: AFunctor<NetworkMessage>,
}

impl Default for MessageHandler {
    fn default() -> Self { MessageHandler::new() }
}

impl MessageHandler {
    pub fn new() -> Self {
        MessageHandler {
            request_parser:  AFunctor::<NetworkRequest>::new("Network::Request"),
            response_parser: AFunctor::<NetworkResponse>::new("Network::Response"),
            packet_parser:   AFunctor::<NetworkPacket>::new("Network::Package"),
            general_parser:  AFunctor::<NetworkMessage>::new("General NetworkMessage"),
            invalid_handler: Rc::new(|| Ok(())),
            unknown_handler: Rc::new(|| Ok(())),
        }
    }

    pub fn add_request_callback(&mut self, callback: NetworkRequestCW) -> &mut Self {
        self.request_parser.add_callback(callback);
        self
    }

    pub fn add_response_callback(&mut self, callback: NetworkResponseCW) -> &mut Self {
        self.response_parser.add_callback(callback);
        self
    }

    pub fn add_packet_callback(&mut self, callback: NetworkPacketCW) -> &mut Self {
        self.packet_parser.add_callback(callback);
        self
    }

    pub fn add_callback(&mut self, callback: NetworkMessageCW) -> &mut Self {
        self.general_parser.add_callback(callback);
        self
    }

    pub fn set_invalid_handler(&mut self, func: Rc<(Fn() -> FunctorResult)>) -> &mut Self {
        self.invalid_handler = func;
        self
    }

    pub fn set_unknown_handler(&mut self, func: Rc<(Fn() -> FunctorResult)>) -> &mut Self {
        self.unknown_handler = func;
        self
    }

    /// It merges into `this` all parsers from `other` `MessageHandler`.
    pub fn merge(&mut self, other: &MessageHandler) -> &mut Self {
        for cb in other.general_parser.callbacks().iter() {
            self.add_callback(Arc::clone(&cb));
        }

        for cb in other.packet_parser.callbacks().iter() {
            self.add_packet_callback(Arc::clone(&cb));
        }

        for cb in other.response_parser.callbacks().iter() {
            self.add_response_callback(Arc::clone(&cb));
        }

        for cb in other.request_parser.callbacks().iter() {
            self.add_request_callback(Arc::clone(&cb));
        }

        self
    }

    pub fn process_message(&self, msg: &NetworkMessage) -> FunctorResult {
        // General
        let general_status = self.general_parser.run_callbacks(msg);

        // Specific
        let specific_status = match msg {
            NetworkMessage::NetworkRequest(ref nr, _, _) => self.request_parser.run_callbacks(nr),
            NetworkMessage::NetworkResponse(ref nr, _, _) => self.response_parser.run_callbacks(nr),
            NetworkMessage::NetworkPacket(ref np, _, _) => self.packet_parser.run_callbacks(np),
            NetworkMessage::UnknownMessage => (self.unknown_handler)(),
            NetworkMessage::InvalidMessage => (self.invalid_handler)(),
        };

        general_status.and(specific_status)
    }
}

pub trait MessageManager {
    fn message_handler(&self) -> Arc<RwLock<MessageHandler>>;
}

#[cfg(test)]
mod message_handler_unit_test {
    use crate::{
        common::functor::FunctorResult,
        connection::MessageHandler,
        network::{NetworkMessage, NetworkPacket, NetworkRequest, NetworkResponse},
    };

    use crate::common::{P2PPeerBuilder, PeerType};
    use std::{
        net::{IpAddr, Ipv4Addr, SocketAddr},
        sync::{Arc, RwLock},
    };

    fn request_handler_func_1(_nr: &NetworkRequest) -> FunctorResult { Ok(()) }
    fn request_handler_func_2(_nr: &NetworkRequest) -> FunctorResult { Ok(()) }
    fn response_handler_func_1(_nr: &NetworkResponse) -> FunctorResult { Ok(()) }
    fn packet_handler_func_1(_np: &NetworkPacket) -> FunctorResult { Ok(()) }

    #[test]
    pub fn test_message_handler_mix() {
        let mut mh = MessageHandler::new();

        mh.add_request_callback(make_atomic_callback!(request_handler_func_1))
            .add_request_callback(make_atomic_callback!(request_handler_func_2))
            .add_request_callback(make_atomic_callback!(|_| { Ok(()) }))
            .add_response_callback(make_atomic_callback!(response_handler_func_1))
            .add_response_callback(make_atomic_callback!(response_handler_func_1))
            .add_packet_callback(make_atomic_callback!(|_| { Ok(()) }))
            .add_packet_callback(make_atomic_callback!(packet_handler_func_1));
        let mh_arc = Arc::new(mh);

        let ip = IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1));
        let p2p_peer = P2PPeerBuilder::default()
            .peer_type(PeerType::Node)
            .addr(SocketAddr::new(ip, 8080))
            .build()
            .unwrap();
        let msg = NetworkMessage::NetworkRequest(NetworkRequest::Ping(p2p_peer), None, None);

        mh_arc.process_message(&msg).unwrap();
    }
}

#[cfg(test)]
mod integration_test {
    use crate::{
        common::{functor::FunctorResult, P2PNodeId, P2PPeerBuilder, PeerType, UCursor},
        connection::{MessageHandler, PacketHandler},
        network::{
            NetworkId, NetworkMessage, NetworkPacket as NetworkPacketEnum, NetworkPacketBuilder,
            NetworkRequest, NetworkResponse,
        },
    };

    use std::{
        net::{IpAddr, Ipv4Addr, SocketAddr},
        sync::{
            atomic::{AtomicUsize, Ordering},
            Arc, RwLock,
        },
    };

    static NETWORK_REQUEST_COUNTER: AtomicUsize = AtomicUsize::new(0);
    static NETWORK_RESPONSE_COUNTER: AtomicUsize = AtomicUsize::new(0);
    static NETWORK_PACKET_COUNTER: AtomicUsize = AtomicUsize::new(0);
    static NETWORK_PACKET_DIRECT_COUNTER: AtomicUsize = AtomicUsize::new(0);
    static NETWORK_PACKET_BROADCAST_COUNTER: AtomicUsize = AtomicUsize::new(0);

    // Test data for `on_network_request_handler`.
    pub fn network_request_handler_data() -> Vec<NetworkMessage> {
        let ip = IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1));
        let p2p_peer = P2PPeerBuilder::default()
            .peer_type(PeerType::Node)
            .addr(SocketAddr::new(ip, 8080))
            .build()
            .unwrap();
        let inner_msg = UCursor::from(b"Message XXX".to_vec());
        let node_id = P2PNodeId::default();

        let data = vec![
            NetworkMessage::NetworkRequest(
                NetworkRequest::Ping(p2p_peer.clone()),
                Some(100),
                Some(42),
            ),
            NetworkMessage::NetworkRequest(NetworkRequest::Ping(p2p_peer.clone()), None, None),
            NetworkMessage::NetworkResponse(NetworkResponse::Pong(p2p_peer.clone()), None, None),
            NetworkMessage::NetworkPacket(
                NetworkPacketBuilder::default()
                    .peer(p2p_peer.clone())
                    .message_id("MSG-ID-1".to_string())
                    .network_id(NetworkId::from(100))
                    .message(Box::new(inner_msg.clone()))
                    .build_broadcast()
                    .unwrap(),
                None,
                None,
            ),
            NetworkMessage::NetworkPacket(
                NetworkPacketBuilder::default()
                    .peer(p2p_peer)
                    .message_id("MSG-ID-2".to_string())
                    .network_id(NetworkId::from(100))
                    .message(Box::new(inner_msg))
                    .build_direct(node_id)
                    .unwrap(),
                None,
                None,
            ),
        ];

        data
    }

    /// Handler function for `NetworkRequest` elements that does nothing.
    fn network_request_handler_1(_nr: &NetworkRequest) -> FunctorResult { Ok(()) }

    /// Handler function for `NetworkRequest` elements. It only increases its
    /// counter.
    fn network_request_handler_2(_nr: &NetworkRequest) -> FunctorResult {
        NETWORK_REQUEST_COUNTER.fetch_add(1, Ordering::SeqCst);
        Ok(())
    }

    /// Creates message handler for testing.
    fn make_message_handler() -> MessageHandler {
        let mut pkg_handler = PacketHandler::new();

        pkg_handler
            .add_direct_callback(make_atomic_callback!(|_pd: &NetworkPacketEnum| {
                NETWORK_PACKET_DIRECT_COUNTER.fetch_add(1, Ordering::SeqCst);
                Ok(())
            }))
            .add_broadcast_callback(make_atomic_callback!(|_pb: &NetworkPacketEnum| {
                NETWORK_PACKET_BROADCAST_COUNTER.fetch_add(1, Ordering::SeqCst);
                Ok(())
            }));

        let mut msg_handler = MessageHandler::new();

        msg_handler
            .add_request_callback(make_atomic_callback!(network_request_handler_1))
            .add_request_callback(make_atomic_callback!(network_request_handler_2))
            .add_request_callback(make_atomic_callback!(|_x: &NetworkRequest| {
                println!(
                    "Network Request {}",
                    NETWORK_REQUEST_COUNTER.load(Ordering::Relaxed)
                );
                Ok(())
            }))
            .add_response_callback(make_atomic_callback!(|_x: &NetworkResponse| {
                NETWORK_RESPONSE_COUNTER.fetch_add(1, Ordering::SeqCst);
                Ok(())
            }))
            .add_packet_callback(make_atomic_callback!(move |p: &NetworkPacketEnum| {
                NETWORK_PACKET_COUNTER.fetch_add(1, Ordering::SeqCst);
                pkg_handler.process_message(p)
            }));

        msg_handler
    }

    #[test]
    pub fn network_request_handler() {
        let mh: MessageHandler = make_message_handler();

        for message in network_request_handler_data() {
            let _status = mh.process_message(&message);
        }

        assert_eq!(NETWORK_REQUEST_COUNTER.load(Ordering::Relaxed), 2);
        assert_eq!(NETWORK_RESPONSE_COUNTER.load(Ordering::Relaxed), 1);
        assert_eq!(NETWORK_PACKET_COUNTER.load(Ordering::Relaxed), 2);
        assert_eq!(NETWORK_PACKET_BROADCAST_COUNTER.load(Ordering::Relaxed), 1);
        assert_eq!(NETWORK_PACKET_DIRECT_COUNTER.load(Ordering::Relaxed), 1);

        for message in network_request_handler_data() {
            let _status = mh.process_message(&message);
        }

        assert_eq!(NETWORK_REQUEST_COUNTER.load(Ordering::Relaxed), 4);
        assert_eq!(NETWORK_RESPONSE_COUNTER.load(Ordering::Relaxed), 2);
        assert_eq!(NETWORK_PACKET_COUNTER.load(Ordering::Relaxed), 4);
        assert_eq!(NETWORK_PACKET_BROADCAST_COUNTER.load(Ordering::Relaxed), 2);
        assert_eq!(NETWORK_PACKET_DIRECT_COUNTER.load(Ordering::Relaxed), 2);
    }
}
