use crate::network::NetworkMessage;
use concordium_common::{
    fails::FunctorError,
    functor::{FuncResult, FunctorResult, UnitFunction, UnitFunctor},
};
use std::sync::{Arc, RwLock};

pub type EmptyCW = UnitFunction<()>;
pub type EmptyFunction = Arc<(Fn() -> FuncResult<()> + Send + Sync + 'static)>;

/// It is a handler for `NetworkMessage`.
#[derive(Clone)]
pub struct MessageHandler {
    valid_handler:   UnitFunctor<NetworkMessage>,
    invalid_handler: Arc<RwLock<EmptyFunction>>,
}

impl Default for MessageHandler {
    fn default() -> Self { MessageHandler::new() }
}

impl MessageHandler {
    pub fn new() -> Self {
        MessageHandler {
            valid_handler:   UnitFunctor::<NetworkMessage>::new(),
            invalid_handler: Arc::new(RwLock::new(Arc::new(|| Ok(())))),
        }
    }

    pub fn add_callback(&self, callback: UnitFunction<NetworkMessage>) -> &Self {
        self.valid_handler.add_callback(callback);
        self
    }

    pub fn set_invalid_handler(&self, func: EmptyFunction) -> &Self {
        *write_or_die!(self.invalid_handler) = func;
        self
    }

    /// It merges into `this` all parsers from `other` `MessageHandler`.
    pub fn add(&self, other: &MessageHandler) -> &Self {
        for cb in read_or_die!(other.valid_handler.callbacks()).iter() {
            self.add_callback(cb.clone());
        }

        self
    }

    pub fn process_message(&self, msg: &NetworkMessage) -> FunctorResult<()> {
        match msg {
            NetworkMessage::NetworkRequest(..)
            | NetworkMessage::NetworkResponse(..)
            | NetworkMessage::NetworkPacket(..) => self.valid_handler.run_callbacks(msg)?,
            NetworkMessage::InvalidMessage => {
                (read_or_die!(self.invalid_handler))().map_err(|x| FunctorError::from(vec![x]))?
            }
        };

        Ok(())
    }
}

#[cfg(test)]
mod message_handler_unit_test {
    use crate::{
        common::{P2PPeerBuilder, PeerType},
        connection::MessageHandler,
        network::{NetworkMessage, NetworkRequest},
    };
    use concordium_common::functor::FuncResult;
    use std::{
        net::{IpAddr, Ipv4Addr, SocketAddr},
        sync::Arc,
    };

    fn request_handler_func_1(_nr: &NetworkMessage) -> FuncResult<()> { Ok(()) }
    fn request_handler_func_2(_nr: &NetworkMessage) -> FuncResult<()> { Ok(()) }
    fn response_handler_func_1(_nr: &NetworkMessage) -> FuncResult<()> { Ok(()) }
    fn packet_handler_func_1(_np: &NetworkMessage) -> FuncResult<()> { Ok(()) }

    #[test]
    pub fn test_message_handler_mix() {
        let mh = MessageHandler::new();

        mh.add_callback(make_atomic_callback!(request_handler_func_1))
            .add_callback(make_atomic_callback!(request_handler_func_2))
            .add_callback(make_atomic_callback!(|_| { Ok(()) }))
            .add_callback(make_atomic_callback!(response_handler_func_1))
            .add_callback(make_atomic_callback!(response_handler_func_1))
            .add_callback(make_atomic_callback!(|_| { Ok(()) }))
            .add_callback(make_atomic_callback!(packet_handler_func_1));
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
        common::{P2PNodeId, P2PPeerBuilder, PeerType},
        connection::MessageHandler,
        network::{
            packet::MessageId, NetworkId, NetworkMessage, NetworkPacket, NetworkPacketType,
            NetworkRequest, NetworkResponse,
        },
    };
    use concordium_common::hybrid_buf::HybridBuf;

    use std::{
        convert::TryFrom,
        net::{IpAddr, Ipv4Addr, SocketAddr},
        sync::{
            atomic::{AtomicUsize, Ordering},
            Arc,
        },
    };

    static NETWORK_MESSAGE_COUNTER: AtomicUsize = AtomicUsize::new(0);

    // Test data for `on_network_request_handler`.
    pub fn network_request_handler_data() -> Vec<NetworkMessage> {
        let ip = IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1));
        let p2p_peer = P2PPeerBuilder::default()
            .peer_type(PeerType::Node)
            .addr(SocketAddr::new(ip, 8080))
            .build()
            .unwrap();
        let inner_msg = HybridBuf::try_from(&b"Message XXX"[..]).unwrap();
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
                Arc::new(NetworkPacket {
                    packet_type: NetworkPacketType::BroadcastedMessage(vec![]),
                    peer:        p2p_peer,
                    message_id:  MessageId::new(&[1u8; 32]),
                    network_id:  NetworkId::from(100),
                    message:     inner_msg.clone(),
                }),
                None,
                None,
            ),
            NetworkMessage::NetworkPacket(
                Arc::new(NetworkPacket {
                    packet_type: NetworkPacketType::DirectMessage(node_id),
                    peer:        p2p_peer,
                    message_id:  MessageId::new(&[2u8; 32]),
                    network_id:  NetworkId::from(100),
                    message:     inner_msg.clone(),
                }),
                None,
                None,
            ),
        ];

        data
    }

    /// Creates message handler for testing.
    fn make_message_handler() -> MessageHandler {
        let msg_handler = MessageHandler::new();

        msg_handler
            .add_callback(make_atomic_callback!(|_x: &NetworkMessage| {
                println!(
                    "Network Request {}",
                    NETWORK_MESSAGE_COUNTER.load(Ordering::Relaxed)
                );
                Ok(())
            }))
            .add_callback(make_atomic_callback!(|_x: &NetworkMessage| {
                NETWORK_MESSAGE_COUNTER.fetch_add(1, Ordering::SeqCst);
                Ok(())
            }));

        msg_handler
    }

    #[test]
    pub fn network_request_handler() {
        let mh: MessageHandler = make_message_handler();

        for message in network_request_handler_data() {
            let _status = mh.process_message(&message);
        }

        assert_eq!(NETWORK_MESSAGE_COUNTER.load(Ordering::Relaxed), 5);

        for message in network_request_handler_data() {
            let _status = mh.process_message(&message);
        }

        assert_eq!(NETWORK_MESSAGE_COUNTER.load(Ordering::Relaxed), 10);
    }
}
