use crate::network::{request::NetworkRequest as NRequest, NetworkRequest};
use concordium_common::functor::{FunctorResult, UnitFunction, UnitFunctor};

pub struct RequestHandler {
    pub ping_handler:          UnitFunctor<NRequest>,
    pub find_node_handler:     UnitFunctor<NRequest>,
    pub ban_node_handler:      UnitFunctor<NRequest>,
    pub unban_node_handler:    UnitFunctor<NRequest>,
    pub handshake_handler:     UnitFunctor<NRequest>,
    pub get_peers_handler:     UnitFunctor<NRequest>,
    pub join_network_handler:  UnitFunctor<NRequest>,
    pub leave_network_handler: UnitFunctor<NRequest>,
    pub retransmit_handler:    UnitFunctor<NRequest>,
}

impl Default for RequestHandler {
    fn default() -> Self { RequestHandler::new() }
}

impl RequestHandler {
    pub fn new() -> Self {
        RequestHandler {
            ping_handler:          UnitFunctor::new(),
            find_node_handler:     UnitFunctor::new(),
            ban_node_handler:      UnitFunctor::new(),
            unban_node_handler:    UnitFunctor::new(),
            handshake_handler:     UnitFunctor::new(),
            get_peers_handler:     UnitFunctor::new(),
            join_network_handler:  UnitFunctor::new(),
            leave_network_handler: UnitFunctor::new(),
            retransmit_handler:    UnitFunctor::new(),
        }
    }

    pub fn add_ping_callback(&self, callback: UnitFunction<NRequest>) -> &Self {
        self.ping_handler.add_callback(callback);
        self
    }

    pub fn add_find_node_callback(&self, callback: UnitFunction<NRequest>) -> &Self {
        self.find_node_handler.add_callback(callback);
        self
    }

    pub fn add_ban_node_callback(&self, callback: UnitFunction<NRequest>) -> &Self {
        self.ban_node_handler.add_callback(callback);
        self
    }

    pub fn add_unban_node_callback(&self, callback: UnitFunction<NRequest>) -> &Self {
        self.unban_node_handler.add_callback(callback);
        self
    }

    pub fn add_handshake_callback(&self, callback: UnitFunction<NRequest>) -> &Self {
        self.handshake_handler.add_callback(callback);
        self
    }

    pub fn add_get_peers_callback(&self, callback: UnitFunction<NRequest>) -> &Self {
        self.get_peers_handler.add_callback(callback);
        self
    }

    pub fn add_join_network_callback(&self, callback: UnitFunction<NRequest>) -> &Self {
        self.join_network_handler.add_callback(callback);
        self
    }

    pub fn add_leave_network_callback(&self, callback: UnitFunction<NRequest>) -> &Self {
        self.leave_network_handler.add_callback(callback);
        self
    }

    pub fn add_retransmit_callback(&self, callback: UnitFunction<NRequest>) -> &Self {
        self.retransmit_handler.add_callback(callback);
        self
    }

    pub fn process_message(&self, msg: &NetworkRequest) -> FunctorResult<()> {
        match msg {
            ref ping_inner_pkt @ NetworkRequest::Ping(_) => {
                self.ping_handler.run_callbacks(ping_inner_pkt)
            }
            ref find_inner_pkt @ NetworkRequest::FindNode(..) => {
                self.find_node_handler.run_callbacks(find_inner_pkt)
            }
            ref ban_inner_pkt @ NetworkRequest::BanNode(..) => {
                self.ban_node_handler.run_callbacks(ban_inner_pkt)
            }
            ref unban_inner_pkt @ NetworkRequest::UnbanNode(..) => {
                self.unban_node_handler.run_callbacks(unban_inner_pkt)
            }
            ref handshake_inner_pkt @ NetworkRequest::Handshake(..) => {
                self.handshake_handler.run_callbacks(handshake_inner_pkt)
            }
            ref get_peers_inner_pkt @ NetworkRequest::GetPeers(..) => {
                self.get_peers_handler.run_callbacks(get_peers_inner_pkt)
            }
            ref join_network_inner_pkt @ NetworkRequest::JoinNetwork(..) => self
                .join_network_handler
                .run_callbacks(join_network_inner_pkt),
            ref leave_network_inner_pkt @ NetworkRequest::LeaveNetwork(..) => self
                .leave_network_handler
                .run_callbacks(leave_network_inner_pkt),
            ref retransmit_inner_pkt @ NetworkRequest::Retransmit(..) => {
                self.retransmit_handler.run_callbacks(retransmit_inner_pkt)
            }
        }
    }
}

#[cfg(test)]
mod request_handler_test {
    use crate::{
        common::{P2PNodeId, P2PPeerBuilder, PeerType},
        connection::RequestHandler,
        network::request::NetworkRequest as NRequest,
        p2p::banned_nodes::tests::dummy_ban_node,
    };

    use std::{
        net::{IpAddr, Ipv4Addr, SocketAddr},
        sync::{
            atomic::{AtomicUsize, Ordering},
            Arc,
        },
    };

    static PING_COUNTER: AtomicUsize = AtomicUsize::new(0);
    static FIND_NODE_COUNTER: AtomicUsize = AtomicUsize::new(0);
    static BAN_NODE_COUNTER: AtomicUsize = AtomicUsize::new(0);

    fn make_request_handler() -> RequestHandler {
        let mut handler = RequestHandler::new();

        handler
            .add_ping_callback(make_atomic_callback!(|_: &NRequest| {
                PING_COUNTER.fetch_add(1, Ordering::SeqCst);
                Ok(())
            }))
            .add_find_node_callback(make_atomic_callback!(|_: &NRequest| {
                FIND_NODE_COUNTER.fetch_add(1, Ordering::SeqCst);
                Ok(())
            }))
            .add_ban_node_callback(make_atomic_callback!(|_: &NRequest| {
                BAN_NODE_COUNTER.fetch_add(1, Ordering::SeqCst);
                Ok(())
            }));

        handler
    }

    fn ut_1_data() -> Vec<NRequest> {
        let ip = IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1));
        let p2p_peer = P2PPeerBuilder::default()
            .peer_type(PeerType::Node)
            .addr(SocketAddr::new(ip, 8080))
            .build()
            .unwrap();
        let node_id = P2PNodeId::default();
        let banning_node = dummy_ban_node(Some(ip));

        let data = vec![
            NRequest::Ping(p2p_peer.clone()),
            NRequest::FindNode(p2p_peer.clone(), node_id),
            NRequest::BanNode(p2p_peer.clone(), banning_node),
        ];
        data
    }

    #[test]
    pub fn ut_1() {
        let rh = make_request_handler();

        for message in ut_1_data() {
            rh.process_message(&message).unwrap();
        }

        assert_eq!(PING_COUNTER.load(Ordering::Relaxed), 1);
        assert_eq!(FIND_NODE_COUNTER.load(Ordering::Relaxed), 1);
        assert_eq!(BAN_NODE_COUNTER.load(Ordering::Relaxed), 1);
    }
}
