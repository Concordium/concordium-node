use crate::{
    common::functor::{AFunctor, AFunctorCW, FunctorResult},
    network::{request::NetworkRequest as NRequest, NetworkRequest},
};

pub struct RequestHandler {
    pub ping_handler:          AFunctor<NRequest>,
    pub find_node_handler:     AFunctor<NRequest>,
    pub ban_node_handler:      AFunctor<NRequest>,
    pub unban_node_handler:    AFunctor<NRequest>,
    pub handshake_handler:     AFunctor<NRequest>,
    pub get_peers_handler:     AFunctor<NRequest>,
    pub join_network_handler:  AFunctor<NRequest>,
    pub leave_network_handler: AFunctor<NRequest>,
}

impl RequestHandler {
    pub fn new() -> Self {
        RequestHandler {
            ping_handler:          AFunctor::<NRequest>::new("Network::Request::Ping"),
            find_node_handler:     AFunctor::new("Network::Request::FindNode"),
            ban_node_handler:      AFunctor::new("Network::Request::BanNode"),
            unban_node_handler:    AFunctor::new("Network::Request::UnbanNode"),
            handshake_handler:     AFunctor::new("Network::Request::Handshake"),
            get_peers_handler:     AFunctor::new("Network::Request::GetPeers"),
            join_network_handler:  AFunctor::new("Network::Request::JoinNetwork"),
            leave_network_handler: AFunctor::new("Network::Request::LeaveNetwork"),
        }
    }

    pub fn add_ping_callback(&mut self, callback: AFunctorCW<NRequest>) -> &mut Self {
        self.ping_handler.add_callback(callback);
        self
    }

    pub fn add_find_node_callback(&mut self, callback: AFunctorCW<NRequest>) -> &mut Self {
        self.find_node_handler.add_callback(callback);
        self
    }

    pub fn add_ban_node_callback(&mut self, callback: AFunctorCW<NRequest>) -> &mut Self {
        self.ban_node_handler.add_callback(callback);
        self
    }

    pub fn add_unban_node_callback(&mut self, callback: AFunctorCW<NRequest>) -> &mut Self {
        self.unban_node_handler.add_callback(callback);
        self
    }

    pub fn add_handshake_callback(&mut self, callback: AFunctorCW<NRequest>) -> &mut Self {
        self.handshake_handler.add_callback(callback);
        self
    }

    pub fn add_get_peers_callback(&mut self, callback: AFunctorCW<NRequest>) -> &mut Self {
        self.get_peers_handler.add_callback(callback);
        self
    }

    pub fn add_join_network_callback(&mut self, callback: AFunctorCW<NRequest>) -> &mut Self {
        self.join_network_handler.add_callback(callback);
        self
    }

    pub fn add_leave_network_callback(&mut self, callback: AFunctorCW<NRequest>) -> &mut Self {
        self.leave_network_handler.add_callback(callback);
        self
    }

    pub fn process_message(&self, msg: &NetworkRequest) -> FunctorResult {
        let spec_status = match msg {
            ref ping_inner_pkt @ NetworkRequest::Ping(_) => {
                self.ping_handler.run_callbacks(ping_inner_pkt)
            }
            ref find_inner_pkt @ NetworkRequest::FindNode(_, _) => {
                self.find_node_handler.run_callbacks(find_inner_pkt)
            }
            ref ban_inner_pkt @ NetworkRequest::BanNode(_, _) => {
                self.ban_node_handler.run_callbacks(ban_inner_pkt)
            }
            ref unban_inner_pkt @ NetworkRequest::UnbanNode(_, _) => {
                self.unban_node_handler.run_callbacks(unban_inner_pkt)
            }
            ref handshake_inner_pkt @ NetworkRequest::Handshake(_, _, _) => {
                self.handshake_handler.run_callbacks(handshake_inner_pkt)
            }
            ref get_peers_inner_pkt @ NetworkRequest::GetPeers(_, _) => {
                self.get_peers_handler.run_callbacks(get_peers_inner_pkt)
            }
            ref join_network_inner_pkt @ NetworkRequest::JoinNetwork(_, _) => self
                .join_network_handler
                .run_callbacks(join_network_inner_pkt),
            ref leave_network_inner_pkt @ NetworkRequest::LeaveNetwork(_, _) => self
                .leave_network_handler
                .run_callbacks(leave_network_inner_pkt),
        };

        spec_status
    }
}

#[cfg(test)]
mod request_handler_test {
    use crate::{
        common::{P2PNodeId, P2PPeerBuilder, PeerType},
        connection::RequestHandler,
        network::request::NetworkRequest as NRequest,
    };

    use std::{
        net::{IpAddr, Ipv4Addr},
        sync::{
            atomic::{AtomicUsize, Ordering},
            Arc, RwLock,
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
            .ip(ip)
            .port(8080)
            .build()
            .unwrap();
        let node_id = P2PNodeId::default();

        let data = vec![
            NRequest::Ping(p2p_peer.clone()),
            NRequest::FindNode(p2p_peer.clone(), node_id),
            NRequest::BanNode(p2p_peer.clone(), p2p_peer),
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
