use std::sync::{ Arc, Mutex };

use network::{ NetworkRequest };
use network::request::{ NetworkRequest as NetworkRequestEnum };

use connection::parse_handler::{ ParseHandler, ParseCallback, ParseCallbackResult };

pub struct RequestHandler {
    pub ping_handler: ParseHandler<NetworkRequestEnum>,
    pub find_node_handler: ParseHandler<NetworkRequestEnum>,
    pub ban_node_handler: ParseHandler<NetworkRequestEnum>,
    pub unban_node_handler: ParseHandler<NetworkRequestEnum>,
    pub handshake_handler: ParseHandler<NetworkRequestEnum>,
    pub get_peers_handler: ParseHandler<NetworkRequestEnum>,
    pub join_network_handler: ParseHandler<NetworkRequestEnum>,
    pub leave_network_handler: ParseHandler<NetworkRequestEnum>
}

impl RequestHandler {

    pub fn new() -> Self {
        RequestHandler {
            ping_handler: ParseHandler::<NetworkRequestEnum>::new(
                    "Network request ping handler".to_string()),
            find_node_handler: ParseHandler::new(
                    "Network request find node handler".to_string()),
            ban_node_handler: ParseHandler::new(
                    "Network request ban node handler".to_string()),
            unban_node_handler: ParseHandler::new(
                    "Network request unban node handler".to_string()),
            handshake_handler: ParseHandler::new(
                    "Network request handshake handler".to_string()),
            get_peers_handler: ParseHandler::new(
                    "Network request get peers handler".to_string()),
            join_network_handler: ParseHandler::new(
                    "Network request join network handler".to_string()),
            leave_network_handler: ParseHandler::new(
                    "Network request leave network handler".to_string()),
        }
    }

    pub fn add_ping_callback(
            mut self, 
            callback: Arc< Mutex< Box< ParseCallback<NetworkRequestEnum>>>>) -> Self {
        self.ping_handler = self.ping_handler.add_callback( callback);
        self
    }

    pub fn add_find_node_callback(
            mut self, 
            callback: Arc< Mutex< Box< ParseCallback<NetworkRequestEnum>>>>) -> Self {
        self.find_node_handler = self.find_node_handler.add_callback( callback);
        self
    }

    pub fn add_ban_node_callback(
            mut self, 
            callback: Arc< Mutex< Box< ParseCallback<NetworkRequestEnum>>>>) -> Self {
        self.ban_node_handler = self.ban_node_handler.add_callback( callback);
        self
    }

    pub fn add_unban_node_callback(
            mut self, 
            callback: Arc< Mutex< Box< ParseCallback<NetworkRequestEnum>>>>) -> Self {
        self.unban_node_handler = self.unban_node_handler.add_callback( callback);
        self
    }

    pub fn add_handshake_callback(
            mut self, 
            callback: Arc< Mutex< Box< ParseCallback<NetworkRequestEnum>>>>) -> Self {
        self.handshake_handler = self.handshake_handler.add_callback( callback);
        self
    }

    pub fn add_get_peers_callback(
            mut self, 
            callback: Arc< Mutex< Box< ParseCallback<NetworkRequestEnum>>>>) -> Self {
        self.get_peers_handler = self.get_peers_handler.add_callback( callback);
        self
    }
 
    pub fn add_join_network_callback(
            mut self, 
            callback: Arc< Mutex< Box< ParseCallback<NetworkRequestEnum>>>>) -> Self {
        self.join_network_handler = self.join_network_handler.add_callback( callback);
        self
    }
    
    pub fn add_leave_network_callback(
            mut self, 
            callback: Arc< Mutex< Box< ParseCallback<NetworkRequestEnum>>>>) -> Self {
        self.leave_network_handler = self.leave_network_handler.add_callback( callback);
        self
    }

    fn process_message(&self, msg: &NetworkRequest) -> ParseCallbackResult {
        match msg {
            ref ping_inner_pkt @ NetworkRequest::Ping(_) => { 
                (&self.ping_handler)(ping_inner_pkt)
            },
            ref find_inner_pkt @ NetworkRequest::FindNode(_, _) => { 
                (&self.find_node_handler)(find_inner_pkt)
            },
            ref ban_inner_pkt @ NetworkRequest::BanNode(_, _) => { 
                (&self.ban_node_handler)(ban_inner_pkt)
            },
            ref unban_inner_pkt @ NetworkRequest::UnbanNode(_, _) => {
                (&self.unban_node_handler)(unban_inner_pkt)
            },
            ref handshake_inner_pkt @ NetworkRequest::Handshake(_, _, _) => {
                (&self.handshake_handler)(handshake_inner_pkt)
            },
            ref get_peers_inner_pkt @ NetworkRequest::GetPeers(_, _) => {
                (&self.get_peers_handler)(get_peers_inner_pkt)
            },
            ref join_network_inner_pkt @ NetworkRequest::JoinNetwork(_, _) => {
                (&self.join_network_handler)(join_network_inner_pkt)
            },
            ref leave_network_inner_pkt @ NetworkRequest::LeaveNetwork(_, _) => { 
                (&self.leave_network_handler)(leave_network_inner_pkt)
            }
        }
    }

}

impl_all_fns!( RequestHandler, NetworkRequestEnum);


#[cfg(test)]
mod request_handler_test {
    use connection::request_handler::{ RequestHandler };
    use common::{ ConnectionType, P2PPeer, P2PNodeId };
    use network::request::{ NetworkRequest as NetworkRequestEnum };

    use std::sync::{ Arc, Mutex };
    use std::net::{ IpAddr, Ipv4Addr };
    use std::sync::atomic::{ AtomicUsize, Ordering, ATOMIC_USIZE_INIT };

    static PING_COUNTER: AtomicUsize = ATOMIC_USIZE_INIT;
    static FIND_NODE_COUNTER: AtomicUsize = ATOMIC_USIZE_INIT;
    static BAN_NODE_COUNTER: AtomicUsize = ATOMIC_USIZE_INIT;

    fn make_request_handler() -> RequestHandler {
        RequestHandler::new()
            .add_ping_callback( make_callback!( |_:&NetworkRequestEnum| {
                PING_COUNTER.fetch_add( 1, Ordering::SeqCst);
                Ok(())
            }))
            .add_find_node_callback( make_callback!( |_:&NetworkRequestEnum| {
                FIND_NODE_COUNTER.fetch_add( 1, Ordering::SeqCst);
                Ok(())
            }))
            .add_ban_node_callback( make_callback!( |_:&NetworkRequestEnum| {
                BAN_NODE_COUNTER.fetch_add( 1, Ordering::SeqCst);
                Ok(())
            }))
    }

    fn ut_1_data() -> Vec<NetworkRequestEnum> {
        let ip = IpAddr::V4(Ipv4Addr::new(127,0,0,1));
        let p2p_peer = P2PPeer::new( ConnectionType::Node, ip, 8080);
        let node_id: P2PNodeId = P2PNodeId::from_ip_port( ip, 8080);

        let data = vec![
            NetworkRequestEnum::Ping( p2p_peer.clone()),
            NetworkRequestEnum::FindNode( p2p_peer.clone(), node_id.clone()),
            NetworkRequestEnum::BanNode( p2p_peer.clone(), p2p_peer.clone())
        ];
        data
    }

    #[test]
    pub fn ut_1() {
        let rh = make_request_handler();

        for message in ut_1_data() {
            (&rh)(&message).unwrap();
        }

        assert_eq!( PING_COUNTER.load(Ordering::Relaxed), 1);
        assert_eq!( FIND_NODE_COUNTER.load(Ordering::Relaxed), 1);
        assert_eq!( BAN_NODE_COUNTER.load(Ordering::Relaxed), 1);
    }
}

