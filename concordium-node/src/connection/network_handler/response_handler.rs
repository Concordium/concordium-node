use common::functor::{ AFunctor, AFunctorCW, FunctorResult };
use network::{ NetworkResponse };

pub struct ResponseHandler {
    pub pong_handler: AFunctor<NetworkResponse>,
    pub find_node_handler: AFunctor<NetworkResponse>,
    pub peer_list_handler: AFunctor<NetworkResponse>,
    pub handshake_handler: AFunctor<NetworkResponse>,
}

impl ResponseHandler {
    pub fn new() -> Self {
        ResponseHandler {
            pong_handler: AFunctor::<NetworkResponse>::new(
                "Network::Response::Pong"),
            find_node_handler: AFunctor::<NetworkResponse>::new(
                "Network::Response::FindNode"),
            peer_list_handler: AFunctor::<NetworkResponse>::new(
                "Network::Response::PeerList"),
            handshake_handler: AFunctor::<NetworkResponse>::new(
                "Network::Response::Handshake"),
        }
    }

    fn process_message(&self, msg: &NetworkResponse) -> FunctorResult {
        match msg {
            ref pong_inner_pkt @ NetworkResponse::Pong(_) => {
                (&self.pong_handler)(pong_inner_pkt)
            },
            ref find_node_inner_pkt @ NetworkResponse::FindNode(_, _) => {
                (&self.find_node_handler)(find_node_inner_pkt)
            },
            ref peer_list_inner_pkt @ NetworkResponse::PeerList(_, _) => {
                (&self.peer_list_handler)(peer_list_inner_pkt)
            },
            ref handshake_inner_pkt @ NetworkResponse::Handshake(_, _, _) => {
                (&self.handshake_handler)(handshake_inner_pkt)
            }
        }
    }

    pub fn add_pong_callback( &mut self, callback: AFunctorCW<NetworkResponse>) -> &mut Self {
        self.pong_handler.add_callback( callback);
        self
    }

    pub fn add_find_node_callback( &mut self, callback: AFunctorCW<NetworkResponse>) -> &mut Self {
        self.find_node_handler.add_callback( callback);
        self
    }

    pub fn add_peer_list_callback( &mut self, callback: AFunctorCW<NetworkResponse>) -> &mut Self {
        self.peer_list_handler.add_callback( callback);
        self
    }

    pub fn add_handshake_callback( &mut self, callback: AFunctorCW<NetworkResponse>) -> &mut Self {
        self.handshake_handler.add_callback( callback);
        self
    }
}

impl_all_fns!( ResponseHandler, NetworkResponse);
