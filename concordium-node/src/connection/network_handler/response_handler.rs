use crate::{
    common::functor::{FunctorResult, Functorable, UnitFunction, UnitFunctor},
    network::NetworkResponse,
};

pub struct ResponseHandler {
    pub pong_handler:      UnitFunctor<NetworkResponse>,
    pub find_node_handler: UnitFunctor<NetworkResponse>,
    pub peer_list_handler: UnitFunctor<NetworkResponse>,
    pub handshake_handler: UnitFunctor<NetworkResponse>,
}

impl Default for ResponseHandler {
    fn default() -> Self { ResponseHandler::new() }
}

impl ResponseHandler {
    pub fn new() -> Self {
        ResponseHandler {
            pong_handler:      UnitFunctor::<NetworkResponse>::new("Network::Response::Pong"),
            find_node_handler: UnitFunctor::<NetworkResponse>::new("Network::Response::FindNode"),
            peer_list_handler: UnitFunctor::<NetworkResponse>::new("Network::Response::PeerList"),
            handshake_handler: UnitFunctor::<NetworkResponse>::new("Network::Response::Handshake"),
        }
    }

    pub fn process_message(&self, msg: &NetworkResponse) -> FunctorResult<()> {
        match msg {
            ref pong_inner_pkt @ NetworkResponse::Pong(_) => {
                self.pong_handler.run_callbacks(pong_inner_pkt)
            }
            ref find_node_inner_pkt @ NetworkResponse::FindNode(_, _) => {
                self.find_node_handler.run_callbacks(find_node_inner_pkt)
            }
            ref peer_list_inner_pkt @ NetworkResponse::PeerList(_, _) => {
                self.peer_list_handler.run_callbacks(peer_list_inner_pkt)
            }
            ref handshake_inner_pkt @ NetworkResponse::Handshake(_, _, _) => {
                self.handshake_handler.run_callbacks(handshake_inner_pkt)
            }
        }
    }

    pub fn add_pong_callback(&mut self, callback: UnitFunction<NetworkResponse>) -> &mut Self {
        self.pong_handler.add_callback(callback);
        self
    }

    pub fn add_find_node_callback(&mut self, callback: UnitFunction<NetworkResponse>) -> &mut Self {
        self.find_node_handler.add_callback(callback);
        self
    }

    pub fn add_peer_list_callback(&mut self, callback: UnitFunction<NetworkResponse>) -> &mut Self {
        self.peer_list_handler.add_callback(callback);
        self
    }

    pub fn add_handshake_callback(&mut self, callback: UnitFunction<NetworkResponse>) -> &mut Self {
        self.handshake_handler.add_callback(callback);
        self
    }
}
