use super::network::{ NetworkRequest, NetworkResponse };

use common::{ P2PPeer, P2PNodeId };

pub trait MessageRequestHandler {
    fn on_ping( & peer:P2PPeer ) -> Result<()>;
    fn on_find_node( & peer:P2PPeer, node_id: P2PNodeId ) -> Result<()>;
}

/*
pub trait MessageHandler {
    fn on_network_request( &self, &nr: NetworkRequest) -> Result<()>;
    fn on_network_response( &self, &nr: NetworkResponse) -> Result<()>;
}
*/
