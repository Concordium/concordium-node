pub mod tls_server_private;
pub mod tls_server;
pub mod unreachable_nodes;
pub mod no_certificate_verification;
pub mod peer_statistics;
pub mod p2p_service_forwarder;
pub mod p2p_node;
pub mod p2p_node_handlers;
pub mod fails;

pub use self::p2p_service_forwarder::*;

pub use self::p2p_node::P2PNode;

#[cfg(test)]
mod tests {
    use std::net::{ IpAddr };
    use std::str::FromStr;

    use crate::network::{ Buckets };
    use crate::common::{ P2PNodeId, P2PPeer, ConnectionType };

    #[test]
    pub fn test_buckets_insert_duplicate_peer_id() {
        let mut bucket = Buckets::new();
        let p2p_self = P2PNodeId::from_ipstring("127.0.0.1:8888");
        let p2p_node_id = P2PNodeId::from_b64_repr(&"Cc0Td01Pk/mKDVjJfsQ3rP7P2J0/i3qRAk+2sQz0MtY=").unwrap();
        let p2p_new_peer = P2PPeer::from(ConnectionType::Node,
                                         p2p_node_id.clone(),
                                         IpAddr::from_str("127.0.0.1").unwrap(),
                                         8888);
        let p2p_new_replacement_peer = P2PPeer::from(ConnectionType::Node,
                                                     p2p_node_id.clone(),
                                                     IpAddr::from_str("127.0.0.1").unwrap(),
                                                     8889);
        bucket.insert_into_bucket(&p2p_new_peer, &p2p_self, vec![]);
        bucket.insert_into_bucket(&p2p_new_replacement_peer, &p2p_self, vec![]);
        assert_eq!(bucket.len(), 1);
    }
}
