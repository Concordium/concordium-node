pub mod tls_server;
pub mod unreachable_nodes;
pub mod no_certificate_verification;
pub mod peer_statistics;
pub mod p2p_node;

/*
use errors::*;
use get_if_addrs;
use mio::net::{ TcpListener, TcpStream };
use mio::*;
use prometheus_exporter::PrometheusServer;
use rustls::{
    Certificate, ClientConfig, ClientSession, NoClientAuth, PrivateKey, RootCertStore,
    ServerCertVerified, ServerCertVerifier, ServerConfig, ServerSession, TLSError
};

use atomic_counter::{ AtomicCounter };
use std::collections::{HashMap, HashSet, VecDeque};
use std::io::{ Error, ErrorKind };
use std::net::{ SocketAddr, IpAddr };
use std::net::IpAddr::{V4, V6};
use std::str::FromStr;
use std::sync::mpsc;
use std::sync::mpsc::Sender;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;
use time;
use time::Timespec;
use utils;
use webpki::DNSNameRef;

use common;
use common::{ ConnectionType, P2PNodeId, P2PPeer };
use network::{ NetworkMessage, NetworkPacket, NetworkRequest, Buckets };
use connection::{ P2PEvent, P2PNodeMode, Connection, SeenMessagesList, TOTAL_MESSAGES_SENT_COUNTER }; 
*/


#[cfg(test)]
mod tests {
    use std::net::{ IpAddr };
    use std::str::FromStr;

    use network::{ Buckets };
    use common::{ P2PNodeId, P2PPeer, ConnectionType };

    #[test]
    pub fn test_buckets_insert_duplicate_peer_id() {
        let mut bucket = Buckets::new();
        let p2p_self = P2PNodeId::from_ipstring("127.0.0.1:8888".to_string());
        let p2p_node_id = P2PNodeId::from_string(&"c19cd000746763871fae95fcdd4508dfd8bf725f9767be68c3038df183527bb2".to_string()).unwrap();
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

    #[test]
    pub fn test_buckets_insert_duplicate_ip_port() {
        let mut bucket = Buckets::new();
        let p2p_self = P2PNodeId::from_ipstring("127.0.0.1:8888".to_string());
        let p2p_node_id = P2PNodeId::from_string(&"c19cd000746763871fae95fcdd4508dfd8bf725f9767be68c3038df183527bb2".to_string()).unwrap();
        let p2p_node_id_2 = P2PNodeId::from_string(&"c19cd000746763871fae95fcdd4508dfd8bf725f9767be68c3038df183527bb3".to_string()).unwrap();
        let p2p_new_peer = P2PPeer::from(ConnectionType::Node,
                                         p2p_node_id.clone(),
                                         IpAddr::from_str("127.0.0.1").unwrap(),
                                         8888);
        let p2p_new_replacement_peer = P2PPeer::from(ConnectionType::Node,
                                                     p2p_node_id_2.clone(),
                                                     IpAddr::from_str("127.0.0.1").unwrap(),
                                                     8888);
        bucket.insert_into_bucket(&p2p_new_peer, &p2p_self, vec![]);
        bucket.insert_into_bucket(&p2p_new_replacement_peer, &p2p_self, vec![]);
        assert_eq!(bucket.len(), 1);
    }
}
