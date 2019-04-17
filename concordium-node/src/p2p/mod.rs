pub mod banned_nodes;
pub mod fails;
pub mod no_certificate_verification;
pub mod p2p_node;
pub mod p2p_node_handlers;
pub mod p2p_service_forwarder;
pub mod peer_statistics;
pub mod tls_server;
pub mod tls_server_private;
pub mod unreachable_nodes;

pub use self::p2p_service_forwarder::*;

pub use self::p2p_node::P2PNode;

#[cfg(test)]
mod tests {
    use crate::{
        common::{P2PNodeId, PeerType},
        configuration::Config,
        connection::P2PEvent,
        network::NetworkMessage,
        p2p::{banned_nodes::BannedNode, P2PNode},
    };
    use failure::Fallible;
    use std::{
        str::FromStr,
        sync::{
            atomic::{AtomicUsize, Ordering},
            mpsc, Arc,
        },
        thread,
    };

    static PORT_OFFSET: AtomicUsize = AtomicUsize::new(0);
    static PORT_START_NODE: u16 = 8888;

    fn next_port_offset_node(slot_size: usize) -> u16 {
        PORT_OFFSET.fetch_add(slot_size, Ordering::SeqCst) as u16 + PORT_START_NODE
    }

    #[test]
    pub fn test_banned_functionalities() -> Fallible<()> {
        let mut node = {
            let node_type = PeerType::Node;
            let id = "000000002dd2b6ed";
            let (pkt_in, _pkt_out) = mpsc::channel::<Arc<NetworkMessage>>();

            let (sender, receiver) = mpsc::channel();
            let _guard = thread::spawn(move || loop {
                if let Ok(msg) = receiver.recv() {
                    match msg {
                        P2PEvent::ConnectEvent(addr) => info!("Received connection from {}", addr),
                        P2PEvent::DisconnectEvent(msg) => info!("Received disconnect for {}", msg),
                        P2PEvent::ReceivedMessageEvent(node_id) => {
                            info!("Received message from {:?}", node_id)
                        }
                        P2PEvent::SentMessageEvent(node_id) => {
                            info!("Sent message to {:?}", node_id)
                        }
                        P2PEvent::InitiatingConnection(addr) => {
                            info!("Initiating connection to {}", addr)
                        }
                        P2PEvent::JoinedNetwork(peer, network_id) => {
                            info!("Peer {} joined network {}", peer.id(), network_id);
                        }
                        P2PEvent::LeftNetwork(peer, network_id) => {
                            info!("Peer {} left network {}", peer.id(), network_id);
                        }
                    }
                }
            });

            let config = Config::new(
                Some("127.0.0.1".to_owned()),
                next_port_offset_node(1),
                vec![],
                100,
            );

            P2PNode::new(
                Some(id.to_owned()),
                &config,
                pkt_in,
                Some(sender),
                node_type,
                None,
            )
        };
        // Empty on init
        let reply = node.get_banlist().expect("lock err");
        assert!(reply.is_empty());

        let to_ban1 = BannedNode::ById(P2PNodeId::from_str("0000000000000022")?);

        // Insertion by id
        node.ban_node(to_ban1)?;
        let reply = node.get_banlist().expect("lock err");
        assert!(reply.len() == 1);
        assert_eq!(reply[0], to_ban1);

        // Duplicates check
        node.ban_node(to_ban1)?;
        let reply = node.get_banlist().expect("lock err");
        assert!(reply.len() == 1);
        assert_eq!(reply[0], to_ban1);

        // Deletion by id
        node.unban_node(to_ban1)?;
        let reply = node.get_banlist().expect("lock err");
        assert!(reply.is_empty());

        let to_ban2 = BannedNode::ByAddr("127.0.0.1".parse()?);

        // Insertion by ip
        node.ban_node(to_ban2)?;
        let reply = node.get_banlist().expect("lock err");
        assert!(reply.len() == 1);
        assert_eq!(reply[0], to_ban2);

        // Duplicates check
        node.ban_node(to_ban2)?;
        let reply = node.get_banlist().expect("lock err");
        assert!(reply.len() == 1);
        assert_eq!(reply[0], to_ban2);

        // Deletion by ip
        node.unban_node(to_ban2)?;
        let reply = node.get_banlist().expect("lock err");
        assert!(reply.is_empty());

        Ok(())
    }
}
