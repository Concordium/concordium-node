pub mod banned_nodes;
pub mod fails;
pub mod noise_protocol_handler;
pub mod p2p_node;
pub mod p2p_node_handlers;
pub mod peer_statistics;
pub mod unreachable_nodes;

pub use self::p2p_node::P2PNode;

#[cfg(test)]
mod tests {
    use crate::{
        common::{P2PNodeId, PeerType},
        p2p::banned_nodes::BannedNode,
        test_utils::*,
    };
    use failure::Fallible;
    use std::str::FromStr;

    #[test]
    pub fn test_banned_functionalities() -> Fallible<()> {
        let port = next_available_port();
        let (node, _) = make_node_and_sync(port, vec![100], PeerType::Node)?;
        // Empty on init
        let reply = node.get_banlist();
        assert!(reply.is_empty());

        let to_ban1 = BannedNode::ById(P2PNodeId::from_str("0000000000000022")?);

        // Insertion by id
        node.ban_node(to_ban1);
        let reply = node.get_banlist();
        assert!(reply.len() == 1);
        assert_eq!(reply[0], to_ban1);

        // Duplicates check
        node.ban_node(to_ban1);
        let reply = node.get_banlist();
        assert!(reply.len() == 1);
        assert_eq!(reply[0], to_ban1);

        // Deletion by id
        node.unban_node(to_ban1);
        let reply = node.get_banlist();
        assert!(reply.is_empty());

        let to_ban2 = BannedNode::ByAddr("127.0.0.1".parse()?);

        // Insertion by ip
        node.ban_node(to_ban2);
        let reply = node.get_banlist();
        assert!(reply.len() == 1);
        assert_eq!(reply[0], to_ban2);

        // Duplicates check
        node.ban_node(to_ban2);
        let reply = node.get_banlist();
        assert!(reply.len() == 1);
        assert_eq!(reply[0], to_ban2);

        // Deletion by ip
        node.unban_node(to_ban2);
        let reply = node.get_banlist();
        assert!(reply.is_empty());

        Ok(())
    }
}
