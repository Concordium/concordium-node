pub mod banned_nodes;
pub mod fails;
pub mod p2p_node;
pub mod unreachable_nodes;

pub use self::p2p_node::{Connections, Networks, P2PNode, Receivers};

#[cfg(test)]
mod tests {
    use crate::{
        common::{P2PNodeId, PeerType},
        p2p::banned_nodes::BannedNode,
        test_utils::*,
    };
    use failure::Fallible;
    use std::{str::FromStr, thread, time::Duration};

    #[test]
    fn test_ban_functionalities() -> Fallible<()> {
        // either that or Node::new should have a bool for the creation of the banlist
        thread::sleep(Duration::from_secs(5));

        let port = next_available_port();
        let node = make_node_and_sync(port, vec![100], PeerType::Node)?;

        // Empty on init
        let reply = node.get_banlist()?;
        assert!(reply.is_empty());

        let to_ban1 = BannedNode::ById(P2PNodeId::from_str("0000000000000022")?);

        // Insertion by id
        node.ban_node(to_ban1)?;
        let reply = node.get_banlist()?;
        assert_eq!(reply.len(), 1);
        assert_eq!(reply[0], to_ban1);

        // Duplicates check
        node.ban_node(to_ban1)?;
        let reply = node.get_banlist()?;
        assert_eq!(reply.len(), 1);
        assert_eq!(reply[0], to_ban1);

        // Deletion by id
        node.unban_node(to_ban1)?;
        let reply = node.get_banlist()?;
        assert!(reply.is_empty());

        let to_ban2 = BannedNode::ByAddr("127.0.0.1".parse()?);

        // Insertion by ip
        node.ban_node(to_ban2)?;
        let reply = node.get_banlist()?;
        assert_eq!(reply.len(), 1);
        assert_eq!(reply[0], to_ban2);

        // Duplicates check
        node.ban_node(to_ban2)?;
        let reply = node.get_banlist()?;
        assert_eq!(reply.len(), 1);
        assert_eq!(reply[0], to_ban2);

        // Deletion by ip
        node.unban_node(to_ban2)?;
        let reply = node.get_banlist()?;
        assert!(reply.is_empty());

        Ok(())
    }

    #[test]
    fn test_node_self_ref() -> Fallible<()> {
        let node = make_node_and_sync(next_available_port(), vec![100], PeerType::Node)?;

        assert!(std::ptr::eq(
            &*node,
            &*node.self_ref.as_ref().unwrap().as_ref()
        ));

        Ok(())
    }
}
