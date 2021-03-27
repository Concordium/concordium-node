//! Central node object handling.

pub mod bans;
#[cfg_attr(any(feature = "s11n_serde", feature = "s11n_capnp"), allow(unreachable_code, unused))]
pub mod connectivity;
pub mod maintenance;
#[cfg_attr(any(feature = "s11n_serde", feature = "s11n_capnp"), allow(unreachable_code, unused))]
pub mod peers;

pub use self::maintenance::{Connections, P2PNode};

#[cfg(test)]
mod tests {
    use crate::{
        common::{P2PNodeId, PeerType},
        p2p::bans::BanId,
        test_utils::*,
    };
    use failure::Fallible;
    use std::str::FromStr;

    #[test]
    fn test_ban_functionalities() -> Fallible<()> {
        let port = next_available_port();
        let (node, dp) = make_node_and_sync(port, vec![100], PeerType::Node, vec![])?;

        // Empty on init
        let reply = node.get_banlist()?;
        assert!(reply.is_empty());

        let to_ban1 = BanId::NodeId(P2PNodeId::from_str("0000000000000022")?);

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

        let to_ban2 = BanId::Ip("127.0.0.1".parse()?);

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

        stop_node_delete_dirs(dp, node);

        Ok(())
    }
}
