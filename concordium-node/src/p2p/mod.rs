//! Central node object handling.

pub mod bans;
#[cfg_attr(feature = "s11n_serde", allow(unreachable_code, unused))]
pub mod connectivity;
pub mod maintenance;
#[cfg_attr(feature = "s11n_serde", allow(unreachable_code, unused))]
pub mod peers;

pub use self::maintenance::{Connections, P2PNode};

#[cfg(test)]
mod tests {
    use crate::{
        common::{p2p_peer::RemotePeerId, PeerType},
        p2p::bans::PersistedBanId,
        test_utils::*,
    };
    use std::net::IpAddr;

    #[test]
    fn test_ban_functionalities() -> Fallible<()> {
        let port = next_available_port();
        let (node, dp) = make_node_and_sync(port, vec![100], PeerType::Node, vec![])?;

        // Empty on init
        let reply = node.get_banlist()?;
        assert!(reply.is_empty());

        let to_ban1 = RemotePeerId::from(22usize);

        // Insertion by id
        assert!(
            !node.drop_by_id(to_ban1),
            "Should have returned false since the peer does not exist."
        );
        let reply = node.get_banlist()?;
        // bans by id are not persisted.
        assert_eq!(reply.len(), 0);

        let to_ban2 = "127.0.0.1".parse::<IpAddr>()?;

        // Insertion by ip
        assert!(
            !node.drop_by_ip_and_ban(to_ban2)?,
            "Should have returned false since the peer does not exist."
        );
        let reply = node.get_banlist()?;
        assert_eq!(reply.len(), 1);
        assert_eq!(reply[0], PersistedBanId::Ip(to_ban2));

        // Duplicates check
        assert!(
            !node.drop_by_ip_and_ban(to_ban2)?,
            "Should have banned the same IP again, returning false since no peer exists."
        );
        let reply = node.get_banlist()?;
        assert_eq!(reply.len(), 1);
        assert_eq!(reply[0], PersistedBanId::Ip(to_ban2));

        // Deletion by ip
        node.unban_node(PersistedBanId::Ip(to_ban2))?;
        let reply = node.get_banlist()?;
        assert!(reply.is_empty());

        stop_node_delete_dirs(dp, node);

        Ok(())
    }
}
