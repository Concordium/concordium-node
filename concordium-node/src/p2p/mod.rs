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
        p2p::bans::{BanId, PersistedBanId},
        test_utils::*,
    };
    use failure::Fallible;

    #[test]
    fn test_ban_functionalities() -> Fallible<()> {
        let port = next_available_port();
        let (node, dp) = make_node_and_sync(port, vec![100], PeerType::Node, vec![])?;

        // Empty on init
        let reply = node.get_banlist()?;
        assert!(reply.is_empty());

        let to_ban1 = BanId::NodeId(RemotePeerId::from(22usize));

        // Insertion by id
        node.drop_and_maybe_ban_node(to_ban1)?;
        let reply = node.get_banlist()?;
        // bans by id are not persisted.
        assert_eq!(reply.len(), 0);

        let to_ban2 = PersistedBanId::Ip("127.0.0.1".parse()?);

        // Insertion by ip
        node.drop_and_maybe_ban_node(to_ban2.into())?;
        let reply = node.get_banlist()?;
        assert_eq!(reply.len(), 1);
        assert_eq!(reply[0], to_ban2.into());

        // Duplicates check
        node.drop_and_maybe_ban_node(to_ban2.into())?;
        let reply = node.get_banlist()?;
        assert_eq!(reply.len(), 1);
        assert_eq!(reply[0], to_ban2.into());

        // Deletion by ip
        node.unban_node(to_ban2)?;
        let reply = node.get_banlist()?;
        assert!(reply.is_empty());

        stop_node_delete_dirs(dp, node);

        Ok(())
    }
}
