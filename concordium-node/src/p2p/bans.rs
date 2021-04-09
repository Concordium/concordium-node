//! Peer ban handling.

use byteorder::{ReadBytesExt, WriteBytesExt};
use failure::{self, Fallible};
use rkv::{StoreOptions, Value};

use crate::{common::p2p_peer::RemotePeerId, connection::ConnChange, p2p::P2PNode};
use crypto_common::{Buffer, Deserial, Serial};

use std::net::{IpAddr, SocketAddr};

const BAN_STORE_NAME: &str = "bans";

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
/// A node can be banned either by its id node or by its address - IP or
/// IP+port. This is used for soft bans only, i.e., bans with limited expiry
/// that are not persisted to an external database.
pub enum BanId {
    NodeId(RemotePeerId),
    Ip(IpAddr),
    Socket(SocketAddr),
}

/// Some bans are persisted to the database so we block reconnects from those
/// peers.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum PersistedBanId {
    Ip(IpAddr),
}

impl From<PersistedBanId> for BanId {
    fn from(pbid: PersistedBanId) -> Self {
        match pbid {
            PersistedBanId::Ip(ip) => Self::Ip(ip),
        }
    }
}

impl BanId {
    /// Attempt to convert to a persistent ban id, if possible.
    /// Only Ip bans are currently persisted.
    fn to_persistent(self) -> Option<PersistedBanId> {
        match self {
            BanId::Ip(ip) => Some(PersistedBanId::Ip(ip)),
            BanId::NodeId(_) => None,
            BanId::Socket(_) => None,
        }
    }
}

impl Serial for PersistedBanId {
    fn serial<W: Buffer + WriteBytesExt>(&self, target: &mut W) {
        match self {
            PersistedBanId::Ip(addr) => {
                target.write_u8(0).expect("Writing to memory is infallible.");
                addr.serial(target);
            }
        }
    }
}

impl Deserial for PersistedBanId {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        let bn = match source.read_u8()? {
            0 => Self::Ip(IpAddr::deserial(source)?),
            _ => bail!("Unsupported type of `BanNode`"),
        };

        Ok(bn)
    }
}

impl P2PNode {
    /// Register the node's connection to be closed, and if the
    /// ban is applicable to be persisted also register the peer as banned.
    pub fn drop_and_maybe_ban_node(&self, peer: BanId) -> Fallible<bool> {
        info!("Banning node {:?}", peer);

        // only write to the database if the ban is meant to be persisted.
        // Check this first to avoid acquiring a lock if we don't need to.
        if let Some(bid) = peer.to_persistent() {
            if let Ok(ban_kvs_env) = self.kvs.read() {
                let mut store_key = Vec::new();
                bid.serial(&mut store_key);
                let ban_store = ban_kvs_env.open_single(BAN_STORE_NAME, StoreOptions::create())?;
                let mut writer = ban_kvs_env.write()?;
                // TODO: insert ban expiry timestamp as the Value
                ban_store.put(&mut writer, store_key, &Value::U64(0))?;
                writer.commit()?;
            } else {
                bail!("Couldn't ban a peer: couldn't obtain a lock over the kvs");
            }
        } // do nothing for peers that cannot be persisted.

        match peer {
            BanId::NodeId(id) => {
                self.register_conn_change(ConnChange::RemovalByNodeId(id));
                Ok(true)
            }
            BanId::Ip(addr) => {
                self.register_conn_change(ConnChange::RemovalByIp(addr));
                Ok(true)
            }
            BanId::Socket(_) => Ok(false),
        }
    }

    /// Remove a node from the banned peer list if it exists.
    /// If the peer is not banned then this does nothing.
    pub fn unban_node(&self, peer: PersistedBanId) -> Fallible<()> {
        info!("Unbanning node {:?}", peer);

        if let Ok(ban_kvs_env) = self.kvs.read() {
            let mut store_key = Vec::new();
            peer.serial(&mut store_key);
            let ban_store = ban_kvs_env.open_single(BAN_STORE_NAME, StoreOptions::create())?;
            let mut writer = ban_kvs_env.write()?;
            ban_store.delete(&mut writer, store_key)?;
            writer.commit()?;
        } else {
            bail!("Couldn't unban a peer: couldn't obtain a lock over the kvs");
        }
        Ok(())
    }

    /// Check whether a specified id has been banned.
    pub fn is_banned(&self, peer: PersistedBanId) -> Fallible<bool> {
        if let Ok(ban_kvs_env) = self.kvs.read() {
            let ban_store = ban_kvs_env.open_single(BAN_STORE_NAME, StoreOptions::create())?;
            let ban_reader = ban_kvs_env.read()?;
            let mut store_key = Vec::new();
            peer.serial(&mut store_key);

            Ok(ban_store.get(&ban_reader, store_key)?.is_some())
        } else {
            bail!("Couldn't check if a peer is banned: read from the ban database.");
        }
    }

    /// Obtain the list of banned nodes.
    pub fn get_banlist(&self) -> Fallible<Vec<PersistedBanId>> {
        if let Ok(ban_kvs_env) = self.kvs.read() {
            let ban_store = ban_kvs_env.open_single(BAN_STORE_NAME, StoreOptions::create())?;

            let ban_reader = ban_kvs_env.read()?;
            let ban_iter = ban_store.iter_start(&ban_reader)?;

            let mut banlist = Vec::new();
            for entry in ban_iter {
                let (mut id_bytes, _expiry) = entry?;
                let node_to_ban = PersistedBanId::deserial(&mut id_bytes)?;
                banlist.push(node_to_ban);
            }

            Ok(banlist)
        } else {
            bail!("Couldn't get the banlist: couldn't obtain a lock over the kvs");
        }
    }

    /// Lift all existing bans.
    pub fn clear_bans(&self) -> Fallible<()> {
        if let Ok(kvs_env) = self.kvs.read() {
            let ban_store = kvs_env.open_single(BAN_STORE_NAME, StoreOptions::create())?;
            let mut writer = kvs_env.write()?;
            ban_store.clear(&mut writer)?;
            writer.commit().map_err(|e| e.into())
        } else {
            bail!("Couldn't clear the bans: couldn't obtain a lock over the kvs");
        }
    }
}
