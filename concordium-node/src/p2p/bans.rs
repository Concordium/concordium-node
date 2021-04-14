//! Peer ban handling.

use crate::{common::p2p_peer::RemotePeerId, connection::ConnChange, p2p::P2PNode, write_or_die};
use byteorder::{ReadBytesExt, WriteBytesExt};
use crypto_common::{Buffer, Deserial, Serial};
use failure::{self, Fallible};
use rkv::{StoreOptions, Value};

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
    /// Register the node's connection to be closed.
    pub fn drop_by_id(&self, id: RemotePeerId) -> bool {
        let maybe_token = self.find_conn_token_by_id(id);
        if let Some(token) = maybe_token {
            self.register_conn_change(ConnChange::RemovalByToken(token));
            true
        } else {
            false
        }
    }

    /// Register the node's connection to be closed and ban the IP.
    pub fn drop_by_ip_and_ban(&self, ip_addr: IpAddr) -> Fallible<bool> {
        info!("Banning IP {}", ip_addr);

        let bid = PersistedBanId::Ip(ip_addr);
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
        };

        let tokens = self.find_conn_tokens_by_ip(ip_addr);
        let res = !tokens.is_empty();
        self.register_conn_change(ConnChange::RemoveAllByTokens(tokens));
        Ok(res)
    }

    pub fn drop_addr(&self, addr: SocketAddr) -> bool {
        write_or_die!(self.config.favorite_addresses).remove(&addr);
        let maybe_token = self.find_conn_to(addr);
        if let Some(token) = maybe_token {
            self.register_conn_change(ConnChange::RemovalByToken(token));
            true
        } else {
            false
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
