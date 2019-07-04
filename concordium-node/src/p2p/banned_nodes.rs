use crate::common::{
    serialization::{Deserializable, ReadArchive, Serializable, WriteArchive},
    P2PNodeId,
};
use failure::{self, format_err, Fallible};
use rkv::{Rkv, StoreOptions, Value};

use std::{collections::HashSet, convert::TryFrom, net::IpAddr, sync::RwLock};

#[derive(Copy, Clone, Debug, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
/// Represents a structure used to manage a ban
///
/// A node can either be banned by its id or
/// by its address.
pub enum BannedNode {
    ById(P2PNodeId),
    ByAddr(IpAddr),
}

impl BannedNode {
    pub fn to_db_repr(&self) -> Box<[u8]> {
        match self {
            BannedNode::ById(id) => id.to_string().into_bytes().into_boxed_slice(),
            BannedNode::ByAddr(addr) => addr.to_string().into_bytes().into_boxed_slice(),
        }
    }
}

impl TryFrom<&[u8]> for BannedNode {
    type Error = failure::Error;

    fn try_from(bytes: &[u8]) -> Fallible<Self> {
        match bytes.len() {
            4 => {
                let mut arr = [0u8; 4];
                arr.copy_from_slice(bytes);
                Ok(BannedNode::ByAddr(IpAddr::from(arr)))
            }
            8 => {
                let mut arr = [0u8; 8];
                arr.copy_from_slice(bytes);
                Ok(BannedNode::ById(P2PNodeId(u64::from_le_bytes(arr))))
            }
            16 => {
                let mut arr = [0u8; 16];
                arr.copy_from_slice(bytes);
                Ok(BannedNode::ByAddr(IpAddr::from(arr)))
            }
            n => Err(format_err!("Invalid ban id length ({})!", n)),
        }
    }
}

/// Combination of nodes banned by id and banned by address
pub struct BannedNodes {
    pub by_id:   HashSet<P2PNodeId>,
    pub by_addr: HashSet<IpAddr>,
}

impl Default for BannedNodes {
    fn default() -> Self { BannedNodes::new() }
}

impl BannedNodes {
    pub fn new() -> BannedNodes {
        BannedNodes {
            by_id:   HashSet::new(),
            by_addr: HashSet::new(),
        }
    }

    /// Inserts a `BannedNode`
    ///
    /// Returns `true` if it was inserted in either sub-sets.
    pub fn insert(&mut self, b: BannedNode) -> bool {
        match b {
            BannedNode::ById(id) => self.by_id.insert(id),
            BannedNode::ByAddr(addr) => self.by_addr.insert(addr),
        }
    }

    /// Removes a `BannedNode`
    ///
    /// Returns `true` if it was removed in either sub-sets.
    pub fn remove(&mut self, b: &BannedNode) -> bool {
        match b {
            BannedNode::ById(id) => self.by_id.remove(id),
            BannedNode::ByAddr(addr) => self.by_addr.remove(addr),
        }
    }

    /// Lookup of a `P2PNodeId`
    pub fn is_id_banned(&self, id: P2PNodeId) -> bool { self.by_id.contains(&id) }

    /// Lookup of a tuple `(IdAddr, u16)`
    pub fn is_addr_banned(&self, addr: IpAddr) -> bool { self.by_addr.contains(&addr) }
}

impl Serializable for BannedNode {
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        match self {
            BannedNode::ById(id) => {
                0u8.serialize(archive)?;
                id.serialize(archive)
            }
            BannedNode::ByAddr(addr) => {
                1u8.serialize(archive)?;
                addr.serialize(archive)
            }
        }
    }
}

impl Deserializable for BannedNode {
    #[inline]
    fn deserialize<A>(archive: &mut A) -> Fallible<BannedNode>
    where
        A: ReadArchive, {
        let bn = match u8::deserialize(archive)? {
            0 => BannedNode::ById(P2PNodeId::deserialize(archive)?),
            1 => BannedNode::ByAddr(IpAddr::deserialize(archive)?),
            _ => bail!("Unsupported type of `BanNode`"),
        };

        Ok(bn)
    }
}

pub fn insert_ban(kvs_handle: &RwLock<Rkv>, id: &[u8]) -> Fallible<()> {
    let ban_kvs_env = safe_read!(kvs_handle)?;
    let ban_store = ban_kvs_env.open_single("bans", StoreOptions::create())?;
    let mut writer = ban_kvs_env.write()?;
    // TODO: insert ban expiry timestamp as the Value
    ban_store.put(&mut writer, id, &Value::U64(0))?;

    Ok(())
}

pub fn remove_ban(kvs_handle: &RwLock<Rkv>, id: &[u8]) -> Fallible<()> {
    let ban_kvs_env = safe_read!(kvs_handle)?;
    let ban_store = ban_kvs_env.open_single("bans", StoreOptions::create())?;
    let mut writer = ban_kvs_env.write()?;

    ban_store.delete(&mut writer, id)?;

    Ok(())
}

#[cfg(test)]
pub mod tests {
    use super::BannedNode;
    use crate::common::P2PNodeId;
    use std::{net::IpAddr, str::FromStr};

    pub fn dummy_ban_node(addr: Option<IpAddr>) -> BannedNode {
        if let Some(addr) = addr {
            BannedNode::ByAddr(addr)
        } else {
            BannedNode::ById(P2PNodeId::from_str("2A").unwrap())
        }
    }
}
