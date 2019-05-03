use crate::common::{
    serialization::{Deserializable, ReadArchive, Serializable, WriteArchive},
    P2PNodeId,
};

use failure::Fallible;

use std::{collections::HashSet, net::IpAddr};

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
    pub fn to_db_repr(&self) -> (Option<String>, Option<String>) {
        match self {
            BannedNode::ById(id) => (Some(id.to_string()), None),
            BannedNode::ByAddr(addr) => (None, Some(addr.to_string())),
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
                archive.write_u8(0)?;
                id.serialize(archive)
            }
            BannedNode::ByAddr(addr) => {
                archive.write_u8(1)?;
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
        let bn = match archive.read_u8()? {
            0 => BannedNode::ById(P2PNodeId::deserialize(archive)?),
            1 => BannedNode::ByAddr(IpAddr::deserialize(archive)?),
            _ => bail!("Unsupported type of `BanNode`"),
        };

        Ok(bn)
    }
}

// Consumes [partially] a `UCursor` for deserializing a `BannedNode`
// pub fn deserialize(pkt: &mut UCursor) -> Fallible<BannedNode> {
// let view = pkt.read_into_view(1)?;
// let buf = view.as_slice();
//
// let banned_node = match buf {
// b"0" => BannedNode::ById({
// let min_packet_size = PROTOCOL_NODE_ID_LENGTH;
// ensure!(
// pkt.len() >= pkt.position() + min_packet_size as u64,
// "Node ID chunk needs {} bytes",
// min_packet_size
// );
//
// let view = pkt.read_into_view(min_packet_size)?;
// let buf = view.as_slice();
// P2PNodeId::from_str(&str::from_utf8(&buf[..PROTOCOL_NODE_ID_LENGTH])?)?
// }),
// b"1" => BannedNode::ByAddr(deserialize_ip(pkt)?),
// _ => bail!("Unrecognized slice for deserializing BannedNode"),
// };
//
// Ok(banned_node)
// }

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
