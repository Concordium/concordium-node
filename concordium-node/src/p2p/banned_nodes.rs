use byteorder::{ReadBytesExt, WriteBytesExt};
use failure::{self, Fallible};

use crate::common::P2PNodeId;
use concordium_common::serial::{NoParam, Serial};

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
}

impl Serial for BannedNode {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        let bn = match source.read_u8()? {
            0 => BannedNode::ById(P2PNodeId::deserial(source)?),
            1 => BannedNode::ByAddr(IpAddr::deserial(source)?),
            _ => bail!("Unsupported type of `BanNode`"),
        };

        Ok(bn)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        match self {
            BannedNode::ById(id) => {
                target.write_u8(0)?;
                id.serial(target)
            }
            BannedNode::ByAddr(addr) => {
                target.write_u8(1)?;
                addr.serial(target)
            }
        }
    }
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
