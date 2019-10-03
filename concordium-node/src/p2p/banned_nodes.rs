use byteorder::{ByteOrder, LittleEndian, ReadBytesExt, WriteBytesExt};
use failure::{self, format_err, Fallible};

use crate::common::P2PNodeId;
use concordium_common::{Serial, SerializeToBytes};

use std::{
    collections::HashSet,
    convert::TryFrom,
    io::{Cursor, Read},
    net::{IpAddr, Ipv4Addr, Ipv6Addr},
};

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
}

impl Serial for BannedNode {
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

impl<'a, 'b: 'a> SerializeToBytes<'a, 'b> for BannedNode {
    type Source = &'a [u8];

    fn deserialize(bytes: &[u8]) -> Fallible<Self> {
        let mut cursor = Cursor::new(bytes);

        let mut t = [0u8; 1];
        cursor.read_exact(&mut t)?;

        match t[0] {
            0 => Ok(BannedNode::ById(P2PNodeId(LittleEndian::read_u64(
                &cursor.get_ref()[1..],
            )))),
            1 => {
                let mut t = [0u8; 1];
                cursor.read_exact(&mut t)?;

                match t[0] {
                    4 => {
                        let mut tgt = [0u8; 4];
                        cursor.read_exact(&mut tgt)?;
                        Ok(BannedNode::ByAddr(IpAddr::V4(Ipv4Addr::from(tgt))))
                    }
                    6 => {
                        let mut tgt = [0u8; 16];
                        cursor.read_exact(&mut tgt)?;
                        Ok(BannedNode::ByAddr(IpAddr::V6(Ipv6Addr::from(tgt))))
                    }
                    _ => bail!("Can't deserialize the IP of a banned node"),
                }
            }
            _ => bail!("Can't deserialize the type of a banned node"),
        }
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        match self {
            BannedNode::ById(id) => {
                target.write_u8(0)?;
                target.write_all(&id.as_raw().to_le_bytes())?;
            }
            BannedNode::ByAddr(addr) => {
                target.write_u8(1)?;
                match addr {
                    IpAddr::V4(ip) => {
                        target.write_u8(4)?;
                        target.write_all(&ip.octets())?;
                    }
                    IpAddr::V6(ip) => {
                        target.write_u8(6)?;
                        target.write_all(&ip.octets())?;
                    }
                }
            }
        }

        Ok(())
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
