use byteorder::{ReadBytesExt, WriteBytesExt};
use failure::{self, Fallible};

use crate::common::P2PNodeId;
use concordium_common::serial::{NoParam, Serial};

use std::net::IpAddr;

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
