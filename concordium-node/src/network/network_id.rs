use byteorder::{ReadBytesExt, WriteBytesExt};
use concordium_common::Serial;

use failure::Fallible;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub struct NetworkId {
    pub id: u16,
}

impl From<u16> for NetworkId {
    fn from(id: u16) -> Self { NetworkId { id } }
}

impl fmt::Display for NetworkId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{:05}", self.id) }
}

impl Serial for NetworkId {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        Ok(NetworkId::from(u16::deserial(source)?))
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> { self.id.serial(target) }
}
