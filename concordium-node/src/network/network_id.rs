use crate::common::serialization::{Deserializable, ReadArchive, Serializable, WriteArchive};

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

impl Serializable for NetworkId {
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        archive.write_u16(self.id)
    }
}

impl Deserializable for NetworkId {
    #[inline]
    fn deserialize<A>(archive: &mut A) -> Fallible<NetworkId>
    where
        A: ReadArchive, {
        Ok(NetworkId::from(archive.read_u16()?))
    }
}
