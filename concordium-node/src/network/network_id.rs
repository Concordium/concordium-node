use crate::network::serialization::{ Serializable, Archive};

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

impl Serializable for NetworkId{
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()> where A: Archive {
        archive.write_u16( self.id)
    }
}
