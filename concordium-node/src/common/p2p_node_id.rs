use crate::network::serialization::{ Serializable, Archive};

use failure::Fallible;
use rand::distributions::{ Uniform, Distribution };
use std::{ fmt
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub struct P2PNodeId(pub u64);

impl Default for P2PNodeId {
    fn default() -> Self {
        let mut rng = rand::thread_rng();
        let n = Uniform::from(0..u64::max_value()).sample(&mut rng);
        P2PNodeId(n)
    }
}

impl fmt::Display for P2PNodeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{:016x}", self.0) }
}

impl std::str::FromStr for P2PNodeId {
    type Err = failure::Error;

    fn from_str(s: &str) -> Fallible<Self> {
        match u64::from_str_radix(s, 16) {
            Ok(n) => Ok(P2PNodeId(n)),
            Err(e) => bail!("Invalid Node Id ({})", e),
        }
    }
}

impl Serializable for P2PNodeId {
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()> where A: Archive {
        archive.write_u64( self.0)
    }
}
