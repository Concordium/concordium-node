//! The node identifier.

use byteorder::{ReadBytesExt, WriteBytesExt};
use crypto_common::{Buffer, Deserial, Serial};
use failure::Fallible;
use rand::distributions::{Distribution, Standard, Uniform};
use std::fmt;

pub type PeerId = u64;

/// An identifier used by the node to identify itself to its peers.
/// It is only in a descriptive manner, for logging and sending to other peers,
/// not for identifying peers locally by the node.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
#[repr(transparent)]
pub struct P2PNodeId(pub PeerId);

impl Distribution<P2PNodeId> for Standard {
    fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> P2PNodeId {
        P2PNodeId(Uniform::new(0, PeerId::max_value()).sample(rng))
    }
}

/// This is implemented manually so that the ID is printed in hex so that
/// it can be recognized.
impl fmt::Debug for P2PNodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{:016x}", self.0) }
}

impl fmt::Display for P2PNodeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{:016x}", self.0) }
}

impl std::str::FromStr for P2PNodeId {
    type Err = failure::Error;

    fn from_str(s: &str) -> Fallible<Self> {
        match PeerId::from_str_radix(s, 16) {
            Ok(n) => Ok(P2PNodeId(n)),
            Err(e) => bail!(
                "Invalid Node Id: ({}). The Node Id should be a hexadecimal string at most 16 \
                 characters long.",
                e
            ),
        }
    }
}

impl Serial for P2PNodeId {
    fn serial<W: Buffer + WriteBytesExt>(&self, target: &mut W) { self.0.serial(target); }
}

impl Deserial for P2PNodeId {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        Ok(P2PNodeId(u64::deserial(source)?))
    }
}

impl P2PNodeId {
    /// Obtain the integer behind the node id.
    pub fn as_raw(self) -> PeerId { self.0 }
}
