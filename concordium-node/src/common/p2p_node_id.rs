/*!
The node identifier.
*/
use anyhow::bail;
use byteorder::{ReadBytesExt, WriteBytesExt};
use crypto_common::{Buffer, Deserial, Serial};
use rand::distributions::{Distribution, Standard};
use std::fmt;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PeerId(pub [u8; noiseexplorer_xx::consts::DHLEN]);

/// todo: documentation.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct P2PNodeId(pub PeerId);

impl Distribution<P2PNodeId> for Standard {
    fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> P2PNodeId {
        P2PNodeId(PeerId(rng.gen::<[u8; noiseexplorer_xx::consts::DHLEN]>()))
    }
}

/// This is implemented manually so that the ID is printed in hex so that
/// it can be recognized.
impl fmt::Debug for P2PNodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &hex::encode(self.0 .0))
    }
}

impl fmt::Display for P2PNodeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", &hex::encode(self.0 .0))
    }
}

impl std::str::FromStr for P2PNodeId {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> anyhow::Result<Self> {
        let mut id_buff = [0u8; noiseexplorer_xx::consts::DHLEN];
        // First we try decode it as `PeerId::AUTHEANTICATED`
        match hex::decode_to_slice(s, &mut id_buff) {
            Ok(()) => Ok(P2PNodeId(PeerId(id_buff))),
            Err(e) => {
                bail!(
                    "Invalid Node Id: ({}). The Node Id should be a hexadecimal representing \
                     either the peers public key or a
 string at most 16 characters long.",
                    e
                )
            }
        }
    }
}

impl Serial for P2PNodeId {
    fn serial<W: Buffer + WriteBytesExt>(&self, target: &mut W) { self.0 .0.serial(target) }
}

impl Deserial for P2PNodeId {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> anyhow::Result<Self> {
        Ok(P2PNodeId(PeerId(<[u8; 32]>::deserial(source)?)))
    }
}

impl P2PNodeId {
    /// Obtain the integer behind the node id.
    pub fn as_raw(self) -> PeerId { self.0 }
}
