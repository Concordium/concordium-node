/*!
The node identifier.
*/
use anyhow::bail;
use byteorder::{ReadBytesExt, WriteBytesExt};
use crypto_common::{Buffer, Deserial, Serial};
use rand::distributions::{Distribution, Standard};
use std::fmt;

/// todo: documentation.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum P2PNodeId {
    // Exists purely for backwards compatibility and should be removed in a future version
    // The textual representation is a hex encoding of the underlying `u64` value.
    DEPRECATED(u64),
    // The new id, this is authenticated in the sense that it is the static public key used
    // in the noise handshake peers exchange when connecting.
    // The textual representation is a base64 encoding of the underling `[u8;32]` value.
    AUTHENTICATED([u8; noiseexplorer_xx::consts::DHLEN]),
}

impl Distribution<P2PNodeId> for Standard {
    fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> P2PNodeId {
        P2PNodeId::AUTHENTICATED(rng.gen::<[u8; noiseexplorer_xx::consts::DHLEN]>())
    }
}

/// This is implemented manually so that the ID is printed in hex so that
/// it can be recognized.
impl fmt::Debug for P2PNodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            P2PNodeId::DEPRECATED(id) => write!(f, "(DEPRECATED ID) {:016x}", id),
            P2PNodeId::AUTHENTICATED(id) => write!(f, "(AUTHENTICATED ID){}", &base64::encode(id)),
        }
    }
}

impl fmt::Display for P2PNodeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            P2PNodeId::DEPRECATED(id) => write!(f, "(DEPRECATED ID) {:016x}", id),
            P2PNodeId::AUTHENTICATED(id) => write!(f, "(AUTHENTICATED ID){}", &base64::encode(id)),
        }
    }
}

impl std::str::FromStr for P2PNodeId {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> anyhow::Result<Self> {
        // First we try decode it as `P2PNodeId::AUTHENTICATED`
        match base64::decode(s) {
            Ok(id) => {
                let buff = id.as_slice();
                if buff.len() != noiseexplorer_xx::consts::DHLEN {
                    bail!(
                        "Invalid node id. The node id should be a base64 encoding of the Noise \
                         pub key. (32 bytes)"
                    );
                } else {
                    let mut id_buff = [0u8; noiseexplorer_xx::consts::DHLEN];
                    id_buff.clone_from_slice(buff);
                    Ok(P2PNodeId::AUTHENTICATED(id_buff))
                }
            }
            Err(_) => {
                // if that fails we try decode is as `P2PNodeId::DEPRECATED`
                match u64::from_str_radix(s, 16) {
                    Ok(id) => Ok(P2PNodeId::DEPRECATED(id)),
                    Err(e) => bail!(
                        "Invalid Node Id: ({}). The Node Id should be a hexadecimal representing \
                         either the peers public key or a
 string at most 16 characters long.",
                        e
                    ),
                }
            }
        }
    }
}

impl Serial for P2PNodeId {
    fn serial<W: Buffer + WriteBytesExt>(&self, target: &mut W) {
        match self {
            P2PNodeId::DEPRECATED(id) => id.serial(target),
            P2PNodeId::AUTHENTICATED(id) => id.serial(target),
        }
    }
}

impl Deserial for P2PNodeId {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> anyhow::Result<Self> {
        // First we deserialize as `P2PNodeId::AUTHENTICATED` type. If that fails we
        // fallback to `P2PNodeId::DEPRECATED`
        match <[u8; 32]>::deserial(source) {
            Ok(id) => Ok(P2PNodeId::AUTHENTICATED(id)),
            Err(_) => Ok(P2PNodeId::DEPRECATED(<u64>::deserial(source)?)),
        }
    }
}
