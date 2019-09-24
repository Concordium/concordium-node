use byteorder::{ReadBytesExt, WriteBytesExt};
use failure::{Error, Fallible};

use crate::common::{fails, P2PNodeId};
use concordium_common::Serial;

use std::{
    cmp::Ordering,
    fmt::{self, Display},
    hash::{Hash, Hasher},
    net::{IpAddr, SocketAddr},
    sync::{atomic::AtomicU64, Arc, RwLock},
};

const PEER_TYPE_NODE: u8 = 0;
const PEER_TYPE_BOOTSTRAPPER: u8 = 1;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub enum PeerType {
    Node,
    Bootstrapper,
}

impl fmt::Display for PeerType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            PeerType::Node => "Node",
            PeerType::Bootstrapper => "Bootstrapper",
        })
    }
}

impl Serial for PeerType {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        match source.read_u8() {
            Ok(PEER_TYPE_NODE) => Ok(PeerType::Node),
            Ok(PEER_TYPE_BOOTSTRAPPER) => Ok(PeerType::Bootstrapper),
            _ => bail!("Unsupported PeerType"),
        }
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        let byte = match self {
            PeerType::Node => PEER_TYPE_NODE,
            PeerType::Bootstrapper => PEER_TYPE_BOOTSTRAPPER,
        };
        Ok(target.write_u8(byte)?)
    }
}

#[derive(Debug, Clone, Copy, Builder)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
#[builder(build_fn(skip))]
pub struct P2PPeer {
    pub id:        P2PNodeId,
    pub addr:      SocketAddr,
    pub peer_type: PeerType,
}

impl P2PPeerBuilder {
    pub fn build(&mut self) -> Fallible<P2PPeer> {
        let id = self.id.unwrap_or_else(P2PNodeId::default);
        self.id(id);

        if let Some((peer_type, (id, addr))) = self
            .peer_type
            .iter()
            .zip(self.id.iter().zip(self.addr.iter()))
            .next()
        {
            Ok(P2PPeer {
                peer_type: *peer_type,
                addr:      *addr,
                id:        *id,
            })
        } else {
            Err(Error::from(fails::MissingFieldsError::new(
                self.peer_type,
                self.id,
                self.addr,
            )))
        }
    }
}

impl P2PPeer {
    pub fn from(peer_type: PeerType, id: P2PNodeId, addr: SocketAddr) -> Self {
        P2PPeer {
            peer_type,
            id,
            addr,
        }
    }

    pub fn id(&self) -> P2PNodeId { self.id }

    pub fn ip(&self) -> IpAddr { self.addr.ip() }

    pub fn port(&self) -> u16 { self.addr.port() }

    pub fn peer_type(&self) -> PeerType { self.peer_type }
}

impl PartialEq for P2PPeer {
    fn eq(&self, other: &P2PPeer) -> bool { self.id == other.id() }
}

impl Eq for P2PPeer {}

impl Hash for P2PPeer {
    fn hash<H: Hasher>(&self, state: &mut H) { self.id.hash(state); }
}

impl Ord for P2PPeer {
    fn cmp(&self, other: &P2PPeer) -> Ordering { self.id.cmp(&other.id()) }
}

impl PartialOrd for P2PPeer {
    fn partial_cmp(&self, other: &P2PPeer) -> Option<Ordering> { Some(self.cmp(other)) }
}

impl Serial for P2PPeer {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        Ok(P2PPeer::from(
            PeerType::deserial(source)?,
            P2PNodeId::deserial(source)?,
            SocketAddr::deserial(source)?,
        ))
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        self.peer_type.serial(target)?;
        self.id.serial(target)?;
        self.addr.serial(target)
    }
}

impl Display for P2PPeer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}/{}:{}", self.id(), self.addr.ip(), self.addr.port())
    }
}

#[derive(Debug, Clone)]
pub struct RemotePeer {
    pub id:        Arc<RwLock<Option<P2PNodeId>>>,
    pub addr:      SocketAddr,
    pub peer_type: PeerType,
}

impl RemotePeer {
    pub fn peer(self) -> Option<P2PPeer> {
        if let Some(id) = &*read_or_die!(self.id) {
            Some(P2PPeer {
                id:        *id,
                addr:      self.addr,
                peer_type: self.peer_type,
            })
        } else {
            None
        }
    }

    pub fn addr(&self) -> SocketAddr { self.addr }

    pub fn peer_type(&self) -> PeerType { self.peer_type }
}

impl From<P2PPeer> for RemotePeer {
    fn from(peer: P2PPeer) -> Self {
        Self {
            id:        Arc::new(RwLock::new(Some(peer.id))),
            addr:      peer.addr,
            peer_type: peer.peer_type,
        }
    }
}

#[derive(Debug, Clone)]
pub struct PeerStats {
    pub id:               u64,
    pub addr:             SocketAddr,
    pub peer_type:        PeerType,
    pub sent:             Arc<AtomicU64>,
    pub received:         Arc<AtomicU64>,
    pub measured_latency: Arc<AtomicU64>,
}

impl PeerStats {
    pub fn new(
        id: u64,
        addr: SocketAddr,
        peer_type: PeerType,
        sent: Arc<AtomicU64>,
        received: Arc<AtomicU64>,
        measured_latency: Arc<AtomicU64>,
    ) -> PeerStats {
        PeerStats {
            id,
            addr,
            peer_type,
            sent,
            received,
            measured_latency,
        }
    }
}
