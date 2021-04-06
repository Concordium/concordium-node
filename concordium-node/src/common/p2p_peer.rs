//! Peer objects.

use byteorder::{ReadBytesExt, WriteBytesExt};
use crypto_common::{Buffer, Deserial, Fallible, Serial};
use rand::{
    distributions::{Standard, Uniform},
    prelude::Distribution,
};

use crate::{common::P2PNodeId, connection::ConnectionStats};

use std::{
    fmt::{self, Display},
    hash::{Hash, Hasher},
    net::{IpAddr, SocketAddr},
    sync::atomic::Ordering as AtomicOrdering,
};

/// Specifies the type of the node - either a regular `Node` or a
/// `Bootstrapper`.
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

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub struct RemotePeerId {
    pub(crate) remote_peer_id: usize,
}

impl Distribution<RemotePeerId> for Standard {
    fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> RemotePeerId {
        RemotePeerId {
            remote_peer_id: Uniform::new(0, usize::max_value()).sample(rng),
        }
    }
}

impl Display for RemotePeerId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:016x}", self.remote_peer_id)
    }
}

impl std::str::FromStr for RemotePeerId {
    type Err = failure::Error;

    fn from_str(s: &str) -> Fallible<Self> {
        match usize::from_str_radix(s, 16) {
            Ok(remote_peer_id) => Ok(RemotePeerId {
                remote_peer_id,
            }),
            Err(e) => bail!("Invalid Remote Peer Id ({})", e),
        }
    }
}

impl From<usize> for RemotePeerId {
    #[inline]
    fn from(remote_peer_id: usize) -> Self {
        Self {
            remote_peer_id,
        }
    }
}

impl From<mio::Token> for RemotePeerId {
    #[inline]
    fn from(token: mio::Token) -> Self {
        Self {
            remote_peer_id: token.0,
        }
    }
}

impl Serial for RemotePeerId {
    fn serial<W: Buffer + WriteBytesExt>(&self, target: &mut W) {
        (self.remote_peer_id as u64).serial(target);
    }
}

impl Deserial for RemotePeerId {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        Ok(RemotePeerId {
            remote_peer_id: u64::deserial(source)? as usize,
        })
    }
}

impl RemotePeerId {
    #[inline]
    pub fn to_token(self) -> mio::Token { mio::Token(self.remote_peer_id) }
}

/// Defines a node in the network.
#[derive(Debug, Clone, Copy, PartialEq)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub struct P2PPeer {
    /// The peer's chosen identifier.
    pub id: P2PNodeId,
    pub addr: SocketAddr,
    pub peer_type: PeerType,
}

impl P2PPeer {
    /// Get the peer's IP address.
    pub fn ip(&self) -> IpAddr { self.addr.ip() }

    /// Get the peer's port.
    pub fn port(&self) -> u16 { self.addr.port() }
}

impl Display for P2PPeer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}/{}:{}", self.id, self.addr.ip(), self.addr.port())
    }
}

/// Defines a remote node in the network.
#[derive(Debug, Copy, Clone, Eq)]
pub struct RemotePeer {
    /// Id by which the node wants to be identifier.
    /// None before the handshake is complete, `Some` after
    /// the handshake completes.
    pub id: Option<P2PNodeId>,
    /// Our local address of the node.
    pub addr: SocketAddr,
    /// Our local identifier for the peer.
    pub local_id: RemotePeerId,
    pub external_port: u16,
    pub peer_type: PeerType,
}

impl PartialEq for RemotePeer {
    fn eq(&self, other: &Self) -> bool { self.local_id == other.local_id }
}

impl Hash for RemotePeer {
    fn hash<H: Hasher>(&self, state: &mut H) { self.local_id.hash(state); } // FIXME: Check that this instance is good.
}

impl RemotePeer {
    /// Converts a remote peer to a regular peer object, as long as its id is
    /// known.
    pub fn peer(&self) -> Option<P2PPeer> {
        if let Some(id) = self.id {
            Some(P2PPeer {
                id,
                addr: self.addr,
                peer_type: self.peer_type,
            })
        } else {
            None
        }
    }

    /// Gets the external socket address of a remote peer.
    pub fn external_addr(&self) -> SocketAddr {
        SocketAddr::new(self.addr.ip(), self.external_port)
    }
}

/// Contains some statistics related to a peer.
#[derive(Debug)]
pub struct PeerStats {
    /// The peer's self identifier. Only used for reporting.
    pub self_id: P2PNodeId,
    pub addr: SocketAddr,
    pub external_port: u16,
    /// Our identifier for the remote peer.
    pub local_id: RemotePeerId,
    pub peer_type: PeerType,
    pub latency: u64,
    pub msgs_sent: u64,
    pub msgs_received: u64,
    pub bytes_sent: u64,
    pub bytes_received: u64,
}

impl PeerStats {
    /// Creates a new peer stats object.
    pub fn new(
        local_id: RemotePeerId,
        self_id: P2PNodeId,
        addr: SocketAddr,
        external_port: u16,
        peer_type: PeerType,
        conn_stats: &ConnectionStats,
    ) -> PeerStats {
        PeerStats {
            local_id,
            self_id,
            addr,
            external_port,
            peer_type,
            latency: conn_stats.get_latency(),
            msgs_sent: conn_stats.messages_sent.load(AtomicOrdering::Relaxed),
            msgs_received: conn_stats.messages_received.load(AtomicOrdering::Relaxed),
            bytes_sent: conn_stats.bytes_sent.load(AtomicOrdering::Relaxed),
            bytes_received: conn_stats.bytes_received.load(AtomicOrdering::Relaxed),
        }
    }

    /// Gets the external address of the peer.
    pub fn external_address(&self) -> SocketAddr {
        SocketAddr::new(self.addr.ip(), self.external_port)
    }
}
