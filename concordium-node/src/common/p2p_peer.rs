//! Types related to identifying peers.

use crate::{common::P2PNodeId, connection::ConnectionStats};
use anyhow::bail;
use byteorder::{ReadBytesExt, WriteBytesExt};
use concordium_base::common::{Buffer, Deserial, Serial};
use rand::{
    distributions::{Standard, Uniform},
    prelude::Distribution,
};
use std::{
    fmt::{self, Display},
    hash::{Hash, Hasher},
    net::{IpAddr, SocketAddr},
    sync::atomic::Ordering as AtomicOrdering,
};

/// Specifies the type of the node - either a regular `Node` or a
/// `Bootstrapper`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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

/// Identifier we assign to a peer when they connect. This is a purely local
/// identifier that is never transmitted over the network, but it is used
/// internally to keep track of peers (e.g., during catchup) and to ban them.
///
/// This peer identifier is currently tied to a connection, meaning that for
/// each active connection there is exactly one RemotePeerId, and this peer id
/// is used to poll as the `mio` token when querying sockets for incoming
/// packets.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RemotePeerId {
    pub(crate) remote_peer_id: usize,
}

/// Sample a random id uniformly among the possible values.
impl Distribution<RemotePeerId> for Standard {
    fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> RemotePeerId {
        RemotePeerId {
            remote_peer_id: Uniform::new(0, usize::max_value()).sample(rng),
        }
    }
}

/// Display as a 0-padded hex value.
impl Display for RemotePeerId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:016x}", self.remote_peer_id)
    }
}

/// Parse from a (possibly 0 padded) hex value.
impl std::str::FromStr for RemotePeerId {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> anyhow::Result<Self> {
        match usize::from_str_radix(s, 16) {
            Ok(remote_peer_id) => Ok(RemotePeerId {
                remote_peer_id,
            }),
            Err(e) => bail!("Invalid Remote Peer Id ({})", e),
        }
    }
}

/// This assumes that usize is not more than 64-bits, which is the case on
/// existing platforms.
impl From<RemotePeerId> for u64 {
    #[inline]
    fn from(x: RemotePeerId) -> Self { x.remote_peer_id as u64 }
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

/// Serialize as big endian 8 bytes.
impl Serial for RemotePeerId {
    fn serial<W: Buffer + WriteBytesExt>(&self, target: &mut W) {
        (self.remote_peer_id as u64).serial(target);
    }
}

impl Deserial for RemotePeerId {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> anyhow::Result<Self> {
        Ok(RemotePeerId {
            remote_peer_id: u64::deserial(source)? as usize,
        })
    }
}

impl RemotePeerId {
    /// Convert a peer id to a mio Token. This is deliberately made into a
    /// method rather than implementing a From trait for documentation
    /// purposes; the name documents intent better then `::from` or `.into`
    /// would.
    #[inline]
    pub fn to_token(self) -> mio::Token { mio::Token(self.remote_peer_id) }
}

/// A remote node in the network.
#[derive(Debug, Copy, Clone, Eq)]
pub struct RemotePeer {
    /// Id by which the node wants to be identified.
    /// None before the handshake is complete, `Some` after
    /// the handshake completes.
    /// This id is only used in logging and externally, i.e., when
    /// reporting peers. It is not used by the node for managing peers.
    pub self_id:       Option<P2PNodeId>,
    /// Our local address of the node.
    pub addr:          SocketAddr,
    /// Our local identifier for the peer.
    pub local_id:      RemotePeerId,
    /// External port communicated to us by the node itself as part of the
    /// handshake. This is the port that the node can be reached at to
    /// initiate connections, as a result this is the port that is
    /// advertised as part of the peer list we serve.
    pub external_port: u16,
    pub peer_type:     PeerType,
}

// This instance is only used for storing peers in buckets, in which case
// it makes sense the way it is defined, but if it is needed for some other
// purpose then it should be considered whether it is adequate to only compare
// the ids.
impl PartialEq for RemotePeer {
    fn eq(&self, other: &Self) -> bool { self.local_id == other.local_id }
}

// This instance is only used for storing peers in buckets, in which case
// it makes sense the way it is defined, but if it is needed for some other
// purpose then it should be considered whether it is adequate to only hash the
// ids.
impl Hash for RemotePeer {
    fn hash<H: Hasher>(&self, state: &mut H) { self.local_id.hash(state); }
}

impl RemotePeer {
    /// Converts a remote peer to a peer object suitable for transmitting over
    /// the network, as long as its id is known, i.e., as long as it is
    /// post-handshake.
    pub fn peer(&self) -> Option<P2PPeer> {
        self.self_id.map(|id| P2PPeer {
            id,
            addr: self.external_addr(),
            peer_type: self.peer_type,
        })
    }

    /// Gets the external socket address of a remote peer.
    pub fn external_addr(&self) -> SocketAddr {
        SocketAddr::new(self.addr.ip(), self.external_port)
    }
}

/// Information about a peer that is transmitted over the network.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct P2PPeer {
    /// The peer's chosen identifier.
    pub id:        P2PNodeId,
    /// The peer's address. Note that this is the address they advertise as part
    /// of the handshake, and thus it is ostensibly the address where it
    /// listens to for new connections.
    pub addr:      SocketAddr,
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

/// Contains some statistics related to a peer.
#[derive(Debug)]
pub struct PeerStats {
    /// The peer's self identifier. Only used for reporting.
    pub self_id:        P2PNodeId,
    pub addr:           SocketAddr,
    pub external_port:  u16,
    /// Our identifier for the remote peer.
    pub local_id:       RemotePeerId,
    pub peer_type:      PeerType,
    pub latency:        u64,
    pub msgs_sent:      u64,
    pub msgs_received:  u64,
    pub bytes_sent:     u64,
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
    /// NB: This is the address on which we can connect to the peer, which is
    /// in general different from the address we are currently connected to
    /// them.
    pub fn external_address(&self) -> SocketAddr {
        SocketAddr::new(self.addr.ip(), self.external_port)
    }
}
