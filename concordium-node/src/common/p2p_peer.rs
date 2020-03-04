use crate::{common::P2PNodeId, connection::ConnectionStats};

use std::{
    fmt::{self, Display},
    hash::{Hash, Hasher},
    net::{IpAddr, SocketAddr},
    sync::{
        atomic::{AtomicU16, Ordering as AtomicOrdering},
        RwLock,
    },
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

/// Defines a node in the network.
#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub struct P2PPeer {
    pub id:        P2PNodeId,
    pub addr:      SocketAddr,
    pub peer_type: PeerType,
}

impl P2PPeer {
    /// Get the peer's IP address.
    pub fn ip(&self) -> IpAddr { self.addr.ip() }

    /// Get the peer's port.
    pub fn port(&self) -> u16 { self.addr.port() }
}

impl From<(PeerType, P2PNodeId, SocketAddr)> for P2PPeer {
    fn from((peer_type, id, addr): (PeerType, P2PNodeId, SocketAddr)) -> Self {
        P2PPeer {
            peer_type,
            id,
            addr,
        }
    }
}

impl PartialEq for P2PPeer {
    fn eq(&self, other: &P2PPeer) -> bool { self.id == other.id }
}

impl Eq for P2PPeer {}

impl Hash for P2PPeer {
    fn hash<H: Hasher>(&self, state: &mut H) { self.id.hash(state); }
}

impl Display for P2PPeer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}/{}:{}", self.id, self.addr.ip(), self.addr.port())
    }
}

/// Defines a remote node in the network.
#[derive(Debug)]
pub struct RemotePeer {
    pub id:                 RwLock<Option<P2PNodeId>>,
    pub addr:               SocketAddr,
    pub peer_external_port: AtomicU16,
    pub peer_type:          PeerType,
}

impl RemotePeer {
    /// Converts a remote peer to a regular peer object, as long as its id is
    /// known.
    pub fn peer(&self) -> Option<P2PPeer> {
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

    /// Gets the external port of a remote peer.
    pub fn peer_external_port(&self) -> u16 { self.peer_external_port.load(AtomicOrdering::SeqCst) }

    /// Gets the external socket address of a remote peer.
    pub fn peer_external_addr(&self) -> SocketAddr {
        SocketAddr::new(self.addr.ip(), self.peer_external_port.load(AtomicOrdering::SeqCst))
    }
}

impl From<P2PPeer> for RemotePeer {
    fn from(peer: P2PPeer) -> Self {
        Self {
            id:                 RwLock::new(Some(peer.id)),
            addr:               peer.addr,
            peer_external_port: AtomicU16::new(peer.addr.port()),
            peer_type:          peer.peer_type,
        }
    }
}

/// Contains some statistics related to a peer.
#[derive(Debug)]
pub struct PeerStats {
    pub id:                 u64,
    pub addr:               SocketAddr,
    pub peer_external_port: u16,
    pub peer_type:          PeerType,
    pub sent:               u64,
    pub received:           u64,
    pub valid_latency:      bool,
    pub measured_latency:   u64,
    pub bytes_sent:         u64,
    pub bytes_received:     u64,
}

impl PeerStats {
    /// Creates a new peer stats object.
    pub fn new(
        id: u64,
        addr: SocketAddr,
        peer_external_port: u16,
        peer_type: PeerType,
        conn_stats: &ConnectionStats,
    ) -> PeerStats {
        PeerStats {
            id,
            addr,
            peer_external_port,
            peer_type,
            sent: conn_stats.messages_sent.load(AtomicOrdering::Relaxed),
            received: conn_stats.messages_received.load(AtomicOrdering::Relaxed),
            valid_latency: conn_stats.valid_latency.load(AtomicOrdering::Relaxed),
            measured_latency: conn_stats.last_latency.load(AtomicOrdering::Relaxed),
            bytes_sent: conn_stats.bytes_sent.load(AtomicOrdering::Relaxed),
            bytes_received: conn_stats.bytes_received.load(AtomicOrdering::Relaxed),
        }
    }

    /// Gets the external address of the peer.
    pub fn external_address(&self) -> SocketAddr {
        SocketAddr::new(self.addr.ip(), self.peer_external_port)
    }
}
