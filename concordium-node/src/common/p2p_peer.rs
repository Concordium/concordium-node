use crate::common::{
    fails, get_current_stamp,
    serialization::{Deserializable, ReadArchive, Serializable, WriteArchive},
    P2PNodeId, PeerType,
};

use failure::Fallible;
use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
    net::{IpAddr, SocketAddr},
};

#[derive(Debug, Clone, Builder)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
#[builder(build_fn(skip))]
pub struct P2PPeer {
    id: P2PNodeId,
    pub addr: SocketAddr,
    #[builder(setter(skip))]
    last_seen: u64,
    peer_type: PeerType,
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
                last_seen: get_current_stamp(),
            })
        } else {
            bail!(fails::MissingFieldsError::new(
                self.peer_type,
                self.id,
                self.addr
            ))
        }
    }
}

impl P2PPeer {
    pub fn from(peer_type: PeerType, id: P2PNodeId, addr: SocketAddr) -> Self {
        P2PPeer {
            peer_type,
            id,
            addr,
            last_seen: get_current_stamp(),
        }
    }

    pub fn id(&self) -> P2PNodeId { self.id }

    pub fn ip(&self) -> IpAddr { self.addr.ip() }

    pub fn port(&self) -> u16 { self.addr.port() }

    pub fn last_seen(&self) -> u64 { self.last_seen }

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

impl Serializable for P2PPeer {
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        self.peer_type.serialize(archive)?;
        self.id.serialize(archive)?;
        self.addr.serialize(archive)
    }
}

impl Deserializable for P2PPeer {
    #[inline]
    fn deserialize<A>(archive: &mut A) -> Fallible<P2PPeer>
    where
        A: ReadArchive, {
        Ok(P2PPeer::from(
            PeerType::deserialize(archive)?,
            P2PNodeId::deserialize(archive)?,
            SocketAddr::deserialize(archive)?,
        ))
    }
}
