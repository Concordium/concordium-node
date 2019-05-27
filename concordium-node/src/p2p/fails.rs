use crate::common::P2PNodeId;
use failure::{Error, Fail};
use std::{
    fmt::{self, Display},
    net::SocketAddr,
};

#[derive(Debug, Fail)]
#[fail(display = "Peer not found")]
pub struct PeerNotFoundError;

#[derive(Debug, Fail)]
#[fail(display = "Peer marked as unreachable, won't try it")]
pub struct UnreachablePeerError;

#[derive(Debug, Fail)]
pub struct DuplicatePeerError {
    pub peer_id_opt: Option<P2PNodeId>,
    pub addr:        SocketAddr,
}

impl Display for DuplicatePeerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(id) = self.peer_id_opt {
            write!(
                f,
                "Already connected to {}/{}:{}",
                id,
                self.addr.ip(),
                self.addr.port()
            )
        } else {
            write!(
                f,
                "Already connected to {}:{}",
                self.addr.ip(),
                self.addr.port()
            )
        }
    }
}

#[derive(Debug, Fail)]
#[fail(display = "Connection requested by banned node")]
pub struct BannedNodeRequestedConnectionError;

#[derive(Debug, Fail)]
#[fail(display = "Missing fields in attempt to create a Tls Server")]
pub struct MissingFieldsOnTlsServerBuilder;

#[derive(Debug, Fail)]
#[fail(
    display = "Maximum amount of peers reached {}/{}",
    amount_of_peers, max_peers
)]
pub struct MaxmimumAmountOfPeers {
    pub amount_of_peers: u16,
    pub max_peers:       u16,
}

#[derive(Debug, Fail)]
#[fail(display = "Thread join error")]
pub struct JoinError {
    #[fail(cause)]
    pub cause: Error,
}
