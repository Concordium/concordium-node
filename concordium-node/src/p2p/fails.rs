use failure::{Error, Fail};

#[derive(Debug, Fail)]
#[fail(display = "Peer not found")]
pub struct PeerNotFoundError;

#[derive(Debug, Fail)]
#[fail(display = "Peer marked as unreachable, won't try it")]
pub struct UnreachablePeerError;

#[derive(Debug, Fail)]
#[fail(display = "Already connected to peer")]
pub struct DuplicatePeerError;

#[derive(Debug, Fail)]
#[fail(display = "Invalid receiver ID for message")]
pub struct EmptyIdInSendRequest;

impl EmptyIdInSendRequest {
    pub fn to_err(self) -> Error { Error::from(self) }
}

#[derive(Debug, Fail)]
#[fail(display = "Connection requested by banned node")]
pub struct BannedNodeRequestedConnectionError;

#[derive(Debug, Fail)]
#[fail(display = "Thread join error")]
pub struct JoinError;
