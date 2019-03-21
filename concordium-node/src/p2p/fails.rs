use failure::{Fail, Error};

#[derive(Debug,Fail)]
#[fail(display = "Peer not found")]
pub struct PeerNotFoundError {}

#[derive(Debug,Fail)]
#[fail(display = "Peer marked as unreachable, won't try it")]
pub struct UnreachablePeerError {}

#[derive(Debug,Fail)]
#[fail(display = "Already connected to peer")]
pub struct DuplicatePeerError {}

#[derive(Debug,Fail)]
#[fail(display = "Invalid receiver ID for message")]
pub struct EmptyIdInSendRequest {}

impl EmptyIdInSendRequest {
    pub fn to_err(self) -> Error {
        Error::from(self)
    }
}
