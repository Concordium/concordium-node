use failure::Fail;
use super::{ConnectionType, P2PNodeId};
use std::net::IpAddr;
use num_bigint::ParseBigIntError;

#[derive(Debug, Fail)]
pub enum P2PPeerParseError {
    #[fail(display = "Empty IP or Port on P2PPeer building")]
    EmptyIpPortError(),
    #[fail(display = "Missing fields on P2PPeer build: conn<{:?}>, id<{:?}>, ip<{:?}>, port<{:?}>", connection_type, id, ip, port)]
    MissingFieldsError {
        connection_type: Option<ConnectionType>,
        id: Option<P2PNodeId>,
        ip: Option<IpAddr>,
        port: Option<u16>
    },
    #[fail(display = "Invalid length for specified IP type: type<{}>", ip_type)]
    InvalidLengthForIP {
        ip_type: String
    },
    #[fail(display = "Invalid IP type specified: type<{}>", ip_type)]
    InvalidIpType {
        ip_type: String
    },
    #[fail(display = "Invalid length for serialized P2PPeer")]
    InvalidLength ()
}

#[derive(Debug, Fail)]
#[fail(display = "Error when parsing P2PNodeId")]
pub struct P2PNodeIdError {}

impl From<ParseBigIntError> for P2PNodeIdError {
   fn from(_: ParseBigIntError) -> Self {
        P2PNodeIdError{}
    }
}

#[derive(Debug, Fail)]
#[fail(display = "Error running functor {}", name)]
pub struct FunctorRunningError {
    name: String
}

impl FunctorRunningError {
    pub fn new(n: String) -> Self {
        FunctorRunningError {
            name: n
        }
    }
}

#[derive(Debug, Fail)]
#[fail(display = "Error in functor result {:?}", errors)]
pub struct FunctorResultError {
    pub errors: std::vec::Vec<failure::Error>
}
