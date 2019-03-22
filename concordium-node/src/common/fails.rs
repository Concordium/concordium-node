use failure::Fail;
use super::{ConnectionType, P2PNodeId};
use std::net::IpAddr;
use num_bigint::ParseBigIntError;

#[derive(Debug, Fail)]
#[fail(display = "Empty IP or Port on P2PPeer building")]
pub struct EmptyIpPortError;

#[derive(Debug, Fail)]
#[fail(display = "Missing fields on P2PPeer build: conn<{:?}>, id<{:?}>, ip<{:?}>, port<{:?}>", connection_type, id, ip, port)]
pub struct MissingFieldsError {
        connection_type: Option<ConnectionType>,
        id: Option<P2PNodeId>,
        ip: Option<IpAddr>,
        port: Option<u16>
}

impl MissingFieldsError {
    pub fn new(connection_type: Option<ConnectionType>,
               id: Option<P2PNodeId>,
               ip: Option<IpAddr>,
               port: Option<u16>) -> MissingFieldsError {
        MissingFieldsError {
            connection_type,
            id,
            ip,
            port
        }
    }
}

#[derive(Debug, Fail)]
#[fail(display = "Invalid length for specified IP type: type<{}>", ip_type)]
pub struct InvalidLengthForIP {
    ip_type: String
}

impl InvalidLengthForIP {
    pub fn new(ip_type: String) -> InvalidLengthForIP {
        InvalidLengthForIP {
            ip_type
        }
    }
}

#[derive(Debug, Fail)]
#[fail(display = "Invalid IP type specified: type<{}>", ip_type)]
pub struct InvalidIpType {
    ip_type: String
}

impl InvalidIpType {
    pub fn new(ip_type: String) -> InvalidIpType {
        InvalidIpType {
            ip_type
        }
    }
}

#[derive(Debug, Fail)]
#[fail(display = "Invalid length for serialized P2PPeer")]
pub struct InvalidLength;

#[derive(Debug, Fail)]
#[fail(display = "Error while parsing P2PNodeId")]
pub struct P2PNodeIdError;

impl From<ParseBigIntError> for P2PNodeIdError {
   fn from(_: ParseBigIntError) -> Self {
        P2PNodeIdError
    }
}
