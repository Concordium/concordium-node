use thiserror::Error;

/// Protocol version relevant for the Rust scheduler.
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub enum ProtocolVersion {
    P9,
    P10,
    P11,
}

/// Protocol version was unknown to Rust scheduler.
#[derive(Debug, Error)]
#[error("Protocol version unknown to Rust scheduler: {0}")]
pub struct UnknownProtocolVersion(u64);

impl TryFrom<u64> for ProtocolVersion {
    type Error = UnknownProtocolVersion;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        match value {
            9 => Ok(ProtocolVersion::P9),
            10 => Ok(ProtocolVersion::P10),
            11 => Ok(ProtocolVersion::P11),
            _ => Err(UnknownProtocolVersion(value)),
        }
    }
}

impl From<ProtocolVersion> for u64 {
    fn from(pv: ProtocolVersion) -> Self {
        match pv {
            ProtocolVersion::P9 => 9,
            ProtocolVersion::P10 => 10,
            ProtocolVersion::P11 => 11,
        }
    }
}
