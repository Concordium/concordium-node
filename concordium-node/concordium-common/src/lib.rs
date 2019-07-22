#![recursion_limit = "1024"]

use byteorder::{NetworkEndian, ReadBytesExt};
use failure::{format_err, Fallible};

use std::{convert::TryFrom, fmt, ops::Deref};

/// # Serialization packets
/// Benchmark of each serialization requires to enable it on features
#[cfg(feature = "s11n_serde")]
#[macro_use]
extern crate serde_derive;

#[cfg(feature = "instrumentation")]
#[macro_use]
extern crate log;

#[macro_use]
extern crate cfg_if;

cfg_if! {
    if #[cfg(feature = "instrumentation")] {
        #[macro_use]
        extern crate prometheus;
        #[macro_use]
        extern crate gotham_derive;
        extern crate hyper;
        extern crate mime;
    }
}

#[macro_use]
pub mod fails;

pub mod cache;
pub mod container_view;
pub mod filters;
pub mod functor;
pub mod indexed_vec;
pub mod stats_export_service;
pub mod ucursor;

pub const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const APPNAME: &str = env!("CARGO_PKG_NAME");

pub use self::{container_view::ContainerView, ucursor::UCursor};

/// Represents a Message to be sent through a channel or a Stop signal
///
/// This type is intended for using when a loop is consuming the receiver
/// output and we want to gracefully stop such loop.
pub enum RelayOrStopEnvelope<T> {
    Relay(T),
    Stop,
}

/// Represents a `Sender<T>` that is promoted to use `RelayOrStopEnvelope`s
pub type RelayOrStopSender<T> = std::sync::mpsc::Sender<RelayOrStopEnvelope<T>>;

/// Represents a `Sender<T>` that is promoted to use `RelayOrStopEnvelope`s
pub type RelayOrStopSyncSender<T> = std::sync::mpsc::SyncSender<RelayOrStopEnvelope<T>>;

/// Represents a `Receiver<T>` that is promoted to use `RelayOrStopEnvelope`s
pub type RelayOrStopReceiver<T> = std::sync::mpsc::Receiver<RelayOrStopEnvelope<T>>;

/// Helper trait to ease readability through the code when dealing with
/// `RelayOrStop` channels
pub trait RelayOrStopSenderHelper<T> {
    /// Sends a `RelayOrStopEnvelope::Stop` message through the channel
    fn send_stop(&self) -> Result<(), std::sync::mpsc::SendError<RelayOrStopEnvelope<T>>>;
    /// Sends the provided `msg` wrapped inside a `RelayOrStopEnvelope::Relay`
    fn send_msg(&self, msg: T) -> Result<(), std::sync::mpsc::SendError<RelayOrStopEnvelope<T>>>;
}

impl<T> RelayOrStopSenderHelper<T> for RelayOrStopSender<T> {
    #[inline]
    fn send_stop(&self) -> Result<(), std::sync::mpsc::SendError<RelayOrStopEnvelope<T>>> {
        self.send(RelayOrStopEnvelope::Stop)
    }

    #[inline]
    fn send_msg(&self, msg: T) -> Result<(), std::sync::mpsc::SendError<RelayOrStopEnvelope<T>>> {
        self.send(RelayOrStopEnvelope::Relay(msg))
    }
}

impl<T> RelayOrStopSenderHelper<T> for RelayOrStopSyncSender<T> {
    #[inline]
    fn send_stop(&self) -> Result<(), std::sync::mpsc::SendError<RelayOrStopEnvelope<T>>> {
        self.send(RelayOrStopEnvelope::Stop)
    }

    #[inline]
    fn send_msg(&self, msg: T) -> Result<(), std::sync::mpsc::SendError<RelayOrStopEnvelope<T>>> {
        self.send(RelayOrStopEnvelope::Relay(msg))
    }
}

pub const SHA256: u8 = 32;

#[derive(Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub struct HashBytes([u8; SHA256 as usize]);

impl HashBytes {
    pub fn new(bytes: &[u8]) -> Self {
        let mut buf = [0u8; SHA256 as usize];
        buf.copy_from_slice(bytes);

        HashBytes(buf)
    }
}

impl From<[u8; 32]> for HashBytes {
    fn from(array: [u8; 32]) -> Self { HashBytes(array) }
}

impl Deref for HashBytes {
    type Target = [u8];

    fn deref(&self) -> &Self::Target { &self.0 }
}

impl AsRef<[u8]> for HashBytes {
    fn as_ref(&self) -> &[u8] { &self }
}

// a short, 8-character beginning of the SHA
impl fmt::Debug for HashBytes {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{:08x}",
            (&self.0[..]).read_u32::<NetworkEndian>().unwrap(),
        )
    }
}

// the full SHA256 in hex
impl fmt::Display for HashBytes {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{:0len$x}",
            (&self.0[..]).read_u128::<NetworkEndian>().unwrap(),
            len = SHA256 as usize,
        )
    }
}

pub trait SerializeToBytes<'a, 'b>
where
    Self: Sized, {
    type Source; // either a byte slice or a mutable cursor (when total size is unknown)

    fn deserialize(source: Self::Source) -> Fallible<Self>;
    fn serialize(&self) -> Box<[u8]>;
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum PacketType {
    Block = 0,
    Transaction,
    FinalizationRecord,
    FinalizationMessage,
    CatchupBlockByHash,
    CatchupFinalizationRecordByHash,
    CatchupFinalizationRecordByIndex,
    CatchupFinalizationMessagesByPoint,
    GlobalStateMetadata,
    GlobalStateMetadataRequest,
}

static PACKET_TYPE_FROM_INT: &[PacketType] = &[
    PacketType::Block,
    PacketType::Transaction,
    PacketType::FinalizationRecord,
    PacketType::FinalizationMessage,
    PacketType::CatchupBlockByHash,
    PacketType::CatchupFinalizationRecordByHash,
    PacketType::CatchupFinalizationRecordByIndex,
    PacketType::CatchupFinalizationMessagesByPoint,
    PacketType::GlobalStateMetadata,
    PacketType::GlobalStateMetadataRequest,
];

impl TryFrom<u16> for PacketType {
    type Error = failure::Error;

    #[inline]
    fn try_from(value: u16) -> Fallible<PacketType> {
        PACKET_TYPE_FROM_INT
            .get(value as usize)
            .copied()
            .ok_or_else(|| format_err!("Unsupported packet type ({})", value))
    }
}

impl fmt::Display for PacketType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match self {
            PacketType::Block => "block",
            PacketType::Transaction => "transaction",
            PacketType::FinalizationRecord => "finalization record",
            PacketType::FinalizationMessage => "finalization message",
            PacketType::CatchupBlockByHash => "catch-up block by hash request",
            PacketType::CatchupFinalizationRecordByHash => {
                "catch-up finalization record by hash request"
            }
            PacketType::CatchupFinalizationRecordByIndex => {
                "catch-up finalization record by index request"
            }
            PacketType::CatchupFinalizationMessagesByPoint => {
                "catch-up finalization messages by point request"
            }
            PacketType::GlobalStateMetadata => "global state metadata",
            PacketType::GlobalStateMetadataRequest => "request for global state metadata",
        };

        write!(f, "{}", name)
    }
}
