#![recursion_limit = "1024"]

use byteorder::{NetworkEndian, ReadBytesExt};

use std::{fmt, ops::Deref};

/// # Serialization packets
/// Benchmark of each serialization requires to enable it on features
#[cfg(feature = "s11n_serde")]
#[macro_use]
extern crate serde_derive;

#[macro_use]
pub mod fails;

pub mod container_view;
pub mod functor;
pub mod ucursor;

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

impl Deref for HashBytes {
    type Target = [u8];

    fn deref(&self) -> &Self::Target { &self.0 }
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
