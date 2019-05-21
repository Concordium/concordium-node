#![recursion_limit = "1024"]

extern crate tempfile;

/// # Serialization packets
/// Benchmark of each serialization requires to enable it on features
#[cfg(feature = "s11n_serde")]
extern crate serde;

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
