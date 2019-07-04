use failure::{Backtrace, Fail};

#[derive(Debug, Fail)]
#[fail(display = "Some field were missing when attempting to build a connection.")]
pub struct MissingFieldsConnectionBuilder;

#[derive(Debug, Fail)]
#[fail(display = "Message processing error: {}", message)]
pub struct MessageProcessError {
    pub message:   &'static str,
    pub backtrace: Backtrace,
}

#[derive(Debug, Fail)]
#[fail(display = "Peer error: {}", message)]
pub struct PeerError {
    pub message: &'static str,
}

#[derive(Debug, Fail)]
#[fail(display = "Log error: {}", message)]
pub struct LogError {
    pub message: &'static str,
}

#[derive(Debug, Fail)]
#[fail(display = "Stats Exporter error: {}", message)]
pub struct StatsExporterError {
    pub message: &'static str,
}

#[derive(Debug, Fail)]
#[fail(display = "Unwanted message: {}", message)]
pub struct UnwantedMessageError {
    pub message: String,
}

/// P2P protocol defines a maximum size per message.
/// This error covers when message exceeds that constraint.
#[derive(Debug, Fail)]
#[fail(
    display = "Message expected size ({} bytes) exceeds the maximum protocol size: {} bytes",
    message_size, protocol_size
)]
pub struct MessageTooBigError {
    pub message_size:  u32,
    pub protocol_size: u32,
}

#[derive(Debug, Fail)]
#[fail(
    display = "Stream operation needs to block to complete, but the blocking operation was \
               requested to not occur"
)]
pub struct StreamWouldBlock;

#[derive(Debug, Fail)]
#[fail(display = "The connection was reset by the remote server")]
pub struct StreamConnectionReset;

#[derive(Debug, Fail)]
#[fail(display = "Connection was terminated by remote peer")]
pub struct PeerTerminatedConnection;

#[macro_export]
macro_rules! map_io_error_to_fail {
    ($e:expr) => {
        $e.map_err(|io_err| {
            use crate::connection::fails::{
                PeerTerminatedConnection, StreamConnectionReset, StreamWouldBlock,
            };
            use failure::Error;
            use std::io::ErrorKind;

            match io_err.kind() {
                ErrorKind::WouldBlock => Error::from(StreamWouldBlock),
                ErrorKind::ConnectionReset => Error::from(StreamConnectionReset),
                ErrorKind::NotFound => Error::from(PeerTerminatedConnection),
                ErrorKind::NotConnected => Error::from(PeerTerminatedConnection),
                _ => Error::from_boxed_compat(Box::new(io_err)),
            }
        })
    };
}
