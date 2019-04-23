use failure::{Backtrace, Fail};

#[derive(Debug, Fail)]
#[fail(display = "Some field were missing when attempting to build a connection.")]
pub struct MissingFieldsConnectionBuilder;

#[derive(Debug, Fail)]
#[fail(display = "Connection didn't even offer 4 bytes to read size")]
pub struct NotEnoughBytesToRead;

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
