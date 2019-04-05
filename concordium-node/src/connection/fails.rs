use failure::{Backtrace, Fail};

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
#[fail(display = "Prometheus error: {}", message)]
pub struct PrometheusError {
    pub message: &'static str,
}

#[derive(Debug, Fail)]
#[fail(display = "Unwanted message: {}", message)]
pub struct UnwantedMessageError {
    pub message: String,
}
