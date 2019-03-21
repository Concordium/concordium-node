use failure::{Fail, Backtrace};

#[derive(Debug,Fail)]
pub enum ConnectionError {
    #[fail(display = "Message processing error: {}", message)]
    MessageProcessError {
        message: String,
        backtrace: Backtrace
    },
    #[fail(display = "Peer error: {}", message)]
    PeerError {
        message: String,
    },
    #[fail(display = "Log error: {}", message)]
    LogError {
        message: String,
    },
    #[fail(display = "Prometheus error: {}", message)]
    PrometheusError {
        message: String,
    }
}
