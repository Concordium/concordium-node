use failure::{Fail, Backtrace};

#[derive(Debug,Fail)]
pub enum ConnectionError {
    #[fail(display = "Message processing error: {}", message)]
    MessageProcessError {
        message: &'static str,
        backtrace: Backtrace
    },
    #[fail(display = "Peer error: {}", message)]
    PeerError {
        message: &'static str,
    },
    #[fail(display = "Log error: {}", message)]
    LogError {
        message: &'static str,
    },
    #[fail(display = "Prometheus error: {}", message)]
    PrometheusError {
        message: &'static str,
    }
}
