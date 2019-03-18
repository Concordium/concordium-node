use failure::{Fail, Backtrace};
use grpcio;

#[derive(Debug, Fail)]
#[fail(display = "Resource was poisoned")]
pub struct PoisonError {
    backtrace: Backtrace
}

impl PoisonError {
    pub fn new() -> PoisonError {
        PoisonError {
            backtrace: Backtrace::new()
        }
    }
}

impl<T> From<std::sync::PoisonError<T>> for PoisonError {
    fn from(_: std::sync::PoisonError<T>) -> Self {
        PoisonError::new()
    }
}


#[derive(Debug, Fail)]
#[fail(display = "RPC building method failed")]
pub struct RpcError {
    #[cause] grpcio_error: grpcio::Error,
    backtrace: Backtrace
}

impl From<grpcio::Error> for RpcError {
    fn from(e: grpcio::Error) -> Self {
        RpcError {
            grpcio_error: e,
            backtrace: Backtrace::new()
        }
    }
}
