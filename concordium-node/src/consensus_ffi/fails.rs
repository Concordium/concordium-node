use std::backtrace::Backtrace;
use thiserror::Error;

#[derive(Debug, Error)]
#[error("Resource was poisoned")]
pub struct PoisonError {
    backtrace: Backtrace, // The backtrace is automatically detected.
}

#[macro_export]
macro_rules! safe_lock {
    ($e:expr) => {
        $e.lock().map_err($crate::fails::PoisonError::from).unwrap_or_else(|e| {
            panic!("{}", e);
        })
    };
}

#[macro_export]
macro_rules! safe_write {
    ($e:expr) => {
        $e.write().map_err($crate::fails::PoisonError::from).unwrap_or_else(|e| {
            panic!("{}", e);
        })
    };
}

#[macro_export]
macro_rules! safe_read {
    ($e:expr) => {
        $e.read().map_err($crate::fails::PoisonError::from).unwrap_or_else(|e| {
            panic!("{}", e);
        })
    };
}
