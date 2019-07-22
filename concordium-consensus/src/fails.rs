use failure::{Backtrace, Fail};

#[derive(Debug, Fail)]
#[fail(display = "Resource was poisoned")]
pub struct PoisonError {
    backtrace: Backtrace,
}

impl<T> From<std::sync::PoisonError<T>> for PoisonError {
    fn from(_: std::sync::PoisonError<T>) -> Self {
        PoisonError {
            backtrace: Backtrace::new(),
        }
    }
}

#[macro_export]
macro_rules! safe_lock {
    ($e:expr) => {
        $e.lock()
            .map_err($crate::fails::PoisonError::from)
            .unwrap_or_else(|e| {
                panic!("{}", e);
            })
    };
}

#[macro_export]
macro_rules! safe_write {
    ($e:expr) => {
        $e.write()
            .map_err($crate::fails::PoisonError::from)
            .unwrap_or_else(|e| {
                panic!("{}", e);
            })
    };
}

#[macro_export]
macro_rules! safe_read {
    ($e:expr) => {
        $e.read()
            .map_err($crate::fails::PoisonError::from)
            .unwrap_or_else(|e| {
                panic!("{}", e);
            })
    };
}