use failure::{Error, Fail, Backtrace};

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

impl PoisonError {
    pub fn to_err(self) -> Error {
        Error::from(self)
    }
}

#[derive(Debug, Fail)]
#[fail(display = "IO error")]
pub struct IOError {
    #[cause] cause: std::io::Error,
    backtrace: Backtrace
}

impl IOError {
    pub fn new(e: std::io::Error) -> IOError {
        IOError {
            cause: e,
            backtrace: Backtrace::new()
        }
    }
}

impl IOError {
    pub fn to_err(self) -> Error {
        Error::from(self)
    }
}
