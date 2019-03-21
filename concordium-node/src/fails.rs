use failure::{Fail, Backtrace};

#[derive(Debug, Fail)]
#[fail(display = "Resource was poisoned")]
pub struct PoisonError {
    backtrace: Backtrace
}

impl<T> From<std::sync::PoisonError<T>> for PoisonError {
    fn from(_: std::sync::PoisonError<T>) -> Self {
        PoisonError {
            backtrace: Backtrace::new()
        }
    }
}

pub type FallibleByPoison<T> = Result<T, PoisonError>;

macro_rules! into_err {
    ($e:expr) => {
        $e.map_err(|x| Error::from_boxed_compat(Box::new(x)))
    }
}
