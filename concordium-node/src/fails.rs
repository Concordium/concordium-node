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

/// Wrap a `lock()` call to map a `PoisonError` into a `failure::Fail`
///
/// This macro is intended to be used with `std::sync::PoisonErrors`.
/// Standard poison errors are usually not convertible into
/// `failure::Error` because they lack implementations for `Send`
/// and `Sync`. That's the reason why they can't be directly converted
/// and have to be converted into an intermediate type (in this
/// case `crate::fails::PoisonError`).
///
/// This allows to use the `?` operator in a much more concise way in functions
/// that expect to return `failure::Fallible` and
/// reduces the boilerplate as `lock()` is a commonly used function.
///
/// # Examples
/// ```
/// use failure::{ Fallible, Error };
/// use p2p_client::safe_lock; // just import the macro
/// use std::sync::{ Arc, Mutex };
///
/// fn foo() -> failure::Fallible<T> {
///     let data = Arc::new(Mutex::new(0));
///     // `let locked_data = data.lock().map_err(p2p_client::fails::PoisonError::from)?;` gets replaced by:
///     let locked_data = safe_lock!(data)?;
///     // ...
/// }
/// ```

#[macro_export]
macro_rules! safe_lock {
    ($e:expr) => {
        $e.lock().map_err($crate::fails::PoisonError::from)
    }
}

/// Wrap a `read()` call to map a `PoisonError` into a `failure::Fail`
///
/// See [safe_lock] for further documentation.
#[macro_export]
macro_rules! safe_read {
    ($e:expr) => {
        $e.read().map_err($crate::fails::PoisonError::from)
    }
}

/// Wrap a `write()` call to map a `PoisonError` into a `failure::Fail`
///
/// See [safe_lock] for further documentation.
#[macro_export]
macro_rules! safe_write {
    ($e:expr) => {
        $e.write().map_err($crate::fails::PoisonError::from)
    }
}

/// Wrap a `std::error::Error` into a `failure::Error`
///
/// This macro is intended to be used with an error that implements
/// `Send + Sync` so it can be directly boxed and converted into
/// `failure::Error`.
///
/// This allows to use the `?` operator inside functions that expect to return
/// `failure::Fallible`.
///
/// # Examples
/// ```
/// use std::io::{Error, ErrorKind};
/// let io_error = Error::new(ErrorKind::Other, "oh no!");
///
/// let failure_error = into_err!(io_error)?;
/// ```
#[macro_export]
macro_rules! into_err {
    ($e:expr) => {
        $e.map_err(|x| failure::Error::from_boxed_compat(Box::new(x)))
    }
}
