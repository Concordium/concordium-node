use failure::{Backtrace, Error, Fail};

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
/// use concordium_common::safe_lock; // just import the macro
/// use std::sync::{ Arc, Mutex };
///
/// fn foo() -> Fallible<()> {
///     let data = Arc::new(Mutex::new(0));
///     // `let locked_data = data.lock().map_err(concordium_common::fails::PoisonError::from)?;` gets replaced by:
///     let locked_data = safe_lock!(data)?;
///     // ...
///     Ok(())
/// }
/// ```
#[macro_export]
macro_rules! safe_lock {
    ($e:expr) => {
        $e.lock().map_err($crate::fails::PoisonError::from)
    };
}

/// Wrap a `read()` call to map a `PoisonError` into a `failure::Fail`
///
/// See [safe_lock] for further documentation.
#[macro_export]
macro_rules! safe_read {
    ($e:expr) => {
        $e.read().map_err($crate::fails::PoisonError::from)
    };
}

/// Wrap a `write()` call to map a `PoisonError` into a `failure::Fail`
///
/// See [safe_lock] for further documentation.
#[macro_export]
macro_rules! safe_write {
    ($e:expr) => {
        $e.write().map_err($crate::fails::PoisonError::from)
    };
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
/// use concordium_common::into_err;
/// use failure::Fallible;
/// use std::io::{Error, ErrorKind};
///
/// fn foo() -> Fallible<()> {
///     let io_error = Error::new(ErrorKind::Other, "oh no!");
///     into_err!(Err(io_error))?;
///     Ok(())
/// }
/// ```
#[macro_export]
macro_rules! into_err {
    ($e:expr) => {
        $e.map_err(|x| failure::Error::from_boxed_compat(Box::new(x)))
    };
}

/// Acquire a resource for writing or panic
///
/// Equivalent to `safe_write` but panicking on error
#[macro_export]
macro_rules! write_or_die {
    ($v:expr) => {
        safe_write!($v).unwrap_or_else(|e| {
            panic!("{}", e);
        })
    };
}

/// Acquire a resource for reading of panic
///
/// Equivalent to `safe_read` but panicking on error
#[macro_export]
macro_rules! read_or_die {
    ($v:expr) => {
        safe_read!($v).unwrap_or_else(|e| {
            panic!("{}", e);
        })
    };
}

/// Lock a resource or panic
///
/// Equivalent to `safe_lock` but panicking on error
#[macro_export]
macro_rules! lock_or_die {
    ($v:expr) => {
        safe_lock!($v).unwrap_or_else(|e| {
            panic!("{}", e);
        })
    };
}

/// Send struct into a channel and panic on error
///
/// Useful for consuming the `Result` returned by a Send
/// to cover all the branches but an error would
/// mean that the program must crash.
///
/// `std::sync::mpsc::SendError` is only issued when the
/// corresponding receiver has been freed before the `send`
/// was done.
///
/// In the case of the `send_queue` of a `P2PNode`,
/// the `Receiver` would only be freed when the node is deallocated
/// and getting an error when sending to the `Sender` from inside
/// the node would mean that things have gone really wrong.
#[macro_export]
macro_rules! send_or_die {
    ($s:expr, $v:expr) => {
        $s.clone()
            .send($v)
            .map_err(|e| {
                panic!(
                    "Corresponding channel receiver has been deallocated too early. Error: {}",
                    e
                );
            })
            .ok()
    };
}

/// Spawns a new thread
///
/// If the OS refuses to create a new thread, this would result in a `panic`.
#[macro_export]
macro_rules! spawn_or_die {
    ($name:expr, $func:expr) => {
        std::thread::Builder::new()
            .name($name.to_owned())
            .spawn($func)
            .expect("OS refused to create a new thread")
    };
}

#[derive(Debug, Fail)]
#[fail(display = "Error running functor: {:?}", errors)]
/// Error returned from the execution of a Functor.
///
/// Contains a list of errors. It can be created through a vector of `Error`s,
/// from a single `Error` or pushed back more items.
pub struct FunctorError {
    pub errors: Vec<Error>,
}

impl FunctorError {
    /// Create a `FunctorError` from a single `Error`
    pub fn create(e: impl Into<Error>) -> FunctorError {
        FunctorError {
            errors: vec![e.into()],
        }
    }
}

impl From<Vec<Error>> for FunctorError {
    fn from(v: Vec<Error>) -> Self { FunctorError { errors: v } }
}
