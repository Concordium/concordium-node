use failure::{Error, Fail};

/// Helper macro to create callbacks from raw function pointers or closures.
#[macro_export]
macro_rules! make_atomic_callback {
    ($callback:expr) => {
        Arc::new(RwLock::new(Box::new($callback)))
    }
}

pub mod afunctor;
pub mod functor;

pub use self::functor::{ FunctorCW, Functor };
pub use self::afunctor::{ AFunctorCW, AFunctor };

pub type FunctorResult = Result<(), Error>;
pub type FunctorCallback<T> = (Fn(&T) -> FunctorResult);

#[derive(Debug, Fail)]
#[fail(display = "Error running functor: {:?}", errors)]
pub struct FunctorError {
    pub errors: Vec<Error>
}

impl FunctorError {
    pub fn new(e: Vec<Error>) ->  FunctorError {
        FunctorError {
            errors: e
        }
    }
}
