use failure::Error;

/// Helper macro to create callbacks from raw function pointers or closures.
#[macro_export]
macro_rules! make_atomic_callback {
    ($callback:expr) => {
        ( format!( "{}:{}",file!(), line!()), Arc::new( Mutex::new( Box::new( $callback))))
    }
}

#[macro_export]
macro_rules! make_callback {
    ($callback:expr) => {
        Rc::new( RefCell:new( $callback ))
    }
}

pub mod fails;
pub mod afunctor;
pub mod functor;

pub use self::functor::{ FunctorCW, Functor };
pub use self::afunctor::{ AFunctorCW, AFunctor };
pub use self::fails::{ FunctorError };

pub type FunctorResult = Result<(), Error>;
pub type FunctorCallback<T> = (Fn(&T) -> FunctorResult);
