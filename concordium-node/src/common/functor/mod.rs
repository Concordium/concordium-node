use failure::Fallible;
use super::fails;

/// Helper macro to create callbacks from raw function pointers or closures.
#[macro_export]
macro_rules! make_atomic_callback {
    ($callback:expr) => {
        // Arc::new( Mutex::new( Box::new( $callback)))
        ( format!( "{}:{}",file!(), line!()), Arc::new( Mutex::new( Box::new( $callback))))
    }
}

#[macro_export]
macro_rules! make_callback {
    ($callback:expr) => {
        Rc::new( RefCell:new( $callback ))
    }
}

pub type FunctorResult = Fallible<()>;
pub type FullFunctorResult = Result<(), fails::FunctorResultError>;
pub type FunctorCallback<T> = (Fn(&T) -> FunctorResult);
pub type FullFunctorCallback<T> = (Fn(&T) -> FullFunctorResult);


pub mod afunctor;
pub mod functor;

pub use self::functor::{ FunctorCW, Functor };
pub use self::afunctor::{ AFunctorCW, AFunctor };
