use std::cell::{ RefCell };
use std::rc::{ Rc };
use failure::{ Error };

use super::{ FunctorResult, FunctorCallback, FunctorError };

pub type FunctorCW<T> = Rc<RefCell<FunctorCallback<T>>>;

pub struct Functor<T> {
    pub name: &'static str,
    pub callbacks: Vec<FunctorCW<T>>
}

impl<T> Functor<T> {
    pub fn new( name: &'static str) -> Self {
        Functor {
            name: name,
            callbacks: Vec::new(),
        }
    }

    /// It adds new callback into this functor.
    ///
    /// Callbacks are executed in the same order they were introduced.
    pub fn add_callback(&mut self, callback: FunctorCW<T> ) -> &mut Self {
        self.callbacks.push( callback );
        self
    }
}

/// Helper macro to run all callbacks using `message` expression as argument.
macro_rules! run_callbacks {
    ($handlers:expr, $message:expr, $errorMsg: expr) => {
            (|x: Vec<Error>| if x.is_empty() {
                Ok(())
            } else {
                Err(FunctorError::new(x))?
            })($handlers.iter()
            .map( |handler| handler.borrow_mut())
            .map( |handler_mut| { (handler_mut)($message) })
            .filter_map(|handler_result| handler_result.err()).collect())
    }
}

impl<T> FnOnce<(&T,)> for Functor<T> {
    type Output = FunctorResult;
    extern "rust-call" fn call_once(self, args: (&T,)) -> FunctorResult
    {
        let msg: &T = args.0;
        run_callbacks!( &self.callbacks, msg, self.name)
    }
}

impl<T> FnMut<(&T,)> for Functor<T> {
    extern "rust-call" fn call_mut(&mut self, args: (&T,)) -> FunctorResult
    {
        let msg: &T = args.0;
        run_callbacks!( &self.callbacks, msg, self.name)
    }
}

impl<T> Fn<(&T,)> for Functor<T> {
    extern "rust-call" fn call(&self, args: (&T,)) -> FunctorResult
    {
        let msg: &T = args.0;
        run_callbacks!( &self.callbacks, msg, self.name)
    }
}
