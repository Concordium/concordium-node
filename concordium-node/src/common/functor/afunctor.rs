use std::sync::{ Arc, RwLock };
use failure::{ Error, bail };
use crate::fails as global_fails;

use super::{ FunctorResult, FunctorCallback, FunctorError };

pub type AFunctorCW<T> = (String, Arc<RwLock<Box<FunctorCallback<T>>>>);

/// It stores any number of functions or closures and it is able to execute them
/// because it implements `Fn`, `FnMut` and `FnOnce`.
///
/// # Examples
/// ```
/// extern crate p2p_client;
///
/// use p2p_client::connection::*;
/// use p2p_client::common::functor::{ AFunctor };
/// use std::rc::{ Rc };
/// use std::sync::{ Arc, RwLock };
/// use std::cell::{ RefCell };
///
/// let acc = Rc::new( RefCell::new(58));
/// let acc_1 = acc.clone();
/// let acc_2 = acc.clone();
///
/// let mut ph = AFunctor::new( "Closures");
///
/// ph.add_callback((String::new(), Arc::new(RwLock::new(Box::new(move |x: &i32| {
///         *acc_1.borrow_mut() += x;
///         Ok(()) })))))
///     .add_callback((String::new(), Arc::new(RwLock::new(Box::new(move |x: &i32| {
///         *acc_2.borrow_mut() *= x;
///         Ok(()) })))));
///
/// let value = 42 as i32;
/// (&ph)(&value).unwrap();     // acc = (58 + 42) * 42
/// assert_eq!( *acc.borrow(), 4200);
///
/// ```
#[derive(Clone)]
pub struct AFunctor<T> {
    name: &'static str,
    callbacks: Vec<AFunctorCW<T>>,
}

unsafe impl<T> Send for AFunctor<T> {}
unsafe impl<T> Sync for AFunctor<T> {}

impl<T> AFunctor<T> {

    pub fn new( name: &'static str) -> Self {
        AFunctor {
            name: name,
            callbacks: Vec::new(),
        }
    }

    /// It adds new callback into this functor.
    ///
    /// Callbacks are executed in the same order they were introduced.
    pub fn add_callback(&mut self, callback: AFunctorCW<T>) -> &mut Self {
        // debug!( "# Functor '{}': Callback added from {}", self.name, callback.0);
        self.callbacks.push(callback);
        self
    }

    pub fn callbacks(&self) -> &Vec<AFunctorCW<T>> {
        &self.callbacks
    }

    /// It executes each callback using `message` as its argument.
    /// All errors from callbacks execution are chained. Otherwise, it will return `Ok(())`.
    fn run_atomic_callbacks(&self, message: &T) -> FunctorResult
    {
        let mut status : Vec<Error> = vec![];

        for i in 0..self.callbacks.len() {
            let (_fn_id, cb) = self.callbacks[i].clone();

            if let Err(e) = match safe_read!(cb) {
                Ok(locked_cb) => {
                    (locked_cb)(message)
                },
                Err(p) => { Err(Error::from(global_fails::PoisonError::from(p))) }
            } {
                status.push(e);
            };
        };

        if !status.is_empty() {
            bail!(FunctorError::new(status))
        } else {
            Ok(())
        }
    }
}

impl<T> FnOnce<(&T,)> for AFunctor<T> {
    type Output = FunctorResult;

    extern "rust-call" fn call_once(self, args: (&T,)) -> FunctorResult
    {
        let msg: &T = args.0;
        self.run_atomic_callbacks( msg)
    }
}

impl<T> FnMut<(&T,)> for AFunctor<T> {
    extern "rust-call" fn call_mut(&mut self, args: (&T,)) -> FunctorResult
    {
        let msg: &T = args.0;
        // run_atomic_callbacks!( &self.callbacks, msg, self.name)
        self.run_atomic_callbacks( msg)
    }
}

impl<T> Fn<(&T,)> for AFunctor<T> {
    extern "rust-call" fn call(&self, args: (&T,)) -> FunctorResult
    {
        let msg: &T = args.0;
        self.run_atomic_callbacks( msg)
    }
}

#[cfg(test)]
mod afunctor_unit_test {

    use crate::common::functor::{ AFunctor, FunctorResult };
    use std::rc::{ Rc };
    use std::sync::{ Arc, RwLock };
    use std::cell::{ RefCell };

    fn raw_func_1( _v: &i32) -> FunctorResult { Ok(()) }
    fn raw_func_2( _v: &i32) -> FunctorResult { Ok(()) }

    /// It tests if raw functions can be added as callback.
    #[test]
    pub fn test_parse_handler_raw_functions() {
        let mut ph = AFunctor::new( "Raw functions");
        ph.add_callback( make_atomic_callback!( raw_func_1 ))
            .add_callback( make_atomic_callback!( raw_func_2 ))
            .add_callback( make_atomic_callback!( raw_func_1 ));

        let value = 42 as i32;
        (&ph)(&value).unwrap();
    }

    /// It tests if closures can be added as callback.
    #[test]
    pub fn test_parse_handler_closure() {
        let mut ph = AFunctor::new( "Closures");

        ph.add_callback( make_atomic_callback!( |_x: &i32| { Ok(()) }))
            .add_callback( make_atomic_callback!( |_x: &i32| { Ok(()) }));

        let value = 42 as i32;
        (&ph)(&value).unwrap();
    }

    /// It tests if we can mix closures and functions.
    #[test]
    pub fn test_parse_handler_mix() {
        let mut ph = AFunctor::new( "Raw function and  Closure");

        ph.add_callback( make_atomic_callback!( raw_func_1 ))
            .add_callback( make_atomic_callback!( raw_func_2 ))
            .add_callback( make_atomic_callback!( |_x: &i32| { Ok(()) }))
            .add_callback( make_atomic_callback!( |_x: &i32| { Ok(()) }));

        let value = 42 as i32;
        (&ph)(&value).unwrap();
    }

    /// Test for complex closures, I mean, closures that copy/move some variables from scope.
    #[test]
    pub fn test_parse_hadler_complex_closure() {
        let shd_counter = Rc::new( RefCell::new(0));
        let shd_counter_1 = shd_counter.clone();
        let shd_counter_2 = shd_counter.clone();

        let mut ph = AFunctor::new( "Complex Closure");

        ph.add_callback( make_atomic_callback!( move |_x: &i32| {
                *shd_counter_1.borrow_mut() += 1;
                Ok(()) }))
            .add_callback( make_atomic_callback!( move |_: &i32| {
                *shd_counter_2.borrow_mut() += 1;
                Ok(()) }));


        let value = 42 as i32;
        (ph)(&value).unwrap();

        assert_eq!( *shd_counter.borrow(), 2);
    }
}
