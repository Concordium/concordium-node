use crate::fails as global_fails;
use failure::{bail, Error};
use std::sync::{Arc, RwLock};

use super::{FunctorCallback, FunctorError, FunctorResult};

pub type FunctorCW<T> = Box<FunctorCallback<T>>;

pub type AFunctorCW<T> = Arc<RwLock<FunctorCW<T>>>;

/// It stores any number of functions or closures and it is able to execute them
/// because it implements `Fn`, `FnMut` and `FnOnce`.
///
/// # Examples
/// ```
/// extern crate p2p_client;
///
/// use p2p_client::{common::functor::AFunctor, connection::*};
/// use std::{
///     cell::RefCell,
///     rc::Rc,
///     sync::{Arc, RwLock},
/// };
///
/// let acc = Rc::new(RefCell::new(58));
/// let acc_1 = Rc::clone(&acc);
/// let acc_2 = Rc::clone(&acc);
///
/// let mut ph = AFunctor::new("Closures");
///
/// ph.add_callback(Arc::new(RwLock::new(Box::new(move |x: &i32| {
///     *acc_1.borrow_mut() += x;
///     Ok(())
/// }))))
/// .add_callback(Arc::new(RwLock::new(Box::new(move |x: &i32| {
///     *acc_2.borrow_mut() *= x;
///     Ok(())
/// }))));
///
/// let value = 42 as i32;
/// ph.run_callbacks(&value).unwrap(); // acc = (58 + 42) * 42
/// assert_eq!(*acc.borrow(), 4200);
/// ```
#[derive(Clone)]
pub struct AFunctor<T> {
    name:      &'static str,
    callbacks: Vec<AFunctorCW<T>>,
}

impl<T> AFunctor<T> {
    pub fn new(name: &'static str) -> Self {
        AFunctor {
            name,
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

    pub fn callbacks(&self) -> &[AFunctorCW<T>] { &self.callbacks }

    /// It executes each callback using `message` as its argument.
    /// All errors from callbacks execution are chained. Otherwise, it will
    /// return `Ok(())`.
    pub fn run_callbacks(&self, message: &T) -> FunctorResult {
        let mut status: Vec<Error> = vec![];

        for i in 0..self.callbacks.len() {
            let cb = self.callbacks[i].to_owned();

            if let Err(e) = match safe_read!(cb) {
                Ok(locked_cb) => (*locked_cb)(message),
                Err(p) => Err(Error::from(global_fails::PoisonError::from(p))),
            } {
                status.push(e);
            };
        }

        if !status.is_empty() {
            bail!(FunctorError::new(status))
        } else {
            Ok(())
        }
    }
}

#[cfg(test)]
mod afunctor_unit_test {
    use crate::common::functor::{AFunctor, FunctorResult};
    use std::{
        cell::RefCell,
        rc::Rc,
        sync::{Arc, RwLock},
    };

    fn raw_func_1(_v: &i32) -> FunctorResult { Ok(()) }
    fn raw_func_2(_v: &i32) -> FunctorResult { Ok(()) }

    /// It tests if raw functions can be added as callback.
    #[test]
    pub fn test_parse_handler_raw_functions() {
        let mut ph = AFunctor::new("Raw functions");
        ph.add_callback(make_atomic_callback!(raw_func_1))
            .add_callback(make_atomic_callback!(raw_func_2))
            .add_callback(make_atomic_callback!(raw_func_1));

        let value = 42 as i32;
        ph.run_callbacks(&value).unwrap();
    }

    /// It tests if closures can be added as callback.
    #[test]
    pub fn test_parse_handler_closure() {
        let mut ph = AFunctor::new("Closures");

        ph.add_callback(make_atomic_callback!(|_x: &i32| { Ok(()) }))
            .add_callback(make_atomic_callback!(|_x: &i32| { Ok(()) }));

        let value = 42 as i32;
        ph.run_callbacks(&value).unwrap();
    }

    /// It tests if we can mix closures and functions.
    #[test]
    pub fn test_parse_handler_mix() {
        let mut ph = AFunctor::new("Raw function and  Closure");

        ph.add_callback(make_atomic_callback!(raw_func_1))
            .add_callback(make_atomic_callback!(raw_func_2))
            .add_callback(make_atomic_callback!(|_x: &i32| { Ok(()) }))
            .add_callback(make_atomic_callback!(|_x: &i32| { Ok(()) }));

        let value = 42 as i32;
        ph.run_callbacks(&value).unwrap();
    }

    /// Test for complex closures, I mean, closures that copy/move some
    /// variables from scope.
    #[test]
    pub fn test_parse_hadler_complex_closure() {
        let shd_counter = Rc::new(RefCell::new(0));
        let shd_counter_1 = Rc::clone(&shd_counter);
        let shd_counter_2 = Rc::clone(&shd_counter);

        let mut ph = AFunctor::new("Complex Closure");

        ph.add_callback(make_atomic_callback!(move |_x: &i32| {
            *shd_counter_1.borrow_mut() += 1;
            Ok(())
        }))
        .add_callback(make_atomic_callback!(move |_: &i32| {
            *shd_counter_2.borrow_mut() += 1;
            Ok(())
        }));

        let value = 42 as i32;
        ph.run_callbacks(&value).unwrap();

        assert_eq!(*shd_counter.borrow(), 2);
    }
}
