use crate::fails::FunctorError;
use failure::Error;
use std::sync::Arc;

/// Helper macro to create callbacks from raw function pointers or closures.
#[macro_export]
macro_rules! make_atomic_callback {
    ($callback:expr) => {
        Arc::new($callback)
    };
}

/// Result of the execution of a Functor
pub type FunctorResult<T> = Result<T, FunctorError>;
/// Result of the execution of a single Function
pub type FuncResult<T> = Result<T, Error>;
type AFuncCW<T, R> = Arc<(Fn(&T) -> FuncResult<R>)>;

pub type UnitFunction<T> = AFuncCW<T, ()>;
pub type BoolFunction<T> = AFuncCW<T, bool>;

/// Functor to be used when side-effects are intended and the actual returning
/// value is disposable
///
/// The returning type inside the `Ok` variant is `()` as it is only important
/// whether it is `Ok(_)` or `Err(_)`. In the case of an error, the actual chain
/// of functor errors is returned.
///
/// An empty functor returns always `Ok(())`.
///
/// # Examples
/// ```
/// # extern crate concordium_common;
/// # use concordium_common::functor::UnitFunctor;
/// # use std::{
/// #    cell::RefCell,
/// #    rc::Rc,
/// #    sync::{Arc, RwLock},
/// # };
/// #
/// let acc = Rc::new(RefCell::new(58));
/// let acc_1 = Rc::clone(&acc);
/// let acc_2 = Rc::clone(&acc);
///
/// let mut ph = UnitFunctor::new();
/// ph.add_callback(Arc::new(move |x: &i32| {
///     *acc_1.borrow_mut() += x;
///     Ok(())
/// }))
///     .add_callback(Arc::new(move |x: &i32| {
///         *acc_2.borrow_mut() *= x;
///         Ok(())
///     }));
///
/// let value = 42 as i32;
/// ph.run_callbacks(&value).unwrap(); // acc = (58 + 42) * 42
/// assert_eq!(*acc.borrow(), 4200);
#[derive(Default)]
pub struct UnitFunctor<T> {
    /// Queue of functions to be executed
    callbacks: Vec<UnitFunction<T>>,
}

impl<T> UnitFunctor<T> {
    pub fn new() -> Self {
        Self {
            callbacks: Vec::new(),
        }
    }

    pub fn add_callback(&mut self, callback: UnitFunction<T>) -> &mut Self {
        self.callbacks.push(callback);
        self
    }

    pub fn callbacks(&self) -> &[UnitFunction<T>] { &self.callbacks }

    pub fn run_callbacks(&self, message: &T) -> FunctorResult<()> {
        self.callbacks.iter().fold(Ok(()), |acum, cb| {
            let res = (cb)(message).map_err(Error::from);

            match acum {
                Ok(_) => match res {
                    Ok(_) => Ok(()),
                    Err(e) => Err(FunctorError::create(e)),
                },
                Err(mut e) => match res {
                    Ok(_) => Err(e),
                    Err(ee) => {
                        e.errors.push(ee);
                        Err(e)
                    }
                },
            }
        })
    }
}

#[cfg(test)]
mod unit_functor_unit_test {
    use crate::functor::{FuncResult, UnitFunctor};
    use std::{cell::RefCell, rc::Rc, sync::Arc};

    fn raw_func_1(_v: &i32) -> FuncResult<()> { Ok(()) }
    fn raw_func_2(_v: &i32) -> FuncResult<()> { Ok(()) }

    /// It tests if raw functions can be added as callback.
    #[test]
    pub fn test_parse_handler_raw_functions() {
        let mut ph = UnitFunctor::new();
        ph.add_callback(make_atomic_callback!(raw_func_1))
            .add_callback(make_atomic_callback!(raw_func_2))
            .add_callback(make_atomic_callback!(raw_func_1));

        let value = 42 as i32;
        ph.run_callbacks(&value).unwrap();
    }

    /// It tests if closures can be added as callback.
    #[test]
    pub fn test_parse_handler_closure() {
        let mut ph = UnitFunctor::new();

        ph.add_callback(make_atomic_callback!(|_x: &i32| { Ok(()) }))
            .add_callback(make_atomic_callback!(|_x: &i32| { Ok(()) }));

        let value = 42 as i32;
        ph.run_callbacks(&value).unwrap();
    }

    /// It tests if we can mix closures and functions.
    #[test]
    pub fn test_parse_handler_mix() {
        let mut ph = UnitFunctor::new();

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

        let mut ph = UnitFunctor::new();

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
