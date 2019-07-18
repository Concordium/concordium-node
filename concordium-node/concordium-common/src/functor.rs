use crate::fails::FunctorError;
use failure::Error;
use std::{
    ops::Deref,
    sync::{Arc, RwLock},
};

/// Helper macro to create callbacks from raw function pointers or closures.
#[macro_export]
macro_rules! make_atomic_callback {
    ($callback:expr) => {
        concordium_common::functor::AFuncCW(Arc::new($callback))
    };
}

/// Result of the execution of a Functor
pub type FunctorResult<T> = Result<T, FunctorError>;
/// Result of the execution of a single Function
pub type FuncResult<T> = Result<T, Error>;

pub struct AFuncCW<T: Send, R: Send>(pub Arc<(Fn(&T) -> FuncResult<R> + Send + Sync + 'static)>);

impl<T: Send, R: Send> Clone for AFuncCW<T, R> {
    fn clone(&self) -> Self { AFuncCW(Arc::clone(&self.0)) }
}

impl<T: Send, R: Send> Deref for AFuncCW<T, R> {
    type Target = Arc<(Fn(&T) -> FuncResult<R> + Send + Sync + 'static)>;

    fn deref(&self) -> &Self::Target { &self.0 }
}

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
/// # use concordium_common::functor::{AFuncCW, UnitFunctor};
/// # use std::ops::{Deref, DerefMut};
/// # use std::sync::{Arc, RwLock};
/// #
/// let acc = Arc::new(RwLock::new(58));
/// let acc_1 = Arc::clone(&acc);
/// let acc_2 = Arc::clone(&acc);
///
/// let mut ph = UnitFunctor::new();
/// let adder = AFuncCW(Arc::new(
///     move |x: &i32| {
///         if let Ok(ref mut val) = acc_1.write() {
///             *val.deref_mut() += x;
///         }
///     Ok(())
///     }
/// ));
/// let multiplier = AFuncCW(Arc::new(
///     move |x: &i32| {
///         if let Ok(ref mut val) = acc_2.write() {
///             *val.deref_mut() *= x;
///         }
///     Ok(())
///     }
/// ));
///
/// ph.add_callback(adder)
///     .add_callback(multiplier);
///
/// let value = 42 as i32;
/// ph.run_callbacks(&value).unwrap(); // acc = (58 + 42) * 42
///
/// if let Ok(value) = acc.clone().read() {
///     assert_eq!(value.deref(), &4200);
/// }
#[derive(Default, Clone)]
pub struct UnitFunctor<T: Send> {
    /// Queue of functions to be executed
    callbacks: Arc<RwLock<Vec<UnitFunction<T>>>>,
}

impl<T: Send> UnitFunctor<T> {
    pub fn new() -> Self {
        Self {
            callbacks: Arc::new(RwLock::new(Vec::new())),
        }
    }

    pub fn add_callback(&mut self, callback: UnitFunction<T>) -> &mut Self {
        write_or_die!(self.callbacks).push(callback);
        self
    }

    pub fn callbacks(&self) -> &RwLock<Vec<UnitFunction<T>>> { &self.callbacks }

    pub fn run_callbacks(&self, message: &T) -> FunctorResult<()> {
        read_or_die!(self.callbacks)
            .iter()
            .fold(Ok(()), |acum, cb| {
                let res = (cb.0)(message).map_err(Error::from);

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
    use crate as concordium_common;
    use crate::functor::{FuncResult, UnitFunctor};
    use std::{
        ops::Deref,
        sync::{Arc, RwLock},
    };

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
        let shd_counter = Arc::new(RwLock::new(0));
        let shd_counter_1 = Arc::clone(&shd_counter);
        let shd_counter_2 = Arc::clone(&shd_counter);

        let mut ph = UnitFunctor::new();

        ph.add_callback(make_atomic_callback!(move |_x: &i32| {
            if let Ok(mut val) = shd_counter_1.write() {
                *val += 1;
            }
            Ok(())
        }))
        .add_callback(make_atomic_callback!(move |_: &i32| {
            if let Ok(mut val) = shd_counter_2.write() {
                *val += 1;
            }
            Ok(())
        }));

        let value = 42 as i32;
        ph.run_callbacks(&value).unwrap();

        if let Ok(ref val) = shd_counter.clone().read() {
            assert_eq!(val.deref(), &2);
        } else {
            panic!();
        }
    }
}
