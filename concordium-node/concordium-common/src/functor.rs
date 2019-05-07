use crate::fails::FunctorError;
use failure::Error;
use std::sync::{Arc, RwLock};

/// Helper macro to create callbacks from raw function pointers or closures.
#[macro_export]
macro_rules! make_atomic_callback {
    ($callback:expr) => {
        Arc::new(RwLock::new(Box::new($callback)))
    };
}

/// Result of the execution of a Functor
pub type FunctorResult<T> = Result<T, FunctorError>;
/// Result of the execution of a single Function
pub type FuncResult<T> = Result<T, Error>;
type FuncItem<T, R> = (Fn(&T) -> FuncResult<R>);
type FuncCW<T, R> = Box<FuncItem<T, R>>;
type AFuncCW<T, R> = Arc<RwLock<FuncCW<T, R>>>;

/// Trait to be implemented by the types that can execute a stack of functions
/// over a given value
pub trait Functorable<T> {
    /// Return type for the `Ok` variant
    type OkValue;
    fn new(_: &'static str) -> Self;
    fn name(&self) -> &'static str;
    /// Adds a new callback into this functor
    ///
    /// Callbacks are executed in the same order they were introduced.
    fn add_callback(&mut self, _: AFuncCW<T, Self::OkValue>) -> &mut Self;
    /// Combinates two `Ok` values
    fn ok_ok(_: Self::OkValue, _: Self::OkValue) -> FunctorResult<Self::OkValue>;
    /// Combinates an accumulated `Err` with a returned `Ok`
    fn err_ok(_: FunctorError, _: Self::OkValue) -> FunctorResult<Self::OkValue>;
    /// Combinates an accumulated `Ok` with a returned `Err`
    fn ok_err(_: Self::OkValue, _: Error) -> FunctorResult<Self::OkValue>;
    /// Combinates an accumulated `Err` with a returned `Err`
    fn err_err(_: FunctorError, _: Error) -> FunctorResult<Self::OkValue>;
    /// Get slice of the underlying callbacks
    fn callbacks(&self) -> &[AFuncCW<T, Self::OkValue>];

    fn combinator(
        acum: FunctorResult<Self::OkValue>,
        res: FuncResult<Self::OkValue>,
    ) -> FunctorResult<Self::OkValue> {
        match acum {
            Ok(acum) => match res {
                Ok(res) => Self::ok_ok(acum, res),
                Err(e) => Self::ok_err(acum, e),
            },
            Err(e) => match res {
                Ok(res) => Self::err_ok(e, res),
                Err(ee) => Self::err_err(e, ee),
            },
        }
    }
    /// Executes the stack of callbacks using `message` as its argument.
    fn run_callbacks(&self, message: &T) -> FunctorResult<Self::OkValue>;
}

#[macro_export]
/// Creates a handler for executing a stack of functions over a given value
///
/// After populated with functions `Fn(&T) -> FuncResult<R>`, a given
/// value of type `T` can be processed through a call to `run_callbacks`.
macro_rules! create_functor_type {
    (
        $F:ident,
        $r:ty,
        $init:expr,
        $ret_if:expr,
        $oo:expr,
        $eo:expr,
        $oe:expr,
        $ee:expr, #[$doc:meta]
    ) => {
        #[$doc]
        pub struct $F<T> {
            /// Name of the Functor
            name: &'static str,
            /// Queue of functions to be executed
            callbacks: Vec<AFuncCW<T, $r>>,
            /// Initial value when running the execution
            initial: $r,
            /// Function to determine if the functor should keep running
            /// in case an early return is wanted
            return_if: Option<Box<Fn(&$r) -> bool>>,
            /// `fold`-like function for combinating the acumulated result and
            /// the actual returned value
            combination: Box<Fn(FunctorResult<$r>, FuncResult<$r>) -> FunctorResult<$r>>,
        }

        impl<T> $F<T> {
            fn inner_new(
                name: &'static str,
                return_if: Option<Box<Fn(&$r) -> bool>>,
                initial: $r,
                combination: Box<Fn(FunctorResult<$r>, FuncResult<$r>) -> FunctorResult<$r>>,
            ) -> Self {
                Self {
                    name,
                    callbacks: Vec::new(),
                    initial,
                    return_if,
                    combination,
                }
            }
        }

        impl<T: 'static> Functorable<T> for $F<T> {
            type OkValue = $r;

            fn new(name: &'static str) -> Self {
                $F::inner_new(name, $ret_if, $init, Box::new(Self::combinator))
            }

            fn add_callback(&mut self, callback: AFuncCW<T, Self::OkValue>) -> &mut Self {
                self.callbacks.push(callback);
                self
            }

            fn callbacks(&self) -> &[AFuncCW<T, Self::OkValue>] { &self.callbacks }

            fn run_callbacks(&self, message: &T) -> FunctorResult<Self::OkValue> {
                let mut acum = Ok(self.initial.clone());
                for cb in self.callbacks.iter() {
                    let res = match safe_read!(cb) {
                        Ok(locked_cb) => (*locked_cb)(message).map_err(Error::from),
                        Err(p) => Err(Error::from(p)),
                    };

                    acum = (self.combination)(acum, res);

                    if let Some(return_if) = &self.return_if {
                        if let Ok(ref val) = acum {
                            if return_if(val) {
                                break;
                            };
                        }
                    }
                }
                acum
            }

            fn name(&self) -> &'static str { self.name }

            fn ok_ok(a: Self::OkValue, b: Self::OkValue) -> FunctorResult<Self::OkValue> {
                $oo(a, b)
            }

            fn err_ok(a: FunctorError, b: Self::OkValue) -> FunctorResult<Self::OkValue> {
                $eo(a, b)
            }

            fn ok_err(a: Self::OkValue, b: Error) -> FunctorResult<Self::OkValue> { $oe(a, b) }

            fn err_err(a: FunctorError, b: Error) -> FunctorResult<Self::OkValue> { $ee(a, b) }
        }
    };
}

create_functor_type!(
    UnitFunctor,
    (),
    (),
    None,
    |_, _| Ok(()),
    |a, _| Err(a),
    |_, b| Err(FunctorError::create(b)),
    |mut a: FunctorError, b| {
        a.errors.push(b);
        Err(a)
    },
    #[doc="Functor to be used when side-effects are intended and the actual returning value is disposable

The returning type is `()` as it is only important wether it is `Ok(_)` or `Err(_)`. In the case of an
error, the actual chain of functor errors is returned.

An empty functor returns always `Ok(())`.

# Examples
```
# extern crate concordium_common;
# use concordium_common::functor::{Functorable, UnitFunctor};
# use std::{
#    cell::RefCell,
#    rc::Rc,
#    sync::{Arc, RwLock},
# };
#
let acc = Rc::new(RefCell::new(58));
let acc_1 = Rc::clone(&acc);
let acc_2 = Rc::clone(&acc);

let mut ph = UnitFunctor::new(\"Closures\");
ph.add_callback(Arc::new(RwLock::new(Box::new(move |x: &i32| {
    *acc_1.borrow_mut() += x;
    Ok(())
}))))
    .add_callback(Arc::new(RwLock::new(Box::new(move |x: &i32| {
        *acc_2.borrow_mut() *= x;
        Ok(())
    }))));

let value = 42 as i32;
ph.run_callbacks(&value).unwrap(); // acc = (58 + 42) * 42
assert_eq!(*acc.borrow(), 4200);
```"]
);
/// Function type returning the unit value `()`
pub type UnitFunction<T> = AFuncCW<T, ()>;

create_functor_type!(
    FilterFunctor,
    bool,
    true,
    Some(Box::new(|x| !x)),
    |a, b| Ok(a && b),
    |_, _| Ok(false),
    |_, _| Ok(false),
    |_, _| Ok(false),
    #[doc="Filter functor with early return

It executes the callbacks in order and in case a `false` is returned,
stops the execution. In case an error is returned by any of the inner
functions, a `Ok(false)` is returned.

An empty functor returns always `true`.

# Example
```
# extern crate concordium_common;
# use concordium_common::functor::{FilterFunctor, Functorable};
# use std::{
#     cell::RefCell,
#     rc::Rc,
#     sync::{Arc, RwLock},
# };
#
let true_shd_counter = Rc::new(RefCell::new(0));
let true_shd_counter_1 = Rc::clone(&true_shd_counter);
let true_shd_counter_2 = Rc::clone(&true_shd_counter);
let true_shd_counter_3 = Rc::clone(&true_shd_counter);

let false_shd_counter = Rc::new(RefCell::new(0));
let false_shd_counter_1 = Rc::clone(&false_shd_counter);
let mut ph = FilterFunctor::new(\"Complex Closure\");
let value = 42 as i32;

ph.add_callback(Arc::new(RwLock::new(Box::new(move |v: &i32| {
    *true_shd_counter_1.borrow_mut() += 1;
    Ok(*v > 41)
}))))
    .add_callback(Arc::new(RwLock::new(Box::new(move |v: &i32| {
        *true_shd_counter_2.borrow_mut() += 1;
        Ok(*v > 41)
    }))))
    .add_callback(Arc::new(RwLock::new(Box::new(move |v: &i32| {
        *false_shd_counter_1.borrow_mut() += 1;
        Ok(*v < 41)
    }))))
    .add_callback(Arc::new(RwLock::new(Box::new(move |v: &i32| {
        *true_shd_counter_3.borrow_mut() += 1;
        Ok(*v > 41)
    }))));

assert!(!ph.run_filters(&value));
assert_eq!(*true_shd_counter.borrow(), 2);
assert_eq!(*false_shd_counter.borrow(), 1);
```"]
);
/// Function type returning a boolean value
pub type FilterFunction<T> = AFuncCW<T, bool>;

impl<T: 'static> FilterFunctor<T> {
    /// Resolves the inner `Result` to yield just a `bool` value
    pub fn run_filters(&self, m: &T) -> bool {
        if let Ok(r) = self.run_callbacks(m) {
            r
        } else {
            false
        }
    }
}

#[cfg(test)]
mod unit_functor_unit_test {
    use crate::functor::{FuncResult, Functorable, UnitFunctor};
    use std::{
        cell::RefCell,
        rc::Rc,
        sync::{Arc, RwLock},
    };

    fn raw_func_1(_v: &i32) -> FuncResult<()> { Ok(()) }
    fn raw_func_2(_v: &i32) -> FuncResult<()> { Ok(()) }

    /// It tests if raw functions can be added as callback.
    #[test]
    pub fn test_parse_handler_raw_functions() {
        let mut ph = UnitFunctor::new("Raw functions");
        ph.add_callback(make_atomic_callback!(raw_func_1))
            .add_callback(make_atomic_callback!(raw_func_2))
            .add_callback(make_atomic_callback!(raw_func_1));

        let value = 42 as i32;
        ph.run_callbacks(&value).unwrap();
    }

    /// It tests if closures can be added as callback.
    #[test]
    pub fn test_parse_handler_closure() {
        let mut ph = UnitFunctor::new("Closures");

        ph.add_callback(make_atomic_callback!(|_x: &i32| { Ok(()) }))
            .add_callback(make_atomic_callback!(|_x: &i32| { Ok(()) }));

        let value = 42 as i32;
        ph.run_callbacks(&value).unwrap();
    }

    /// It tests if we can mix closures and functions.
    #[test]
    pub fn test_parse_handler_mix() {
        let mut ph = UnitFunctor::new("Raw function and  Closure");

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

        let mut ph = UnitFunctor::new("Complex Closure");

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

#[cfg(test)]
mod filter_functor_unit_test {
    use crate::functor::{FilterFunctor, FuncResult, Functorable};
    use std::{
        cell::RefCell,
        rc::Rc,
        sync::{Arc, RwLock},
    };

    fn true_func(v: &i32) -> FuncResult<bool> { Ok(*v > 41) }
    fn false_func(v: &i32) -> FuncResult<bool> { Ok(*v < 41) }

    /// It tests if raw functions can be added as callback.
    #[test]
    pub fn test_parse_handler_raw_functions() {
        let mut ph = FilterFunctor::new("Raw functions");
        let value = 42 as i32;
        ph.add_callback(make_atomic_callback!(true_func))
            .add_callback(make_atomic_callback!(true_func))
            .add_callback(make_atomic_callback!(true_func));

        assert!(ph.run_callbacks(&value).unwrap());

        ph.add_callback(make_atomic_callback!(false_func));
        assert!(!ph.run_callbacks(&value).unwrap());
    }

    /// It tests if closures can be added as callback.
    #[test]
    pub fn test_parse_handler_closure() {
        let mut ph = FilterFunctor::new("Closures");
        let value = 42 as i32;
        ph.add_callback(make_atomic_callback!(|v: &i32| { Ok(*v > 41) }));
        assert!(ph.run_callbacks(&value).unwrap());
        ph.add_callback(make_atomic_callback!(|v: &i32| { Ok(*v < 41) }));
        assert!(!ph.run_callbacks(&value).unwrap());
    }

    /// It tests if we can mix closures and functions.
    #[test]
    pub fn test_parse_handler_mix() {
        let mut ph = FilterFunctor::new("Raw function and  Closure");
        let value = 42 as i32;
        ph.add_callback(make_atomic_callback!(true_func))
            .add_callback(make_atomic_callback!(|v: &i32| { Ok(*v > 41) }));
        assert!(ph.run_callbacks(&value).unwrap());
        ph.add_callback(make_atomic_callback!(false_func))
            .add_callback(make_atomic_callback!(|v: &i32| { Ok(*v > 41) }));
        assert!(!ph.run_callbacks(&value).unwrap());
    }

    /// Test for complex closures, I mean, closures that copy/move some
    /// variables from scope.
    #[test]
    pub fn test_parse_hadler_complex_closure() {
        let true_shd_counter = Rc::new(RefCell::new(0));
        let true_shd_counter_1 = Rc::clone(&true_shd_counter);
        let true_shd_counter_2 = Rc::clone(&true_shd_counter);
        let true_shd_counter_3 = Rc::clone(&true_shd_counter);

        let false_shd_counter = Rc::new(RefCell::new(0));
        let false_shd_counter_1 = Rc::clone(&false_shd_counter);

        let mut ph = FilterFunctor::new("Complex Closure");
        let value = 42 as i32;

        ph.add_callback(make_atomic_callback!(move |v: &i32| {
            *true_shd_counter_1.borrow_mut() += 1;
            Ok(*v > 41)
        }))
        .add_callback(make_atomic_callback!(move |v: &i32| {
            *true_shd_counter_2.borrow_mut() += 1;
            Ok(*v > 41)
        }))
        .add_callback(make_atomic_callback!(move |v: &i32| {
            *false_shd_counter_1.borrow_mut() += 1;
            Ok(*v < 41)
        }))
        .add_callback(make_atomic_callback!(move |v: &i32| {
            *true_shd_counter_3.borrow_mut() += 1;
            Ok(*v > 41)
        }));

        assert!(!ph.run_callbacks(&value).unwrap());
        assert_eq!(*true_shd_counter.borrow(), 2);
        assert_eq!(*false_shd_counter.borrow(), 1);
    }
}
