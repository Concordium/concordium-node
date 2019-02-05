use std::sync::{ Arc, Mutex };

// use std::sync::atomic::{ AtomicUsize, ATOMIC_USIZE_INIT, Ordering };

use common::functor::{ FunctorCallback, FunctorResult };
use errors::{ ErrorWrapper, ErrorKindWrapper };

// pub type AFunctorCW<T> = Arc< Mutex< Box< FunctorCallback<T>>>>;
pub type AFunctorCW<T> = (String, Arc< Mutex< Box< FunctorCallback<T>>>>);

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
/// use std::sync::{ Arc, Mutex };
/// use std::cell::{ RefCell };
///
/// let acc = Rc::new( RefCell::new(58));
/// let acc_1 = acc.clone();
/// let acc_2 = acc.clone();
///
/// let mut ph = AFunctor::new( "Closures");
///
/// ph.add_callback( Arc::new( Mutex::new( Box::new( move |x: &i32| {
///         *acc_1.borrow_mut() += x;
///         Ok(()) }))))
///     .add_callback( Arc::new( Mutex::new( Box::new( move |x: &i32| {
///         *acc_2.borrow_mut() *= x;
///         Ok(()) }))));
///
/// let value = 42 as i32;
/// (&ph)(&value).unwrap();     // acc = (58 + 42) * 42
/// assert_eq!( *acc.borrow(), 4200);
///
/// ```
#[derive(Clone)]
pub struct AFunctor<T> {
    name: &'static str,
    callbacks: Vec< AFunctorCW<T> >,
}

unsafe impl<T> Send for AFunctor<T> {}
unsafe impl<T> Sync for AFunctor<T> {}

// static DEEP_COUNTER: AtomicUsize = ATOMIC_USIZE_INIT;

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
    pub fn add_callback(&mut self, callback: AFunctorCW<T> ) -> &mut Self {
        // debug!( "# Functor '{}': Callback added from {}", self.name, callback.0);
        self.callbacks.push( callback);
        self
    }

    pub fn callbacks(&self) -> &Vec<AFunctorCW<T>> {
        &self.callbacks
    }

    fn run_atomic_callbacks(&self, message: &T) -> FunctorResult
    {
        let mut status = Ok(());

        // DEEP_COUNTER.fetch_add( 1, Ordering::SeqCst);
        // let indent = String::from_utf8( vec![ b'+'; DEEP_COUNTER.load(Ordering::SeqCst)])
        //        .unwrap();

        for i in 0..self.callbacks.len() {
            // let cb = & self.callbacks[i];
            let (_fn_id, cb) = self.callbacks[i].clone();

            // Lock callback and run it
            let status_step = match cb.lock() {
                Ok(locked_cb) => {
                    // debug!( "# {} Run afunctor '{}' callback({}/{}) at {}",
                    //         indent, self.name, fn_id, i+1, self.callbacks.len());
                    (locked_cb)( message)
                },
                Err(_) => {
                    Err( ErrorWrapper::from_kind(
                            ErrorKindWrapper::LockingError(
                                format!( "Atomic callback cannot be locked at {}", self.name))))
                }
            };

            // Fold error.
            match status_step {
                Ok(_) => {},
                Err(step_err) => {
                    status = match status {
                        Ok(_) => Err(step_err),
                        Err(chain_err) => {
                            Err( ErrorWrapper::with_chain( chain_err,
                                ErrorKindWrapper::FunctorRunningError( self.name)))
                        }
                    }
                }
            };
        }

        // DEEP_COUNTER.fetch_sub( 1, Ordering::SeqCst);

        status
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

    use common::functor::{ AFunctor, FunctorResult };
    use std::rc::{ Rc };
    use std::sync::{ Arc, Mutex };
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
