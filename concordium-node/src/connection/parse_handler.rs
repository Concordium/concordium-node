use std::sync::{ Arc, Mutex };

/// Helper macro to run all callbacks using `message` expression as argument.
///
/// # TODO 
/// It should accumulate errors from different handlers into a unique composed error.
macro_rules! run_callbacks{
    ($handlers:expr, $message:expr, $errorMsg: expr) => {
        $handlers.iter()
            .map( |handler_mtx| handler_mtx.lock())
            .filter_map( |handler_guard| handler_guard.ok())
            .map( |handler| { (handler)($message) })
            .fold( Ok(()), |status, handler_result|{
                match handler_result {
                    Err(e) => Err(e),
                    Ok(_) => status
                }
            })
            .map_err( |_| $errorMsg.to_string())
    }
}

pub type ParseCallbackResult = Result<(), String>;
pub type ParseCallback<T> = Fn(&T) -> ParseCallbackResult;

/// It stores any number of functions or closures and it is able to execute them
/// because it implements `Fn`, `FnMut` and `FnOnce`.
///
/// # Examples
/// ```
/// extern crate p2p_client;
/// use p2p_client::connection::*;
/// use p2p_client::connection::parse_handler::{ ParseHandler };
/// use std::rc::{ Rc };
/// use std::sync::{ Arc, Mutex };
/// use std::cell::{ RefCell };
///
/// let acc = Rc::new( RefCell::new(58));
/// let acc_1 = acc.clone();
/// let acc_2 = acc.clone();
///
/// let ph = ParseHandler::new( "Closures".to_string())
///            .add_callback( Arc::new( Mutex::new( Box::new( move |x: &i32| {
///                *acc_1.borrow_mut() += x;
///                Ok(()) 
///            }))))
///            .add_callback( Arc::new( Mutex::new( Box::new( move |x: &i32| { 
///                *acc_2.borrow_mut() *= x;
///                Ok(()) 
///            }))));
///
/// let value = 42 as i32;
/// (&ph)(&value).unwrap();     // acc = (58 + 42) * 42 
/// assert_eq!( *acc.borrow(), 4200); 
///
/// ```
#[derive(Clone)]
pub struct ParseHandler<T> {
    pub error_msg: String,
    pub callbacks: Vec< Arc < Mutex < Box< Fn(&T) -> ParseCallbackResult > > > >
}

unsafe impl<T> Send for ParseHandler<T> {}
unsafe impl<T> Sync for ParseHandler<T> {}

impl<T> ParseHandler<T> {

    pub fn new( error_msg: String ) -> Self {
        ParseHandler {
            error_msg: error_msg.clone(),
            // 3. callbacks: Arc::new( Vec::new())
            callbacks: Vec::new()
        }
    }

    /// It adds new callback into this functor.
    /// 
    /// Callbacks are executed in the same order they were introduced.
    pub fn add_callback(mut self, callback: Arc< Mutex < Box< ParseCallback<T> > > > ) -> Self 
    {
        self.callbacks.push( callback );
        self
    }
}

impl<T> FnOnce<(&T,)> for ParseHandler<T> {
    type Output = ParseCallbackResult;
    extern "rust-call" fn call_once(self, args: (&T,)) -> ParseCallbackResult
    {
        let msg: &T = args.0;
        run_callbacks!( &self.callbacks, msg, &self.error_msg)
    }
}

impl<T> FnMut<(&T,)> for ParseHandler<T> {
    extern "rust-call" fn call_mut(&mut self, args: (&T,)) -> ParseCallbackResult
    {
        let msg: &T = args.0;
        run_callbacks!( &self.callbacks, msg, &self.error_msg)
    }
}

impl<T> Fn<(&T,)> for ParseHandler<T> {
    extern "rust-call" fn call(&self, args: (&T,)) -> ParseCallbackResult
    {
        let msg: &T = args.0;
        run_callbacks!( &self.callbacks, msg, &self.error_msg)
    }
}

#[cfg(test)]
mod parse_handler_unit_test {
    use connection::parse_handler::{ ParseHandler, ParseCallbackResult };
    use std::rc::{ Rc };
    use std::sync::{ Arc, Mutex };
    use std::cell::{ RefCell };

    fn raw_func_1( _v: &i32) -> ParseCallbackResult { Ok(()) }
    fn raw_func_2( _v: &i32) -> ParseCallbackResult { Ok(()) }

    /// It tests if raw functions can be added as callback.
    #[test]
    pub fn test_parse_handler_raw_functions() {
        let ph = ParseHandler::new( "Raw functions".to_string())
            .add_callback( make_callback!( raw_func_1 ))
            .add_callback( make_callback!( raw_func_2 ))
            .add_callback( make_callback!( raw_func_1 ));

        let value = 42 as i32;
        (&ph)(&value).unwrap();
    }

    /// It tests if closures can be added as callback.
    #[test]
    pub fn test_parse_handler_closure() {
        let ph = ParseHandler::new( "Closures".to_string())
            .add_callback( make_callback!( |_x: &i32| { Ok(()) }))
            .add_callback( make_callback!( |_x: &i32| { Ok(()) }));

        let value = 42 as i32;
        (&ph)(&value).unwrap();
    }

    /// It tests if we can mix closures and functions.
    #[test]
    pub fn test_parse_handler_mix() {
        let ph = ParseHandler::new( "Raw function and  Closure".to_string())
            .add_callback( make_callback!( raw_func_1 ))
            .add_callback( make_callback!( raw_func_2 ))
            .add_callback( make_callback!( |_x: &i32| { Ok(()) }))
            .add_callback( make_callback!( |_x: &i32| { Ok(()) }));

        let value = 42 as i32;
        (&ph)(&value).unwrap();
    }

    /// Test for complex closures, I mean, closures that copy/move some variables from scope.
    #[test]
    pub fn test_parse_hadler_complex_closure() {
        let shd_counter = Rc::new( RefCell::new(0));
        let shd_counter_1 = shd_counter.clone();
        let shd_counter_2 = shd_counter.clone();

        let ph = ParseHandler::new( "Complex Closure".to_string())
            .add_callback( make_callback!( move |_x: &i32| { 
                *shd_counter_1.borrow_mut() += 1; 
                Ok(()) }))
            .add_callback( make_callback!( move |_: &i32| {
                *shd_counter_2.borrow_mut() += 1;
                Ok(()) }));


        let value = 42 as i32;
        (ph)(&value).unwrap();

        assert_eq!( *shd_counter.borrow(), 2);
    }
}


