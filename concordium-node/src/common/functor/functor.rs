use failure::bail;

use super::{ FunctorResult, FunctorCallback, FunctorError };

pub type FunctorCW<T> = Box<FunctorCallback<T>>;

pub struct Functor<T> {
    pub name: &'static str,
    pub callbacks: Vec<FunctorCW<T>>
}

impl<T> Functor<T> {
    pub fn new( name: &'static str) -> Self {
        Functor {
            name,
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

fn run_callbacks<T>(handlers: &[FunctorCW<T>], message: &T) -> FunctorResult {
    let errors = handlers.iter()
        .map(|handler| handler(message))
        .filter_map(|result| result.err())
        .collect::<Vec<_>>();

    if errors.is_empty() {
        Ok(())
    } else {
        bail!(FunctorError::new(errors))
    }
}

impl<T> FnOnce<(&T,)> for Functor<T> {
    type Output = FunctorResult;
    extern "rust-call" fn call_once(self, args: (&T,)) -> FunctorResult
    {
        let msg: &T = args.0;
        run_callbacks(&self.callbacks, msg)
    }
}

impl<T> FnMut<(&T,)> for Functor<T> {
    extern "rust-call" fn call_mut(&mut self, args: (&T,)) -> FunctorResult
    {
        let msg: &T = args.0;
        run_callbacks(&self.callbacks, msg)
    }
}

impl<T> Fn<(&T,)> for Functor<T> {
    extern "rust-call" fn call(&self, args: (&T,)) -> FunctorResult
    {
        let msg: &T = args.0;
        run_callbacks(&self.callbacks, msg)
    }
}
