use failure::{ Error, Fail };

#[derive(Debug, Fail)]
#[fail(display = "Error running functor: {:?}", errors)]
pub struct FunctorError {
    errors: Vec<Error>
}

impl FunctorError {
    pub fn new(e: Vec<Error>) ->  FunctorError {
        FunctorError {
            errors: e
        }
    }
}
