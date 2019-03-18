use failure::{Fail, Backtrace};

#[derive(Debug,Fail)]
#[fail(display = "Message processing error")]
pub struct MessageProcessError {
    message: String,
    backtrace: Backtrace
}

impl MessageProcessError {
    pub fn new(e: String) -> Self {
        MessageProcessError {
            message: e,
            backtrace: Backtrace::new()
        }
    }
}
