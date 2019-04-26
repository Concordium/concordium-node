#[cfg(unix)]
extern crate curryrsunix as curryrs;
#[cfg(windows)]
extern crate curryrswin as curryrs;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;

#[macro_use]
mod fails;
pub mod block;
pub mod common;
pub mod consensus;
pub mod parameters;
pub mod transaction;
