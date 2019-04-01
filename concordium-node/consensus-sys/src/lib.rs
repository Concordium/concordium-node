#![allow(dead_code)]
#[cfg(unix)]
extern crate curryrsunix as curryrs;
#[cfg(windows)]
extern crate curryrswin as curryrs;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;

pub mod consensus;
