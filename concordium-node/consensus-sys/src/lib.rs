#![feature(box_syntax, box_patterns)]
#![allow(dead_code)]
#[cfg(unix)]
extern crate curryrsunix as curryrs;
#[cfg(windows)]
extern crate curryrswin as curryrs;
extern crate libc;
#[macro_use]
extern crate lazy_static;
extern crate byteorder;
#[macro_use]
extern crate log;

pub mod consensus;
