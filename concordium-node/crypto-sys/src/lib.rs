#![allow(non_camel_case_types)]

extern crate base64;
include!( concat!( env!("OUT_DIR"), "/bindings.rs"));

mod key_pair;

pub use key_pair::KeyPair;
