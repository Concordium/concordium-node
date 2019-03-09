#![allow(non_camel_case_types)]

extern crate sha2;
extern crate base58;
extern crate base64;
include!( concat!( env!("OUT_DIR"), "/bindings.rs"));

static ADDRESS_SCHEME: u8 = 2;

mod key_pair;

pub use key_pair::KeyPair;
