//! Distributed Hash Table.
//!
//! The goal of this project is to provide flexible implementation of DHT
//! for different kind of Rust applications. There will be loosely coupled parts:
//!
//! 1. DHT neighborhood table implementation, will be represented by
//!    `GenericNodeTable` trait and `KNodeTable` implementation.
//! 2. Generic DHT logic implementation in `Service` and `service::Handler`
//!    structures.
//! 3. Generic bits for implementing protocols in `service::Handler` structure
//!    and `protocol` module.
//! 4. (In the future) simple implementations for testing purposes.

#![crate_name = "dht"]
#![crate_type = "lib"]

#[macro_use]
extern crate log;
extern crate rand;
extern crate rustc_serialize;

pub use base::GenericIdentifier;
pub use base::GenericNodeTable;
pub use base::Node;
pub use knodetable::KNodeTable;
pub use service::Service;

mod base;
mod knodetable;
pub mod protocol;
pub mod service;
mod utils;
