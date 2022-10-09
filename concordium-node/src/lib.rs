#![recursion_limit = "1024"]

#[macro_use]
extern crate log;
#[macro_use]
extern crate lazy_static;

cfg_if! {
    if #[cfg(feature = "instrumentation")] {
        #[macro_use]
        extern crate prometheus;
        #[macro_use]
        extern crate gotham_derive;
    }
}

#[macro_use]
extern crate cfg_if;

#[cfg(test)]
#[macro_use]
extern crate quickcheck;

/// Client's version.
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
/// Client's name.
pub const APPNAME: &str = env!("CARGO_PKG_NAME");

pub mod common;
pub mod configuration;

pub mod connection;

pub mod network;
pub mod p2p;
pub mod plugins;

#[cfg(feature = "network_dump")]
pub mod dumper;
pub mod rpc;
pub mod stats_export_service;
pub mod utils;

pub mod consensus_ffi;

#[cfg(any(test, bench, feature = "test_utils"))]
pub mod test_utils;

pub mod flatbuffers_shim;

#[cfg(target_os = "macos")]
mod macos_log;

pub mod grpc2;
mod health;
