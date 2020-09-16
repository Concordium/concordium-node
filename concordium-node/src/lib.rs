#![recursion_limit = "1024"]

#[macro_use]
extern crate log;

#[cfg(feature = "elastic_logging")]
#[macro_use]
extern crate elastic_derive;

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

#[macro_use]
extern crate failure;

#[cfg(all(test, not(feature = "s11n_capnp")))]
#[macro_use]
extern crate quickcheck;

#[macro_use]
extern crate concordium_common;

#[cfg(feature = "s11n_serde")]
#[macro_use]
extern crate serde_derive;

/// Client's version.
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
/// Client's name.
pub const APPNAME: &str = env!("CARGO_PKG_NAME");

pub mod common;
pub mod configuration;
#[cfg_attr(any(feature = "s11n_serde", feature = "s11n_capnp"), allow(unreachable_code, unused))]
pub mod connection;

pub mod network;
pub mod p2p;
pub mod plugins;

#[cfg(feature = "network_dump")]
pub mod dumper;
pub mod rpc;
pub mod stats_export_service;
pub mod utils;

pub mod test_utils;

#[cfg(feature = "s11n_capnp")]
pub mod p2p_capnp;

pub mod flatbuffers_shim;
