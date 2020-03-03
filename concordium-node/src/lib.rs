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

pub const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const APPNAME: &str = env!("CARGO_PKG_NAME");
const DEFAULT_DNS_PUBLIC_KEY: &str =
    "58C4FD93586B92A76BA89141667B1C205349C6C38CC8AB2F6613F7483EBFDAA3";
const ENV_DNS_PUBLIC_KEY: Option<&str> = option_env!("CORCORDIUM_PUBLIC_DNS_KEY");
pub fn get_dns_public_key() -> &'static str { ENV_DNS_PUBLIC_KEY.unwrap_or(DEFAULT_DNS_PUBLIC_KEY) }

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

pub mod test_utils;

#[cfg(feature = "s11n_capnp")]
pub mod p2p_capnp;

#[cfg(feature = "s11n_fbs")]
pub mod flatbuffers_shim;
