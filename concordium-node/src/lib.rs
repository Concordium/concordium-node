#![recursion_limit = "1024"]
#![allow(bare_trait_objects)] // until grpc updates to Rust 2018

#[macro_use]
extern crate log;
#[macro_use]
extern crate derive_builder;
#[cfg(not(target_os = "windows"))]
extern crate get_if_addrs;
#[cfg(not(target_os = "windows"))]
extern crate grpciounix as grpcio;
#[cfg(target_os = "windows")]
extern crate grpciowin as grpcio;

cfg_if! {
    if #[cfg(feature = "elastic_logging")] {
        #[macro_use]
        extern crate elastic_derive;
    }
}

#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate cfg_if;
#[cfg(target_os = "windows")]
extern crate ipconfig;

#[macro_use]
extern crate failure;

#[macro_use]
#[cfg(test)]
extern crate quickcheck;

#[macro_use]
extern crate concordium_common;

#[cfg(feature = "s11n_serde")]
#[macro_use]
extern crate serde_derive;

#[cfg(feature = "s11n_serde_cbor")]
extern crate serde_cbor;

#[cfg(feature = "s11n_capnp")]
extern crate capnp;

#[cfg(feature = "s11n_fbs")]
extern crate flatbuffers;

pub const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const APPNAME: &str = env!("CARGO_PKG_NAME");
const DEFAULT_DNS_PUBLIC_KEY: &str =
    "58C4FD93586B92A76BA89141667B1C205349C6C38CC8AB2F6613F7483EBFDAA3";
const ENV_DNS_PUBLIC_KEY: Option<&str> = option_env!("CORCORDIUM_PUBLIC_DNS_KEY");
pub fn get_dns_public_key() -> &'static str { ENV_DNS_PUBLIC_KEY.unwrap_or(DEFAULT_DNS_PUBLIC_KEY) }

#[macro_use]
pub mod fails;
#[macro_use]
pub mod common;
pub mod configuration;
pub mod connection;

pub mod client;
pub mod crypto;
#[macro_use]
pub mod network;
pub mod p2p;

pub mod dumper;
pub mod proto;
pub mod rpc;
pub mod stats_engine;
pub mod utils;

pub mod test_utils;

#[cfg(feature = "s11n_capnp")]
pub mod p2p_capnp;

#[cfg(feature = "s11n_fbs")]
pub mod flatbuffers_shim;
